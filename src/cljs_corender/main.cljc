(ns cljs-corender.main
  #?(:clj (:require [cljs.repl]
             [clojure.walk :as w]
             [cljs.repl.node]
             [clojure.edn]
             [clojure.set]
             [clojure.java.io :as io]
             [clojure.pprint]
             [cljs.analyzer :as ana]
             [cljs.env]
             [clojure.java.shell :as shell]
             [cljs.build.api :as bapi]
             [clojure.tools.cli :refer [parse-opts]]
             [cljs.analyzer.api :as ana-api]))
  #?(:clj (:import (java.io File LineNumberReader InputStreamReader PushbackReader)
            (clojure.lang RT Reflector))))


#?(:clj
   (do

(defn- source-fn
  [sb]
  (cljs.repl/source-fn @cljs.env/*compiler* sb))

(defn- remove-env [data]
  (clojure.walk/prewalk (fn [node] (if (map? node)
                                     (apply dissoc node [:env])
                                     node))
                        data))

(defn- format-init-invocation [init-val caller-ns]
  (when-let [rel-var (-> init-val :info :name)]
    (let [caller-ns-reqs    (:requires
                             (get (:cljs.analyzer/namespaces @cljs.env/*compiler*)
                                  caller-ns))
          rel-ns            (symbol (namespace rel-var))
          remapped-ns       (get caller-ns-reqs rel-ns rel-ns)
          abs-ns            (if-not (empty? (str remapped-ns)) remapped-ns caller-ns)
          absolute-path-var (symbol (str abs-ns "/" (name rel-var)))]
      (with-meta absolute-path-var
        (assoc  (-> init-val :info)
                :ns abs-ns)))))

(def ^:private total-checked-invokes (atom #{}))
(def ^:private total-unchecked-invokes (atom #{}))

(defn- absolute-ns-invoke [invoke]
  (let [invoke-meta (meta invoke)]
    (with-meta (symbol (str (:ns invoke-meta) "/" (name invoke)))
      invoke-meta)))

(defn- failable [f]
  (try
    (f)
   (catch Exception  e
     (println (str "corender err" e))
     nil)))

(defn- form-invokes [invoke str-form]
  (let [form    (failable #(remove-env (ana-api/analyze
                                        (assoc (ana-api/empty-env)
                                               :ns
                                               (:ns (meta invoke)))
                                        (binding [*ns* (ana-api/find-ns (:ns (meta invoke)))]
                                          (read-string str-form)))))
        invokes (atom #{})
        _!      (failable #(clojure.walk/postwalk
                            (fn [node] (if (and (= (type node)
                                                  clojure.lang.MapEntry)
                                               (= (first node) :fn))
                                        (do
                                          (when-let [full-invoke
                                                     (format-init-invocation
                                                      (second node)
                                                      (:ns (meta invoke)))]
                                            (when-not (@invokes full-invoke)
                                              (swap! invokes conj full-invoke)))
                                          node)
                                        node))
                            (:init form)))]
    (swap! total-checked-invokes (fn [i]  (disj (set (conj i invoke)) nil)))
    (swap! total-unchecked-invokes (fn [i] (->> i
                                               (concat @invokes)
                                               (remove nil?)
                                               (remove #(@total-checked-invokes %)))))
    (doseq [unchecked-invoke @total-unchecked-invokes]
      (if (#{'cljs.core} (:ns (meta unchecked-invoke)))
        (do
          (swap! total-checked-invokes (fn [i]  (disj (set (conj i unchecked-invoke)) nil)))
          (swap! total-unchecked-invokes (fn [i]
                                           (->> i
                                                (concat @invokes)
                                                (remove nil?)
                                                (remove #(@total-checked-invokes %))))))

        unchecked-invoke))
    (when-let [unchecked-invoke (first @total-unchecked-invokes)]
      (recur unchecked-invoke (str (source-fn unchecked-invoke))))))

(def ^:private escaper {"\"" "\\\""})

(defn- deeper-invokes [form]
  (reset! total-checked-invokes #{})
  (reset! total-unchecked-invokes #{})
  ;;NOTE though sensible :ns should be supplied in meta
  (form-invokes (with-meta 'corender-init
                  {:ns (ns-name *ns*)})
                ;;TODO swap rand-int to counter/munged identifier
                (clojure.string/escape (str "(defn corender-"
                                            (gensym)
                                            " [] " form ")")
                                       escaper))
  (disj @total-checked-invokes 'corender-init))

;;NOTE use ' to prevent ns collision
(defmacro cancelable [form]
  (let [res (deeper-invokes form)]
    (list 'let (vec (mapcat (fn [i] [(symbol (str (name i) "corender-f")) i]) res))
              (list 'with-redefs
                    (vec
                     (mapcat (fn [i]
                               [i
                                (list 'fn (vector '& 'args)
                                      (list 'println (list '.-corenderLock 'js/window))
                                      (list 'when-not (list '.-corenderLock 'js/window)
                                            (list 'apply
                                                  (symbol (str (name i) "corender-f"))
                                                  'args)))])
                                 res))
                    form))))

(defmacro ^:dev analyze-from-repl [str-form]
  (ana-api/analyze
    (ana-api/empty-env)
   (clojure.edn/read-string str-form)))))
