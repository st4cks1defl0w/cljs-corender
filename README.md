# cljs-corender

Clojure(Script) tool for in-depth control of js execution flow aiming to efficiently
(co)use JS thread, inspired by RN InteractionManager.

## Fine control of execution with analyzer and macros

In JS event loop is stuck at the borders of the function definition, there is no way to
cancel a long-running, CPU-hogging event from another function. However, in cljs we can traverse
AST tree and set up an interrupting/cancelling condition for each function in the call tree with macros, therefore creating an ability to control execution at each 
"stack frame"; in other words, we are able to interfere with JS execution flow at any
stage in cljs context.

For example, in RN context we can setup cancellations if the event has low priority and
is slowing down animations.
We can even save state (WIP) of the event for later rescheduling.

## Usage

``` clojure
(defn my-cancelable-fn []
  (cancelable
  (let [j {:a :b} k 3] ())))

```
