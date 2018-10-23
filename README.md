Wasmscript
=========

Better name pending.

When working with the web, one usually uses JavaScript to write interactive programs with JavaScript. But JavaScript is in many ways a bad language - it has many ways one can easily make a mistake and it has many subtleties. This is not to say that one cannot write good JavaScript, but the amount of frameworks suggests that this is not an easy task.

Wasmscript is my attempt to write a better language for working on the web. The end result should satisfy the folowing goals:

* Difficult to write bad programs - This is not the same as easy to write good programs. If you make a mistake the compiler should stop you.

* Easy to get started - I do not want to install npm, bower, webpack, typescript and yarn to make an interactive slideshow. Ideally, one should be able to install one executable, call that on the correct file and have an output ready to go.

* High-level - When writing a simple website I don't need to thinking about idividual bits. Although performance is a concern on the web, it doesn't need to blisteringly fast. 

* Expressive - I want users to be able extract out patterns in their code easily

* Concurrency - Writing websites means that we may want to do things at the same time. It'd be nice to do this without an event loop of callback hell for the user.

* Reasonably fast - I don't want to wait 30 seconds for a pop-up to load

* Interactive developent - I want to be able to change the code and see it straight away on my demo site.

* Simple - If I have to remember a lot of new syntax I'm going to forget it and move on.

* Designed for the Web - Putting text on a webpage should be simple - not something I have to jump through hoops for.

I'm aware that this may be quite a bit goal, but this is a hobby project - if I get 10% of this achieved I'll be overjoyed.

Why not X?
----------

I've tried some other languages for the web, and none of them achieve all of these goals.

* JavaScript - Modern JavaScript is hard to get started with, and it is really easy to write bad code

* TypeScript - Better than JavaScript, but has a real curve to getting started. No interactive development.

* Clojurescript - Clojure and Clojurescript is what inspired this, but it is not simple, and can be a real pain to get started. The differences between Clojures and Clojurescript can be confusing

* Haskell with GHCJS - Haskell is my prefered language, but working with GHCJS can be painful. Reflex-platform makes it possible, but it is far more than a simple download. It is also quite slow

* CommonLisp with Parenscript - This may be sutible, but it feels like an add on to common lisp, not a core part of the language. Can be hard to get started

* Elm - Elm is great language if you want to work in the confines of what the developers want. It fails at the expressive test.

* Rust/C++ with Webassembly - On the desktop I like both of these languages, but on the web they are too low level and have no real story about how to put text on a page.

So what are you going to do about it?
------------------------------------

The plan is for Wasmscript to be a typed lisp like language that compiles to webassembly, but has a repl built in haskell that can run iteratively with reflex. I'd like to use an affine type system instead of a garbage collector as well, as that should make my life easier. 

Below is some ideal syntax for a simple counter.

```
(defun draw-button
  [amount-clicks :i32]
  :html-node
  "Create the html-node for the page"
  (button (concat "You have pressed the button" (show amount-clicks) " times!"))
  )

;;(type-of 'updating-node) 
;; (a -> :html-node) -> (:html-node -> a -> a) -> a -> :vary :html-node

(defun updating-button
  [start-clicks : i32]
  "Create a varying button"
  (update-node 'draw-button #((block-until (click %1) %2)))
  )

(defun main
  (append-to-page
    (updating-button 0)))
```

Here we define the dom using functions, and add reactivity with a fold function. Functions can implicitly block (like block-until), but we will have a concurrent function for sequencing many actions.

We have two higher kinded data types (:vary and :signal) that are like event and behaviour from reflex. Block until will block until its signal emits.

So far
------

So far there is a simple repl and compile which works as long as you only use f64s. You can define functions and use recursion and cond to do simple maths. More is coming!

Quickstart
-----------

You need to install node, stack and wat2wasm. Run stack build to build the main application. The main application has a few ways of running. Repl will let you run interactive programs. Eval will evalulate a file in a slow way, using a naive haskell backend. This can be considered a reference implementation, but it is slow. Compile will take a wasmScript file and export it as a wat file, which can be compiles to wasm.

In the examples directory is a program to compute the nth fiboncaci number. To run it as a webassembly file, do the following.

```bash
stack build
stack exec wasmScript -- compile -w example/example.wscript
node run.js
```
