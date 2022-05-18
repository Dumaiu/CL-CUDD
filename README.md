
* FIXME: The node initialization functions are inherently not thread-safe, because they're doing "something with a pointer" before entering a critical section and calling (cudd-ref).  As a consequence, they should be reclassified as internals.
* TODO: Print both a node's index, which is constant and isomorphic to a BDD variable, and its place in the variable ordering.

* NOTE [2022-05-17 Tue]: Inspecting `cudd:*managers*` in Emacs prevents these from being garbage-collected.  Use `(hash-table-count)` instead.

* DONE: Make 2-2-zdd-bdd-bridge.lisp thread-safe
    * DONE 2-1-zdd-set-operations.lisp 
    * TODO: Could that be causing a race condition during (test-system)?
* TODO: It's a problem to have `reordering-method` defined after 2-0-0-manager.lisp loads.

* DONE: Unit-test with autosifting enabled.
    * NOTE: Having reordering turned on is 99% of the time responsible for causing the bug.
    * FIXME: Should we be getting multiple finalizers for nodes w/ the same index?  Or is that a mistake?
* DONE: Rename 'index' in `bdd-variable-node` to `variable-id`
* TODO Q: Why is `(cudd-regular)` defined entirely Lisp, instead of calling C|-`Cudd_Regular()` through CFFI?
* TODO: If this all **still** doesn't work, use (without-gcing) inside the critsec.
* DONE Override (gc) in :cudd.
* DONE Try calling `Cudd_Ref()` for *every* BDD node.
    * This is the 'WIP/cudd-ref-everything' branch. 
* TODO Save a printable description of a node in its finalizer closure for debugging.

* DONE: Even without autoreordering, there's an assertion which always fails in some node finalizer when calling (cudd-unit-test-repeatedly).
    * The problem was with some of the kargs to (print-node-pointer-to-string).  For `bdd-variable-node` and `bdd-constant-node`, they weren't all getting there.
    * DONE: There's a TYPE-ERROR in another node finalizer.

* DONE: All the Log4CL strings must be precomputed, to prevent race conditions.
    * TODO: This will have performance consequences.  See if we can skip it.
* DONE Should we be increasing the ref count for constants? <a id="ref-count-constants"><a/>
   - The example in the CUDD manual at 'Basic BDD Manipulation' makes it look as though, yes, these should be reference-counted.
- DONE [2022-04-11 Mon]: If so, change the (assert) checks in 2-4-hook.lisp.

* DONE `(node-function)` operations should compare their operands' managers with `(eq)`.
    * TODO: Unit-test.

* DONE: Replace `%mp%` appearances with '(manager node)' calls.
    * Except for the files named previously.

* [2022-02-02 Wed] TODO: Wrappers for:
    * `cudd-read-keys`
    * `cudd-read-dead`
    * `cudd-dead-are-counted`
    * `cudd-turn-on-count-dead`
* [2022-01-12 Wed] TODO: Convert a BDD to a `:cl-graph` graph.
* TODO: [optimize] (cudd:support-index)
* TODO: Make (node-add|or|xor) variadic?
* TODO: Some of my additions have probably not been to the correct file.  I tend to put everything in the 'util' one.
* TODO: (node-xor) needs ADD support.
* TODO: On SBCL, the file '2-1-add.lisp' sometimes needs to be double-complied.  Moving the def of `*add-apply-doc*` to another form might be enough.
    * [2022-01-12 Wed] TODO Is this fixed yet?
* TODO: Config constant for disabling logging
* Working on the 'unimplemented.lisp' C wrappers.
* TODO: `cl-cudd.build.asd` should be more customizable.
    * TODO: support CUDD ≠ v3.0.0
* NB: In order to use a precompiled CUDD library, I've removed :cl-cudd.build from the dependency list for :cl-cudd
* Similarly--to accommodate existing CUDD source and build directories, the groveller (`src/1-1-1-grovel.lisp`) now looks for directories 'cudd/' and 'build-cudd/' within the 'cl-cudd3/** dir.  These can be symlinks.

* [2022-03-02 Wed] A procedure for testing finalizers: 
```lisp 
(make :cl-cudd)
(setf cudd:config/debug-consistency-checks t)
(log:config cudd:cudd-logger :trace)
(test-system :cl-cudd)

(trivial-garbage:gc :full t :verbose t)
```


* [2022-02-22 Tue] `cl-cudd:config/guard-pointer-access`: When T, the `(manager-pointer)` function raises an exception if the `manager` being queried has a null CUDD pointer.  When NIL, this check is skipped.

* [2021-12-08 Wed] 
    * Initialization of `*manager*` is now guarded.
        * TODO: A command to easily force reinitialization.

* [2021-12-07 Tue]
    * `config/signal-memory-errors`: one of `(:error :log NIL)`.  Default: `:error`.
        * `:error | :log`: an error message is logged every time a node finalizer has to deal with a `sb-sys:memory-fault-error`.
        * `:error`: In addition to the `log4cl` message evoked with `:log`, re-throws the exception.  Note that SBCL traps and converts it to a warning.
        * `NIL`:  Be careful disabling this, as the `sb-sys:memory-fault-error` will be silently squelched!
        * FIXME: Avoid the SBCL dependency `sb-sys:memory-fault-error` present in `2-0-1-node.lisp`.
    * `config/debug-consistency-checks`: See docstring.
        * NOTE: There is also a correlation to a large number of `memory-fault-error`s being thrown; see `config/signal-memory-errors`.

* [2021-11-30 Tue] Removed the  `update-asdf` instructions from 'cl-cudd.asd':

```lisp
;;;; Autogenerated ASD file for system "CL-CUDD"
;;;; In order to regenerate it, run update-asdf
;;;; from shell (see https://github.com/phoe-krk/asd-generator)
;;;; For those who do not have update-asdf,
;;;; run `ros install asd-generator` (if you have roswell installed)
;;;; There are also an interface available from lisp:
;;;; (asd-generator:regen &key im-sure)
```

  * TODO: How do you configure `asd-generator`?


* [2021-11-18 Thu] Reenabled the code in '2-0-1-node.lisp' responsible for invoking `cudd-recursive-deref` when a node gets GC'd by Lisp.  CUDD leaks memory otherwise!
  * FIXME: There is now a bug which sometimes causes this:
  
  ```
  cuddGarbageCollect: problem in table 6
  dead count != deleted
  This problem is often due to a missing call to Cudd_Ref
  or to an extra call to Cudd_RecursiveDeref.
  See the CUDD Programmer's Guide for additional details.fatal error encountered in SBCL pid ...:
  SIGABRT received.
  ```
	* TODO Can we trap the SIGABRT?  Or change it to something less fatal?

Text of Masataro Asai's README below.

Common Lisp binding to CUDD [![Build Status](https://travis-ci.org/guicho271828/CL-CUDD.svg?branch=master)](https://travis-ci.org/guicho271828/CL-CUDD)
===========================

This is a fork of original CL-CUDD using the modern common lisp convension.

* **Supported implementations**: SBCL, CCL and ECL.
* **Requirements**: make, curl
* **Developmental State**: After some refurbishment, now it loads reliably and all tests pass.
* **TODOs**:
    * ~~Automatic variable reordering~~
    * ~~Variable grouping API~~
    * ~~GC hook and control API~~
    * Higher-order layer for set manipulation
    * benchmarking
* **Related work**: 
  trivialib.bdd is another common lisp library for BDDs, which is entirely written in lisp. CUDD is more on the state-of-the-art side.

What are BDDs and CUDD?
-------------

BDDs (Binary Decision Diagrams) are awesome datastructures that can compactly represent exponentially large number of datasets, as well as allowing the direct computation over the compressed representation, i.e., you can take the sum/product/union/intersection of the datasets without decompressing the data!

[CUDD](http://vlsi.colorado.edu/~fabio/CUDD/)
is a famous C implementation of BDDs and its relatives: 
Multi-Terminal Binary Decision Diagrams (MTBDDs, also known as Algebraic DD / ADDs) and
Zero-suppressed Decision Diagrams.

References:

+ [Wikipedia](http://en.wikipedia.org/wiki/Binary_decision_diagram) contains lots of information.
+ [Binary Decision Diagrams](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=1675141) by Akers et al
+ [Symbolic Boolean manipulation with ordered binary-decision diagrams](http://www.cse.chalmers.se/edu/course/TDA956/Papers/acmcs92.pdf) by RE Bryant (this sorta made BDDs truely practical I guess)
+ ADDs : [Multi-Terminal Binary Decision Diagrams: An Efficient Data Structure for Matrix Representation](http://repository.cmu.edu/cgi/viewcontent.cgi?article=1456&context=compsci) by Clarke et al
+ ZDDs (survey) : [An Introduction to Zero-Suppressed Binary Decision Diagrams](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.500.6132&rep=rep1&type=pdf) by Alan Mishchenko

[trivialib.bdd](http://quickdocs.org/trivialib.bdd/) is another common lisp library for BDDs, which is entirely written in lisp. CUDD is more on the state-of-the-art side.

Building/Loading the system
---------------------------
The system is asdf-loadable.
This version of CL-CUDD automatically fetches CUDD v3.0.0 from http://vlsi.colorado.edu/~fabio/CUDD/ via curl.
The archive is expanded in the ASDF system directory and builds its dynamic library, which is then loaded by CL-CUDD.

To test the system, evaluate `(asdf:test-system :cl-cudd.test)`.
It also writes the visualizations of the decision diagrams to the system directory in DOT format.
If you have Graphviz installed, the test script also tries to convert the results into pdfs.

The binding(s)
--------------
The binding consists of two layers:
The lower layer has `cl-cudd.baseapi` package.
This layer is a very thin wrapper around the C library,
passes raw pointers around and requires that you take care of reference counting.

Above this layer there is a package named `cl-cudd` (with a nickname `cudd`).
It wraps the pointers from the lower layer, takes care of reference counting for you, 
defines several high-level operations, and also
adds documentation from the CUDD manual.

DD Construction Examples
------------------------

See [EXAMPLES.md](EXAMPLES.md).

System structure
----------------

### Low-level

This is loosely based on the SWIG-extracted information and is using CFFI-Grovel
to actually map C symbols to lisp symbols.  If you want to use this layer, then
it would be best to have a look at the CUDD manual.

You can use the low-level system just as you would use the C API of
CUDD. This also means that you have to do all the reference counting
yourself, with one exception: The reference count of the return value
each CUDD function that returns a node is increased if it is not
`null`. If it is `null`, a signal of type `cudd-null-pointer-error` is
raised.

### High-level

The high level API automatically wraps the CUDD nodes in an instance
of class `node`. ADD nodes are wrapped in an instance of `add-node`
and BDD nodes are wrapped in an instance of type `bdd-node`.

This enables runtime type checking (so that you don't stick ADD nodes
into BDD functions or vice-versa) and also automatic reference counting.

Almost all CUDD functions need to refer to a CUDD manager. In the
high-level API this manager is contained in special variable
`*manager*`. You can bind a manager using the macro `with-manager`.
You can also create a manager by
`(make-instance 'manager :pointer (cudd-init 0 0 256 262144 0))`.

All functions of package `CL-CUDD` are documented using the original or
slightly modified documentation of CUDD.

History
-------

The initial version was automatically generated using [SWIG](http://www.swig.org) by Utz-Uwe Haus.
The second version was adapted to the needs by Christian von Essen <christian.vonEssen@imag.fr>.
Later, @Neronus made a git repository on Github and @rpgoldman made a few bugfixes.
Finally @guicho271828 (Masataro Asai) has modernized the repository according to the recent practice in common lisp:  unit tests, Travis-CI support, better documentation and additional support for ZDDs.


Known problems
--------------

Using the GC to do reference counting automatically has its own share of problems:

1. References may be freed very late.

   Nodes will be dereferenced only if your CL implementation thinks
   that it's time for it. This is usually when itself is running out
   of memory. Because you are usually only holding on to the top of
   a diagram, you are not using as much memory in CL as you are using
   in CUDD. Hence the GC might come pretty late while CUDD is happily
   accumulating memory.

   The solution to that is to try to call the garbage collector
   manually every so often using for example
   TRIVIAL-GARBAGE:GC

Solved problems
---------------

~~References may be freed too early~~

The old text below is wrong. CUDD's reference counting GC does not work this
way.  According to CUDD's manual, its GC happens when:

1. A call to cuddUniqueInter , to cuddUniqueInterZdd , to cuddUnique-
   Const, or to a function that may eventually cause a call to them.
2. A call to Cudd RecursiveDeref , to Cudd RecursiveDerefZdd , or to a
   function that may eventually cause a call to them.

Thus the GC does not occur at arbitrary code path, as assumed below.

     The following two examples demonstrate the problem.

        (defun foo (dd)
          (let ((ptr (node-pointer dd)))
            ;; please don't GC me here
            (cudd-do-something ptr)))

    In this example the GC might decide to run where there is the
    comment.
    In that case, provided that nothing outside of the function call
    holds on to `dd`, the reference count of `ptr` might be decreased,
    go down to zero and the node vanishes before `cudd-do-something` is
    called.
