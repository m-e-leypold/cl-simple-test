<
;;; * -- (C) 2022  M E Leypold ------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-simple.test -- a simple testing framework for common lisp.
;;;   Copyright (C) 2022  M E Leypold
;;;
;;;   This program is free software: you can redistribute it and/or modify
;;;   it under the terms of the GNU General Public License as published by
;;;   the Free Software Foundation, either version 3 of the License, or
;;;   (at your option) any later version.
;;;
;;;   This program is distributed in the hope that it will be useful,
;;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;   GNU General Public License for more details.
;;;
;;;   You should have received a copy of the GNU General Public License
;;;   along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;;   For altermative licensing options, see README.md
;;;
;;;
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;
;;;   Will be changed to defaults when cl-simple-test has reached sufficient maturity.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition ---------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-test
  (:documentation "A simple test framework working with ```ASSERT'''")
  (:use :common-lisp :cl-ppcre)
  (:export
   :assert-condition
   :assert-no-condition
   :assert-and-capture-condition

   ;; defining tests

   :deftest
   :*tests*
   :reset-test-definitions

   ;; running tests

   :*current-test*
   :run-tests
   :reset-run-state

   ;; test results

   :*failed*
   :*passed*

   ;; parameters

   :*signal-after-run-tests*
   :*drop-into-debugger*

   ;; restarts
   :next-test
   :abort-tests
   ))

(in-package :de.m-e-leypold.cl-simple-test)

;;; * -- Development infrastructure -------------------------------------------------------------------------|

(defun load-tests ()
  (asdf:load-system "de.m-e-leypold.cl-simple-test/tests"))

;;; * -- Defining tests -------------------------------------------------------------------------------------|

(defparameter *tests*  '()
  "Contains the tests defined with `DEFTEST' (as symbols).")

(defparameter *current-test*  nil
  "Contains the currently running test as a a symbol")

(defmacro deftest (name args doc &body body)
  "Define a test.

   Defines a test procedure bound to the symbol NAME and registers it for later execution with
   `RUN-TESTS'.

   The lambda-list ARGS needs to be empty. The rationale that it needs to be given regardless
   is that this increases the readability. A reader will register immediately that a DEFTEST
   form defines a procedure.

   Every test must have a docstring DOC that describes what is tested in the test and how.

   Specification: `TEST:DEFINING-TESTS', `TEST:FAILING-ASSERTIONS-IN-TESTS'.
                  Execute (load-tests) before or load test.lisp.
"
  (assert (not args) nil
	  (format nil
		  "Argument list in DEFTEST ~S not empty, but ~S. Must be empty when using ~a."
		  name
		  args
		  (package-name (symbol-package 'deftest))))

  (assert (stringp doc) nil
	  (format nil
		  "No docstring in DEFTEST ~S. Docstrings are required."
		  name))
  `(progn
     (setf *tests* (adjoin  (quote ,name) *tests*))
     (defun ,name ()
       ,doc
       (let ((*current-test* (quote ,name))) ,@body))))


;;; * -- Running tests --------------------------------------------------------------------------------------|

(defparameter *failed* '()
  "Contains the failed tests after running the tests with ```RUN-TESTS'''")

(defparameter *passed* '()
  "Contains the passed tests after running the tests with ```RUN-TESTS'''")

(defvar *drop-into-debugger* nil
  "
  If T, ```RUN-TEST''' will let a failing test drop into the debugger, otherwise
  the condition will be handeled after printing the error message and the tests
  will continue.

  The default is NIL, i.e. not to drop into debugger. This mode is geared towards batch testing.
")

(defvar *signal-after-run-tests* T
  "
  Wether to signal an `ERROR' at the end of `RUN-TEST' if any of the tests failed.

  The default is T (yes, signal).
"
 )

(defun test-failed (c)
  "Registers `*CURRENT-TEST' as failed."
  (push *current-test* *failed*)
  (format t "~a~%" c)
  (format t "~&**** !FAILED: ~a~%" *current-test*)
  nil
  )

(defun test-passed ()
  "Registers `*CURRENT-TEST' as passed."
  (push *current-test* *passed*)
  (format t "~&  => PASSED ~a~%~%" *current-test*)
  t
  )


(defun first-docline (sym)
  "Extract the first line of the documentation as tagline for logging"
  (let ((docstring (documentation sym 'FUNCTION)))
    (if docstring
	(multiple-value-bind (prefix first-line?)
	    (CL-PPCRE:scan-to-strings  "^[\\n ]*([^ \\n].*)" docstring)
	  (declare (ignorable prefix))
	  (if first-line?
	      (aref first-line? 0))))))

(defun run-tests ()

  "
  Runs all tests from `*TESTS*' sorting them accordingly into `*FAILED*' and `*PASSED*'.

  Conditions of type `ERROR' (like as signalled by an `ASSERT') signalled in the tests result in immediate
  abortion of its execution and the symbol for the test being pushed to`*FAILED*'.

  The symbols for tests that execute without signalling an `ERROR' are pushed to `*PASSED*'.

  Parameters modifying behaviour:

  - `*DROP-INTO-DEBUGGER*' -- instead of just registering a test as failed when a condition is
     signalled, let the signal propagate upwards the stack, so that restarts NEXT-TEST or
     ABORT-TESTS can be selected. Default behaviour: Handle the signal in `RUN-TESTS'
     internally.

  - '*SIGNAL-AFTER-RUN-TESTS*' -- Signal an error at the end of `RUN-TESTS' if any tests
     failed. This is the default.
    

  Specification: `TEST:RUNNING-TESTS', `TEST:FAILING-ASSERTIONS-DURING-RUN-TESTS'.
                 Execute (load-tests) before or load test.lisp.

  See also: `TEST:FAILING-ASSERTIONS-IN-TESTS'.
  "

  (setf *failed* '())
  (setf *passed* '())
  (format t "~&------------------------------------------------~%~%")
  (format t "~&*tests*~8t = ~a~%~%" *tests*)

  (restart-case

      (dolist (test (reverse *tests*))
	(let ((*current-test* test))

	  (format t "---- ~a::~a ----~%" (package-name (symbol-package test)) test)
	  (let ((tagline (first-docline test)))
	    (if tagline
		(format t "  ;; ~a~%" tagline)))

	  (if *drop-into-debugger*
	      (progn
		(restart-case
		    (progn
		      (handler-bind
			  ((error #'test-failed))
			(apply (symbol-function test) nil))
		      (test-passed))
		  (next-test () t)))
	      (handler-case
		  (progn
		    (apply (symbol-function test) nil)
		    (test-passed))
		(simple-error (c) (test-failed c))))))

    (abort-tests () t))

  (format t "~&------------------------------------------------~%")

  (if *failed*
      (progn
	(format t "FAILED: ~a of ~a tests: ~a~%"
		(length *failed*) (length *tests*) (reverse *failed*))
	(if *signal-after-run-tests*
	    (error (format nil "~a of ~a tests failed: ~a."
			   (length *failed*) (length *tests*) (reverse *failed*)))))
      (format t "ALL PASSED (~a tests)~%" (length *tests*)))
  (reverse *failed*))

;;; * -- Resetting state ------------------------------------------------------------------------------------|

(defun reset-test-run-state ()
  (setf *current-test* nil))

(defun reset-test-definitions ()
  (reset-test-run-state)
  (setf *tests* '()))
