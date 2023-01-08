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
   :*failed*
   :*drop-into-debugger*
   :*re-signal*
   :assert-condition
   :assert-no-condition
   :assert-and-capture-condition
   :deftest
   :run-tests

   :*tests*
   :*current-test*
   :reset-test-definitions
   :reset-run-state
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

   Specification: See `TEST:DEFINING-TESTS' and `TEST:FAILING-ASSERTIONS-IN-TESTS'.
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


;;; * -- Resetting state ------------------------------------------------------------------------------------|

(defun reset-test-run-state ()
  (setf *current-test* nil))

(defun reset-test-definitions ()
  (reset-test-run-state)
  (setf *tests* '()))

