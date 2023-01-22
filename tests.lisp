;;; ------------------------------------------------------------------------*- common-lisp -*-|
;;;
;;;   de.m-e-leypold.cl-simple-test -- a simple testing framework for common lisp.
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
;;;   For alternative licensing options, see README.md
;;;
;;;
;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Define package ----------------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-test/tests
  (:documentation "Testing cl-simple-test")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-test
   :de.m-e-leypold.cl-simple-utils)
  (:export
   :run-tests-local
   :defining-tests
   :running-tests
   :failing-assertions-in-tests
   :failing-assertions-during-run-tests   
   ))

(in-package :de.m-e-leypold.cl-simple-test/tests)

(inject-package-local-nickname "TEST"
			       :de.m-e-leypold.cl-simple-test/tests
			       :de.m-e-leypold.cl-simple-test)


;;; * Infrastructure for testing simple-test ----------------------------------------------------------------|

(defparameter *program-name* "tests.lisp")

(define-condition test-failure (condition)
  ((test-name
    :reader   test-name
    :initarg  :test-name
    :initform nil
    )
   (failed-condition
    :reader  failed-condition
    :initarg :failed-condition
    :initform nil)
   (explanation
    :reader  explanation
    :initarg :explanation
    :initform nil)))

(defmethod print-object ((failure test-failure) stream)

  "Print a `TEST-FAILURE' instance."

  (print-unreadable-object (failure stream :type t)
    (format stream ":test-name ~a" (test-name failure))
    (if (failed-condition failure)
	(format stream " :cond ~S" (failed-condition failure)))
    (if (explanation failure)
	(format stream " :explanation ~S" (explanation failure)))))

(defvar *tests-local* '())
(defvar *current-test-local* nil)

(defmacro assert-local (cond)
  `(progn
     (format t "~&  Checking: ~S.~%" (quote ,cond))
     (if ,cond
	 t
	 (progn
	   (format t "  *** Check failed in ~S ***~%" *current-test-local*)
	   (error 'test-failure
		  :test-name *current-test-local*
		  :failed-condition (quote ,cond))))))

(defun test-failure (&key failed-condition explanation)
  (error 'test-failure
	 :test-name *current-test-local*
	 :failed-condition failed-condition
	 :explanation explanation))


(defmacro deftest-local (name args docstring &body body)
  (assert (not args) nil (format nil "arguments of DEFTEST-LOCAL ~S must be empty" name))
  `(progn
     (setf *tests-local* (adjoin (quote ,name) *tests-local*))
     (defun ,name ()
     ,docstring
     (let ((*current-test-local* (quote ,name)))
       (format t "~&~a: Test ~S ...~%" *program-name* *current-test-local*)
       (format t "~%     ~a~%" (documentation (quote ,name) 'function))
       (progn ,@body)
       (format t "~&~a: => OK (~S).~%" *program-name* *current-test-local*)))))

(defun run-tests-local ()
  (format t "~&~%~a: Will run ~a tests: ~S.~%"
	  *program-name* (length *tests-local*) (reverse *tests-local*))
  (format t "  First failing test will abort this test run.~%")
  (dolist (test (reverse *tests-local*))
    (format t "~%")
    (funcall test))
  (format t "~%~a: All ~a tests succeeded: ~a.~%"	  
	  *program-name* (length *tests-local*) (reverse *tests-local*)))

;;; * The tests themselves ----------------------------------------------------------------------------------|
;;; ** Infrastructure & test devices ------------------------------------------------------------------------|

(defvar *flags* '()
  "A variable into which symbols will be adjoined to trace that forms have actually been evaluated.

   See `SET-FLAG'.")

(defun set-flag (name)
  (setf *flags* (adjoin name *flags*)))

(defun flag-set-p (name)
  (find name *flags*))

(defun clear-flags ()
  (setf *flags* '()))


(defun reset-all-state ()
  (reset-test-definitions)
  (clear-flags))

(defun explain (message)
  (format t "~&  ~a~%" message))

(defmacro trace-expr (expr)
  `(format t "~&  ~S => ~S~%" (quote ,expr) ,expr))

;;; ** Defining Tests ---------------------------------------------------------------------------------------|

(deftest-local defining-tests ()
  "Checking `DEFTEST' -- defines tests.

     Specification:

     1. (DEFTEST F () ...) will define a test of name F. Symbol F will be pushed to *TESTS* (to be available
        for use by `RUN-TEST').

     2. A function of the same name F will will be defined, containing the body given in DEFTEST and wrapped
        into a form, so that during execution of F, `*CURRENT-TEST*' is set to F.

     3. The documentation string is obligatory and will be attached to the function.

     If there is no documentation string given, this will result in an error at macro expansion time.
"
  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining tests T1, T2, which push *CURRENT-TEST* as flags.")

  (deftest t1 ()
      "t1 doc"
    (format t "~&  t1 here.~&")
    (set-flag *current-test*))

  (deftest t2 ()
      "t2 doc"
    (format t "~&  t2 here.~&")
    (set-flag *current-test*))

  (assert-local (equal *tests* '(t2 t1)))

  (explain "Executing tests t2, t1.")
  (t2)
  (t1)

  (trace-expr *flags*)
  (assert-local (equal *flags* '(t1 t2)))

  (explain "Checking docstrings.")

  (assert-local (equal "t1 doc" (documentation 't1 'function)))
  (assert-local (equal "t2 doc" (documentation 't2 'function)))

  (handler-case

      (macroexpand '(deftest t3 () t))

    (error () )  ; that's what it the result should be.

    (condition (e)
      (test-failure
       :explanation
       (format nil
	       "Expanding DEFTEST T3 should have signalled an `error' condition, instead it signalled ~S" e)))
    (:NO-ERROR (e1 e2)
      (declare (ignorable e1 e2))
      (test-failure
       :explanation "No error signalled by DEFTEST T3 supposed to trigger a failing assertion"))))

;;; ** Running tests ----------------------------------------------------------------------------------------|

(deftest-local running-tests ()
    "
    Checking: `RUN-TESTS' runs tests in order of their definition.

    If no condition is signalled:

    1.`RUN-TESTS' will run tests in the order of their definition and not signal.

    2. No tests will be flagged as failed, i.e. `*FAILED*' will be empty.

    3. All tests (as symbols) will be pushed into `*PASSED*'.
"

  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining some tests (T1-4) that register their execution by setting flags. Some fail (T2, T4).")

  (deftest t1 ()
      "t1 executes without failure"
    (set-flag 't1))

  (deftest t2 ()
      "t2 has a failing assertion"
    (set-flag 't2))

  (deftest t3 ()
      "t3 executes without failure"
    (set-flag 't3))

  (handler-case
      (run-tests)
    (condition (c) (test-failure :explanation
				 (format nil "RUN-TESTS signalled ~S, but should not have" (type-of c)))))
  (assert (equal *failed* '()))
  (assert (equal *flags*  '(T3 T2 T1))) ; run order
  (assert (equal *passed* '(T3 T2 T1))))


;;; ** Assertion  handling ----------------------------------------------------------------------------------|
;;; *** Errors are not handled in the test procedures -------------------------------------------------------|

(deftest-local failing-assertions-in-tests ()
    "Checking: Errors raised by assertions in tests escape the test functions.

     Context: `ASSERT' signals a `CONDITION' of type `SIMPLE-ERROR' if the predicate given is not true.

     Specification: When invoking a test function directly, such a `SIMPLE-ERROR' will not be handled by the
     test function, but escape from the test function.
"

  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining test T1, which has a failing assertion.")

  (deftest t1 ()
      ""
    (assert (= 3 (+ 1 1))))

  (explain "Invoking this test: An error is signalled.")

  (handler-case (t1)
    (error () )  ; that's what it the result should be.

    (condition (e)
      (test-failure
       :explanation
       (format nil "T1 should have signalled an `error' condition, instead it signalled ~S" e)))

    (:NO-ERROR (e1 e2)
      (declare (ignorable e1 e2))
      (test-failure
       :explanation "No error signalled by test function T1 supposed to trigger a failing assertion"))))

;;; *** Error handling by `RUN-TESTS' -----------------------------------------------------------------------|

(deftest-local failing-assertions-during-run-tests ()
    "Checks `RUN-TESTS': Executing the tests; signalled errors count as failed.

     `RUN-TESTS' executes tests.

     1. In order of their definition.
     2. Tests that execute without signalling will be counted as passed and registered in `*PASSED*' (as
        symbol).
     3. Tests that signal an error (as from a failing assertion) are counted as failed and registered in
        `*FAILED*'.
     4. Signalled conditions different from `ERROR' just escape the tests.

     5. After running all tests, if any tests failed, `RUN-TESTS' will signal an error with a message like
        \"#<SIMPLE-ERROR \"2 of 4 tests failed: (T2 T4).\" {1004CC4EA3}>\"

     This is the default behaviour: To just continue running tests and signal an error at the end if any
     failed.

     Some variables allow to modify this behaviour:

     6. If `*SIGNAL-AFTER-RUN-TESTS*' is NIL, no error according to (5) will be signalled at the end of
        `RUN-TESTS'. Instead `RUN-TESTS' will just return the list of failed tests (in order of their
        execution).

     7. When setting *DROP-INTO-DEBUGGER*, `ERROR' signals escape `RUN-TEST' and are not handled internally.

     8. When signalling, a restart `NEXT-TEST' is available to continue with the next test (either by a
        handler or interactively in the debugger.

     9. When signalling, a restart `ABORT-TEST' is available to abort testing, but still evaluate
        whether tests failed and process failures according to (5).
"

  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining some tests (T1-4) that register their execution by setting flags. Some fail (T2, T4).")

  (deftest t1 ()
      "t1 executes without failure"
    (set-flag 't1))

  (deftest t2 ()
      "t2 has a failing assertion"
    (set-flag 't2)
    (assert nil)
    (set-flag 't2.not-aborted))

  (deftest t3 ()
      "t3 executes without failure"
    (set-flag 't3))

  (deftest t4 ()
      "t4 signals an error"
    (set-flag 't4)
    (error "t4 error signal")
    (set-flag 't4.not-aborted))

  (explain "Running the defined tests.")

  (handler-case
      (run-tests)

    (error (e)
      (let ((message
	      (format nil "~S" e)))
	(trace-expr message)

	(explain "Resulting error message must be a test summary.")
	(assert-local (cl-ppcre:scan "^#<SIMPLE-ERROR.*2 of 4 tests failed: [(]T2 T4[)][.].*[>]" message))))

    (condition (e)
	       (test-failure
       :explanation
       (format nil "RUN-TEST should have signalled an `error' condition, instead it signalled ~S." e)))

    (:NO-ERROR (e1 e2)
      (declare (ignorable e1 e2))
      (test-failure
       :explanation "No error signalled by `RUN-TEST', but it should have.")))

    (assert-local (equal *FAILED* '(T4 T2)))
    (assert-local (equal *PASSED* '(T3 T1)))

  (explain "RUN-TESTS again with *SIGNAL-AFTER-RUN-TESTS* off")

  (let* ((*signal-after-run-tests* nil)
	 (failed (run-tests)))

    (trace-expr failed)
    (assert-local (equal failed '(T2 T4))))

  (explain "RUN-TESTS again with *DROP-into-debugger*, restarting with NEXT-TEST")

  (let ((handler-invocations 0)
	(failed '()))
    (handler-bind
	((error #'(lambda (c)
		    (declare (ignorable c))
		    (format t "  error handler: ~S." c)
		    (incf handler-invocations)
		    (invoke-restart 'next-test)))

	 (condition #'(lambda (c)
			(test-failure
			 :explanation
			 (format nil
				 "run-tests signalled ~s but should have signalled an error." (type-of c))))))

      (let ((*drop-into-debugger* t)
	    (*signal-after-run-tests* nil))
	(setf failed (run-tests))))

    (assert-local (equal failed '(T2 T4)))
    (assert-local (= 2 handler-invocations))
    (assert-local (equal *passed* '(T3 T1)))
    (assert-local (equal *failed* '(T4 T2))))

  (explain "RUN-TESTS again with *DROP-into-debugger*, restarting with ABORT-TESTS")

  (let ((handler-invocations 0)
	(failed '()))
    (handler-bind
	((error #'(lambda (c)
		    (declare (ignorable c))
		    (incf handler-invocations)
		    (invoke-restart 'abort-tests)))
	 (condition #'(lambda (c)
			(test-failure
			 :explanation
			 (format nil
				 "run-tests signalled ~s but should have signalled an error." (type-of c))))))

      (let ((*drop-into-debugger* t)
	     (*signal-after-run-tests* nil))
	(setf failed (run-tests))))

    (assert-local (equal failed '(T2)))
    (assert-local (= 1 handler-invocations))
    (assert-local (equal *passed* '(T1)))
    (assert-local (equal *failed* '(T2)))))


;;; * -------------------------------------------------------------------------------------------------------|
;;;   WRT the outline-* and comment-* variables, see the comment in test.lisp
;;;
;;; Local Variables:
;;; eval: (progn (outshine-mode 1) (column-enforce-mode 1) (toggle-truncate-lines 1))
;;; fill-column: 110
;;; column-enforce-column: 110
;;; outline-regexp: ";;; [*]\\{1,8\\} "
;;; comment-add: 2
;;; End:
