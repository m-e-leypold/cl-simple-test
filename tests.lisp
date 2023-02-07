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
   :de.m-e-leypold.cl-simple-utils
   :de.m-e-leypold.cl-simple-utils/wrapped-streams)

  (:import-from
   :de.m-e-leypold.cl-simple-utils/basic-test
   :assert! :run-tests! :deftest!
   :deftest-registry!
   :end-test-registry!
   :set-flag :clear-flags :*flags*
   :explain :trace-expr
   :test-failure)

  (:export
   :run-tests!
   :defining-tests
   :running-tests
   :failing-assertions-in-tests
   :failing-assertions-during-run-tests
   :current-test-maintenance
   :asserting-for-conditions
   :asserting-for-no-condition
   ))

(in-package :de.m-e-leypold.cl-simple-test/tests)

(inject-package-local-nickname "TEST"
			       :de.m-e-leypold.cl-simple-test/tests
			       :de.m-e-leypold.cl-simple-test)

(deftest-registry!)

(defun reset-all-state ()
  (reset-test-definitions)
  (clear-flags))

(defun run-tests* ()
  (with-maybe-indented-output (:prefix "   | ")
    (run-tests)))

;;; * The tests themselves ----------------------------------------------------------------------------------|
;;; ** Defining Tests ---------------------------------------------------------------------------------------|

(deftest! defining-tests ()
    "
    `DEFTEST' defines tests.

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

  (assert! (equal *tests* '(t2 t1)))

  (explain "Executing tests t2, t1.")
  (t2)
  (t1)

  (trace-expr *flags*)
  (assert! (equal *flags* '(t1 t2)))

  (explain "Checking docstrings.")

  (assert! (equal "t1 doc" (documentation 't1 'function)))
  (assert! (equal "t2 doc" (documentation 't2 'function)))

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

(deftest! current-test-maintenance ()
    "
    `*CURRENT-TEST*' is set to the symbol of the currently executing test during execution of a test.

    This is also true if the test is not executed under the control of `RUN-TEST' but instead invoked
    directly.
"
  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining some tests (T1-3) that register their execution by pushing *CURRENT-TEST* to *FLAGS*.")

  (deftest t1 ()
      "t1 executes without failure"
    (set-flag *current-test*))

  (deftest t2 ()
      "t2 executes without failure"
    (set-flag *current-test*))

  (deftest t3 ()
      "t3 executes without failure"
    (set-flag *current-test*))

  (explain "Executing the tests under control of RUN-TESTS.")
  (run-tests*)
  (assert! (equal *flags* '(T3 T2 T1)))

  (explain "Now running T1, T3 by invoking them directly")
  (clear-flags)
  (assert! (not *flags*)) ;; sanity check
  (t1)
  (t2)
  (assert! (equal *flags* '(T2 T1))))

(deftest! running-tests ()
    "
    `RUN-TESTS' runs tests in order of their definition; no signal means success.

    If no condition is signalled:

    1.`RUN-TESTS' will run tests in the order of their definition and not signal.

    2. No tests will be flagged as failed, i.e. `*FAILED*' will be empty.

    3. All tests (as symbols) will be pushed into `*PASSED*'.
"

  (explain "Resetting cl-simple-test.")
  (reset-all-state)

  (explain "Defining some tests (T1-3) that register their execution by setting flags. None fail.")

  (deftest t1 ()
      "t1 executes without failure"
    (set-flag 't1))

  (deftest t2 ()
      "t2 executes without failure"
    (set-flag 't2))

  (deftest t3 ()
      "t3 executes without failure"
    (set-flag 't3))

  (handler-case
      (run-tests*)
    (condition (c) (test-failure :explanation
				 (format nil "RUN-TESTS signalled ~S, but should not have" (type-of c)))))
  (assert (equal *failed* '()))
  (assert (equal *flags*  '(T3 T2 T1))) ; run order
  (assert (equal *passed* '(T3 T2 T1))))


;;; ** Assertion  handling ----------------------------------------------------------------------------------|
;;; *** Errors are not handled in the test procedures -------------------------------------------------------|

(deftest! failing-assertions-in-tests ()
    "
    Errors signaled by assertions in tests escape the test functions.

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

(deftest! failing-assertions-during-run-tests ()
    "
    `RUN-TESTS' executes defined tests; signalled errors count as failed.

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
      (run-tests*)

    (error (e)
      (let ((message
	      (format nil "~S" e)))
	(trace-expr message)

	(explain "Resulting error message must be a test summary.")
	(assert! (cl-ppcre:scan "^#<SIMPLE-ERROR.*2 of 4 tests failed: [(]T2 T4[)][.].*[>]" message))))

    (condition (e)
	       (test-failure
       :explanation
       (format nil "RUN-TEST should have signalled an `error' condition, instead it signalled ~S." e)))

    (:NO-ERROR (e1 e2)
      (declare (ignorable e1 e2))
      (test-failure
       :explanation "No error signalled by `RUN-TEST', but it should have.")))

    (assert! (equal *FAILED* '(T4 T2)))
    (assert! (equal *PASSED* '(T3 T1)))

  (explain "RUN-TESTS again with *SIGNAL-AFTER-RUN-TESTS* off")

  (let* ((*signal-after-run-tests* nil)
	 (failed (run-tests*)))

    (trace-expr failed)
    (assert! (equal failed '(T2 T4))))

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
	(setf failed (run-tests*))))

    (assert! (equal failed '(T2 T4)))
    (assert! (= 2 handler-invocations))
    (assert! (equal *passed* '(T3 T1)))
    (assert! (equal *failed* '(T4 T2))))

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
	(setf failed (run-tests*))))

    (assert! (equal failed '(T2)))
    (assert! (= 1 handler-invocations))
    (assert! (equal *passed* '(T1)))
    (assert! (equal *failed* '(T2)))))

;;; ** Special assertion macros -----------------------------------------------------------------------------|

(define-condition C1 (condition) ())
(define-condition C2 (condition) ())
(define-condition C1+ (C1)       ())


(deftest! asserting-for-conditions ()
    "
    Testing `ASSERT-CONDITION'.

    `ASSERT-CONDITION' checks if a form signal a condition of a specific type.

	 (assert-condition <cond>
	    <body>)

    will

    1. Signal `ERROR' if the execution of <body> doesn't signal any condition.

    2. If it signals a condition, assert that signalled condition is of type <cond> or derived from <cond>.  If
       it is not, the condition signalled by `ASSERT' will escape the form.
"

  (explain "Trying with a body that does not signal")

  (handler-case

      (assert-condition 'C1
	)

    (error (c)
      (format t "OK, got: ~a~%" c)) ; that's what it the result should be.

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-CONDITION with non-signalling body should have"
		 "signalled an `error' condition, instead it signalled ~S")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (test-failure
       :explanation "No error signalled by ASSERT-CONDITION with non-signalling body")))

  (explain "Trying with a body that signals a different condition")

  (handler-case

      (assert-condition 'C1
	(signal 'C2))

    (error (c)
      (format t "OK, got: ~5:i~a~%" c)) ; that's what it the result should be.

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-CONDITION with body signalling different condition than expected"
		 "should have signalled an `error' condition, instead it signalled ~S")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (test-failure
       :explanation (here-text*
		      "No error signalled by ASSERT-CONDITION with"
		      "body signalling different condition than expected"))))

  (explain "Trying with a body that signal a condition derived from the specified one")

  (handler-case

    (assert-condition 'C1
      (signal 'C1+))

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-CONDITION with body signalling a derived condition"
		 "should not have signalled any condition, instead it signalled ~a~%")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (format t "OK, no signal.~%")))

  (explain "Trying with a body that signals the expected condition")

  (handler-case

    (assert-condition 'C1
      (signal 'C1))

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-CONDITION with body signalling a the expected condition"
		 "should not have signalled any condition, instead it signalled ~a~%")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (format t "OK, no signal.~%"))))


(deftest! asserting-for-no-condition ()
    "
    Testing `ASSERT-NO-CONDITION'.

    `ASSERT-NO-CONDITION' checks if a form does not signal a condition.

	 (assert-no-condition <cond>
	    <body>)

    will

    1. Signal `ERROR' if the execution of <body> does signal any condiotion.

    2. If it does not signal a condition, the ASSERT-NO-CONDITION form will evaluate to the value of BODY.
"

  (explain "Trying abody that does not signals")

  (handler-case

      (assert-no-condition
	(progn ))

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-NO-CONDITION with non-signalling"
		 "should not have signalled any condition, instead it signalled ~a~%")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (format t "OK, no signal.~%")))


  (explain "Trying a body that signals")

  (handler-case

    (assert-no-condition
      (signal 'C1))

    (error (c)
      (format t "OK, got: ~5:i~a~%" c)) ; that's what it the result should be.

    (condition (c)
      (test-failure
       :explanation
       (format nil
 	       (here-text*
		 "ASSERT-CONDITION with body signalling"
		 "should have signalled an `error' condition, instead it signalled ~S")
	       c)))

    (:NO-ERROR (x)
      (declare (ignorable x))
      (test-failure
       :explanation (here-text*
		      "No error signalled by ASSERT-CONDITION with body signalling"))))

  )

;;; * Package epilog ----------------------------------------------------------------------------------------|

(end-test-registry!)

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
