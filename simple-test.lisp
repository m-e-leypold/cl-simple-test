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
;;;   For user and technical documentation start with the documentatin string of Symbol
;;;   `de.m-e-leypold.cl-simple-test::DOC'.
;;;
;;; * -- Options --------------------------------------------------------------------------------------------|
;;;
;;;   Will be changed to defaults when cl-simple-test has reached sufficient maturity.

(declaim (optimize (speed 0) (debug 3) (safety 3)))

;;; * -- Package definition ---------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-test
  (:documentation "

   DE.M-E-LEYPOLD.CL-SIMPLE-TEST is a simple test framework working with variations of `ASSERT'.


   Defining Tests
   --------------

   Define tests with `DEFTEST'. `*TESTS*' will contain the all defined tests as a list of
   symbols.


   Asserting expectations
   ----------------------

   Assert expectations in the test bodies with

   - `CL:ASSERT': Given expression must evaluate to true.
   - `ASSERT-CONDITION': Body must signal given condition type.
   - `ASSERT-NO-CONDITION': Body must not signal any condition.
   - `ASSERT-AND-CAPTURE-CONDITION': Body must signal given condition type and the
      instance will be captured in a variable.


   Executing test
   --------------

   Execute all defined tests with `RUN-TESTS'. Failing tests will be recorded in `*FAILED*',
   passing tests in `*PASSED*'.

   At test run-time `*CURRENT-TEST*' always contains the symbol of the test that is executed at
   the moment. `*TESTS*' contains the symbols of all currently defined tests which might prove
   useful for creating a test summary report.


   Parameters
   ----------

   Two parameters influence test execution:

   - `*DROP-INTO-DEBUGGER*' (default: NIL) -- Drop into the debugger when a test fails. This is
     useful for stopping at once at the failing test and of course have a chance to debug what
     goes wrong.

   - `*SIGNAL-AFTER-RUN-TESTS*' (default: T) -- After all tests have run, signal if any test
     failed (if `*FAILED*' is not empty). This is useful in batch mode to get a process exit
     code that indicates that s.th. failed when `*DROP-INTO-DEBUGGER*' is NIL. The backtrace in
     turn is not very useful in this case.

   The default setup would be typical for a batch run where we want to run all tests and just
   fail or pass at the end, e.g. for running tests in a CI pipeline.

   The setup (SETF *DROP-INTO-DEBUGGER* T) would be typical for an interactive or batch test
   run where all we want is to stop at the first failing test. This often makes sense in cases
   where the tests check layers of functionality that build on each other and it does not make
   sense to continue with later tests if one of the layers fail.


   Restarts during `RUN-TEST'
   --------------------------

   Mostly for interactive use, two restarts are established by `RUN-TESTS':

   - `NEXT-TEST' -- Continue with the next test in the list.
   - `ABORT-TESTS' -- Abort testing.


   Test resets
   -----------

   The following two functions are mostly useful for testing the CL-SIMPLE-TESTS framework
   itself.

   - `RESET-TEST-DEFINITIONS' will clear `*TESTS*' with the result that the test definitions
     are \"forgotten\".

   - `RESET-RUN-STATE' resets the definitions (as above) and clears `*CURRENT-TEST*'.

   Tests defined with `DEFTEST' are just normal function. They can be run by invoking them
   explicitely. When doing so, the signal handlers and restarts referenced above will not be be
   installed while the tests executes. This means any condition signal esacping from the test
   body will result in the debugger being invoked if `*DEBUGGER-HOOK*' is set.


   Further information
   -------------------

   Please refer to the documentation of above symbols for further information.

   The tests in package DE.M-E-LEYPOLD.CL-SIMPLE-TEST/TESTS also serve as a more detailed
   specification. See `DE.M-E-LEYPOLD.CL-SIMPLE-TEST/TESTS::*REGISTRY*' for an overview.

   Note: You will have either to run the tests or execute (load-tests) before the test symbols
   and their documentation become available in your lisp process.
   ")

  (:use :common-lisp :cl-ppcre)
  (:import-from :de.m-e-leypold.cl-simple-utils
   :defpackage-doc
   :defrestart)
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

   ;; restart

   :next-test
   :abort-tests
   ))

(in-package :de.m-e-leypold.cl-simple-test)
(defpackage-doc -doc-)                       ;; -doc- will contain package doc string


;;; * -- Development infrastructure -------------------------------------------------------------------------|

(defun load-tests ()
  (asdf:load-system "de.m-e-leypold.cl-simple-test/tests"))

;;; * -- Defining tests -------------------------------------------------------------------------------------|

(defparameter *tests*  '()
  "Contains the tests defined with `DEFTEST' (as symbols).")

(defparameter *current-test*  nil
  "
  Contains the currently running test as a a symbol, NIL if no test is running.

  Specification: `TEST::DEFINING-TESTS', `TEST::CURRENT-TEST-MAINTENANCE'.
       See also: `-DOC-'.
")

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

   See also: `-DOC-'.
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
  "Contains the failed tests after executing tests with `RUN-TESTS'.")

(defparameter *passed* '()
  "Contains the passed tests after executing tests with `RUN-TESTS'.")

(defvar *drop-into-debugger* nil
  "
  If T, `RUN-TEST' will let a failing test drop into the debugger, otherwise the condition will
  be handled after printing the error message and the tests will continue.

  The default is NIL, i.e. not to drop into debugger. This mode is geared towards batch
  testing.
")

(defvar *signal-after-run-tests* T
  "
  Whether to signal an `ERROR' at the end of `RUN-TEST' if any of the tests failed.

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

(defrestart next-test ()
  "
  Restart to continue with the next test in the list. Established by `RUN-TESTS'.
")

(defrestart abort-tests ()
  "
  Restart to abort testing. Established by `RUN-TESTS'.
")

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

        ;; We set *CURRENT-TEST*, so auxilliary functions like TEST-PASSED can access it during
        ;; loop iteration. Strictly speaking this is not necessary (we could pass TEST
        ;; explcicitely to the auxilliaries) but it make handling of "what test is currently
        ;; executing?" more uniform, also for a possible later integration of reporting hooks.

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

;;; * -- Assertions -----------------------------------------------------------------------------------------|

(defmacro assert-condition (condition &body body)
  "
  Execute BODY, assert that BODY signals condition CONDITION or one derived from CONDITION.

  - Signal `ERROR' if no condition has been raise
  - Let `ASSERT' signal error if condition type is not as expected.

  Specification: `TEST::ASSERTING-FOR-CONDITIONS'.
       See also: `-DOC-'.
"
  `(handler-case
       (progn ,@body)
     (condition (c) (assert (typep c ,condition)))
     (:no-error (&rest rest)
       (declare (ignorable rest))
       (assert nil nil "~a did not signal a condition" (cons 'progn (quote ,body))))))

;;; * -- Resetting state ------------------------------------------------------------------------------------|

(defun reset-test-run-state ()
  (setf *current-test* nil))

(defun reset-test-definitions ()
  (reset-test-run-state)
  (setf *tests* '()))
