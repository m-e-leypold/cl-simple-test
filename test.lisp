;;; ------------------------------------------------------------------------*- common-lisp -*-|
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
;;; * Options -----------------------------------------------------------------------------------------------|

(declaim (optimize (speed 0) (space 0) (compilation-speed 0) (debug 3) (safety 3)))

;;; * Load system to be tested & the tests ------------------------------------------------------------------------------|

(asdf:load-system "de.m-e-leypold.cl-simple-test/tests")

;;; * Define sandbox for tests ------------------------------------------------------------------------------|

(defpackage :de.m-e-leypold.cl-simple-test/run-tests
  (:documentation "Testing cl-simple-test")
  (:use
   :common-lisp
   :de.m-e-leypold.cl-simple-test/tests))

(in-package :de.m-e-leypold.cl-simple-test/run-tests)

;;; * Actually executing the tests --------------------------------------------------------------------------|

(run-tests-local)

;;; * -------------------------------------------------------------------------------------------------------|
;;; WRT the outline-* and comment-* variables, see the comment in test.lisp

;;; Local Variables:
;;; eval: (progn (outshine-mode 1) (column-enforce-mode 1) (toggle-truncate-lines 1))
;;; fill-column: 110
;;; column-enforce-column: 110
;;; outline-regexp: ";;; [*]\\{1,8\\} "
;;; comment-add: 2
;;; End:
