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
;;;   For altermative licensing options, see README.md
;;;
;;; ---- System -----------------------------------------------------------------------------|

(defsystem "de.m-e-leypold.cl-simple-test"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Simple assertion based testing"
  :depends-on ("cl-ppcre" "de.m-e-leypold.cl-simple-utils")
  :components ((:file "simple-test")))


(defsystem "de.m-e-leypold.cl-simple-test/tests"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("cl-ppcre"
	       "de.m-e-leypold.cl-simple-test"
	       "de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/basic-test")
  :description "Tests and specifications for CL-SIMPLE-TEST"
  :components ((:file "tests")))

(defsystem "de.m-e-leypold.cl-simple-test/prerequisites"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :depends-on ("cl-ppcre"
	       "de.m-e-leypold.cl-simple-utils"
	       "de.m-e-leypold.cl-simple-utils/basic-test")
  :description "Just all external prerequisites"
  :components ())

(defsystem "de.m-e-leypold.cl-simple-test/load-all"
  :author "M E Leypold [elegant-weapons ( AT) m-e-leypold (DOT) de]"
  :licence "GPL3"
  :description "Load all systems in CL-SIMPLE-TEST"
  :depends-on ("de.m-e-leypold.cl-simple-test" "de.m-e-leypold.cl-simple-test/tests"))



