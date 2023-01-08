#
#   de.m-e-leypold.cl-simple.test -- a simple testing framework for common lisp.
#   Copyright (C) 2022  M E Leypold
#   
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#   
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#   
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
#   For altermative licensing options, see README.md
#

# The intended effect is, to load all files into the fasl cache. Checking the warnings is a side
# effect that, since the cache is nor cleared, will go away on the second invocation.
#
all:: check-warnings

clean::
	rm -f *~ *.log *.fasl

CHECK-PREP = sbcl --noinform --disable-debugger \
                  --eval '(asdf:load-system "de.m-e-leypold.cl-simple-test/prerequisites")' --quit
LOAD       = sbcl --noinform --disable-debugger \
                  --eval '(asdf:load-system "de.m-e-leypold.cl-simple-test/tests")' --quit
CHECK      = sbcl --noinform --disable-debugger --load test.lisp --quit

check::
	$(CHECK)
	@echo

# The procedures below are for the original author of this package.

git-setup:                          # This are the upstream repositories
	git remote rm GITLAB || true
	git remote rm GITHUP || true
	git remote add GITLAB git@gitlab.com:m-e-leypold/cl-simple-test.git
	git remote add GITHUB git@github.com:m-e-leypold/cl-simple-test.git
	git fetch GITLAB
	git fetch GITHUB

publish:                            # We only release from main
	git push GITLAB main
	git push GITHUB main
	git push origin main


clean-fasl-cache:
	rm -rf $(HOME)/.cache/common-lisp

check-warnings:
	$(CHECK-PREP) >CHECK-PREP.log 2>&1
	$(LOAD) >CHECK.log 2>&1
	! grep -C8 -i "warn" CHECK.log  # This could be smarter
	@echo
	@echo "No warnings detected."

stricter-check: clean-fasl-cache check-warnings

check-all: check stricter-check

