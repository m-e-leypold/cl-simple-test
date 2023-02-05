#
#   de.m-e-leypold.cl-simple-utils -- Some utility functions.
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
#   For alternative licensing options, see README.md
#

all:: check-warnings

ASD-FILE        = $(wildcard *.asd)
PRIMARY-SYSTEM  = $(ASD-FILE:%.asd=%)
SHORT-NAME      = $(lastword $(subst ., ,$(PRIMARY-SYSTEM)))
TEST-RUNNER     = $(strip $(wildcard test.lisp))
AUTHOR-ID      ?= m-e-leypold
GITLAB         ?= git@gitlab.com:$(AUTHOR-ID)/$(SHORT-NAME).git
GITHUB         ?= git@github.com:$(AUTHOR-ID)/$(SHORT-NAME).git
ORIGIN         ?= LSD:projects/$(SHORT-NAME).git

$(info PRIMARY-SYSTEM = $(PRIMARY-SYSTEM))
$(info SHORT-NAME     = $(SHORT-NAME))
$(info TEST-RUNNER    = $(SHORT-NAME))

clean::
	rm -f *~ *.log *.fasl	

CHECK-PREP = sbcl --noinform --disable-debugger --eval '(asdf:load-system "$(PRIMARY-SYSTEM)/prerequisites")' --quit
LOAD       = sbcl --noinform --disable-debugger --eval '(asdf:load-system "$(PRIMARY-SYSTEM)/load-all")' --quit

ifeq ($(TEST-RUNNER),)
check::    # There are no checks here
else
CHECK      = sbcl --noinform --disable-debugger --load $(TEST-RUNNER) --quit
check::
	$(CHECK)
	@echo
endif

# The procedures below are for the original author of this package.

dev: git-setup Project

git-setup:                          # This are the upstream repositories
	git remote rm GITLAB || true
	git remote rm GITHUB || true
	git remote add GITLAB $(GITLAB)
	git remote add GITHUB $(GITHUB)
	git fetch GITLAB
	git fetch GITHUB

Project:
	git clone -b project --single-branch . Project
	cd Project && git remote add UPSTREAM $(ORIGIN)
	cd Project && git fetch UPSTREAM
	cd Project && git merge UPSTREAM/project
	cd Project && git push UPSTREAM project
	cd Project && git push origin project


publish: publish-source publish-project 

publish-project:
	cd Project && git branch | grep '^[*] project$$' # We only release from project
	cd Project && \
           if git status -s | grep -v '^??'; \
	      then git status -s ; false; \
           else true; \
        fi
	cd Project && git push origin project
	git push

publish-source: check-all
	git branch | grep '^[*] main$$' # We only release from main
	if git status -s | grep -v '^??'; \
	   then git status -s ; false; \
           else true; \
        fi
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
	@grep compiling CHECK.log
	@echo "No warnings detected."

stricter-check: clean-fasl-cache check-warnings

check-all: check stricter-check

