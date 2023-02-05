CL-SIMPLE-TEST --- very simple assert based testing for common lisp
===================================================================

This micro framework will mostly be used to bootstrap a more ambitious
framework (namely cl-test).

Compatibility
-------------

CL-SIMPLE-TEST is only expected to work on SBLC currently. Since in
packages it builds upon, there are conditionals that are not
implemented for other platforms, it would come as a surprise to the
author(s) if anything worked on any other platform.

Documentation
-------------

Documentation of this system is available as extensively linked
documentation strings in the packages themselves. Slime here functions
as hypertext facility using `C-c C-d C-d` (`slime-describe-symbol`).

In order to read the documentation you need

- emacs
- load slime and start a lisp REPL (only SBCL works these days)
- Load "tests.lisp" or "load.lisp" with `C-c C-l`
  (`slime-load-file`). It is recommended to run the tests at the
  beginning so you know everything is working with your platform
  properly.  
- Then continue with the package documentation string as described below.

In the following block, position the cursor on `-DOC` and run `M-x
slime-describe-symbol`. This should open a `*slime-description*`
buffer with the package documentation string from where you can follow
to the referenced functions and variables.

    (in-package DE.M-E-LEYPOLD.CL-SIMPLE-TEST) ;; This is only for the benefit of SLIME
    -DOC-                                      ;; M-x slime-describe-symbol on this

At the moment there is no way to export the documentation as text,
html or pdf.

License and copyright
---------------------

CL-SIMPLE-TEST is copyrighted by M E Leypold and licensed to you by
the GPL Version 3 or later (see file LICENSE.md)

>  de.m-e-leypold.cl-simple.test -- a simple testing framework for common lisp.
>  Copyright (C) 2022 - 2023  M E Leypold
>
>  This program is free software: you can redistribute it and/or
>  modify it under the terms of the GNU General Public License as
>  published by the Free Software Foundation, either version 3 of the
>  License, or (at your option) any later version.
>
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
>
>  You should have received a copy of the GNU General Public License
>  along with this program.  If not, see
>  <https://www.gnu.org/licenses/>.
>
>  For alternative licensing options, see README.md

Alternative licensing is available upon request (see the section below
titled "Alternative licensing options") and requires written
permission by the author of this software.

How I understand the GPL as applied to common lisp source
---------------------------------------------------------

My understanding of the GPL is, that the obligations for source
redistribution only come into force when one distributes a "linked"
"binary" of an application. Then _all_ the source for the application
must be distributed and be subject to "the same conditions" for
re-distribution, that is, the GPL.

The latter is what people mean when they say that "the GPL infects
other modules of the software". Note, though, that this is not really
the case, since you can just not re-license source to which one
doesn't hold the copyright. Generally here is no automatic
infection. The possible exception is source whose copyright is held by
the distributor of the binary, which by the act of re-distribution
alone might come under GPL, but I really think this is a gray area.

Instead you simply cannot distribute binaries in cases where the
application mixes parts that are under GPL and parts that are not
compatible with the GPL (e.g. not requiring re-distribution, not
allowing re-distribution or not imposing GPL compatible obligation on
the receiver of the source).

In the case of lisp, "binaries" are system images (dumps) that have
all the necessary packages for an application already
loaded. Obviously if not all of those loaded packages have GPL
compatible licenses or one wants to keep some of those package
proprietary, then one cannot distribute those images.

But one can still 

- Distribute a loader-script that first loads the necessary packages
  from source or (cached) FASL files and then starts the application.
- Generate an image from source at the time when the application is
  installed on a specific machine (e.g. in a post-install script).
- Generate images that one only wants to use for oneself.

So in which situation is the prohibition of distributing images (again
applicable only in case of packages with mixed licenses as explained
above) really a problem? Because you need to distribute images? I see
two scenarios:

1. You do not want to disclose the full source to the user. I'm sorry
   to say that I don't support this with the GPL edition of
   CL-SIMPLE-TEST or other lisp package soft mine --- at least not out
   of the box. You either buy a different license from me (see below
   and subject to changing circumstances), or you use some other
   solution instead.
   
2. You want to distribute "pre-built" images of what is essentially an
   open source application, but built with packages that are not GPL
   compatible and cannot switch to a different delivery option (like
   the loader script mentioned above).
   
   In this case I'm very willing to listen and find a solution with
   you. Just mail me under the obfuscated address in the ASD file.

Note that I'm not a lawyer, so you better get professional advice if
you have special licensing needs and want to mix sources that are not
under GPL with CL-SIMPLE-TEST or if you do not understand the GPL. I
cannot help you there. Don't rely on my advice if with respect to
other GPLed software.

Here I just want to express clearly, that I do not see a problem ---
don't see my rights violated --- if you load CL-SIMPLE-TEST into lisp
systems with a different runtime license or together with packages
that have GPL incompatible licenses or packages you want to keep
proprietary. You just must not distribute binaries (dumps, images)
that have these modules loaded together with CL-SIMPLE-TEST. I'm
pretty sure the latter _is_ a violation of the GPL.


Alternative licensing options
-----------------------------

I'm willing to license this software under licenses different from the
GPL, including a BSD 3-Clause license or a non-exclusive individual
license. Note that the previous sentence does not constitute a grant of
such a license.

In order to obtain a license, the interested party must request such a
license (e.g. by use of the obfuscated e-mail in the ASD file),
explain why and for what purpose they require such a license and
negotiate the license contract with the copyright holder. 

Dependent on the merit of the request and benefit of the requester's
purpose to the public a fee might be incurred for the grant of such a
license. The size of the fee is subject to the negotiations.

The rough rule of thumb is, that open source licenses will be free and
licenses for "I want to use this for my closed proprietary software"
will incur some fees (or a testimonial).

Only a written grant of the license, signed by the copyright holder,
permits you to use the software under terms different from the GPL.

Dependent on the arrangements with future contributors the option of
different licenses might become restricted or unavailable for future
versions of the software without any advance notice.

Contributions
-------------

Contribution will be welcome. I'm aware that many small open source
project are basically one person shows with never a contribution by a
third party. So I'm not really expecting contributions and have no
preconceived idea how to handle them.

If you contribute to this package, before integration we'll have to
discuss the copyright issue. Since I've chosen (at the moment) the (in
some sense) restrictive GPL, but want keep open the option to go to a
more liberal license (like BSD), we need to discuss how to handle this
in way that either future license changes are possible or that we go
to a very permissive license like BSD at the time when we integrate
your contribution.

As I said: Contributions are welcome and I'll do my uttermost to make
it possible to contribute.

