#!/usr/bin/sbcl --script

(require 'asdf)

(load (merge-pathnames "clsh.asd" (directory-namestring *load-truename*)))
(let ((*compile-verbose* t)
	  (*load-verbose* t))
  (asdf:load-system "clsh"))
(sb-ext:save-lisp-and-die "clsh" :executable t :toplevel #'clsh:repl)
