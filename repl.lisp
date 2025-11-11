(defpackage :common-lisp-shell
  (:use :common-lisp)
  (:nicknames :clsh)
  (:export :repl))

(in-package :clsh)

(define-symbol-macro *working-directory* *default-pathname-defaults*)
(setf (documentation '*working-directory* 'variable) "Synonym for *DEFAULT-PATHNAME-DEFAULTS*")

(defvar *prompt* "CLSH> "
  "Controls the shell's prompt. Should either be a string or a function that returns a string or displays a prompt.")

(defun display-prompt ()
  "Displays the shell's prompt through *PROMPT*"
  (if (functionp *prompt*)
	  (let ((ret (funcall *prompt*)))
		(when ret (format t ret)))
	  (format t "~a" *prompt*))
  (finish-output))

(defun change-directory (path)
  "Changes the working directory of the shell. Does not affect the process's working directory."
  (let ((resolved-path (truename path)))
	(cond
	  ((uiop:directory-exists-p resolved-path)
	   (setf *working-directory* resolved-path))
	  ((uiop:file-exists-p resolved-path)
	   (error "File ~A is not a directory" (namestring resolved-path)))
	  (t
	   (error "Directory does not exist: ~A" (namestring resolved-path))))))

(defun repl ()
  "The Common Lisp Shell. A Common Lisp REPL with core facilities built-in."
  (in-package :clsh)
  (catch :exit-repl
	(loop
	  (display-prompt)
	  (handler-case
		  (format t "~s~%" (eval (read *standard-input*)))
		(error (e)
		  (format t "Error: ~s~%" e))))))

(defun exit ()
  "Exits the Common Lisp Shell."
  (throw :exit-repl nil))
