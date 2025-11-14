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
	  (handler-case
		  (let ((ret (funcall *prompt*)))
			(when ret (format t ret)))
		(error (e)
		  (format t "Prompt failing with error `~A`. Please fix the *PROMPT* variable.~%CLSH> " e)))
	  (format t "~A" *prompt*))
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

(define-condition exit (error) ()
  (:documentation "Condition to exit the shell"))

(defun exit ()
  "Exits the Common Lisp Shell."
  (signal 'exit))

(defun repl ()
  "The Common Lisp Shell. A Common Lisp REPL with core facilities built-in."
  (let ((*package* (find-package :clsh)))
	(loop
	  (display-prompt)
	  (handler-case (format t "~S~%" (eval (read *standard-input*)))
		(sb-sys:interactive-interrupt (c) (declare (ignore c))
		  (terpri))
		(exit (e) (declare (ignore e))
		  (return))
		(end-of-file (e) (declare (ignore e))
		  (terpri)
		  (return))
		(error (e)
		  (format t "Error: ~S~%" e))))))
