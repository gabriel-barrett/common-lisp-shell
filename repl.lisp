(in-package :clsh)

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

(define-condition exit (error) ()
  (:documentation "Condition to exit the shell"))

(defun exit ()
  "Exits the Common Lisp Shell."
  (signal 'exit))

(defun read-command (stream)
  "Reads expressions until it finds a newline and collects them into a list."
  (loop :for expr := (read-preserving-whitespace stream)
		:collect expr
		:until (loop :for char := (peek-char nil stream)
					 :when (equal char #\;)
					   :do (progn (read-line stream) (return t))
					 :while (member char '(#\Space #\Tab))
					 :do (read-char stream)
					 :finally (return (equal char #\Newline)))))

(defun repl ()
  "The Common Lisp Shell. A Common Lisp REPL with core facilities built-in."
  (let ((*package* (find-package :clsh)))
	(initial-setup)
	(loop
	  (display-prompt)
	  (handler-case (format t "~S~%" (eval (read-command *standard-input*)))
		(sb-sys:interactive-interrupt (c) (declare (ignore c))
		  (terpri))
		(exit (e) (declare (ignore e))
		  (return))
		(end-of-file (e) (declare (ignore e))
		  (terpri)
		  (return))
		(error (e)
		  (format t "Error: ~S~%" e))))))
