(defpackage :common-lisp-shell-utils
  (:use :common-lisp :clsh)
  (:nicknames :clsh-utils))

(in-package :clsh-utils)

(define-symbol-macro *working-directory* *default-pathname-defaults*)
(setf (documentation '*working-directory* 'variable) "Synonym for *DEFAULT-PATHNAME-DEFAULTS*")

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
(setf (fdefinition 'cd) #'change-directory)

(defun id (x) "Identity function." x)

(defun echo (&rest args)
  "Echoes (formats) all arguments."
  (format t "~{~A~^ ~}~%" args))
