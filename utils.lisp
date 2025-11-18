(in-package :clsh)

(define-symbol-macro *working-directory* *default-pathname-defaults*)
(setf (documentation '*working-directory* 'variable) "Synonym for *DEFAULT-PATHNAME-DEFAULTS*")

(defvar *home* #P"/tmp/" "Home directory.")
(defvar *data-dir* #P"/tmp/common-lisp-shell/data" "Data directory.")

(defun change-directory (&optional path)
  "Changes the working directory of the shell. Does not affect the process's working directory."
  (let ((resolved-path (truename (or path *home*))))
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

(defun getenv (var)
  "Retrieve the value of an environment variable"
  (uiop:getenv (string var)))

(defun initial-setup ()
  "Loads configuration files and sets up global variables."
  (let* ((home (user-homedir-pathname))
		 (config-dir (merge-pathnames #P".config/common-lisp-shell/" home))
		 (data-dir (merge-pathnames #P"data/" config-dir))
		 (config-path (merge-pathnames #P"config.lisp" config-dir)))
	(setf *home* home)
	(setf *data-dir* data-dir)
	(ensure-directories-exist data-dir)
	(when (probe-file config-path)
      (load config-path))))
