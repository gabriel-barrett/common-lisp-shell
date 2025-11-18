(asdf:defsystem "clsh"
  :serial t
  :components ((:file "package")
			   (:file "utils" :depends-on ("package"))
               (:file "repl" :depends-on ("package"))))
