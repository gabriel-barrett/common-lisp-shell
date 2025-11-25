(asdf:defsystem "clsh"
  :serial t
  :depends-on ("eclector")
  :components ((:file "package")
			   (:file "utils" :depends-on ("package"))
               (:file "repl" :depends-on ("package"))))
