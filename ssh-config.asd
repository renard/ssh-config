(asdf:defsystem #:ssh-config
  :serial t
  :description ""
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :depends-on (#:cl-cli #:cl-fad)
  :pathname "src"
  :components  ((:file "ssh-config")
		(:file "ssh-config-cli")))
