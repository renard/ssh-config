(asdf:defsystem #:ssh-config
  :serial t
  :description ""
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :pathname "src"
  :depends-on (#:cl-yaclyaml)
  :components ((:file "ssh-config")))

(asdf:defsystem #:ssh-config-cli
  :depends-on (#:ssh-config)
  :pathname "."
  :components ((:file "ssh-config-cli")))
