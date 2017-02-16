(asdf:defsystem #:ssh-config
  :serial t
  :description ""
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :depends-on (#:cl-cli #:cl-fad #:image-builder)
  :pathname "src"
  :components  (
		(:file "ssh-config-ng")
		(:file "ssh-config-cli")
		))
