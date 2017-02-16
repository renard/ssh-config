;; -*- lisp -*-
(:custom-systems
 (;;(:url "https://github.com/mabragor/esrap-liquid.git" :method :git)
  ;;(:url "https://github.com/mabragor/cl-yaclyaml.git" :method :git)
  (:url "https://github.com/renard/cl-cli.git" :method :git)
  (:url "https://github.com/renard/cl-image-builder.git" :method :git))
 :packages (:ssh-config)
 :entry-point "ssh-config:main"
 :output-file "update-ssh-config")
