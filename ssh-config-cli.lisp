(defpackage #:ssh-config-cli
  (:use #:cl)
  (:export
   #:main))

(in-package #:ssh-config-cli)

(defun main (argv)
  (ssh-config:generate-ssh-config))

