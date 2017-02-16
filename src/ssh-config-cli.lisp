(defpackage #:ssh-config
  (:use #:cl #:cl-fad)
  (:export
   #:*verbose*
   #:*version*
   #:*pc-timeout*
   #:*nc-timeout*
   #:*debug*
   #:*help*
   #:main))

(in-package #:ssh-config)

(defvar *pc-timeout*)
(defvar *nc-timeout*)
(defvar *verbose*)
(defvar *version*)
(defvar *debug*)
(defvar *help*)

(defvar *version-string* "1.0")


(defun help ()
    (cl-cli:help *options* *commands*
		       :prog-name (path:basename (uiop:argv0))
		       :version *version-string*)
	  (uiop:quit))  


(defparameter *options*
  '((*pc-timeout* 10 "Timeout for ProxyCommand"
     :alias ("-t")
     :params ("TIMEOUT") :type integer)
    (*nc-timeout* 60 "Timeout for nc in ProxyCommand"
     :alias ("-n")
     :params ("TIMEOUT") :type integer)
    (*verbose* nil "Run in verbose mode" :alias ("-v"))
    (*version* nil "Display version" :alias ("-V"))
    (*debug* nil "Run in debug mode" :alias ("-d"))
    (*help* nil "Display help screen" :alias ("-h"))))

(defparameter *commands* nil)

(cl-cli:defcommand-to-list *commands*
    ("upgrade")
  (&key (rebuild nil "Do a full rebuild"))
  "Self upgrade (must be run in a git checkout of the sources)."

 (handler-case
     (if ssh-config:*debug*
	 (if rebuild
	     (image-builder:build-image)
	     (image-builder:upgrade :verbose ssh-config:*verbose*))

	 (handler-bind ((warning #'muffle-warning))
	   	 (if rebuild
	     (image-builder:build-image)
	     (image-builder:upgrade :verbose ssh-config:*verbose*))))
    (error (c)
      (format *error-output* "~%Fatal: ~a~%" c)
      (if ssh-config:*debug*
    	  (invoke-debugger c)
    	  (uiop:quit 2)))))

(cl-cli:defcommand-to-list *commands*
    ("help")
    nil
    "Display help."
  (help))

(ssh-config::make-ssh-config-struct)

(defun main (argv)
  (multiple-value-bind (ret opts-vars opts-values sub-func sub-opts argv)
      (cl-cli:run-command argv *options* *commands*)
    (declare (ignorable sub-func sub-opts argv))
    ;; (format t
    ;; 	    "ret ~S~%opts-vars ~S~%opts-values ~S~%sub-func ~S~%sub-opts ~S~%argv ~S~%" ret opts-vars opts-values sub-func sub-opts argv)
    (unless ret
	  (cl-cli:with-environment
	    opts-vars opts-values
	    (handler-case
		(progn
		  (when ssh-config:*version*
		    (cl-cli:version
		     (path:basename (uiop:argv0))
		     *version-string*
		     :quit 0))
		  (when ssh-config:*help* (help))
		  ;; (format t "v: ~a, d: ~a, h: ~a~%" *verbose* *debug* *help*)
		  (ssh-config:generate-ssh-config))
	      (condition (c)
		(format *error-output* "Something went wrong: ~a~%" c)
		(if ssh-config:*debug*
		    (invoke-debugger c)
		    (uiop:quit 1))))))))

  ;; (when (string= "update" (nth 1 argv))
  ;;   (image-builder:upgrade :verbose t))
  ;; (ssh-config:generate-ssh-config)
 

