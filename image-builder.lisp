(require :asdf)


;; using sbcl:
;; time sbcl --no-sysinit --no-userinit --load image-builder.lisp

;; using CCL
;; time ccl64  -n -l image-builder.lisp


;; (in-package #:asdf)
	
;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")
	
(asdf:defsystem :quicklisp-abcl
  :description
  "Load Quicklisp from the network if it isn't already installed. <urn:abcl.org/release/1.3.0-dev/contrib/quicklisp-abcl#0.2.0>"
  :version "0.2.0"
  :components nil)

;; #+nil::needs-abcl-asdf((:iri "http://beta.quicklisp.org/quicklisp.lisp"))
;;    #+nil::in-order-to ((asdf:compile-op (ql::install)))  ;;; FIXME tickle the internal Quicklisp setup



(defun install-quicklisp ()
  ""
  (let* ((path "build/quicklisp")
	 (quicklisp-init (merge-pathnames "setup.lisp" path))
	 (asdf:*central-registry* (list *default-pathname-defaults*)))
    ;; (pushnew *default-pathname-defaults* asdf:*central-registry*)
    (if (probe-file quicklisp-init)
	(progn
	  (format t "Loading ~a~%" quicklisp-init)
	  (load quicklisp-init))
	(progn
	  (format t "Creating ~a~%" path)
	  (ensure-directories-exist path)
	  (asdf:run-shell-command
	   (format nil "curl -o ~a http://beta.quicklisp.org/quicklisp.lisp"
		   quicklisp-init))
	  (load quicklisp-init)
	  (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART") :path path)
	  ;; Make sure our project is known my quicklisp ans avoid symlink loops.
	  ;; (with-open-file (stream "build/quicklisp/local-projects/system-index.txt"
	  ;; 			  :direction :output :if-exists :supersede)
	  ;;   (format stream "~{~a~%~}"
	  ;; 	    (directory
	  ;; 	     (make-pathname
	  ;; 	      :directory (directory-namestring
	  ;; 			  *default-pathname-defaults*)
	  ;; 	      :name :wild
	  ;; 	      :type "asd"))))

	  ;; (asdf:run-shell-command
	  ;;  (format nil "ln -nfs ~a build/quicklisp/local-projects/"
	  ;; 	   (string-right-trim
	  ;; 	    "/"
	  ;; 	    (directory-namestring *default-pathname-defaults*))))

	  ))))


(defun clone-git ()
  (asdf:run-shell-command "git clone https://github.com/mabragor/esrap-liquid.git build/quicklisp/local-projects/esrap-liquid")
  (asdf:run-shell-command "git clone https://github.com/mabragor/cl-yaclyaml.git build/quicklisp/local-projects/cl-yaclyaml"))


(defun load-project (projects)
  ""
  (format t "Loading build/quicklisp/setup.lisp~%")
  (load "build/quicklisp/setup.lisp")
  (let ((projects (if (listp projects) projects (list projects)))
	(asdf:*central-registry* (list *default-pathname-defaults*)))
    (loop for project in projects
	  do (progn
	       (format t "Loading ~a~%" project)
	       (funcall (intern "QUICKLOAD" "QUICKLISP") project)))))


(defun split-string-by-char (string &key (char #\:))
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (remove-if #'(lambda(x) (string= "" x))
	     (loop for i = 0 then (1+ j)
		   as j = (position char string :start i)
		   collect (subseq string i j)
		   while j)))


(defun string-to-function-symbol(string)
  (let ((elements (split-string-by-char (string-upcase string) :char #\:)))
    (apply #'intern (reverse elements))))

(defun write-image (entry-point)
  ""
  (let ((entry-function (string-to-function-symbol entry-point)))
    #+sbcl
    (progn
      (format t "Compiling image for SBCL ~a~%" entry-function)
      (sb-ext:save-lisp-and-die
       "image.exe"
       :purify t :executable t :compression t
       :toplevel #'(lambda() (funcall entry-function sb-ext:*posix-argv*))))

    #+clozure
    (progn
      (format t "Compiling image for CCL ~a~%" entry-function)
      (save-application
       "image.ccl.exe"
       :toplevel-function #'(lambda()
			      (funcall entry-function
				       (car ccl:*command-line-argument-list*)))
       :prepend-kernel t))))


(install-quicklisp)
(clone-git)
(load-project '(:ssh-config :ssh-config-cli))
(write-image "ssh-config-cli::main")


;; (defun component-present-p (value)
;;   (and value (not (eql value :unspecific))))

;; (defun directory-pathname-p  (p)
;;   (and
;;    (not (component-present-p (pathname-name p)))
;;    (not (component-present-p (pathname-type p)))
;;    p))

;; (defun pathname-as-directory (name)
;;   (let ((pathname (pathname name)))
;;     (when (wild-pathname-p pathname)
;;       (error "Can't reliably convert wild pathnames."))
;;     (if (not (directory-pathname-p name))
;;       (make-pathname
;;        :directory (append (or (pathname-directory pathname) (list :relative))
;;                           (list (file-namestring pathname)))
;;        :name      nil
;;        :type      nil
;;        :defaults pathname)
;;       pathname)))


;; (defun directory-wildcard (dirname)
;;   (make-pathname
;;    :name :wild
;;    :type #-clisp :wild #+clisp nil
;;    :defaults (pathname-as-directory dirname)))


;; (defun list-directory (dirname)
;;   (when (wild-pathname-p dirname)
;;     (error "Can only list concrete directory names."))
;;   (let ((wildcard (directory-wildcard dirname)))

;;     #+(or sbcl cmu lispworks)
;;     (directory wildcard)

;;     #+openmcl
;;     (directory wildcard :directories t)

;;     #+allegro
;;     (directory wildcard :directories-are-files nil)

;;     #+clisp
;;     (nconc
;;      (directory wildcard)
;;      (directory (clisp-subdirectories-wildcard wildcard)))

;;     #-(or sbcl cmu lispworks openmcl allegro clisp)
;;     (error "list-directory not implemented")))


;; (defun walk-directory (dirname fn &key directories (test (constantly t)))
;;   (labels
;;       ((walk (name)
;;          (cond
;;            ((directory-pathname-p name)
;;             (when (and directories (funcall test name))
;;               (funcall fn name))
;;             (dolist (x (list-directory name)) (walk x)))
;;            ((funcall test name) (funcall fn name)))))
;;     (walk (pathname-as-directory dirname))))

