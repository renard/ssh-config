;;;;
;;;; This package helps you to create custom standalone Common Lisp images
;;;; and to compile your project into a standalone app.
;;;;
;;;; Basically you have to define the *image-builder-config* and run (from
;;;; shell)
;;;;
;;;; - with SBCL:
;;;;     sbcl --no-sysinit --no-userinit --load image-builder.lisp
;;;; - with CCL
;;;;    ccl64  -n -l image-builder.lisp
;;;;

(require :asdf)

(asdf:defsystem #:image-builder
  :description "Build standalone Common Lisp images and apps"
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :components nil)

(defpackage #:image-builder
  (:use #:cl)
  (:export
   #:*quicklisp-setup-url*
   #:*build-directory*
   #:*quicklisp-bootstrap-file*
   #:*quicklisp-directory*
   #:*quicklisp-local-projects-directory*
   #:*quicklisp-init-file*
   #:*image-builder-config*))

(in-package #:image-builder)
  
(defparameter *quicklisp-setup-url*
  "http://beta.quicklisp.org/quicklisp.lisp"
  "URL from where to download QuickLisp.")

(defparameter *build-directory* #P"build/"
	"Root of build directory")

(defparameter *quicklisp-bootstrap-file*
  (merge-pathnames #P"quicklisp.lisp" *build-directory*)
  "Path to the quicklisp bootstrap file.")


(defparameter *quicklisp-directory* 
  (merge-pathnames #P"quicklisp/" *build-directory*)
  "Quicklisp intallation directory")

(defparameter *quicklisp-local-projects-directory* 
  (merge-pathnames #P"local-projects/" *quicklisp-directory*)
  "Quicklisp local projects directory")

(defparameter *quicklisp-init-file*
  (merge-pathnames #P"setup.lisp" *quicklisp-directory*)
  "Path to the quicklisp setup file.")


(defparameter *image-builder-config*
  '(:custom-modules
    ((:url "https://github.com/mabragor/esrap-liquid.git" :method :git)
     (:url "https://github.com/mabragor/cl-yaclyaml.git" :method :git))
    :local-projects (:ssh-config :ssh-config-cli)
    :entry-point "ssh-config-cli:main"
    :file-output "image"
    :options-sbcl (:purify t :executable t :compression t)
    :options-ccl (:error-handler :quit :prepend-kernel t)))

(defun install-quicklisp ()
  "Load Quicklisp from local build tree.

If Quicklisp *QUICKLISP-INIT-FILE* is not found, download it from
*QUICKLISP-SETUP-URL* into *QUICKLISP-DIRECTORY* and load it."
  (let ((asdf:*central-registry* (list *default-pathname-defaults*)))
    (if (probe-file *quicklisp-init-file*)
	(progn
	  (format t "Loading ~a~%" *quicklisp-init-file*)
	  (load *quicklisp-init-file*))
	(progn
	  (format t "Creating ~a~%" *quicklisp-directory*)
	  (ensure-directories-exist *quicklisp-directory*)
	  (asdf:run-shell-command
	   (format nil "curl -o ~a ~a"
		   *quicklisp-bootstrap-file* *quicklisp-setup-url*))
	  (load *quicklisp-bootstrap-file*)
	  (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART")
		   :path *quicklisp-directory*)))))


(defun clone-git (git-config)
  (let* ((url (getf git-config :url))
	 (directory (when url (pathname-name url)))
	 (target (when directory
		   (merge-pathnames directory *quicklisp-local-projects-directory*))))
    (when target
      (unless (probe-file target)
	(asdf:run-shell-command
	 (format nil "git clone ~a ~a" url target))))))


(defun load-project (projects)
  "Load quicklisp PROJECTS."
  (let ((asdf:*central-registry* (list *default-pathname-defaults*)))
    (funcall (intern "QUICKLOAD" "QUICKLISP") projects)))


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
    (when elements
      (apply #'intern (reverse elements)))))

(defun write-image (conf)
  ""
  (let ((entry-function (string-to-function-symbol
			 (getf conf :entry-point))))
    #+sbcl
    (let ((args (append
		 (cons (format nil "~a.sbcl.exe" (getf conf :file-output))
		       (getf conf :options-sbcl))
		 (when entry-function
		   (list
		    :toplevel
		    #'(lambda()
			(funcall entry-function sb-ext:*posix-argv*)))))))
      (apply #'sb-ext:save-lisp-and-die args))

    #+ccl
    (let ((args (append
		 (cons (format nil "~a.ccl.exe" (getf conf :file-output))
		       (getf conf :options-ccl))
		 (when entry-function
		   (list :toplevel-function
			 #'(lambda()
			     (funcall entry-function
				      (car ccl:*command-line-argument-list*))))))))
      (apply #'ccl:save-application args))))


(defun build-image (opts)
  (install-quicklisp)
  (loop for git in (getf opts :custom-modules)
	do (clone-git git))
  (load-project (getf opts :local-projects))
  (write-image opts))

			
(build-image *image-builder-config*)
