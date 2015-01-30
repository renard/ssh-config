(require :asdf)


;; using sbcl:
;; time sbcl --no-sysinit --no-userinit --load image-builder.lisp

;; using CCL
;; time ccl64  -n -l image-builder.lisp
	
;; Quicklisp defines:
;;(defvar *setup-url* "http://beta.quicklisp.org/quickstart/setup.lisp")
	
(asdf:defsystem :quicklisp-abcl
  :description
  "Load Quicklisp from the network if it isn't already installed. <urn:abcl.org/release/1.3.0-dev/contrib/quicklisp-abcl#0.2.0>"
  :version "0.2.0"
  :components nil)


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
    :file-output "image"))




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
    (apply #'intern (reverse elements))))

(defun write-image (entry-point output)
  ""
  (let ((entry-function (string-to-function-symbol entry-point)))
    #+sbcl
    (progn
      (format t "Compiling image for SBCL ~a~%" entry-function)
      (sb-ext:save-lisp-and-die
       (format nil "~a.sbcl.exe" output)
       :purify t :executable t :compression t
       :toplevel #'(lambda() (funcall entry-function sb-ext:*posix-argv*))))

    #+ccl
    (progn
      (format t "Compiling image for CCL ~a~%" entry-function)
      (save-application
       (format nil "~a.ccl.exe" output)
       :toplevel-function #'(lambda()
			      (funcall entry-function
				       (car ccl:*command-line-argument-list*)))
       :prepend-kernel t))))


(install-quicklisp)
(loop for git in (getf *image-builder-config* :custom-modules)
      do (clone-git git))
(load-project (getf *image-builder-config* :local-projects))
(write-image (getf *image-builder-config* :entry-point)
	     (getf *image-builder-config* :file-output))
