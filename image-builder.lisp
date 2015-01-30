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



(defun install-quicklisp ()
  ""
  (let* ((path "build/quicklisp")
	 (quicklisp-init (merge-pathnames "setup.lisp" path))
	 (asdf:*central-registry* (list *default-pathname-defaults*)))
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
	  (funcall (intern "INSTALL" "QUICKLISP-QUICKSTART") :path path)))))


(defun clone-git ()
  (asdf:run-shell-command "git clone https://github.com/mabragor/esrap-liquid.git build/quicklisp/local-projects/esrap-liquid")
  (asdf:run-shell-command "git clone https://github.com/mabragor/cl-yaclyaml.git build/quicklisp/local-projects/cl-yaclyaml"))


(defun load-project (projects)
  ""
  (format t "Loading build/quicklisp/setup.lisp~%")
  (load "build/quicklisp/setup.lisp")
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

(defun write-image (entry-point)
  ""
  (let ((entry-function (string-to-function-symbol entry-point)))
    #+sbcl
    (progn
      (format t "Compiling image for SBCL ~a~%" entry-function)
      (sb-ext:save-lisp-and-die
       "image.sbcl.exe"
       :purify t :executable t :compression t
       :toplevel #'(lambda() (funcall entry-function sb-ext:*posix-argv*))))

    #+ccl
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
