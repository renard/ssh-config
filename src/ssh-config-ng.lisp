;;

(defpackage #:ssh-config
  (:use #:cl)
  (:export #:generate-ssh-config
	   #:*ssh-options*
	   #:*ssh-options-extra*))

(in-package #:ssh-config)


(defparameter *ssh-options*
  '("AddressFamily"
    "BatchMode"
    "BindAddress"
    "CanonicalDomains"
    "CanonicalizeFallbackLocal"
    "CanonicalizeHostname"
    "CanonicalizeMaxDots"
    "CanonicalizePermittedCNAMEs"
    "ChallengeResponseAuthentication"
    "CheckHostIP"
    "Cipher"
    "Ciphers"
    "ClearAllForwardings"
    "Compression"
    "CompressionLevel"
    "ConnectionAttempts"
    "ConnectTimeout"
    "ControlMaster"
    "ControlPath"
    "ControlPersist"
    "DynamicForward"
    "EnableSSHKeysign"
    "EscapeChar"
    "ExitOnForwardFailure"
    "ForwardAgent"
    "ForwardX11"
    "ForwardX11Timeout"
    "ForwardX11Trusted"
    "GatewayPorts"
    "GlobalKnownHostsFile"
    "GSSAPIAuthentication"
    "GSSAPIClientIdentity"
    "GSSAPIDelegateCredentials"
    "GSSAPIKeyExchange"
    "GSSAPIRenewalForcesRekey"
    "GSSAPIServerIdentity"
    "GSSAPITrustDns"
    "HashKnownHosts"
    "Host"
    "HostbasedAuthentication"
    "HostKeyAlgorithms"
    "HostKeyAlias"
    "HostName"
    "IdentitiesOnly"
    "IdentityFile"
    "IgnoreUnknown"
    "IPQoS"
    "KbdInteractiveAuthentication"
    "KbdInteractiveDevices"
    "KexAlgorithms"
    "LocalCommand"
    "LocalForward"
    "LogLevel"
    "MACs"
    "Match"
    "NoHostAuthenticationForLocalhost"
    "NumberOfPasswordPrompts"
    "PasswordAuthentication"
    "PermitLocalCommand"
    "PKCS11Provider"
    "Port"
    "PreferredAuthentications"
    "Protocol"
    "ProxyCommand"
    "ProxyUseFdpass"
    "PubkeyAuthentication"
    "RekeyLimit"
    "RemoteForward"
    "RequestTTY"
    "RhostsRSAAuthentication"
    "RSAAuthentication"
    "SendEnv"
    "ServerAliveCountMax"
    "ServerAliveInterval"
    "StrictHostKeyChecking"
    "TCPKeepAlive"
    "Tunnel"
    "TunnelDevice"
    "UsePrivilegedPort"
    "User"
    "UserKnownHostsFile"
    "VerifyHostKeyDNS"
    "VisualHostKey"
    "XAuthLocation")
  "List of recognized ssh_config(5) options."
  )

(defparameter *ssh-options-extra*
  '("group" "name" "prefix" "hosts")
  "")

(defmacro make-ssh-config-struct()
  (let ((opt-list (loop for opt in ssh-config::*ssh-options*
			collect (list
				 (intern (string-upcase opt) "KEYWORD")
				 nil)))
	(other-options (loop for opt in ssh-config::*ssh-options-extra*
			  collect (list (intern (string-upcase opt) "KEYWORD")
					nil))))
  `(defstruct ssh-opt ,@other-options ,@opt-list)))




(defun print-ssh-config (conf &key (indent "") (stream t))
  (when (ssh-opt-name conf)
    (format stream "~aHost ~(~:[~a~;~:*~a~a~:[~; ~3:*~a~*~a~]~] ~:*~a~)~%" indent
	    (ssh-opt-prefix conf)
	    (ssh-opt-name conf)
	    (ssh-opt-hostname conf)
	    )
    (loop for opt in *ssh-options*
	  for opt-symb = (intern (string-upcase opt) "KEYWORD")
	  for value = (slot-value conf opt-symb)
	  when (and value (not (eq :host opt-symb)))
	    do (format stream "~a~4T~a ~a~%" indent opt
		       (cond
			 ((eq (type-of value) 'symbol)
			  (format nil "~(~a~)" value))
			 (t value))))))


(defun print-ssh-config-debug (conf &key (indent "") (stream t))
  (if conf
      (loop for opt in (append *ssh-options* *ssh-options-extra*)
	 for opt-symb = (intern (string-upcase opt) "KEYWORD")
	 for value = (slot-value conf opt-symb)
	 when (and value (not (eq :host opt-symb)))
	 do (format stream "#- ~a~4T~a ~a~%" indent opt
		    (cond
		      ((eq (type-of value) 'symbol)
		       (format nil "~(~a~)" value))
		      (t value))))
      (format stream "#- ~a~%" conf))
)



(defun load-configuration(file)
  "Load IMAGE-BUILDER configuration from FILE and return a CONFIGURATION
structure."
  (with-open-file (stream file :external-format :utf-8)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (handler-case
	  (read-from-string data)
	(condition (c)
	(format t "Fatal: Loading ~a: ~a~%" file c)
	  (if ssh-config::*debug*
	    (invoke-debugger c)
	    (uiop:quit 1)))))))
    

(defun generate-host (host)
  (let ((host-copy (loop for (k v) on host by #'cddr
		      when  (not (member k '( :hosts)))
		      nconc (list k v))))
    (apply #'make-ssh-opt host-copy)))



(defun merge-hosts (a b)
  ;; (format t "a: ~a~%" (type-of a))
  ;; (format t "b: ~a~%" (type-of b))
  ;; (format t "~%~%--- a ---~%")
  ;; (print-ssh-config-debug a)
  ;; (format t "--- b ---~%")
  ;; (print-ssh-config-debug b)
  (when (ssh-opt-p a)
    (loop for opt in (append *ssh-options* *ssh-options-extra*)
       for slot = (intern (string-upcase opt) "KEYWORD")
       when (and
	     (not (member slot '(:Port)))
	     (not (slot-value b slot))
	     (slot-value a slot))
       do (progn
	    ;; (format t "## Merging ~(~a~) ~((~a)~) -> ~((~a)~)~%"
	    ;; 	    slot
	    ;; 	    (slot-value b slot)
	    ;; 	    (slot-value a slot))
	    (setf (slot-value b slot) (slot-value a slot)))
	 ))
  ;; (format t "--- A ---~%")
  ;; (print-ssh-config-debug a)
  ;; (format t "--- B ---~%")
  ;; (print-ssh-config-debug b)
  ;; (format t "--- - ---~%~%")
  b)

(defun host-to-ssh-conf (hosts &key (parent nil) (group nil) (stream t) (indent ""))
  (loop for host in hosts
     do (progn
	  (if (getf host :group)
	      (let ((this-group  (generate-host host)))
		(when (and ssh-config::*debug* (getf host :group))
		  (format t "~a~(~a (group)~:[~; <- ~:*~a~]~)~%" indent (getf host :group) (if group (ssh-opt-group group) "NOGROUP")))
		(format stream "###~%### group ~a~%###~%~%" (slot-value this-group :group))
		(host-to-ssh-conf (getf host :hosts)
				:parent parent
				:group (merge-hosts group this-group)
				:indent (format nil "  ~a" indent)
				:stream stream))
	  

	   (let ((new-host (generate-host host)))
	     (when parent
	       (when ssh-config::*debug* 
		 (format t "~a~(~a (host)~:[~; <- ~:*~a~]~)~%" indent (getf host :name) (if group (ssh-opt-name parent) "NOGROUP")))
	       (setf (slot-value new-host :ProxyCommand)
		     (or (slot-value new-host :ProxyCommand)
			 (format
			  nil
			  "ssh -o ConnectTimeout=10 -q -t ~(~:[~a~;~:*~a~a~]~) nc -w 60 %h %p"
			  (or (slot-value group :Prefix)
			      (ssh-opt-prefix parent))
			  (or (ssh-opt-name parent) (ssh-opt-hostname parent))))))
	     
	    (print-ssh-config (merge-hosts group  new-host) :stream stream)
	    (format stream "~%")


	    (if
	     (getf host :hosts)
	     (let ((this-parent  (generate-host host)))
	       ;(format stream "# host list~%")
	       (host-to-ssh-conf (getf host :hosts)
				 :group group
				 :parent this-parent
				 :indent (format nil "  ~a" indent)
				 :stream stream)))

	   
	    )))))
;; (defun host-to-ssh-conf (hosts &key (parent nil) (stream t))
;;   (loop for host in hosts
;; 	if (and host (getf host :hosts))
;; 	  do (let ((this-parent  (generate-host host)))
;; 	       (when (ssh-opt-name this-parent))
;; 		 (print-ssh-config this-parent :stream stream)
;; 		 (format stream "~%")
;; 	       ;; (setq this-parent (merge-hosts parent this-parent))
;; 	       (when (ssh-opt-group parent)
;; 		 (setf (slot-value this-parent :PROXYCOMMAND)
;; 		       (format nil "ssh -o ConnectTimeout=10 -q -t ~(~a~) nc -w 60 %h %p" (ssh-opt-name parent)))
;; 	       )
	       
;; 	       (host-to-ssh-conf (getf host :hosts) :parent this-parent :stream stream))
;; 	else
;;      do (when host
;; 	  (let ((h (generate-host host)))
;; 	    (setq h (merge-hosts parent h))
;; 	    (print-ssh-config h :stream stream)
;; 	    (format stream "~%")))))



(defun generate-ssh-config (&key (src "~/.ssh/hosts.lisp") (dest "~/.ssh/config"))
  (let ((hosts (load-configuration src))
	(tmp-dest (format nil "~a;tmp" dest)))
    (when ssh-config::*verbose*
      (format t "Loading definition from ~a~%" src)
      (format t "Writing ssh configuration to ~a~%" dest))

    (handler-case
	(with-open-file (stream tmp-dest
			 :direction :output :if-exists :supersede)
	  (format stream "# Generated from ~a~%# Do not edit manually~%~%" src)
	  (host-to-ssh-conf hosts :stream stream))
      (condition (c)
	(format t "Fatal: ~a~%" c)
	(if ssh-config::*debug*
	    (invoke-debugger c)
	    (uiop:quit 1))))
    
    (rename-file (truename tmp-dest) dest)))



;; (defun generate-ssh-config()
;;   (host-to-ssh-conf
;;    (load-configuration #P"/Users/renard/.ssh/hosts.lisp"))
;;   )
;; (host-to-ssh-conf (load-configuration #P"/Users/renard/Src/ssh-config/src/host.lisp"))

;; (with-open-file (stream #P"/tmp/ssh.config" :direction :output :if-exists :supersede)
;;   (host-to-ssh-conf
;;    (load-configuration #P"/tmp/hosts.lisp")
;;    :stream stream))
