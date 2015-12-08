(defpackage #:ssh-config
  (:use #:cl)
  (:export
   #:generate-ssh-config))

(in-package #:ssh-config)


;; Automatically generated using
;; zgrep '^\.It Cm' /usr/share/man/man5/ssh_config.5.gz  | sed 's/^.It Cm \(.*\)/    "\1"/' | sort
(defvar *ssh-options*
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

;; (defun parse-yaml (yaml-file)
;;   (format t "Parsing configuration file ~a~%" yaml-file)
;;   (let ((yaml (cl-yaclyaml:yaml-load-file yaml-file  :size-limit 65536)))
;;     (format t "done~%")
;;     yaml))


;; (defun hash->plist (item)
;;   "Convert ITEM to a PLIST representation"
;;   (cond
;;     ((listp item) (loop for i in item collect (hash->plist i)))
;;     ((hash-table-p item)
;;      (loop for k being the hash-keys of item
;; 	   for v = (gethash k item)
;; 	   for kw = (intern (string-upcase  k) "KEYWORD")
;; 	   nconc (list kw (hash->plist v))))
;;     (t item)))



(defun print-group (group-hash &key (stream t) parent prefix group (indent ""))
  (let* ((group (or (gethash "group" group-hash) group))
	 (prefix (or (gethash "prefix" group-hash) prefix))
	 (hosts (gethash "hosts" group-hash)))
    (when group 
      (format stream "# ~a~%" group))
    (loop for host being the hash-keys of hosts
	  do (let ((host-conf (gethash host hosts)))
	       (progn
		 (princ (format nil "~a~a~%" indent host))
		 (finish-output))
	       ;;(force-output *standard-output*)
	       ;;(force-output t)
	       (format stream "Host ~:[~a~;~:*~a~a~:[~; ~3:*~a~*~a~]~]~%" prefix host
		       (if (hash-table-p host-conf)
			   (gethash "HostName" host-conf)
			   host-conf))
	       (cond
	       	 ((hash-table-p host-conf)
	       	  (loop for conf-opt being the hash-keys of host-conf
			when (member conf-opt *ssh-options* :test #'string= )
	       		do (format stream "~4T~a ~a~%" conf-opt
				   (gethash conf-opt host-conf))))
		 (host-conf (format stream "~4THostName ~a~%" host-conf)))
	       (when parent
		 (format stream
			 "~4TProxyCommand ssh -o ConnectTimeout=5 -q -t ~a nc -w 60 %h %p~%"
			 parent))

	       (format stream "~%")
	       (when (and (hash-table-p host-conf)
			  (gethash "hosts" host-conf))
		 (print-group host-conf
			      :stream stream
			      :parent (or (gethash "HostName" host-conf)
					  (format nil "~:[~a~;~:*~a~a~]" prefix host))
			      :prefix prefix
			      :indent (format nil "~a  " indent)))))))


;; (defun generate-ssh-config (&key (src "~/.ssh/hosts.yml") (dest "~/.ssh/config"))
;;   (let* ((yaml-hash (ssh-config::parse-yaml src))
;; 	 (config (when yaml-hash (gethash "ssh-config" yaml-hash))))
;;     (with-open-file (stream dest :direction :output :if-exists :supersede)
;;       (format stream "# Generated from ~a~%# Do not edit manually~%~%" src)
;;       (when config
;; 	(loop for item in config
;; 	      do (print-group item :stream stream))))))
	 
    


(defun load-configuration(file)
  "Load IMAGE-BUILDER configuration from FILE and return a CONFIGURATION
structure."
  (with-open-file (stream file :external-format :utf-8)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      (read-from-string data))))


(defun parse-ssh-lisp (hosts &key (stream t) user parent prefix group (indent ""))
  (loop for host in hosts
	do (let ((group (or (getf host :group) group))
		 (prefix (or (getf host :prefix) prefix)))
	     (when (and ssh-config::*debug* (getf host :group))
	       (format t "~a~(~a~:[~; <- ~:*~a~]~)~%" indent group parent))
	     
	     (when (getf host :name)
	       (let ((hostname (format nil "~(~a~)" (getf host :name))))
		 (when ssh-config::*debug* (format t "~a~a~%" indent hostname))
		 (format stream "Host ~(~:[~a~;~:*~a~a~:[~; ~3:*~a~*~a~]~]~:[~; ~:*~a~]~)~%"
			 prefix hostname
			 (or (getf host :HostName) hostname)
			 (getf host :HostName))
		 (loop for option in *ssh-options*
		       for kw = (intern (string-upcase option) "KEYWORD")
		       for val = (getf host kw)
		       when val
			 do (format stream "~4T~a ~a~%" option
				    (cond
				      ((eq (type-of val) 'symbol)
				       (format nil "~(~a~)" val))
				      (t val))))
		 (when user
		   (format stream "~4TUser ~a~%" user))
		 (when (and parent (not (getf host :ProxyCommand)))
		   (format stream
			   "~4TProxyCommand ssh -o ConnectTimeout=~a -q -t ~(~:[~;~:*~a~]~a~) nc -w ~a %h %p~%"
			   ssh-config::*pc-timeout* prefix parent
			   ssh-config::*nc-timeout*))))
	     (format stream "~%")
	       ;; Recursive host definition
	     (cond
	       ((listp (getf host :hosts))
		(parse-ssh-lisp (getf host :hosts)
				:stream stream
				:parent (or
					 (when (getf host :name)
					   (format nil "~(~a~)" (getf host :name)))
					 parent)
				:user (or
				       (when (getf host :User)
					 (format nil "~(~a~)" (getf host :User)))
				       user)
				:prefix prefix
				:group group
				:indent (format nil "  ~a" indent))))
)))

;;asdcasd

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
	  (parse-ssh-lisp hosts :stream stream))
      (condition (c)
	(format t "Fatal: ~a~%" c)
	(if ssh-config::*debug*
	    (invoke-debugger c)
	    (uiop:quit 1))))
    
    (rename-file (truename tmp-dest) dest)))


;; (defun get-from-lisp(file)
;;   (let* ((yaml-hash (ssh-config::parse-yaml file)))
;;     (hash->plist yaml-hash)))
	 

  ;; (let ((lisp (load-configuration file)))
  ;;   (parse-ssh-lisp lisp)))
