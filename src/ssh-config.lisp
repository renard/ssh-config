(defpackage #:ssh-config
  (:use #:cl)
  (:export
   #:generate-ssh-config))

(in-package #:ssh-config)

(defvar *ssh-options* '(
     "AddressFamily"
     "BatchMode"
     "BindAddress"
     "CanonicalDomains"
     "CanonicalizeFallbackLocal"
     "CanonicalizeHostname"
     "CanonicalizeMaxDots"
     "CanonicalizePermittedCNAMEs"
     "ChallengeResponseAuthentication"
     "CheckHostIP"
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
     "GSSAPIKeyExchange"
     "GSSAPIClientIdentity"
     "GSSAPIServerIdentity"
     "GSSAPIDelegateCredentials"
     "GSSAPIRenewalForcesRekey"
     "GSSAPITrustDns"
     "HashKnownHosts"
     "HostbasedAuthentication"
     "HostKeyAlgorithms"
     "HostKeyAlias"
     "HostName"
     "IdentitiesOnly"
     "IdentityFile"
     "IgnoreUnknown"
     "KbdInteractiveAuthentication"
     "KbdInteractiveDevices"
     "KexAlgorithms"
     "LocalCommand"
     "LocalForward"
     "LogLevel"
     "NoHostAuthenticationForLocalhost"
     "NumberOfPasswordPrompts"
     "PasswordAuthentication"
     "PermitLocalCommand"
     "PKCS11Provider"
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
     "TunnelDevice"
     "UsePrivilegedPort"
     "UserKnownHostsFile"
     "VerifyHostKeyDNS"
     "VisualHostKey"
     "XAuthLocation"))

(defun parse-yaml (yaml-file)
  (cl-yaclyaml:yaml-load-file yaml-file  :size-limit 65536))


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



(defun print-group (group-hash &key (stream t) parent prefix group)
  (let* ((group (or (gethash "group" group-hash) group))
	 (prefix (or (gethash "prefix" group-hash) prefix))
	 (hosts (gethash "hosts" group-hash)))
    (when group 
      (format stream "# ~a~%" group))
    (loop for host being the hash-keys of hosts
	  do (let ((host-conf (gethash host hosts)))
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
			      :prefix prefix))))))


(defun generate-ssh-config (&key (src "~/.ssh/hosts.yml") (dest "~/.ssh/config"))
  (let* ((yaml-hash (ssh-config::parse-yaml src))
	 (config (when yaml-hash (gethash "ssh-config" yaml-hash))))
    (with-open-file (stream dest :direction :output :if-exists :supersede)
      (format stream "# Generated from ~a~%# Do not edit manually~%~%" src)
      (when config
	(loop for item in config
	      do (print-group item :stream stream))))))
	 
    
