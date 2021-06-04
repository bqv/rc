(define-module (rc home leaf)
	       #:use-module (gnu home)
	       #:use-module (gnu home-services)
	       #:use-module (gnu home-services ssh)
	       #:use-module (gnu home-services shells)
	       #:use-module (gnu home-services files)
	       #:use-module (gnu services)
	       #:use-module (gnu packages admin)
	       #:use-module (guix gexp)
	       #:export (env))

(define (env)
  (home-environment
    (home-directory "/home/leaf")
   ;(symlink-name ".guix-home-env")
    (packages (list htop))
    (services
      (list
	(service home-bash-service-type
		 (home-bash-configuration
		   (guix-defaults? #t)
		   (bash-profile '("\
				   export HISTFILE=$XDG_CACHE_HOME/.bash_history"))))

	(simple-service 'test-config
			home-files-service-type
			(list `("config/test.conf"
				,(plain-file "tmp-file.txt"
					     "the content of ~/.config/test.conf"))))

	(service home-ssh-service-type
		 (home-ssh-configuration
		   (extra-config
		     (list
		       (ssh-include "config.*")
		       (ssh-host "savannah"
				 '((compression . #f)))))))
	(service home-gnupg-service-type
		 (home-gnupg-configuration
		   (gpg-agent-config
		     (home-gpg-agent-configuration
		       (ssh-agent? #t)))))
	))))
