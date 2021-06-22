(define-module (rc home)
  #:use-module (rc home leaf)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:export (users
            homes
            current-user
            current-home

            leaf))

(define users
  (hash-fold (lambda (k v p) (cons k p))
	     '()
	     (module-submodules (current-module))))

(define-public homes 
  (filter-map
    (lambda (user)
      (if (eq? user 'factors) #f
          (cons (symbol->string user)
                (module-ref (hashq-ref (module-submodules (current-module)) user) 'env))))
    users))

(define (current-user) (getlogin))

(define (current-home) ((assoc-ref homes (current-user))))

(define (leaf os) ((assoc-ref homes "leaf") os))

(define-public (delay-inferiors) ; for stuff that moves way too fucking fast
  (let* ((channels
	   (list (channel
		   (name 'guix)
		   (url "https://git.savannah.gnu.org/git/guix.git")
		   (commit "d027858e70c4a37aca90b1d4ecb2f0421a95d987")
		   (introduction
		     (make-channel-introduction
		       "9edb3f66fd807b096b48283debdcddccfea34bad"
		       (openpgp-fingerprint "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
		 (channel
		   (name 'rde)
		   (url "https://git.sr.ht/~abcdw/rde")
		   (commit "6d40090ef287a4523fee32d4350b4dcd5fe90f0b"))
		 (channel
		   (name 'guix-gaming-games)
		   (url "https://gitlab.com/guix-gaming-channels/games.git")
		   (commit "b89dc67d0609b63c06d73a4b52d758380feb1373"))
		 (channel
		   (name 'nonguix)
		   (url "https://gitlab.com/nonguix/nonguix")
		   (commit "d81564f21e7d8800e6f6187fe2e1f6476e06bc30")
		   (introduction
		     (make-channel-introduction
		       "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
		       (openpgp-fingerprint "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
		 (channel
		   (name 'flat)
		   (url "https://github.com/flatwhatson/guix-channel.git")
		   (commit "9eeca8a9976d815234c03289fca4bedc9f2667d0"))))
	 (inferior (inferior-for-channels channels)))
    `((firefox . ,(first (lookup-inferior-packages inferior "firefox")))
      (emacs . ,(first (lookup-inferior-packages inferior "emacs-pgtk-native-comp"))))))

(define-public (delayed sym)
  (assoc-ref (delay-inferiors) sym))
