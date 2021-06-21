(define-module (rc system factors guix)
  #:use-module (gcrypt pk-crypto)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:export (modify-guix-service))

(define (modify-guix-service services)
  (modify-services services
    (guix-service-type config =>
                       (guix-configuration
                         (inherit config)
                         (substitute-urls
                           (append
                             (list "https://bordeaux.guix.gnu.org"
                                   "https://mirror.brielmaier.net")
                             %default-substitute-urls))
                         (authorized-keys
                           (append
                             %default-authorized-guix-keys
                             (list (plain-file
                                     "delta.pub"
                                     (canonical-sexp->string
                                       (sexp->canonical-sexp
                                         '(public-key
                                            (ecc
                                              (curve Ed25519)
                                              (q #vu8(177 124 150 128 236 7 193 6
                                                      46 162 178 109 30 193 60 5
                                                      113 105 163 220 184 241 42 113
                                                      88 88 131 115 240 26 162 224))))
                                         )))
                                   (plain-file
                                     "mirror.brielmair.net.pub"
                                     (canonical-sexp->string
                                       (sexp->canonical-sexp
                                         '(public-key
                                            (ecc
                                              (curve Ed25519)
                                              (q #vu8(117 20 248 215 41 219 25 53
                                                      71 10 88 28 227 133 30 217
                                                      253 111 31 155 175 225 216 190
                                                      199 122 147 26 219 122 67 55))))
                                         )))
                                   (plain-file
                                     "bordeaux.guix.gnu.org.pub"
                                     (canonical-sexp->string
                                       (sexp->canonical-sexp
                                         '(public-key
                                            (ecc
                                              (curve Ed25519)
                                              (q #vu8(125 96 41 2 211 162 219 184
                                                      63 138 15 185 134 2 167 84
                                                      197 73 59 11 119 140 141 29
                                                      212 224 244 29 225 77 227 79))))
                                         ))))
                             (list)))))))
