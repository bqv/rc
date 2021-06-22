(define-module (guix scripts format)
  #:use-module (guix ui)
  #:use-module (guix grafts)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix scripts)
  #:use-module (guix scripts package)
  #:use-module (guix scripts build)
  #:use-module (guix scripts system search)
  #:use-module ((guix status) #:select (with-status-verbosity))
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-37)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:export (guix-format))

(define (show-help)
  (display (G_ "Usage: ... | guix format
Format stdin and return it pretty-printed to stdout\n"))
    (newline)
  (show-bug-report-information))


;; Helper methods for maintaining comments and whitespace.
(define (copy-line-comment)
  (let ((char (read-char)))
    (if (not (eof-object? char))
      (if (eq? char #\newline)
        (newline)
        (begin (write-char char) (copy-line-comment))))))
(define (maintain-empty-lines)
  (let ((char1 (read-char)) (char2 (peek-char)))
    (if (and (eq? char1 #\newline) (eq? char2 #\newline))
      (write-char (read-char)))))

;; The main method. This reads from and writes to stdin/stdout.
(define (scmfmt)
  (let ((char (peek-char)))
    (if (not (eof-object? char))
      (begin
        (cond ((eq? char #\;) (copy-line-comment))
              ((eq? char #\newline) (maintain-empty-lines))
              ((char-whitespace? char) (read-char))
              (#t (pretty-print (read))))
        (scmfmt)))))


;; Cmd
(define-command (guix-format . args)
  (synopsis "format guile scheme code")

  (with-error-handling
    (format #f "~A" args)
    (scmfmt)))
