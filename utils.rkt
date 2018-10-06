#lang racket/base

(provide (all-defined-out))

(require
  libuuid
  logger
  sha
  srfi/19
  racket/bool (only-in racket/date date-display-format
                                   date->seconds
                                   date*->seconds)
  racket/file racket/function racket/list racket/string
  web-server/dispatch
  web-server/servlet
  web-server/servlet-env)

(define (common-head)
  '(head
     (meta ([charset "UTF-8"]))
     (meta ([content "The simple online calendar"] [name "description"]))
     (title "Calenio")))

(define (get-cookie name cookies)
  (define result
    (findf (lambda (c)
             (string=? name (client-cookie-name c)))
           cookies))
  (cond
    [result (client-cookie-value result)]
    [else   #f]))

(define (pairfind pairs symbol)
  (cond
    ([empty? pairs]
     #f)
    ([symbol=? (car (first pairs)) symbol]
     (cdr (first pairs)))
    (else
     (pairfind (rest pairs) symbol))))

(define (get-post req sym)
  (pairfind (request-bindings req) sym))

(define (correct-password? username password)
  (with-input-from-file (build-path username "pwhash")
    (thunk (equal? (read) (sha512 (string->bytes/utf-8 password))))
  ))

(define (create-or-fetch-session-key username)
  (define path (build-path username "session"))
  (cond
    [(file-exists? path)
     (with-input-from-file path
       (thunk (read)))]
    [else
     (define uuid (uuid-generate))
     (with-output-to-file path
       (thunk (writeln uuid)))
     uuid]))

(define (empty-string? str)
  (not (non-empty-string? str)))

(define (user-exists? username)
  (with-handlers ([exn:fail:contract?
                    (lambda (exn)
                      (erro exn)
                      #f)])
    (string->path-element username)
    (directory-exists? username)
  ))

;; TODO cleanse path name
(define (create-user username password)
  (with-handlers ([exn:fail:contract?
                    (lambda (exn)
                      (erro exn)
                      #f)])
    (define path-element (string->path-element username))
    (make-directory username)
    (with-output-to-file (build-path path-element "pwhash")
      (thunk
        (write (sha512 (string->bytes/utf-8 password)))
        ))
    )
  )

(define (valid-username? str)
  (with-handlers ([exn:fail:contract?
                    (lambda (exn)
                      (erro exn)
                      #f)])
    (string->path-element str)))

(define (seconds->datestring seconds)
  (date->string (seconds->date seconds)))

(define (entry->html con)
  (define file (first con))
  (define entry (second con))
  (define description (hash-ref entry 'description #f))
  (define from (hash-ref entry 'from #f))
  (define to (hash-ref entry 'to #f))
  `(p ,(seconds->datestring from) " - " ,(seconds->datestring to) ": " ,description
      " " (a ([href ,(path->string file)]) "invite")
      (a ([href ,(path->string file)]) "delete")
      )
  )

(define (current-year)
  (define now (seconds->date (current-seconds)))
  (date-year now)
  )

(define (current-week)
  (define now (seconds->date (current-seconds)))
  (date-week-number now 1)
  )

(define (logged-in? req)
  (define username (get-cookie "username" (request-cookies req)))
  (define session (get-cookie "session" (request-cookies req)))
  (cond
    [(and username session (user-exists? username))
     (if
       (with-input-from-file (build-path username "session")
         (thunk (equal? (read) session)))
       username
       #f)]
    [else
      #f]))

