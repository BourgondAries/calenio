#lang racket/base

(provide (all-defined-out))


(require
  (for-syntax racket/base)
  libuuid
  logger
  sha
  srfi/19
  racket/bool (only-in racket/date date-display-format
                                   date->seconds
                                   date*->seconds)
  racket/file racket/format racket/function racket/list racket/string
  syntax/parse/define
  web-server/dispatch
  web-server/servlet
  web-server/servlet-env)

(define (today)
  (define now (seconds->date (current-seconds)))
  (string-append (~a #:align 'right #:left-pad-string "0" #:min-width 4 (number->string (date-year now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-month now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-day now)))))

(define (user-dir username)
  (build-path "user" username))

(define (common-head req)
  `(head
     (meta ([charset "UTF-8"]))
     (meta ([content "The simple online calendar"] [name "description"]))
     (title "Calenio")
     ,@(if (logged-in? req)
       `(
         (link ([href "/css"] [rel "stylesheet"] [type "text/css"]))
         (script ([src "/js"]))
         )
        '()
     )))

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
  (with-input-from-file (build-path (user-dir username) "pwhash")
    (thunk (equal? (read) (sha512 (string->bytes/utf-8 password))))
  ))

(define (read-css req)
  (with-handlers ([exn:fail:filesystem?
                    (lambda (exn)
                      (erro "unable to read file")
                      "")])
    (if (logged-in? req)
      (file->string (build-path (user-dir (logged-in? req)) "css"))
      (file->string (build-path "settings" "default.css")))
  ))

(define (read-js req)
  (with-handlers ([exn:fail:filesystem?
                    (lambda (exn)
                      (erro "unable to read file")
                      "")])
    (if (logged-in? req)
      (file->string (build-path (user-dir (logged-in? req)) "js"))
      (file->string (build-path "settings" "default.js")))
  ))

(define (create-or-fetch-session-key username)
  (define path (build-path (user-dir username) "session"))
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
    (directory-exists? (user-dir username))
  ))

;; TODO cleanse path name
(define (create-user username password)
  (with-handlers ([exn:fail:contract?
                    (lambda (exn)
                      (erro exn)
                      #f)])
    (define path-element (string->path-element username))
    (make-directory* (user-dir path-element))
    (with-output-to-file (build-path (user-dir path-element) "pwhash")
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

(define ((entry->html username week year) con)
  (define file (first con))
  (define entry (second con))
  (define description (hash-ref entry 'description #f))
  (define from (hash-ref entry 'from #f))
  (define to (hash-ref entry 'to #f))
  `(div ([class "content"])
     (span ([class "from"]) ,(seconds->datestring from))
     " - "
     (span ([class "to"]) ,(seconds->datestring to))
     ": "
     (span ([class "description"]) ,description)
      " " (a ([href ,(path->string (build-path "/" "invite" username (number->string week) (number->string year) file))]) "invite")
      " "
     (button ([class "delete-button"] [form ,(path->string file)] [type "submit"] [value "delete"]) "delete")
     (form
       ([action ,(path->string (build-path "/" "delete" (number->string week) (number->string year) file))] [id ,(path->string file)] [method "post"])
     ; (input ([class "delete-button"] [type "submit"] [value "delete"]))
     )
   )
  )

(define (acsrf req)
  req ; TODO implement anti csrf
  )

(define (current-year)
  (define now (seconds->date (current-seconds)))
  (date-year now)
  )

(define (current-month)
  (define now (seconds->date (current-seconds)))
  (date-month now)
  )

(define (current-day)
  (define now (seconds->date (current-seconds)))
  (date-day now)
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
       (with-input-from-file (build-path (user-dir username) "session")
         (thunk (equal? (read) session)))
       username
       #f)]
    [else
      #f]))

(define (date->date-string date)
  (string-append
    (~a #:align 'right #:left-pad-string "0" #:min-width 4 (date-year date))
    "-"
    (~a #:align 'right #:left-pad-string "0" #:min-width 2 (date-month date))
    "-"
    (~a #:align 'right #:left-pad-string "0" #:min-width 2 (date-day date)))
  )

(define (date->time-string date)
  (string-append
    (~a #:align 'right #:left-pad-string "0" #:min-width 2 (date-hour date))
    ":"
    (~a #:align 'right #:left-pad-string "0" #:min-width 2 (date-minute date))
  ))

(define-syntax-parser bind
  ([_ request:expr name:id ...+]
   #'(begin
       (define name (get-post request 'name)) ...)))
