#lang racket/base

(provide dispatch file-not-found)

(require
  "utils.rkt"
  libuuid
  logger
  sha
  srfi/19
  racket/bool (only-in racket/date date-display-format
                                   date->seconds
                                   date*->seconds)
  racket/file racket/format racket/function racket/list racket/string
  web-server/dispatch
  web-server/servlet
  web-server/servlet-env)

(define (log-out req)
  (define ex "Thu, 01 Jan 1970 00:00:01 GMT")
  (redirect-to (url index-page) #:headers (list (cookie->header (make-cookie "username" "" #:expires ex))
                                                (cookie->header (make-cookie "session" "" #:expires ex)))))

(define (index-page req)
  (cond
    [(logged-in? req)
     (redirect-to (url* user-page (logged-in? req)))]
    [else
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head)
          (body
            (p "Welcome to Calenio")
            (form ([action ,(url login-page)] [method "post"])
              (input ([name "username"] [placeholder "username"] [type "text"]))
              (input ([name "password"] [placeholder "password"] [type "password"]))
              (input ([type "submit"])))
            (a ([href ,(url new-account)]) (p "New account"))
            (a ([href ,(url about-page)]) (p "About"))
            (a ([href ,(url for-developers-page)]) (p "For developers"))
            )))]))

(define (file-not-found _)
  (response/xexpr
    #:code 404 #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head)
       (body (p "file not found")
             (a ([href "/"]) "back to index")))))

(define (login-page req)
  (define username (get-post req 'username))
  (define password (get-post req 'password))
  (trce (request-client-ip req)) ; TODO Add this to a database of last created users with a count
  (cond
    [(empty-string? username)
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head)
          (body (p "no username provided")
                (a ([href ,(url index-page)]) "back to index"))))]
    [(not (user-exists? username))
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head)
          (body (p "user does not exist")
                (a ([href ,(url index-page)]) "back to index"))))]
    [(not (correct-password? username password))
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head)
          (body (p "wrong password")
                (a ([href ,(url index-page)]) "back to index"))))]
    [else
     (define session (create-or-fetch-session-key username))
     (redirect-to (url* user-page username)
       #:headers (list (cookie->header (make-cookie "username" username))
                       (cookie->header (make-cookie "session" session)))
       permanently)]
    ))

(define (new-account req)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head)
       (body
         (p "Create a new account")
         (form ([action "/new-account"] [method "post"])
           (input ([name "username"] [placeholder "username"] [type "text"]))
           (input ([name "password"] [placeholder "password"] [type "password"]))
           (input ([name "password*"] [placeholder "confirm password"] [type "password"]))
           (input ([type "submit"])))
         (a ([href "/"]) "back to index")
         ))))

(define (new-account-post req)
  (define username (get-post req 'username))
  (info username)
  (define password (get-post req 'password))
  (info password)
  (define password* (get-post req 'password*))
  (info password*)
  (cond
    [(not (equal? password password*))
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           `(html
              ,(common-head)
              (body (p "passwords do not match")
                    (a ([href ,(url new-account)]) "back to new user"))))]
    [(empty-string? username)
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           `(html
              ,(common-head)
              (body (p "no username provided")
                    (a ([href ,(url new-account)]) "back to new user"))))]
    [(user-exists? username)
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           `(html
              ,(common-head)
              (body (p "user already exists")
                    (a ([href ,(url new-account)]) "back to new user"))))]
    [(not (valid-username? username))
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           `(html
              ,(common-head)
              (body (p "username invalid - must be a sequence of characters excluding '/'")
                    (a ([href ,(url new-account)]) "back to new user"))))]
    [else
     ; (redirect-to <site> #:headers (list (cookie->header <your cookie>)) temporarily|permanent)
     (create-user username password)
     (define session (create-or-fetch-session-key username))
     (with-output-to-file (build-path username "css")
       (thunk (displayln ".navbar a { padding-right: 1em; }")))
     (with-output-to-file (build-path username "js")
       (thunk (displayln "")))
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           #:headers (list (cookie->header (make-cookie "username" username))
                           (cookie->header (make-cookie "session" session)))
           `(html
              ,(common-head)
              (body (p "account made")
                    (a ([href ,(url* user-page username)]) "click here to go to your account")
                    (a ([href ,(url index-page)]) "back to new user"))))]
  ))

(define (menu req)
  (define username (logged-in? req))
  `((div ([class "navbar"])
    ,(log-out-button req)
    ,(if (= (current-week) 0)
      `(a ([href ,(url* user-page-specific username 52 (sub1 (current-year)))]) "<<")
      `(a ([href ,(url* user-page-specific username (sub1 (current-week)))]) "<<"))
    (a ([href ,(url* user-page username)]) "now")
    ,(if (= (current-week) 52)
      `(a ([href ,(url* user-page-specific username 0 (add1 (current-year)))]) ">>")
      `(a ([href ,(url* user-page-specific username (add1 (current-week)))]) ">>"))
    (a ([href ,(url* add-entry)]) "add")
    (a ([href ,(url* settings-page)]) "settings"))))

(define (user-page req ar)
  (cond
    [(logged-in? req)
     (response/xexpr
       `(html
          (head
            (style ,(read-css (logged-in? req)) )
            )
          (body
            ,@(menu req)
            (div ([class "content"]) ,@(generate-user-page req ar))
            )))]
    [else
      (redirect-to (url index-page))]))

(define (user-page-specific req ar n [year (current-year)])
  (cond
    [(logged-in? req)
     (response/xexpr
       `(html
          (head
            (style ,(read-css (logged-in? req)) )
            )
          (body
            (div ([class "navbar"])
              ,(log-out-button req)
              ,(if (= n 0)
                `(a ([href ,(url* user-page-specific ar 52 (sub1 year))]) "<<")
                (if (= (current-year) year)
                  `(a ([href ,(url* user-page-specific ar (sub1 n))]) "<<")
                  `(a ([href ,(url* user-page-specific ar (sub1 n) year)]) "<<")))
              (a ([href ,(url* user-page ar)]) "now")
              ,(if (> (add1 n) 52)
                `(a ([href ,(url* user-page-specific ar 0 (add1 year))]) ">>")
                (if (= (current-year) year)
                  `(a ([href ,(url* user-page-specific ar (add1 n))]) ">>")
                  `(a ([href ,(url* user-page-specific ar (add1 n) year)]) ">>")))
              (a ([href ,(url* add-entry)]) "add")
              (a ([href ,(url* settings-page)]) "settings"))
            (div ([class "content"]) ,@(generate-user-page req ar n year))
            )))]
    [else
      (redirect-to (url index-page))]))

(define (generate-user-page req ar [week (current-week)] [year (current-year)])
  (cond
    [(string=? (logged-in? req) ar)
     (define now (current-date))
     (define root (build-path ar "calendar" (number->string year) (number->string week)))
     (with-handlers ([exn?
                       (lambda (exn)
                         (trce exn)
                         (list '(p "there are no plans this week")))])
       (define entries (directory-list root))
       (map (entry->html week year)
         (sort
           (filter identity
             (for/list ([entry entries])
               (with-input-from-file (build-path root entry)
                 (thunk
                   (define result (read))
                   (cond
                     [(hash? result) (list entry result)]
                     [else (warn `("File does not contain #hash: " ,entry)) #f])))))
           <
           #:key (lambda (x) (hash-ref (second x) 'from 0))))
       )
     ]
    [else
     '()])
  )

(define (about-page req)
  (response/xexpr
    `(html
       (body
         (h1 "What is Calenio?")
         (p "Calenio is a calendar application.")
         (h2 "What distinguishes Calenio from other services?")
         (p "Calenio lets the user choose the visuals and other extraneous functionality. Unlike most heavy calendar applications, basic Calenio offers a simple list of text. This makes Calenio extremely fast.")
         (h3 "How do I use Calenio?")
         (p "Make an account and start adding calendar entries into it!")
         (a ([href ,(url index-page)]) "back to index")
         ))))

(define (log-out-button req)
  (cond
    [(logged-in? req)
     `(a ([href ,(url log-out)]) "log out")]
    [else
     '()]
  ))

(define (for-developers-page req)
  (response/xexpr
    `(html
       (body
         ,(log-out-button req)
         (h1 "Calenio Public API")
         (div
           (p "Calenio is a REST-based application that communicates over HTTP.")
           (p "One first needs to get the username and session cookies: " (code "POST /login") " with POST data: " (code "username=<username>, password=<password>"))
           (h2 "After having logged in")
           (p "To add a calendar entry: " (code "GET /add/<from-date>/<to-date>") " an example: " (code "GET /add/2018-05-03T13:45:00/2018-05-03T:14:00:00"))
           )
         (a ([href ,(url index-page)]) "back to index")
         ))))

(define (today)
  (define now (seconds->date (current-seconds)))
  (string-append (~a #:align 'right #:left-pad-string "0" #:min-width 4 (number->string (date-year now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-month now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-day now)))))

(define (add-entry req)
  (cond
    [(logged-in? req)
     (response/xexpr
       `(html
          (head
            (style ,(read-css (logged-in? req)) )
            )
          (body
            (div ([class "navbar"]) ,@(menu req))
            (form ([action "/add"] [id "add-entry-form"] [method "post"])
            (textarea ([form "add-entry-form"] [name "description"] [placeholder "description"]))
            (p "from: ")
            (input ([name "from-date"] [placeholder "from-date"] [type "date"] [value ,(today)]))
            (input ([name "from-time"] [placeholder "from-time"] [type "time"] [value "00:00"]))
            (p "to: ")
            (input ([name "to-date"] [placeholder "to-date"] [type "date"] [value ,(today)]))
            (input ([name "to-time"] [placeholder "to-time"] [type "time"] [value "23:59"]))
            (input ([type "submit"]))
            ))))]
    [else
     (redirect-to (url not-logged-in))]
  ))

(define (add-entry-post req)
  (cond
    [(logged-in? req)
     (define username (logged-in? req))
     (define description (get-post req 'description))
     (define from-date (get-post req 'from-date))
     (define from-time (get-post req 'from-time))
     (define to-date (get-post req 'to-date))
     (define to-time (get-post req 'to-time))
     ;; Processing
     (warn (string-append from-date "T" from-time))
     (define from (string->date (string-append from-date "T" from-time) "~Y-~m-~dT~H:~M"))
     (define to (string->date (string-append to-date "T" to-time) "~Y-~m-~dT~H:~M"))
     (trce (date*->seconds to))
     (trce (date-week-number to 1))
     (warn from to)
     (erro description req)
     (define year (date-year from))
     (define week (date-week-number from 1))
     (define path (build-path username "calendar" (number->string year) (number->string week)))
     (make-directory* path)
     (with-output-to-file (build-path path (uuid-generate))
       (thunk (writeln (hash 'description description
                             'from (date->seconds from)
                             'to (date->seconds to)))))
     (redirect-to (url* user-page username))
     ]
    [else
     (redirect-to (url not-logged-in))]
  ))

(define (not-logged-in req)
  (response/xexpr
    `(html
       (body
         (p "you are not logged in")
         (a ([href "/"]) "go to index page"))))
  )
(define (settings-page req)
  (cond
    [(logged-in? req)
     (response/xexpr #:preamble #"<!DOCTYPE html>"
       `(html
          (head
            (style ,(read-css (logged-in? req)) )
            )
          (body
            (div ([class "navbar"]) ,@(menu req))
            (p "paste CSS/JS settings here")
            (form ([action ,(url* settings-post)] [id "form"] [method "post"])
              (textarea ([form "form"] [name "css"] [placeholder "CSS"]) ,(read-css (logged-in? req)))
              (textarea ([form "form"] [name "js"] [placeholder "Javascript"]))
              (input ([type "submit"]))
              )
            )))
     ]
    [else
     (redirect-to (url login-page))
     ])
  )

(define (settings-post req)
  (cond
    [(logged-in? req)
     (define css (get-post req 'css))
     (define js (get-post req 'js))
     (define path (build-path (logged-in? req)))
     (with-output-to-file (build-path path "css") #:exists 'replace
       (thunk (displayln css)))
     (with-output-to-file (build-path path "js") #:exists 'replace
       (thunk (displayln js)))
     (redirect-to (url* user-page (logged-in? req)))]
    [else
     (redirect-to (url login-page))])
  )

(define (test-method req)
  (trce req)
  (response/xexpr #:preamble #"<!DOCTYPE html>"
    `(html
       (body (p "Strong")))))

(define (login-barrier req)
  (cond
    [(and (logged-in? req) (bytes=? (request-method req) #"POST"))
     (dispatch* req)]
    [(logged-in? req)
     (dispatch* req)]
    [else
     (redirect-to (url not-logged-in))]))

(define (delete-entry req week year uuid)
  (trce uuid)
  (redirect-to (url index-page)))

(define-values (dispatch* url*)
  (dispatch-rules
    (["add"]             add-entry)
    (["add"]                                       #:method "post" add-entry-post)
    (["delete" (number-arg) (number-arg) (string-arg)] #:method "post"
     delete-entry)
    (["u"  (string-arg)]                           user-page)
    (["u" (string-arg) (number-arg)]               user-page-specific)
    (["u" (string-arg) (number-arg) (number-arg)]  user-page-specific)
    (["settings"]                                  settings-page)
    (["settings"]                                  #:method "post" settings-post)
    ))

(define-values (dispatch url)
  (dispatch-rules
    (("login")          #:method "post" login-page)
    (("not-logged-in")  #:method "get" not-logged-in)
    (("about")           #:method "get" about-page)
    (("developers")      #:method "get" for-developers-page)
    (("new-account")    #:method "get" new-account)
    (("new-account")     #:method "post" new-account-post)
    (("log-out")         #:method "get" log-out)
    (("")                #:method "get" index-page)
    (else login-barrier)))

