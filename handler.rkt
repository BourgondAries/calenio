#lang racket/base

(provide dispatch file-not-found)

(require
  (for-syntax racket/base)
  "utils.rkt"
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

(define (log-out req)
  (define expire-1970 "Thu, 01 Jan 1970 00:00:01 GMT")
  (redirect-to
    (url index-page)
    #:headers
      (list
        (cookie->header (make-cookie "username" "" #:expires expire-1970))
        (cookie->header (make-cookie "session"  "" #:expires expire-1970)))))

(define (index-page req)
  (cond
    [(logged-in? req)
     (redirect-to (url* user-page (logged-in? req)))]
    [else
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head req)
          (body
            (p "Welcome to Calenio")
            (form ([action ,(url login-page)] [method "post"])
              (input ([name "username"] [placeholder "username"] [type "text"]))
              (input ([name "password"] [placeholder "password"] [type "password"]))
              (input ([type "submit"] [value "Log in"])))
            (p "For more information"
              (ul
                (li (a ([href ,(url new-account)]) "New account"))
                (li (a ([href ,(url about-page)]) "About"))
                " "
                (li (a ([href ,(url for-developers-page)]) "For developers"))))
            )))]))

(define (file-not-found req)
  (response/xexpr
    #:code 404 #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p "file not found")
             (a ([href "/"]) "back to index")))))

(define (error-response-login req message)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p ,message)
             (a ([href ,(url index-page)]) "back to login page")))))

(define (login-page req)
  (bind req username password)
  (trce (request-client-ip req)) ; TODO Add this to a database of last created users with a count
  (cond
    [(empty-string? username)
     (error-response-login req "no username provided")]
    [(not (user-exists? username))
     (error-response-login req "user does not exist")]
    [(not (correct-password? username password))
     (error-response-login req "wrong password")]
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
       ,(common-head req)
       (body
         (p "Create a new account")
         (form ([action "/new-account"] [method "post"])
           (input ([name "username"] [placeholder "username"] [type "text"]))
           (input ([name "password"] [placeholder "password"] [type "password"]))
           (input ([name "password*"] [placeholder "confirm password"] [type "password"]))
           (input ([type "submit"])))
         (p (a ([href "/"]) "back to index"))
         ))))

(define (error-response req message)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p ,message)
             (a ([href ,(url new-account)]) "back to new user")))))

(define (new-account-post req)
  (bind req username password password*)
  (cond
    [(not (string=? password password*))
     (error-response req "passwords do not match")]
    [(> (string-length password) 2048)
     (error-response req "password too long (max 2048 characters)")]
    [(empty-string? username)
     (error-response req "no username provided")]
    [(> (string-length username) 200)
     (error-response req "username too long (max 200 characters)")]
    [(user-exists? username)
     (error-response req "user already exists")]
    [(not (valid-username? username))
     (error-response req "username invalid - must be a sequence of characters excluding '/'")]
    [else
     (create-user username password)
     (define session (create-or-fetch-session-key username))
     (define base (build-path "file" "user" username))
     (make-directory* base)
     (with-output-to-file (build-path base "css")
       (thunk (displayln ".navbar > a { padding-right: 0em; }\n.content .description { color: #39CCCC; }\n.content .from { color: #3D9970; }\n.content .to { color: #FF851B; }\n")))
     (with-output-to-file (build-path base "js")
       (thunk (displayln "")))
     (response/xexpr
           #:preamble #"<!DOCTYPE html>"
           #:headers (list (cookie->header (make-cookie "username" username))
                           (cookie->header (make-cookie "session" session)))
           `(html
              ,(common-head req)
              (body (p "Account made"
                    (p (a ([href ,(url* user-page username)]) "click here to go to your account"))))))]
  ))

(define (menu req)
  (define username (logged-in? req))
  `((p ([class "navbar"])
    ,(log-out-button req)
    " "
    ,(if (= (current-week) 0)
      `(a ([href ,(url* user-page-specific username 52 (sub1 (current-year)))]) "<<")
      `(a ([href ,(url* user-page-specific username (sub1 (current-week)))]) "<<"))
    " "
    (a ([href ,(url* user-page username)]) "now")
    " "
    ,(if (= (current-week) 52)
      `(a ([href ,(url* user-page-specific username 0 (add1 (current-year)))]) ">>")
      `(a ([href ,(url* user-page-specific username (add1 (current-week)))]) ">>"))
    " "
    (a ([href ,(url* add-entry)]) "add")
    " "
    (a ([href ,(url* settings-page)]) "settings"))))

(define (user-page req ar)
  (cond
    [(logged-in? req)
     (response/xexpr
       #:preamble #"<!DOCTYPE html>"
       `(html
          ,(common-head req)
          (body
            ,@(menu req)
            ,(generate-user-page req ar)
            )))]
    [else
      (redirect-to (url index-page))]))

(define (user-page-specific req ar n [year (current-year)])
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body
         (p ([class "navbar"])
           ,(log-out-button req)
           " "
           ,(if (= n 0)
             `(a ([href ,(url* user-page-specific ar 52 (sub1 year))]) "<<")
             (if (= (current-year) year)
               `(a ([href ,(url* user-page-specific ar (sub1 n))]) "<<")
               `(a ([href ,(url* user-page-specific ar (sub1 n) year)]) "<<")))
           " "
           (a ([href ,(url* user-page ar)]) "now")
           " "
           ,(if (> (add1 n) 52)
             `(a ([href ,(url* user-page-specific ar 0 (add1 year))]) ">>")
             (if (= (current-year) year)
               `(a ([href ,(url* user-page-specific ar (add1 n))]) ">>")
               `(a ([href ,(url* user-page-specific ar (add1 n) year)]) ">>")))
           " "
           (a ([href ,(url* add-entry)]) "add")
           " "
           (a ([href ,(url* settings-page)]) "settings"))
         ,(generate-user-page req ar n year)
         ))))

(define (generate-user-page req ar [week (current-week)] [year (current-year)])
  (cond
    [(string=? (logged-in? req) ar)
     (define now (current-date))
     (define root (build-path ar "calendar" (number->string year) (number->string week)))
     (with-handlers ([exn?
                       (lambda (exn)
                         (trce exn)
                         '(div ([id "calendar"]) (p "there are no plans this week")))])
       (define entries (directory-list root))
       (define r
         (map (entry->html ar week year)
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
             #:key (lambda (x) (hash-ref (second x) 'from 0)))))
       `(div ([id "calendar"])
         ,@(if (empty? r)
           (list '(p "there are no plans this week"))
           r))
       )
     ]
    [else
     '(div ([id "calendar"]) (p "you do not have permission to view someone else's profile"))])
  )

(define (about-page req)
  (response/xexpr
    `(html
       ,(common-head req)
       (body
         (h1 "What is Calenio?")
         (p "Calenio is a calendar application.")
         (h2 "What distinguishes Calenio from other services?")
         (p "Calenio lets the user choose the visuals and other extraneous functionality. Unlike most heavy calendar applications, basic Calenio offers a simple list of text. This makes Calenio extremely fast.")
         (h3 "How do I use Calenio?")
         (p "Make an account and start adding calendar entries into it!")
         (h3 "Inspiration")
         (p "Calenio is inspired by " (a ([href "https://motherfuckingwebsite.com"]) "motherfucking website") ", " (a ([href "https://suckless.org"]) "suckless") ", and my friends at work who always joke about bloated as fuck software being so bullshit.")
         (a ([href ,(url index-page)]) "back to index")
         ))))

(define (log-out-button req)
  `(a ([href ,(url log-out)]) "log out")
  )

(define (for-developers-page req)
  (response/xexpr
    `(html
       ,(common-head req)
       (body
         (h1 "Calenio Public API")
         (div
           (p "Calenio is a REST-based application that communicates over HTTP.")
           (p "One first needs to get the username and session cookies by logging in: " (code "POST /login") " with POST data: " (code "username=<username>, password=<password>"))
           (h2 "After having logged in")
           (p "To add a calendar entry: " (code "POST /add") " with POST data `description`, `from-date`, `from-time`, `to-date`, and `to-time`. An example: " (code "POST /add") " with post data: " (code "description=An Example&from-date=2018-06-12&from-time=18:33&to-date=2018-06-12&to-time=19:00"))
           )
         (a ([href ,(url index-page)]) "back to index")
         ))))

(define (today)
  (define now (seconds->date (current-seconds)))
  (string-append (~a #:align 'right #:left-pad-string "0" #:min-width 4 (number->string (date-year now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-month now))) "-"
                 (~a #:align 'right #:left-pad-string "0" #:min-width 2 (number->string (date-day now)))))

(define (add-entry req)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body
         ,@(menu req)
         (form ([action ,(url* add-entry-post)] [id "add-entry-form"] [method "post"])
           (textarea ([id "description"] [form "add-entry-form"] [name "description"] [placeholder "description"]))
           (p ([id "from"]) "from: "
             (input ([id "from-date"] [name "from-date"] [placeholder "from-date"] [type "date"] [value ,(today)]))
             (input ([id "from-time"] [name "from-time"] [placeholder "from-time"] [type "time"] [value "13:15"])))
           (p ([id "to"]) "to: "
             (input ([id "to-date"] [name "to-date"] [placeholder "to-date"] [type "date"] [value ,(today)]))
             (input ([id "to-time"] [name "to-time"] [placeholder "to-time"] [type "time"] [value "14:00"])))
           (input ([type "submit"]))
           )))))

(define (add-entry-post req)
  (define username (logged-in? req))
  (define description (get-post req 'description))
  (cond
    [(> (string-length description) 2048)
     (redirect-to (url* too-much-description))]
    [else
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
     (redirect-to (url* user-page username))]))

(define (not-logged-in req)
  (response/xexpr
    `(html
       (body
         (p "you are not logged in")
         (a ([href "/"]) "go to index page"))))
  )
(define (settings-page req)
  (response/xexpr
    #:preamble #"<!DOCTYPE html>"
    #:headers (list (cookie->header (make-cookie "reload" (number->string (random)))))
    `(html
       ,(common-head req)
       (body
         ,@(menu req)
         (p "paste custom CSS/JS here")
         (form ([action ,(url* settings-post)] [id "form"] [method "post"])
           (textarea ([form "form"] [name "css"] [placeholder "CSS"]) ,(read-css (logged-in? req)))
           (textarea ([form "form"] [name "js"] [placeholder "Javascript"]) ,(read-js (logged-in? req)))
           (input ([type "submit"]))
           )
         (form ([action "/"]) [method "post"]
           (input ([type "text"]))
           )
         (p "you may need to clear your browser cache for your changes to take effect (in the future, we want to set a cookie that adds an argument to get a different stylesheet (e.g. 'GET /file/user/css?r=32'), so we do not need the user to manually clear his cache)")
         ))))

(define (settings-post req)
  (define css (get-post req 'css))
  (define js (get-post req 'js))
  (cond
    [(> (string-length css) 2048)
     (redirect-to (url* too-much-css))]
    [(> (string-length js) 2048)
     (redirect-to (url* too-much-js))]
    [else
     (define path (build-path "file" "user" (logged-in? req)))
     (make-directory* path)
     (with-output-to-file (build-path path "css") #:exists 'replace
       (thunk (displayln css)))
     (with-output-to-file (build-path path "js") #:exists 'replace
       (thunk (displayln js)))
     (redirect-to (url* user-page (logged-in? req)))]))

(define (test-method req)
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
  (define username (logged-in? req))
  (with-handlers ([exn?
                    (lambda (exn)
                      (erro+ exn))])
    (delete-file (build-path username "calendar" (number->string year) (number->string week) uuid)))
  (redirect-to (url index-page)))

(define (invite-entry req username week year uuid)
  (define file
    (with-handlers ([exn?
                      (lambda (exn)
                        (erro+ exn)
                        (hash))])
      (with-input-from-file (build-path username "calendar"
                                        (number->string year)
                                        (number->string week) uuid)
                            (thunk (read)))))
  (define desc (hash-ref file 'description ""))
  (define from (seconds->date (hash-ref file 'from "")))
  (define to (seconds->date (hash-ref file 'to "")))
  (define from-date (date->date-string from))
  (define to-date (date->date-string to))
  (define from-time (date->time-string from))
  (define to-time (date->time-string to))
  (trce from-date)
  (response/xexpr
    `(html
       ,(common-head req)
       (body
         ,@(menu req)
         (p "If you wish to invite others, please send them the current address")
         (form ([action "/add"] [id "add-entry-form"] [method "post"])
         (textarea ([form "add-entry-form"] [name "description"] [placeholder "description"]) ,desc)
         (p "from: ")
         (input ([name "from-date"] [placeholder "from-date"] [type "date"] [value ,from-date]))
         (input ([name "from-time"] [placeholder "from-time"] [type "time"] [value ,from-time]))
         (p "to: ")
         (input ([name "to-date"] [placeholder "to-date"] [type "date"] [value ,to-date]))
         (input ([name "to-time"] [placeholder "to-time"] [type "time"] [value ,to-time]))
         (input ([type "submit"]))
         )))))

(define (too-much-css req)
  (response/xexpr
    #:code 404 #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p "too much css code (limit: 2048 characters)")
             (a ([href "/"]) "back to user")))))

(define (too-much-js req)
  (response/xexpr
    #:code 404 #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p "too much js code (limit: 2048 characters)")
             (a ([href "/"]) "back to user")))))

(define (too-much-description req)
  (response/xexpr
    #:code 404 #:preamble #"<!DOCTYPE html>"
    `(html
       ,(common-head req)
       (body (p "description too long (limit: 2048 characters)")
             (a ([href "/"]) "back to user")))))

(define-values (dispatch* url*)
  (dispatch-rules
    (["add"]            add-entry)
    (["add"]                                       #:method "post" add-entry-post)
    (["delete" (number-arg) (number-arg) (string-arg)] #:method "post"
     delete-entry)
    (["invite" (string-arg) (number-arg) (number-arg) (string-arg)] invite-entry)
    (["u"  (string-arg)]                           user-page)
    (["u" (string-arg) (number-arg)]               user-page-specific)
    (["u" (string-arg) (number-arg) (number-arg)]  user-page-specific)
    (["settings"]                                  settings-page)
    (["settings"]                                  #:method "post" settings-post)
    (["too-much-css"]            too-much-css)
    (["too-much-description"]            too-much-description)
    (["too-much-js"]            too-much-js)
    ))

(define-values (dispatch url)
  (dispatch-rules
    (["about"]           #:method "get" about-page)
    (["developers"]      #:method "get" for-developers-page)
    (["login"]           #:method "post" login-page)
    (["log-out"]         #:method "get" log-out)
    (["not-logged-in"]   #:method "get" not-logged-in)
    (["new-account"]     #:method "get" new-account)
    (["new-account"]     #:method "post" new-account-post)
    ([""]                #:method "get" index-page)
    (else                login-barrier)))
