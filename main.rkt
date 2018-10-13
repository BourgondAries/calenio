#! /usr/bin/env racket
#lang racket/base

(require reloadable
         web-server/servlet
         web-server/servlet-env)

(define dispatch (reloadable-entry-point->procedure
  (make-reloadable-entry-point 'dispatch "handler.rkt")))
(define file-not-found (reloadable-entry-point->procedure
  (make-reloadable-entry-point 'file-not-found "handler.rkt")))

(reload!)

(serve/servlet dispatch
  #:stateless? #t
  #:listen-ip #f
  #:port 8000
  #:server-root-path (current-directory)
  #:servlet-regexp #px"^/(?!file/).*$"
  #:command-line? #t
  #:file-not-found-responder file-not-found
  #:ssl? #f
  )
