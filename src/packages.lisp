;; -*- lisp -*-

(in-package :cl-user)

(defpackage :com.sondesh.database
  (:nicknames :sondesh)
  (:nicknames #:<tt)
  (:use :common-lisp :cl-who :hunchentoot)
  (:export
   #:journey
   ))

;(defpackage :com.sondesh.database.facebook
 ; (:use :cl :it.bese.yaclml :iterate)
  ;(:documentation "Facebook library.")
  ;(:nicknames :fbml :<fb))
