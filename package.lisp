;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:smrg
  (:use #:cl #:utils #:ccom4 #:ccoffice #:achar #:cl-ppcre #:wax #:str #:local-time #:splt)
  (:shadowing-import-from #:str #:split #:prune)
  (:shadowing-import-from #:local-time #:today #:date #:timestamp)
  (:export #:smrg-start))
