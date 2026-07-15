;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package #:cl-user)


(defpackage #:smrg
  (:use #:cl #:ccom4 #:ccoffice #:achar #:cl-ppcre #:wax #:str #:local-time)
  (:shadowing-import-from #:str #:split)
  (:shadowing-import-from #:local-time #:today #:date #:timestamp)
  (:export #:smrg-start))
