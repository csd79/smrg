;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package :smrg)
#.(enable-ccom-syntax)


;;; ---------------------------------------------------------------
;;; Global vars


(defparameter *independent-exe* nil)
(defparameter *cmd-codepage* '(win32:code-page :id 852))


;;; ---------------------------------------------------------------
;;; Utils


(defun run-it (command)
  (with-output-to-string (output)
    (uiop:run-program
     command
     :output output
     :error-output output
     :ignore-error-status t
     :external-format *cmd-codepage*)))

(defun connect-to-sharepoint-doclibrary (drive-letter path username password)
  (run-it (format nil "net use ~a: \"~a\"  /user:~a \"~a\" /persistent:no"
                  (string-upcase drive-letter)
                  path username password)))

(defun disconnect-drive (drive-letter)
  (run-it (format nil "net use ~a: /delete /y" (string-upcase drive-letter))))







;;; ---------------------------------------------------------------
;;; Main


(defparameter *pkeys* '(:workdir :tkdirs :template :drive :archive))


(defun worker (obj command)
  (wg-msg "~a" command))


(defun init-app ()
  (make-instance 'wax-app
   :state `(:workdir  ""
            :tkdirs   ""
            :template ""
            :drive    "X"
            :archive  "")
   :execute-fn #'worker))


(defun start ()
  (in-package :smrg)

  (let ((obj (init-app)))

    ;; Initialize app state
    (load-state obj :keys *pkeys*)

    ;; GUI
    (wg-window "Rendkívüli események összegzése" 80

      "Munkakönyvtár a dokumentumtárban"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :workdir) text))
       (get-state obj :workdir))

      "TK könyvtárak"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :tkdirs) text))
       (get-state obj :tkdirs))

      "Táblázat sablon"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (get-state obj :template))

      "Csatolt meghajtó betûjele"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :drive) text))
       (get-state obj :drive))

      "Archív alkönyvtárak"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :archive) text))
       (get-state obj :archive))

      (wg-button "Összeolvasás"
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :merge)))

      (wg-button "Fájlok cseréje"
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :swap))))))

























;;; ---------------------------------------------------------------
;;; Sandbox


(defparameter *fi1* "https://sps.kk.gov.hu/org/jszf/Megosztott dokumentumok/Személyügyi Osztály/smrg teszt/TK 1/TK1.xlsx")

;(defparameter *fi2* "")
;(defparameter *fi3* "")


(defparameter *doclib-drive* "x")


(defun connect-test ()
  (connect-to-sharepoint-doclibrary
   *doclib-drive*
   "https://sps.kk.gov.hu/org/jszf/Megosztott dokumentumok/Személyügyi Osztály/smrg teszt"
   "cselovszkid" "GeorgeLucas77?")
  (print (run-it (format nil "dir ~a:" (string-upcase *doclib-drive*))))
  (disconnect-drive *doclib-drive*))


(defun test (file)
  (with-ccom-context
    (with-workbook (:wbook wbook :open file :wsvars (ws1) :save nil)
      (print (xcell ws1 1 1)))))





#.(disable-ccom-syntax)
