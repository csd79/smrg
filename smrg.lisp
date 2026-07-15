;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package :smrg)
#.(enable-ccom-syntax)


;;; ---------------------------------------------------------------
;;; Global vars


(defparameter *independent-exe* nil)
(defparameter *cmd-codepage* '(win32:code-page :id 852))

(defparameter *tk-list*
  '("Bajai Tankerületi Központ"
    "Balassagyarmati Tankerületi Központ"
    "Balatonfüredi Tankerületi Központ"
    "Békéscsabai Tankerületi Központ"
    "Belsõ-Pesti Tankerületi Központ"
    "Berettyóújfalui Tankerületi Központ"
    "Ceglédi Tankerületi Központ"
    "Debreceni Tankerületi Központ"
    "Dél-Budai Tankerületi Központ"
    "Dél-Pesti Tankerületi Központ"
    "Dunakeszi Tankerületi Központ"
    "Dunaújvárosi Tankerületi Központ"
    "Egri Tankerületi Központ"
    "Érdi Tankerületi Központ"
    "Észak-Budapesti Tankerületi Központ"
    "Észak-Pesti Tankerületi Központ"
    "Esztergomi Tankerületi Központ"
    "Gyõri Tankerületi Központ"
    "Gyulai Tankerületi Központ"
    "Hajdúböszörményi Tankerületi Központ"
    "Hatvani Tankerületi Központ"
    "Hódmezõvásárhelyi Tankerületi Központ"
    "Jászberényi Tankerületi Központ"
    "Kaposvári Tankerületi Központ"
    "Karcagi Tankerületi Központ"
    "Kazincbarcikai Tankerületi Központ"
    "Kecskeméti Tankerületi Központ"
    "Kelet-Pesti Tankerületi Központ"
    "Kiskõrösi Tankerületi Központ"
    "Kisvárdai Tankerületi Központ"
    "Közép-Budai Tankerületi Központ"
    "Közép-Pesti Tankerületi Központ"
    "Külsõ-Pesti Tankerületi Központ"
    "Mátészalkai Tankerületi Központ"
    "Mezõkövesdi Tankerületi Központ"
    "Miskolci Tankerületi Központ"
    "Mohácsi Tankerületi Központ"
    "Monori Tankerületi Központ"
    "Nagykanizsai Tankerületi Központ"
    "Nyíregyházi Tankerületi Központ"
    "Pápai Tankerületi Központ"
    "Pécsi Tankerületi Központ"
    "Salgótarjáni Tankerületi Központ"
    "Sárospataki Tankerületi Központ"
    "Sárvári Tankerületi Központ"
    "Siófoki Tankerületi Központ"
    "Soproni Tankerületi Központ"
    "Szegedi Tankerületi Központ"
    "Székesfehérvári Tankerületi Központ"
    "Szekszárdi Tankerületi Központ"
    "Szerencsi Tankerületi Központ"
    "Szigetszentmiklósi Tankerületi Központ"
    "Szigetvári Tankerületi Központ"
    "Szolnoki Tankerületi Központ"
    "Szombathelyi Tankerületi Központ"
    "Tamási Tankerületi Központ"
    "Tatabányai Tankerületi Központ"
    "Váci Tankerületi Központ"
    "Veszprémi Tankerületi Központ"
    "Zalaegerszegi Tankerületi Központ"))

(declaim (inline the-now familiar-namestring-p namestring-with-valid-date-p))


;;; ---------------------------------------------------------------
;;; General utils


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

(defun short-tk-name (tk-name)
  (unwords (list (first (words tk-name)) "TK")))

(defun the-now ()
  (adjust-timestamp (now) (offset :hour 2)))

(defun date-string (&key (date nil) (day-offset 0))
  (let ((date (if date
                (parse-timestring date :date-separator #\.)
                (the-now))))
    (format-timestring
     nil
     (timestamp+ date day-offset :day)
     :format '((:year 4) "." (:month 2) "." (:day 2) "."))))


;;; ---------------------------------------------------------------
;;; wax-app readers


(defmethod workdir-namestring ((obj wax-app))
  (concatenate 'string
   (get-state obj :drive)
   ":"
   (ensure-prefix "\\"
    (ensure-suffix "\\" (get-state obj :workdir)))))

(defmethod template-namestring ((obj wax-app))
  (concatenate 'string
   (workdir-namestring obj)
   (get-state obj :template)))

(defmethod tk-dir-namestring ((obj wax-app) tk-name)
  (concatenate 'string
   (workdir-namestring obj)
   (get-state obj :tkdirs)
   (ensure-suffix "\\" (short-tk-name tk-name))))

(defmethod tk-archive-dir-namestring ((obj wax-app) tk-name)
  (ensure-suffix "\\"
   (concatenate 'string
    (tk-dir-namestring obj tk-name)
    (get-state obj :archive))))


;;; ---------------------------------------------------------------
;;; Filename utils


(defmethod all-archiveables ((obj wax-app) tk-name)
  (mapcar #'namestring
          (uiop:directory-files
           (tk-dir-namestring obj tk-name) "*.xlsx")))

(defun familiar-namestring-p (filename tk-name)
  (search (first (words tk-name)) filename))

(defun namestring-with-valid-date-p (filename exclusive-date)
  (not (search exclusive-date filename)))

(defun valid-archiveables (all-sources tk-name exclusive-date)
  (remove-if #'(lambda (namestring)
                 (let ((filename (pathname-name namestring)))
                   (not (and (familiar-namestring-p filename tk-name)
                             (namestring-with-valid-date-p filename exclusive-date)))))
             all-sources))


;;; ---------------------------------------------------------------
;;; Filename interface


(defmethod mergeable-namestring ((obj wax-app) tk-name present-date)
  "Namestring of the file for given TK-NAME and PRESENT-DATE, or NIL if it doesn't exist."
  (let ((namestring
         (concatenate 'string
          (tk-dir-namestring obj tk-name)
          (date-string :date present-date) " "
          (short-tk-name tk-name)
          " bejelentések.xlsx")))
    (and (probe-file namestring)
         namestring)))

(defmethod archiveable-namestrings ((obj wax-app) tk-name exclusive-date)
  "A list of namestrings in TK-NAME's directory whose date isn't EXCLUSIVE-DATE."
  (let ((all-sources (all-archiveables obj tk-name)))
    (valid-archiveables all-sources tk-name exclusive-date)))

(defmethod archive-namestrings ((obj wax-app) archiveables tk-name)
  "A list of destination namestrings when moving ARCHIVEABLES."
  (mapcar #'(lambda (source-file)
              (namestring (make-pathname
                           :name (pathname-name source-file)
                           :type (pathname-type source-file)
                           :defaults (tk-archive-dir-namestring obj tk-name))))
          archiveables))

(defmethod foreign-namestrings ((obj wax-app) tk-name)
  "A list of nemaestrings in TK-NAME's directory who belong to another TK."
  (remove-if #'(lambda (namestring)
                 (let ((filename (pathname-name namestring)))
                   (familiar-namestring-p filename tk-name)))
             (all-archiveables obj tk-name)))


;;; ---------------------------------------------------------------
;;; Swap files





(defun tt ()
  (let ((obj (make-instance 'wax-app
              :state `(:workdir  "Users\\cselovszkid\\Downloads\\smrg-teszt\\"
                       :tkdirs   "TK-k\\"
                       :template "bejelentések.xlsx"
                       :drive    "C"
                       :archive  "Archív\\"))))
    (print (workdir-namestring obj))
    (print (template-namestring obj))
    (print (tk-dir-namestring obj "Közép-Pesti Tank.Közp"))
    (print (tk-archive-dir-namestring obj "Veszprémi TK"))
    (print (mergeable-namestring obj "Miskolci Tankerületi Központ" "2026.07.14"))
    (print (mergeable-namestring obj "Balassagyarmati Tankerületi Központ" "2026.07.14"))
    (let ((archiveables (archiveable-namestrings obj "Balassagyarmati" "2026.07.15")))
      (pprint archiveables)
      (pprint (archive-namestrings obj archiveables "Balassagyarmati TK")))
    (print (foreign-namestrings obj "Balassagyarmati"))
    ))
    

;;; ---------------------------------------------------------------
;;; Main


(defparameter *pkeys* '(:workdir :tkdirs :template :drive :archive :result))


(defun worker (obj command)
  (wg-msg "~a" command))


(defun init-app ()
  (make-instance 'wax-app
   :state `(:workdir    ""
            :tkdirs     ""
            :template   ""
            :drive      "X"
            :archive    ""
            :result     ""
            :merge-date ""
            :swap-date  ""
            )
   :execute-fn #'worker))


(defun start ()
  (in-package :smrg)

  (let ((obj (init-app)))

    ;; Initialize app state
    (load-state obj :keys *pkeys*)

    ;; GUI
    (wg-window "Rendkívüli esemény értesítõk összegzése" 80

      "" ""

      " Csatolt meghajtó betûjele"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :drive) text))
       (get-state obj :drive))

      " Munkakönyvtár a dokumentumtárban"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :workdir) text))
       (get-state obj :workdir))

      " TK könyvtárak"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :tkdirs) text))
       (get-state obj :tkdirs))

      " Archív alkönyvtárak"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :archive) text))
       (get-state obj :archive))

      " Táblázat sablon"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (get-state obj :template))

      " Eredmény helye"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :result) text))
       (get-state obj :result))

      ""
      (wg-button "Összeolvasás"
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :merge)))

      "" ""
      
      " Feldolgozott állományok dátuma"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (date-string))

      " Következõ napi dátum"
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (date-string :day-offset 1))
      
      ""
      (wg-button "Fájlok cseréje"
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :swap)))

      "" "")))

























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
