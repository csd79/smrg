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

(defun replaced-string (string)
  (coerce
   (loop for i from 0 below (length string) collecting #\*)
   'string))

(defun valid-credential-p (cred)
  (and cred (stringp cred) (string/= cred "")))


;;; ---------------------------------------------------------------
;;; SharePoint connection


(defun drive-connected-p (letter)
  "Return T if drive LETTER: is conntected, otherwise NIL."
  (when (probe-file (concatenate 'string (string-upcase letter) ":\\")) t))

(defun connect-to-sharepoint-doclibrary (drive-letter path username password)
  "Try to connect PATH as DRIVE-LETTER: with given credentials, return cmd message."
  (run-it (format nil "net use ~a: \"~a\"  /user:~a \"~a\" /persistent:no"
                  (string-upcase drive-letter)
                  path username password)))

(defun disconnect-drive (drive-letter)
  "Disconnect DRIVE-LETTER:, return cms message."
  (run-it (format nil "net use ~a: /delete /y" (string-upcase drive-letter))))

(defun drive-connected-to (drive-letter)
  "Return the path connected as DRIVE-LETTER:, or NIL if it isn't returned by cmd."
  (let ((message (run-it (format nil "net use ~a:" (string-upcase drive-letter)))))
    (multiple-value-bind (mstart mend rstart rend)
        (cl-ppcre:scan "[.|\\r\\n|\\r|\\n]*Távoli név *(.*)[.|\\r\\n|\\r|\\n]*" message)
      (declare (ignore mstart mend))
      (when rstart
        (subseq message (svref rstart 0) (svref rend 0))))))


;;; ---------------------------------------------------------------
;;; wax-app readers


(defmethod subdir-namestring ((obj wax-app))
  (concatenate 'string
   (ensure-suffix "\\" (concatenate 'string (get-state obj :drive) ":"))
   ))

(defmethod template-namestring ((obj wax-app))
  (concatenate 'string
   (subdir-namestring obj)
   (get-state obj :template)))

(defmethod tk-dir-namestring ((obj wax-app) tk-name)
  (concatenate 'string
   (subdir-namestring obj)
   (ensure-suffix "\\" (get-state obj :tkdirs))
   (ensure-suffix "\\" (short-tk-name tk-name))))

(defmethod tk-archive-dir-namestring ((obj wax-app) tk-name)
  (concatenate 'string
   (tk-dir-namestring obj tk-name)
   (ensure-suffix "\\" (get-state obj :archive))))


;;; ---------------------------------------------------------------
;;; Filename utils


(defmethod tk-workbooks ((obj wax-app) tk-name)
  (mapcar #'namestring
          (uiop:directory-files
           (tk-dir-namestring obj tk-name) "*.xlsx")))

(defun familiar-namestring-p (filename tk-name)
  (search (first (words tk-name)) filename))

(defun namestring-with-valid-date-p (filename exclusive-date)
  (not (search exclusive-date filename)))

(defun tk-expired (all-sources tk-name exclusive-date)
  (remove-if #'(lambda (namestring)
                 (let ((filename (pathname-name namestring)))
                   (not (and (familiar-namestring-p filename tk-name)
                             (namestring-with-valid-date-p filename exclusive-date)))))
             all-sources))


;;; ---------------------------------------------------------------
;;; Filename interface


(defmethod tk-pending-namestring ((obj wax-app) tk-name present-date
                                  &key (only-existing nil)
                                       (only-on-existing-path t))
  "Namestring of the file for given TK-NAME and PRESENT-DATE, or NIL if it doesn't exist."
  (let* ((path (tk-dir-namestring obj tk-name))
         (namestring
          (concatenate 'string
            path
            (date-string :date present-date) " "
            (short-tk-name tk-name)
            " bejelentések.xlsx")))
    (and
     ;; Existing file?
     (if only-existing
       (probe-file namestring)
       t)
     ;; On existing path?
     (if only-on-existing-path
       (probe-file path)
       t)
     namestring)))

(defmethod tk-expired-namestrings ((obj wax-app) tk-name exclusive-date)
  "A list of namestrings in TK-NAME's directory whose date isn't EXCLUSIVE-DATE."
  (let ((all-sources (tk-workbooks obj tk-name)))
    (tk-expired all-sources tk-name exclusive-date)))

(defmethod tk-archived-namestrings ((obj wax-app) expired tk-name)
  "A list of destination namestrings when moving EXPIRED."
  (mapcar #'(lambda (source-file)
              (namestring (make-pathname
                           :name (pathname-name source-file)
                           :type (pathname-type source-file)
                           :defaults (tk-archive-dir-namestring obj tk-name))))
          expired))

(defmethod tk-foreign-namestrings ((obj wax-app) tk-name)
  "A list of nemaestrings in TK-NAME's directory who belong to another TK."
  (remove-if #'(lambda (namestring)
                 (let ((filename (pathname-name namestring)))
                   (familiar-namestring-p filename tk-name)))
             (tk-workbooks obj tk-name)))


;;; ---------------------------------------------------------------
;;; SP connection


(defmethod connection-ensured-p ((obj wax-app))
  (block ensured
    (let ((first-run?   (get-state obj :first-run))
          (username     (get-state obj :username))
          (drive-letter (get-state obj :drive))
          (doclib-path  (get-state obj :workdir)))
      (cond

       ;; On first run, if drive letter is taken, display warning and return NIL.
       ((and first-run?
             (drive-connected-p drive-letter))
        (progn (wg-msg (format nil "Hiba a dokumentumtár csatlakoztatásakor:~2%~
A megadott betûjel (~a:) már foglalt, válasszon másikat!" drive-letter))
          (return-from ensured nil)))

       ;; Otherwise, on first run, take credentials & connect to doclib:
       (first-run?
        (multiple-value-bind (name pw)
            (wg-login "KLIK bejelentkezés" username)
          ;; If credentials are incomplete, display warning & return NIL.
          (unless (and (valid-credential-p name)
                       (valid-credential-p pw))
            (wg-msg (format nil "Hiányos azonosító adatok:~2%Felhasználónév: ~a~%Jelszó: ~a"
                            (if (valid-credential-p name) name "<üres>")
                            (if (valid-credential-p pw) (replaced-string pw) "<üres>")))
            (return-from ensured nil))
          ;; Credentials taken, connet to doclib.
          (let ((message (connect-to-sharepoint-doclibrary drive-letter doclib-path name pw)))
            (unless (drive-connected-p drive-letter)
              ;; If unsuccessfull, display cmd message & return NIL.
              (wg-msg "Hiba a dokumentumtár csatlakoztatásakor:~2%~a" message)
              (return-from ensured nil)))
          ;; Successful connection, update username & first run, return T.
          (setf (get-state obj :first-run) nil
                (get-state obj :username)  name))
        (return-from ensured t))

       ;; On later runs, check if drive is connected with the correct path:
       ((not first-run?)
        ;; If drive is not connected, display dialog and either retry as first run or return NIL.
        (unless (drive-connected-p drive-letter)
          (when (wg-confirm (format nil "A(z) ~a: meghajtó nincs csatolva,~
újra csatlakozik a beállított munkakönyvtárral?" drive-letter))
            (setf (get-state obj :first-run) t)
            (connection-ensured-p obj)))
        ;; If connected path is not what was set, display warning, disconnect and return NIL.
        (unless (string= doclib-path (drive-connected-to drive-letter))
          (wg-msg (format nil "Nem a megadott munkakönyvtár van csatlakoztatva a ~a: meghajtón,~
csatlakozás bontása" drive-letter))
          (disconnect-drive drive-letter)
          (return-from ensured nil))
        ;; Correct path is connected, return T.
        (return-from ensured t))))))


(defun et ()
  (connection-ensured-p
   (make-instance 'wax-app :state
    `(:first-run  t
      :username   "cselovszkid"
      :drive      "x"
      :workdir    "\\\\sps.kk.gov.hu@SSL\\DavWWWRoot\\org\\jszf\\Megosztott dokumentumok\\Személyügyi Osztály\\smrg teszt"))))


(defun connect-test ()
  (connect-to-sharepoint-doclibrary
   "x"
   "\\\\sps.kk.gov.hu@SSL\\DavWWWRoot\\org\\jszf\\Megosztott dokumentumok\\Személyügyi Osztály\\smrg teszt"
   "cselovszkid" "GeorgeLucas77?")
;  (print (run-it (format nil "dir ~a:" (string-upcase *doclib-drive*))))
;  (disconnect-drive *doclib-drive*)
  )


;;; ---------------------------------------------------------------
;;; Merge files


;; Hogyan derül ki, hogy a fájl amit éppen hozzáadunk az összegzéshez, meg van nyitva egy TK-s kollégánál?


;;; ---------------------------------------------------------------
;;; Archive files


(defun paste-column (wsheet column start-row end-row value)
  (let ((height (1+ (- end-row start-row))))
    (setf (xrange wsheet column start-row column end-row)
          (make-array `(,height 1) :initial-element value))))


;; Hogyan derül ki, hogy a fájl amit éppen archiválunk, meg van nyitva egy TK-s kollégánál?
;; Ha nyitva van, a RENAME-FILE simán átteszi az Archive mappába. Amikor a szerkesztõ fél
;; ráment, az Excel kiírja hogy a megadott elérési út nem tallható, és felajánlja egy
;; másolat mentését!
(defmethod archive-and-refresh-files ((obj wax-app))
  (with-progress-new ("Korábbi táblázatok archiválása" obj :limit (length *tk-list*))
    (catch 'archive-exit
      (let ((swap-date (get-state obj :swap-date))
            (temp-file (new-temp-filename "xlsx")))
        ;; Prepare fresh template file.
        (uiop:copy-file (template-namestring obj) temp-file)
        (with-workbook (:wbook wbook :open temp-file :wsvars (wsheet))
          ;; Paste swap dates
          (paste-column wsheet 1 2 51 swap-date)
          ;; Iterate of TKs.
          (loop for tk in *tk-list*
                for short-tk    = (short-tk-name tk)
                for tk-expired  = (tk-expired-namestrings obj short-tk swap-date)
                for tk-archived = (tk-archived-namestrings obj tk-expired short-tk) doing
                (disp obj "~%~a~%Archivált fájlok:~%" (astring-upcase tk))
                (if tk-expired
                  ;; If there are files to archive, do them and display their names.
                  (loop for src in tk-expired
                        for dst in tk-archived doing
                        (disp obj "~a.xlsx~%" (pathname-name src))
                        (rename-file src dst))
                  ;; Otherwise, display <none>.
                  (disp obj "<semmi>~%"))
                ;; Create fresh file.
                (let ((fresh-file (tk-pending-namestring obj short-tk swap-date)))
                  (if fresh-file
                    ;; TK path exists, go ahead
                    (progn
                      ;; Edit it in temp dir.
                      (paste-column wsheet 2 2 51 tk)
                      (!'save wbook)
                      (uiop:copy-file temp-file fresh-file)
                      (disp obj "Következõ napi állomány: ~a~%" fresh-file))
                    ;; TK path doesn't exist, display <none>.
                    (disp obj "TK mappa hiányzik!~%")))))
        ;; Delete temporary file.
        (delete-file temp-file)))))


(defun st ()
  (archive-and-refresh-files
   (make-instance 'wax-app
   :state `(
            ;:first-run  t
            ;:username   nil
            ;:workdir    ""
            :tkdirs     "TK-k"
            :template   "bejelentések.xlsx"
            :drive      "x"
            :archive    "Archív"
            ;:result     ""
            :merge-date "2026.07.14."
            :swap-date  "2026.07.15."
            ))))

(defun tt ()
  (let ((obj (make-instance 'wax-app
              :state `(:workdir  "Users\\cselovszkid\\Downloads\\smrg-teszt\\"
                       :tkdirs   "TK-k\\"
                       :template "bejelentések.xlsx"
                       :drive    "c"
                       :archive  "Archív\\"))))
    (print (subdir-namestring obj))
    (print (template-namestring obj))
    (print (tk-dir-namestring obj "Közép-Pesti Tank.Közp"))
    (print (tk-archive-dir-namestring obj "Veszprémi TK"))
    (print (tk-pending-namestring obj "Miskolci Tankerületi Központ" "2026.07.14"))
    (print (tk-pending-namestring obj "Balassagyarmati Tankerületi Központ" "2026.07.14"))
    (let ((expired (tk-expired-namestrings obj "Balassagyarmati" "2026.07.15")))
      (pprint expired)
      (pprint (tk-archived-namestrings obj expired "Balassagyarmati TK")))
    (print (tk-foreign-namestrings obj "Balassagyarmati"))
    ))



;;; ---------------------------------------------------------------
;;; Main


(defparameter *pkeys* '(:username :workdir :tkdirs :template :drive :archive :result))


(defun init-app ()
  (make-instance 'wax-app
   :state `(:first-run  t
            :username   nil
            :workdir    ""
            :tkdirs     ""
            :template   ""
            :drive      "X"
            :archive    ""
            :result     ""
            :merge-date ""
            :swap-date  "")
   :execute-fn #'worker))


(defun worker (obj command)
  (when (connection-ensured-p obj)
    (case command
      (:merge   (wg-msg "Merge!"))
      (:archive (archive-and-refresh-files obj)))))


(defun smrg-start ()
  (in-package :smrg)
  (let ((obj (init-app)))
    ;; Initialize app state  -------------------------------------------
    (load-state obj :keys *pkeys*)
    ;; GUI  ------------------------------------------------------------
    (wg-window `(:title "Rendkívüli esemény értesítõk összegzése"
                 :best-width 700
                 :max-height 80
                 :confirm-destroy-function ,#'(lambda (&rest rest)
                                                (declare (ignore rest))
                                                (let ((drive-letter (get-state obj :drive)))
                                                  (when (and drive-letter
                                                             (stringp drive-letter)
                                                             (drive-connected-p drive-letter))
                                                    (disconnect-drive drive-letter)
                                                    (wg-msg (format
                                                             nil "Kapcsolat bontva a dokumentumtárral (~a:)."
                                                             drive-letter))))
                                                t))

      "" ""
      " KLIK felhasználónév" ;  ----------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :username) text))
       (get-state obj :username))
      " Csatolt meghajtó betûjele" ;  ----------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :drive) text))
       (get-state obj :drive))
      " Munkakönyvtár a dokumentumtárban" ;  ---------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :workdir) text))
       (get-state obj :workdir))
      " TK könyvtárak" ;  ----------------------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :tkdirs) text))
       (get-state obj :tkdirs))
      " Archív alkönyvtárak" ;  ----------------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :archive) text))
       (get-state obj :archive))
      " Táblázat sablon" ;  --------------------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (get-state obj :template))
      " Eredmény helye" ;  ---------------------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :result) text))
       (get-state obj :result))
      ""
      (wg-button "Összeolvasás" ;  -------------------------------------
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :merge)))
      "" ""
      " Feldolgozott állományok dátuma" ;  -----------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (date-string))
      " Következõ napi dátum" ;  ---------------------------------------
      (wg-text-input 
       #'(lambda (text &rest rest)
           (declare (ignore rest))
           (setf (get-state obj :template) text))
       (date-string :day-offset 1))
      ""
      (wg-button "Fájlok cseréje" ;  -----------------------------------
        #'(lambda (interface)
            (declare (ignore interface))
            (save-state obj :keys *pkeys*)
            (wax-execute obj t :archive)))
      "" "")))


;;; ---------------------------------------------------------------
;;; Sandbox




#.(disable-ccom-syntax)
