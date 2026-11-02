;;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 
                                                                              ;

(in-package :smrg)
#.(enable-ccom-syntax)


;;; ---------------------------------------------------------------
;;; Global vars


(defparameter *independent-exe* nil)
(defparameter *cmd-codepage* '(win32:code-page :id 852))


(defparameter *smrg-styles*
  `(:normal    ,(wax::defstyle)
    :head      ,(wax::defstyle :font "Segoe Print" :size 11 :bold t
                               :fg (wax::hexrgb->color "#ffffff") :bg (wax::hexrgb->color "#000088"))
    :errorhead ,(wax::defstyle :font "Segoe Print" :size 11 :bold t
                               :fg (wax::hexrgb->color "#ffffff") :bg (wax::hexrgb->color "#ff2828"))
    :subhead   ,(wax::defstyle :bold t :fg (wax::hexrgb->color "#000088"))
    :ok        ,(wax::defstyle :bg (wax::hexrgb->color "#88ff88"))
    :warn      ,(wax::defstyle :bg (wax::hexrgb->color "#ff8888"))
    :critical  ,(wax::defstyle :bold t
                               :bg (wax::hexrgb->color "#ff2828") :fg (wax::hexrgb->color "#ffffff"))))


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

(defun skip-weekend (timestamp)
  (if (member (timestamp-day-of-week timestamp) '(6 0))
    (skip-weekend (timestamp+ timestamp 1 :day))
    timestamp))

(defun date-string (&key (date nil) (day-offset 0) (skip-weekend t))
  (let* ((date     (if date
                     (parse-timestring date :date-separator #\.)
                     (the-now)))
         (shifted  (timestamp+ date day-offset :day))
         (shifted* (if skip-weekend
                     (skip-weekend shifted)
                     shifted)))
    (format-timestring nil shifted*
                       :format '((:year 4) "." (:month 2) "." (:day 2) "."))))

(defun replaced-string (string)
  (coerce
   (loop for i from 0 below (length string) collecting #\*)
   'string))

(defun valid-credential-p (cred)
  (and cred (stringp cred) (string/= cred "")))

(defun paste-column (wsheet column start-row end-row value)
  (let ((height (1+ (- end-row start-row))))
    (setf (xrange wsheet column start-row column end-row)
          (if (listp value)
            (make-array `(,height 1) :initial-contents value)
            (make-array `(,height 1) :initial-element value)))))

(defun ensure-tk-dirs (parent-path)
  (dolist (tk *tk-list*)
    (ensure-directories-exist
;    (print
     (concatenate 'string (str:ensure-suffix "\\" (namestring parent-path))
                  (short-tk-name tk)))))


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
   (ensure-suffix "\\" (concatenate 'string (get-state obj :drive) ":"))))

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
   (ensure-suffix "\\" (get-state obj :archives))
   (ensure-suffix "\\" (short-tk-name tk-name))))


(defun at ()
  (let* ((obj
          (make-instance
           'wax-app
           :state
           `(:first-run  t
             :username   nil
             :workdir    ""
             :tkdirs     "TK-k"
             :template   ""
             :drive      "X"
             :archives   "\\\\kozpfs\\Org\\Jogi és Személyügyi Fõosztály\\Személyügyi Osztály\\CsD\\_arch_\\"
             :result     ""
             :merge-date "2026.07.23."
             :swap-date  "2026.07.24.")))
         (src (tk-archive-dir-namestring obj "Balatonfüredi Tankerületi Központ")))
    (print (probe-file (pathname src)))
    (uiop:copy-file (concatenate 'string src "x.txt")
                    (concatenate 'string src "y.txt"))))


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
            (short-tk-name tk-name) " "
;            " Rendkívüli események jelentése.xlsx")))
            (get-state obj :template))))
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
  "A list of namastrings in TK-NAME's directory who belong to another TK."
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
          (doclib-path  (get-state obj :workdir))
          (result       (get-state obj :result)))
      (cond
       ;; If result directory doesn't exist, display warning & return NIL.
       ((not (probe-file result))
         (wg-msg "A megadott 'Eredmény helye' mappa nem található:~%~a" result)
         (return-from ensured nil))

       ;; On first run, if drive letter is taken, ask if it's ok & return appropriate value.
       ((and first-run?
             (drive-connected-p drive-letter))
#|        (let ((ok? (wg-confirm "Hiba a dokumentumtár csatlakoztatásakor:~2%~
A megadott betûjel (~a:) már foglalt! Használja a csatlakoztatott mappát?" drive-letter)))|#
        (let ((ok? (wg-confirm "A megadott betûjel (~a:) már foglalt! Használja a csatlakoztatott mappát?"
                               drive-letter)))
          (cond (ok?
                 (setf (get-state obj :first-run) nil)
                 (return-from ensured t))
                ((not ok?)
                 (wg-msg "Adjon meg másik meghajtó betûjelet és próbálja újra!")
                 (setf (get-state obj :first-run) t)
                 (return-from ensured nil)))))

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
        (unless (string-equal (str:ensure-suffix "\\" doclib-path)
                              (str:ensure-suffix "\\" (drive-connected-to drive-letter)))
          (wg-msg (format nil "Nem a megadott munkakönyvtár van csatlakoztatva a ~a: meghajtón,~
csatlakozás bontása" drive-letter))
          (disconnect-drive drive-letter)
          (return-from ensured nil))
        ;; Correct path is connected, return T.
        (return-from ensured t))))))


;;; ---------------------------------------------------------------
;;; Merge files


(defun merge-filename (s)
  (lambda (filename)
    (format s "~%$[style=:subhead]~a.xlsx$[clr]~%" filename)))


(defun merge-success (s)
  (lambda (filename rowcount)
    (declare (ignore filename))
    (format s "$[style=:ok]~a sor másolva.$[clr]~%" rowcount)))

(defun merge-error (obj s)
  (lambda (filename condition)
    (declare (ignore filename))
    (let ((error-message
           (format nil "$[style=:critical]Hiba$[clr]: ~a~%" condition)))
      (format s error-message)
      (wax::queue-errorlog obj error-message))))

(defun merge-iterate (intf) (lambda () (progress intf)))

(defun merge-verify-insheet (obj s)
  (lambda (wsheet)
    (let* ((opts (get-state obj :choices))
           (chk  (get-state obj :chk-column))
           (last (find-last-row wsheet 2 chk)))
      ;; Iterate over column checks defined in _params_
      (loop for (column options) on opts by #'cddr doing
            ;; Going down the rows
            (loop for row from 2 upto last
                  for cell-value = (xcell wsheet column row) doing
                  ;; When cell value is not among the allowed options, display message.
                  (unless (member cell-value options :test #'string=)
                    (format s "$[style=:critical]Érvénytelen érték$[clr] ~a~a: ~a~%"
                            (column column) row cell-value)))))))
      

(defmethod merge-files ((obj wax-app))
;  (check-optionals obj))
;  (with-progress-new ("Aktuális fájlok egyesítése" obj :limit (length *tk-list*))
  (with-progress-window (intf (length *tk-list*) obj :title "Aktuális fájlok egyesítése"
                              :stream s :error-summary nil
                              :custom-styles *smrg-styles*)
;    (capi:execute-with-interface intf #'switch-styles)
    (catch 'merge-exit
      (let* ((merge-date  (get-state obj :merge-date))
             (output-file (concatenate 'string
;                                       (get-state obj :result)
                                       (str:ensure-suffix "\\" (get-state obj :result))
                                       merge-date
                                       " Összesített rendkívüli esemény jelentések "
                                       (timestamp (get-universal-time))
                                       ".xlsx"))
             (infiles     (loop for tk in *tk-list* collecting
                                (tk-pending-namestring obj (short-tk-name tk) merge-date
                                                       :only-existing nil
                                                       :only-on-existing-path nil)))
             (chk-column  (get-state obj :chk-column)))
;        (join-worksheets infiles output-file obj intf s :check-column 7 :width 16)
;        (join-worksheets infiles output-file :check-column 7 :width 16
        (join-worksheets infiles output-file :check-column chk-column :width 16
                         :opening-filename-callback (merge-filename s)
                         :file-error-callback       (merge-error obj s)
                         :file-success-callback     (merge-success s)
                         :iterate-callback          (merge-iterate intf)
                         :verify-insheet-callback   (merge-verify-insheet obj s))
        ;; Fixing numbering.
        (with-local-copies ((outf/ output-file))
          (with-workbook (:open outf/ :wsvars (outsheet) :save t)
            (let ((last-row (find-last-row outsheet 2 2)))
              (paste-column
               outsheet 1 2 last-row
               (loop for i from 1 upto (1- last-row) collecting
                     (list (format nil "~a." i)))))))))))


;;; ---------------------------------------------------------------
;;; Archive files


#|(defmethod ensure-tk-dirs-exist ((obj wax-app))
  "Ensire all TK dirs are present in both the operative and the archive dirs."
  (flet ((make-paths (parent)
           (mapcar #'(lambda (tk)
                       (concatenate 'string parent
                                    (str:ensure-suffix "\\" (short-tk-name tk))))
                   *tk-list*)))
    (let* ((operative-dir (concatenate 'string
                           (str:ensure-suffix ":\\" (get-state obj :drive))
                           (str:ensure-suffix "\\" (get-state obj :tkdirs))))
           (archive-dir   (str:ensure-suffix "\\" (get-state obj :archives)))
           (all-dirs      (append (make-paths operative-dir)
                                  (make-paths archive-dir))))
      (dolist (dir all-dirs) (ensure-directories-exist dir)))))|#
(defun ensure-file-dir-exists (file)
  (ensure-directories-exist (make-pathname
                             :host (pathname-host file)
                             :directory (pathname-directory file))))


(defmethod archive-and-refresh-files ((obj wax-app))
  ;; Show progress window
  (with-progress-window (intf (length *tk-list*) obj :title "Korábbi táblázatok archiválása" :stream s
                              :custom-styles *smrg-styles*)
;    ;; Ensure all TK subdirs exist in both the operative and archive directories.
;    (ensure-tk-dirs-exist obj)
    (catch 'archive-exit
      (let ((swap-date (get-state obj :swap-date))
            (temp-file (new-temp-filename "xlsx")))
        ;; Prepare fresh template file.
        (uiop:copy-file (template-namestring obj) temp-file)
        (with-workbook (:wbook wbook :open temp-file :wsvars (wsheet))
          ;; Paste swap dates
          (paste-column wsheet 6 2 181 swap-date)
          ;; Iterate of TKs.
          (loop for tk in *tk-list*
                for short-tk    = (short-tk-name tk)
                for tk-expired  = (tk-expired-namestrings obj short-tk swap-date)
                for tk-archived = (tk-archived-namestrings obj tk-expired short-tk) doing
                (format s "~%$[style=:head]~a$[clr]~%$[style=:subhead]Archivált fájlok:$[clr]~%" short-tk)
                (if tk-expired
                  ;; If there are files to archive, do them and display their names.
                  (loop for src in tk-expired
                        for dst in tk-archived doing
                        ;; Make sure archival TK dir exists
                        (ensure-file-dir-exists dst)
                        ;; Handling errors caused by expired file being edited.
                        (multiple-value-bind (result condition)
                            (ignore-errors (rename-file src dst) t)
                          (declare (ignore result))
                          (if (not condition)
                            (format s "~a.xlsx~%" (pathname-name src))
                            (let ((error-message
                                   (format nil "$[style=:critical]Kezeletlen hiba a ~a.xlsx fájl archiválásakor:$[clr] ~a~%"
                                           (pathname-name src)
                                           (wax::condition-string condition))))
                              (format s error-message)
                              (wax::queue-errorlog obj error-message)))))
                  ;; Otherwise, display <none>.
                  (format s "<semmi>~%"))
                ;; Create fresh file.
                (let ((fresh-file (tk-pending-namestring obj short-tk swap-date
                                                         :only-on-existing-path nil)))
                  ;; Make sure operative TK dir exists
                  (ensure-file-dir-exists fresh-file)
                  (if fresh-file
                    ;; TK path exists, go ahead
                    (if (probe-file fresh-file)
                      (format s "A következõ napi állomány már létezik, $[style=:warn]kihagyás$[clr].~%")
                      (progn
                        ;; Edit it in temp dir.
                        (paste-column wsheet 2 2 181 (first (words tk)))
                        (!'save wbook)
                        (uiop:copy-file temp-file fresh-file)
                        (format s "$[style=:subhead]Következõ napi állomány:$[clr]~%~a.xlsx~%" (pathname-name fresh-file))))
                    ;; TK path doesn't exist, display <none>.
                    (format s "$[style=:criticalTK mappa hiányzik!$[clr]~%")))
                ;; Step progress bar.
                (progress intf)))
        ;; Delete temporary file.
        (delete-file temp-file)))))


;;; ---------------------------------------------------------------
;;; List files


(defun namestring-date (namestring)
  (first (str:words (pathname-name namestring))))


(defmethod list-files ((obj wax-app))
  (with-progress-window (intf (length *tk-list*) obj :title "Elérhetõ fájlok listázása" :stream s
                              :custom-styles *smrg-styles*)
    (let* ((results  '())
           (foreigns '())
           (pane     (wax::text-disp intf))
           (pane-top (wax::scroll-pos pane)))
      ;; Collecting results.
      (loop for tk in *tk-list* doing
            (push (tk-workbooks obj tk) results)
            (push (tk-foreign-namestrings obj tk) foreigns)
            (progress intf))
      (setf results (apply #'nconc (nreverse results))
            foreigns (apply #'nconc (nreverse foreigns)))
      ;; Displaying foreign files.
      (when foreigns
        (format s "$[style=:errorhead]Rossz könyvtárban lévõ fájlok:$[clr]~2%")
        (dolist (foreign foreigns)
          (format s "$[style=:warn]  ~a$[clr]~%" foreign)))
      ;; Displaying editable files grouped by date.
      (when results
        (format s "~2%$[style=:head]A TK-k számára jelenleg elérhetõ fájlok dátum szerint csoportosítva:$[clr]~2%")
        (let ((unique-dates (delete-duplicates
                             (mapcar #'(lambda (namestring)
                                         (namestring-date namestring))
                                     results)
                             :test #'string=)))
          ;; Going through individual dates.
          (dolist (date unique-dates)
            (let ((filtered (remove-if #'(lambda (namestring)
                                           (string/= (namestring-date namestring) date))
                                       results)))
              (format s "~2%$[style=:subhead]~a (~a db.):$[clr]~2%" date (length filtered))
              (dolist (namestring filtered)
                (format s "    ~a~%" (pathname-name namestring)))))))
      ;; Scroll back to top.
      (wax::scroll-back pane pane-top))))


;;; ---------------------------------------------------------------
;;; Main


(defparameter *pkeys* '(:username :workdir :tkdirs :template :drive :archives :result))


(defun init-app ()
  (make-instance 'wax-app
   :state `(:first-run  t
            :username   nil
            :workdir    ""
            :tkdirs     ""
            :template   ""
            :drive      "X"
            :archives   ""
            :result     ""
            :merge-date ,(date-string)
            :swap-date  ,(date-string :day-offset 1 :skip-weekend t))
   :execute-fn #'worker))


(defun worker (obj command)
  (when (connection-ensured-p obj)
    (case command
      (:merge   (merge-files obj))
      (:archive (archive-and-refresh-files obj))
      (:list    (list-files obj)))))


(defun smrg-start ()
  (in-package :smrg)
  (uiop:setup-temporary-directory) ; Without this, UIOP would use the tempdir of the build system.
  (let ((obj (init-app)))
    ;; Initialize app state  -------------------------------------------
    (load-state obj :keys *pkeys*)
    ;; Load "_params_.lisp"
    (setf (state obj)
          (append (state obj)
                  (first (load-forms (appfile "_params_.lisp") :count 1) )))
    ;; GUI  ------------------------------------------------------------
    (wg-window `(:title "Rendkívüli esemény jelentések összegzése"
                 :best-width 800
                 :max-height 80
                 :confirm-destroy-function
                 ,#'(lambda (&rest rest)
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
               " Csatolt meghajtó" ;  ----------------------------------
               (wg-text-input 
                #'(lambda (text &rest rest)
                    (declare (ignore rest))
                    (setf (get-state obj :drive) text))
                (get-state obj :drive))
               " SPS munkakönyvtár" ;  ---------------------------
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



               " Archívum" ;  ----------------------------------------
               (wg-text-input 
                #'(lambda (text &rest rest)
                    (declare (ignore rest))
                    (setf (get-state obj :archives) text))
                (get-state obj :archives))



               " Táblázat sablon" ;  --------------------------------------------
               (wg-text-input 
                #'(lambda (text &rest rest)
                    (declare (ignore rest))
                    (setf (get-state obj :template) text))
                (get-state obj :template))
               " Eredmény" ;  ---------------------------------------------
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
                              (wax-execute obj :errorsink-on t :args :merge)))
               "" ""
               " Összeolvasás dátuma" ;  -----------------------------
               (wg-text-input 
                #'(lambda (text &rest rest)
                    (declare (ignore rest))
                    (setf (get-state obj :merge-date) text))
                (get-state obj :merge-date))
               " Következõ napi dátum" ;  ---------------------------------------
               (wg-text-input 
                #'(lambda (text &rest rest)
                    (declare (ignore rest))
                    (setf (get-state obj :swap-date) text))
                (get-state obj :swap-date))
               ""
               (wg-button "Fájlok cseréje" ;  -----------------------------------
                          #'(lambda (interface)
                              (declare (ignore interface))
                              (save-state obj :keys *pkeys*)
                              (wax-execute obj :errorsink-on t :args :archive)))
               "" ""
               (wg-button "Elérhetõ fájlok" ;  -----------------------------------
                          #'(lambda (interface)
                              (declare (ignore interface))
                              (save-state obj :keys *pkeys*)
                              (wax-execute obj :errorsink-on t :args :list)))
               )))


;;; ---------------------------------------------------------------
;;; Sandbox




#.(disable-ccom-syntax)
