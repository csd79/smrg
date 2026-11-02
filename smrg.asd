(require "automation")

(defsystem "smrg"
  :description    "Merging spreadsheets on a SharPoint doc library"
  :author         "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version        "0.06"
  :depends-on     ("utils" "ccom4" "ccoffice" "cl-ppcre" "achar" "wax" "str" "local-time" "splt")
  :serial         t
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0) (space 0)))
					(funcall next))
  :components     ((:file "package")
                   (:file "fli-templates")
                   (:file "smrg")))
