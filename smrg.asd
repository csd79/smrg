(require "automation")

(defsystem "smrg"
  :description "Merging spreadsheets on a SharPoint doc library"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.02"
  :depends-on  ("ccom4" "ccoffice" "cl-ppcre" "achar" "wax" "str" "local-time")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "smrg")))
