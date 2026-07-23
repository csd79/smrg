(require "automation")

(defsystem "smrg"
  :description "Merging spreadsheets on a SharPoint doc library"
  :author      "Denes Cselovszki <denes.cselovszki@gmail.com>"
  :version     "0.04"
  :depends-on  ("utils" "ccom4" "ccoffice" "cl-ppcre" "achar" "wax" "str" "local-time" "splt")
  :serial      t
  :components  ((:file "package")
                (:file "fli-templates")
                (:file "smrg")))
