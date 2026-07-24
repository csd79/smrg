;;; -*- Mode: Common-Lisp; Author: denes.cselovszky@gmail.com -*- 

(in-package "CL-USER")
(load "c:\\Users\\cselovszkid\\.lispworks")
(asdf:load-system "smrg")

(in-package "SMRG")
(setf *independent-exe* t)
(lw:deliver 'smrg:smrg-start
    "c:\\Users\\cselovszkid\\common-lisp\\smrg\\smrg-v0.04.exe"
    5
    :keep-symbols '(*appdir* *independent-exe*)
    :interface :capi
    :console :io
    :multiprocessing t
;    :console t
    :error-on-interpreted-functions t
    :keep-eval t
    :keep-lisp-reader t
    :symbol-names-action nil
    :startup-bitmap-file nil
    :kill-dspec-table nil
    :keep-package-manipulation t
    :keep-debug-mode t
    :compact t
;    :keep-function-name :all
;    :keep-conditions :all
;    :packages-to-keep-externals '(smrg wax ccoffice) ; fn-s called indirectly - wax might not be needed, try without it!
;    :keep-load-function t
    )
