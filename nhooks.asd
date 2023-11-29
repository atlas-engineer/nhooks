;;;; nhooks.asd
;;; Adapted from serapeum/contrib/hooks

(defsystem "nhooks"
  :version "1.2.2"
  :description "Improved hooks facility inspired by Serapeum."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :homepage "https://github.com/atlas-engineer/nhooks"
  :bug-tracker "https://github.com/atlas-engineer/nhooks/issues"
  :source-control (:git "https://github.com/atlas-engineer/nhooks.git")
  :license "MIT"
  :depends-on ("bordeaux-threads" "serapeum" "closer-mop")
  :components ((:file "package")
               (:file "nhooks"))
  :in-order-to ((test-op (test-op "nhooks/tests"))))

(defsystem "nhooks/tests"
  :depends-on ("nhooks" "lisp-unit2")
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests"))
  :perform (test-op (op c)
                    (eval-input
                     "(lisp-unit2:run-tests
                       :package :nhooks/tests
                       :run-contexts #'lisp-unit2:with-summary-context)")))
