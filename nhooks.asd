;;;; nhooks.asd
;;; Adapted from serapeum/contrib/hooks

(defsystem "nhooks"
  :version "1.1.1"
  :description "Improved hooks facility inspired by Serapeum."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("bordeaux-threads" "serapeum" "closer-mop")
  :in-order-to ((test-op (test-op "nhooks/tests")))
  :components ((:file "package")
               (:file "nhooks")))

(defsystem "nhooks/tests"
  :description "Test suite for nhooks."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("serapeum" "fiveam")
  :perform (test-op (o c) (symbol-call :5am :run! (read-from-string "nhooks/tests::nhooks")))
  :components ((:file "tests")))
