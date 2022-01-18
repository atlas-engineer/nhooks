;;;; nhooks.asd
;;; Adapted from serapeum/contrib/hooks

(defsystem "nhooks"
  :description "Improved hooks facility built on serapeum."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("serapeum")
  :in-order-to ((test-op (test-op "nhooks/tests")))
  :components ((:file "nhooks")))

(defsystem "nhooks/tests"
  :description "Test suite for nhooks."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("serapeum" "fiveam")
  :perform (test-op (o c) (symbol-call :5am :run! (read-from-string "nhooks/tests::nhooks")))
  :components ((:file "tests")))
