;;;; nhooks.asd
;;; Adapted from serapeum/contrib/hooks

(defsystem "nhooks"
  :version "1.2.0"
  :description "Improved hooks facility inspired by Serapeum."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("bordeaux-threads" "serapeum" "closer-mop")
  :in-order-to ((test-op (test-op "nhooks/tests")
                         (test-op "nhooks/tests/compilation")))
  :components ((:file "package")
               (:file "nhooks")))

(defsystem "nhooks/tests"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-test-system
  :description "Test suite for nhooks."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :license "MIT"
  :depends-on ("nhooks")
  :targets (:package :nhooks/tests)
  :serial t
  :pathname "tests/"
  :components ((:file "package")
               (:file "tests")))

(defsystem "nhooks/tests/compilation"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-compilation-test-system
  :undocumented-symbols-to-ignore (:name :description :fn :place :value ; handler slots
                                         :handler-type :handlers-alist :combination) ;hook slots
  :depends-on ("nhooks")
  :packages (:nhooks))
