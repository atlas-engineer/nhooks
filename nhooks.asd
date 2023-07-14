;;;; nhooks.asd
;;; Adapted from serapeum/contrib/hooks

(defsystem "nhooks"
  :version "1.2.1"
  :description "Improved hooks facility inspired by Serapeum."
  :author "Qiantan Hong <qhong@alum.mit.edu>"
  :homepage "https://github.com/atlas-engineer/nhooks"
  :bug-tracker "https://github.com/atlas-engineer/nhooks/issues"
  :source-control (:git "https://github.com/atlas-engineer/nhooks.git")
  :license "MIT"
  :depends-on ("bordeaux-threads" "serapeum" "closer-mop")
  :in-order-to ((test-op (test-op "nhooks/tests")
                         (test-op "nhooks/tests/compilation")))
  :components ((:file "package")
               (:file "nhooks")))

(defsystem "nhooks/submodules"
  :defsystem-depends-on ("nasdf")
  :class :nasdf-submodule-system)

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
  :depends-on ("nhooks")
  :packages (:nhooks))
