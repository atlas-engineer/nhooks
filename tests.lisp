(defpackage :nhooks/tests
  (:use :cl :alexandria :fiveam))
(in-package :nhooks/tests)

(def-suite nhooks)
(in-suite nhooks)

(defmacro prove-is (x y)
  `(is (equal ,x ,y)))

(defmacro prove-isnt (x y)
  `(is (not (equal ,x ,y))))

(defmacro subtest (name &body body)
  `(test ,(intern name)
     ,@body))

(defun void-function ()
  (format t "Void handler"))

(defvar test-hook (make-instance 'nhooks:hook-void))
(nhooks:add-hook test-hook #'void-function)

(subtest "Run default void hook"
  (prove-is (length (nhooks:handlers test-hook))
            1)
  (prove-is (nhooks:run-hook test-hook)
            '(nil)))

(defun add1 (n)
  (1+ n))

(declaim (ftype (function (number) number) mul2))
(defun mul2 (n)
  (* 2 n))

(subtest "Run default numeric hook"
  (prove-is (nhooks:run-hook
             (make-instance 'nhooks:hook-number->number :handlers (list #'add1))
             17)
            '(18)))

(subtest "Run default numeric hook with multiple handlers"
  (prove-is (nhooks:run-hook
             (make-instance 'nhooks:hook-number->number
                            :handlers (list #'add1
                                            (make-instance 'nhooks:handler
                                                           :fn (lambda (n) (* 2 n))
                                                           :name 'mul2)))
             17)
            '(18 34)))

(subtest "Don't add duplicate handlers."
  (prove-is (let ((hook (make-instance 'nhooks:hook-number->number :handlers (list #'add1))))
              (nhooks:add-hook hook #'add1)
              (nhooks:run-hook hook 17))
            '(18))
  (prove-is (let ((hook (make-instance 'nhooks:hook-number->number :handlers (list #'add1))))
              (nhooks:add-hook hook (make-instance 'nhooks:handler :fn (lambda (n) (+ 1 n)) :name 'add1))
              (nhooks:run-hook hook 17))
            '(18)))

(subtest "Combine handlers"
  (prove-is (let ((hook (make-instance 'nhooks:hook-number->number
                         :handlers (list #'add1 #'mul2)
                         :combination #'nhooks:combine-composed-hook)))
              (nhooks:run-hook hook 17))
            35)
  (prove-is (let ((hook (make-instance 'nhooks:hook-number->number
                         :combination #'nhooks:combine-composed-hook)))
              (nhooks:run-hook hook 17))
            17))

(subtest "Remove handler from hook"
  (prove-is (let* ((handler1 #'add1)
                   (hook (make-instance 'nhooks:hook-number->number
                          :handlers (list handler1
                                          (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
              (nhooks:remove-hook hook 'mul3)
              (nhooks:remove-hook hook handler1)
              (nhooks:run-hook hook 17))
            nil))

(subtest "Disable hook"
  (let* ((handler1 #'add1)
         (handler2 #'mul2))
    (prove-is (let* ((hook (make-instance 'nhooks:hook-number->number
                            :handlers (list handler1
                                            (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
                (nhooks:disable-hook hook)
                (length (nhooks:disabled-handlers hook)))
              2)
    (prove-is (let* ((hook
                      (make-instance 'nhooks:hook-number->number
                       :handlers (list (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
                (nhooks:disable-hook hook)
                (nhooks:add-hook hook handler1)
                (nhooks:disable-hook hook)
                (eq (first (nhooks:disabled-handlers hook))
                    handler1))
              t)
    (prove-is (let* ((hook
                       (make-instance 'nhooks:hook-number->number
                        :handlers (list handler1
                                        (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
                (nhooks:disable-hook hook)
                (nhooks:enable-hook hook)
                (length (nhooks:disabled-handlers hook)))
              0)
    (prove-is (let* ((hook
                       (make-instance 'nhooks:hook-number->number
                        :handlers (list handler1 handler2))))
                (nhooks:disable-hook hook handler1)
                (list (first (nhooks:handlers hook))
                      (first (nhooks:disabled-handlers hook))))
              (list handler2 handler1))))

(subtest "Don't accept lambdas without names."
  (signals simple-error
    (make-instance 'nhooks:handler :fn (lambda (n) (+ 1 n)))))

(subtest "Global hooks"
  (prove-is (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo)))
              (eq hook (nhooks:find-hook 'foo)))
            t)
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo)))
    (prove-is (nhooks:find-hook 'foo)
              hook))
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo
                :object #'mul2)))
    (prove-isnt (nhooks:find-hook 'foo)
                hook))
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo
                :object #'mul2)))
    (prove-is (nhooks:find-hook 'foo #'mul2)
              hook)))

(subtest "Find handler"
  (let* ((add-handler #'add1)
         (mul-handler #'mul2)
         (other-handler #'void-function)
         (handlers (list add-handler mul-handler)))
    (prove-is (nhooks:find-handler 'add1 handlers)
              add-handler)
    (prove-is (nhooks:find-handler mul-handler handlers)
              mul-handler)
    (prove-is (nhooks:find-handler other-handler handlers)
              nil)))
