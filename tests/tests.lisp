(in-package #:nhooks/tests)

(defun void-function ()
  (format t "Void handler"))

(defvar test-hook (make-instance 'nhooks:hook-void))
(nhooks:add-hook test-hook #'void-function)

(define-test default-hook ()
  "Run default void hook"
  (assert= (length (nhooks:handlers test-hook))
           1)
  (assert-equal (nhooks:run-hook test-hook)
                '(nil)))

(defun add1 (n)
  (1+ n))

(declaim (ftype (function (number) number) mul2))
(defun mul2 (n)
  (* 2 n))

(define-test default-numeric-hook ()
  "Run default numeric hook"
  (assert-equal (nhooks:run-hook
                 (make-instance 'nhooks:hook-number->number :handlers (list #'add1))
                 17)
                '(18)))

(define-test defualt-with-multiple-handlers ()
  "Run default numeric hook with multiple handlers"
  (assert-equal (nhooks:run-hook
                 (make-instance 'nhooks:hook-number->number
                                :handlers (list #'add1
                                                (make-instance 'nhooks:handler
                                                               :fn (lambda (n) (* 2 n))
                                                               :name 'mul2)))
                 17)
                '(18 34)))

(define-test no-duplicates ()
  "Don't add duplicate handlers."
  (assert-equal (let ((hook (make-instance 'nhooks:hook-number->number :handlers (list #'add1))))
                  (nhooks:add-hook hook #'add1)
                  (nhooks:run-hook hook 17))
                '(18))
  (assert-equal (let ((hook (make-instance 'nhooks:hook-number->number :handlers (list #'add1))))
                  (nhooks:add-hook hook (make-instance 'nhooks:handler :fn (lambda (n) (+ 1 n)) :name 'add1))
                  (nhooks:run-hook hook 17))
                '(18)))

(define-test combine-handlers ()
  "Combine handlers"
  (assert= (let ((hook (make-instance 'nhooks:hook-number->number
                                      :handlers (list #'add1 #'mul2)
                                      :combination #'nhooks:combine-composed-hook)))
             (nhooks:run-hook hook 17))
           35)
  (assert= (let ((hook (make-instance 'nhooks:hook-number->number
                                      :combination #'nhooks:combine-composed-hook)))
             (nhooks:run-hook hook 17))
           17))

(define-test remove-hook ()
  "Remove handler from hook"
  (assert-eq (let* ((handler1 #'add1)
                    (hook (make-instance 'nhooks:hook-number->number
                                         :handlers (list handler1
                                                         (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
               (nhooks:remove-hook hook 'mul3)
               (nhooks:remove-hook hook handler1)
               (nhooks:run-hook hook 17))
             nil))

(define-test disable-hook ()
  "Disable hook"
  (let* ((handler1 #'add1)
         (handler2 #'mul2))
    (assert= (let* ((hook (make-instance 'nhooks:hook-number->number
                                         :handlers (list handler1
                                                         (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
               (nhooks:disable-hook hook)
               (length (nhooks:disabled-handlers hook)))
             2)
    (assert-eq (let* ((hook
                        (make-instance 'nhooks:hook-number->number
                                       :handlers (list (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
                 (nhooks:disable-hook hook)
                 (nhooks:add-hook hook handler1)
                 (nhooks:disable-hook hook)
                 (eq (first (nhooks:disabled-handlers hook))
                     handler1))
               t)
    (assert= (let* ((hook
                      (make-instance 'nhooks:hook-number->number
                                     :handlers (list handler1
                                                     (make-instance 'nhooks:handler :fn (lambda (n) (* 3 n)) :name 'mul3)))))
               (nhooks:disable-hook hook)
               (nhooks:enable-hook hook)
               (length (nhooks:disabled-handlers hook)))
             0)
    (assert-equal (let* ((hook
                           (make-instance 'nhooks:hook-number->number
                                          :handlers (list handler1 handler2))))
                    (nhooks:disable-hook hook handler1)
                    (list (first (nhooks:handlers hook))
                          (first (nhooks:disabled-handlers hook))))
                  (list handler2 handler1))))

(define-test no-unnamed-lambdas ()
  "Don't accept lambdas without names."
  (assert-error 'simple-error
                (make-instance 'nhooks:handler :fn (lambda (n) (+ 1 n)))))

(define-test global-hooks ()
  "Global hooks"
  (assert-true (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo)))
                 (eq hook (nhooks:find-hook 'foo))))
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo)))
    (assert-true (eq (nhooks:find-hook 'foo) hook)))
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo
                :object #'mul2)))
    (assert-false (eq (nhooks:find-hook 'foo)
                      hook)))
  (let ((hook (nhooks:define-hook 'nhooks:hook-number->number 'foo
                :object #'mul2)))
    (assert-true (eq (nhooks:find-hook 'foo #'mul2)
                     hook))))

(define-test find-handler ()
  "Find handler"
  (let* ((add-handler #'add1)
         (mul-handler #'mul2)
         (other-handler #'void-function)
         (handlers (list add-handler mul-handler)))
    (assert-eq (nhooks:find-handler 'add1 handlers)
               add-handler)
    (assert-eq (nhooks:find-handler mul-handler handlers)
               mul-handler)
    (assert-eq (nhooks:find-handler other-handler handlers)
               nil)))

(define-test wait ()
  "Wait on"
  (let* ((x 17)
         (side-effect 0)
         (hook (make-instance 'nhooks:hook-number->number
                              :handlers (list #'mul2))))
    (let ((th (bt:make-thread (lambda ()
                                (nhooks:wait-on hook y
                                  (incf side-effect)
                                  (1+ y))))))
      (bt:make-thread (lambda ()
                        (sleep 0.5)
                        (nhooks:run-hook hook x)))
      (assert= (bt:join-thread th) 18)
      (assert= side-effect 1)
      (nhooks:run-hook hook x)
      (assert= side-effect 1))))
