(in-package :nhooks)

(defclass handler ()
  ((name :initarg :name
         :accessor name
         :type symbol
         :initform nil
         :documentation "
Name of the handler.
It defaults to the function name if `fn' is a named function.
This is useful so that the user can build handlers out of anonymous functions.")
   (description :initarg :description
                :accessor description
                :type string
                :initform ""
                :documentation "
Description of the handler.  This is purely informative.")
   (fn :initarg :fn
     :accessor fn
     :type function
     :initform (required-argument 'fn)
     :documentation "
The handler function.  It can be an anonymous function.")
   (place :initarg :place
          :accessor place
          :type (or symbol list)
          :initform nil
          :documentation "
If the handler is meant to be a setter, PLACE describes what is set.
PLACE can be a symbol or a pair (CLASS SLOT).
This can be left empty if the handler is not a setter.")
   (value :initarg :value
          :accessor value
          :type t
          :initform nil
          :documentation "
If the handler is meant to be a setter, VALUE can be used to describe what FN is
going to set to PLACE.
In particular, PLACE and VALUE can be used to compare handlers.
This can be left empty if the handler is not a setter."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "Handlers are wrappers around functions used in typed hooks.
They serve two purposes as opposed to regular functions:

- They can embed a NAME so that anonymous functions can be conveniently used in hooks.
- If the handler is meant to be a setter, the PLACE and VALUE slots can be used
  to identify and compare setters.

With this extra information, it's possible to compare handlers and, in particular, avoid duplicates in hooks.

Handlers are `funcall'able. If you subclass those, don't forget to add
a `closer-mop:funcallable-standard-class' to retain this property."))

(defmethod print-object ((handler handler) stream)
  (print-unreadable-object (handler stream :type t :identity t)
    (format stream "~a" (name handler))))

(defun probe-ftype (function ftype)
  "Invoke compiler to probe the type of FUNCTION.

If type of FUNCTION contradicts with FTYPE, raise an error.

If FTYPE is nil, nothing is done."
  (when ftype
    (handler-case
        (let ((*error-output* (make-broadcast-stream))) ;; silence!
          (compile nil
                   `(lambda ()
                      (let ((fn ,function))
                        (declare (type ,ftype fn))
                        fn))))
      (style-warning (c)
        (error "Handler function ~a does not match expected type ~a.
Detail: ~a" function ftype c))))
  (values))

(defmethod initialize-instance :after ((handler handler) &key &allow-other-keys)
  (closer-mop:set-funcallable-instance-function
   handler (lambda (&rest args)
             (apply (fn handler) args)))
  (with-slots (name fn) handler
    (setf name (or name (name fn)))
    (unless name
      (error "Can't make a handler without a name"))))

(defmethod equals ((fn1 handler) (fn2 handler))
  "Return non-nil if FN1 and FN2 are equal.
Handlers are equal if they are setters of the same place and same value, or if
their names are equal."
  (cond
    ((or (and (place fn1)
              (not (place fn2) ))
         (and (place fn2)
              (not (place fn1) )))
     nil)
    ((and (place fn1)
          (place fn2))
     (and (equal (place fn1)
                 (place fn2))
          (equal (value fn1)
                 (value fn2))))
    (t
     (eq (name fn1)
         (name fn2)))))

(defmethod equals ((h handler) (f function)) (eq (name h) (name f)))
(defmethod equals ((f function) (h handler)) (eq (name h) (name f)))
(defmethod equals ((f1 function) (f2 function)) (eq (name f1) (name f2)))
(defmethod equals (obj (fn handler)) (eq (name fn) obj))
(defmethod equals ((fn handler) obj) (eq (name fn) obj))
(defmethod equals (obj (fn function)) (eq (name fn) obj))
(defmethod equals ((fn function) obj) (eq (name fn) obj))
(defmethod equals (obj1 obj2) (eq obj1 obj2))

(defmethod name ((symbol symbol)) symbol)
(defmethod name ((fn function))
  (let ((fname (nth-value 2 (function-lambda-expression fn))))
    (when (and (symbolp fname)
               (not (keywordp fname)))
      fname)))
(defmethod fn ((symbol symbol)) (symbol-function symbol))
(defmethod fn ((function function)) function)
(defmethod description ((symbol symbol)) (documentation symbol 'function))

(defclass hook ()
  ((handler-type :initarg :handler-type
                 :reader handler-type
                 :initform nil
                 :documentation
                 "The exptected function type of handlers.")
   (handlers-alist :initarg :handlers-alist
                   :accessor handlers-alist
                   :type list
                   :initform '()
                   :documentation "A list with elements of the form (HANDLER . ENABLE-P).

`run-hook' only runs HANDLERs associated with non nil ENABLE-P.  This
is useful it the user wishes to disable some or all handlers without
removing them from the hook.")
   (combination :initarg :combination
                :accessor combination
                :type (or symbol function)
                :initform #'default-combine-hook
                :documentation "
This can be used to reverse the execution order, return a single value, etc."))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "This hook class serves as support for typed-hook.

Typing in hook is crucial to guarantee that a hook is well formed, i.e. that
its handlers accept the right argument types and return the right value types.

Hooks are `funcall'able. If you subclass those, don't forget to add a
`closer-mop:funcallable-standard-class' to retain this
property. `define-hook-type' does that for you."))

(defmethod initialize-instance :after ((hook hook) &key handlers disabled-handlers &allow-other-keys)
  (closer-mop:set-funcallable-instance-function hook (alexandria:curry #'run-hook hook))
  (setf (handlers-alist hook)
        (append (mapcar (alexandria:rcurry #'cons t) handlers)
                (mapcar (alexandria:rcurry #'cons nil) disabled-handlers)
                (handlers-alist hook)))
  (dolist (handler (mapcar #'first (handlers-alist hook)))
    (restart-case
        (probe-ftype (fn handler) (handler-type hook))
      (remove-handler () :report "Remove this handler."
        (remove-hook hook handler))
      (reckless-continue () :report "Retain this handler nonetheless."))))

(defmethod handlers ((hook hook)) (mapcar #'first (remove-if-not #'rest (handlers-alist hook))))
(defmethod disabled-handlers ((hook hook)) (mapcar #'first (remove-if #'rest (handlers-alist hook))))

(defmacro with-disable-handler-restart ((handler) &body body)
  "This is intended to wrap all handler executions."
  `(restart-case
       (progn ,@body)
     (disable-handler ()
       :report
       (lambda (stream)
         (format stream "Disable handler ~a which causes the error." ,handler))
       (disable-hook *hook* ,handler))))

(defmethod default-combine-hook ((hook hook) &rest args)
  "Return the list of the results of the HOOK handlers applied from youngest to
oldest to ARGS.
Return '() when there is no handler.
This is an acceptable `combination' for `hook'."
  (mapcan (lambda (handler-entry)
            (when (cdr handler-entry)
              (with-disable-handler-restart ((first handler-entry))
                (with-hook-restart
                  (list (apply (first handler-entry) args))))))
          (handlers-alist hook)))

(defmethod combine-hook-until-failure ((hook hook) &rest args)
  "Return the list of values until the first nil result.
Handlers after the failing one are not run.

This is an acceptable `combination' for `hook'."
  (let ((result nil))
    (loop for (handler . enable-p) in (handlers-alist hook)
          when enable-p
            do (let ((res (with-disable-handler-restart (handler)
                            (with-hook-restart
                              (apply handler args)))))
               (push res result)
               (unless res (return))))
    (nreverse result)))

(defmethod combine-hook-until-success ((hook hook) &rest args)
  "Return the value of the first non-nil result.
Handlers after the successful one are not run.

You need to check if the hook has handlers to know if a NIL return value is due
to all handlers failing or an empty hook.

This is an acceptable `combination' for `hook'."
  (loop for (handler . enable-p) in (handlers-alist hook)
          thereis (and enable-p
                       (with-disable-handler-restart (handler)
                         (with-hook-restart
                           (apply handler args))))))

(defmethod combine-composed-hook ((hook hook) &rest args)
  "Return the result of the composition of the HOOK handlers on ARGS, from
oldest to youngest.
Without handler, return ARGS as values.
This is an acceptable `combination' for `hook'."
  (let ((result args)
        (reversed-alist (reverse (handlers-alist hook))))
    (loop for (handler . enable-p) in reversed-alist
          when enable-p
            do (with-disable-handler-restart (handler)
                 (with-hook-restart
                   (setf result (multiple-value-list (apply handler result))))))
    (values-list result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun add-hook-internal (hook handler &key append)
  "Add HANDLER to HOOK if not already in it.
Return HOOK.
HANDLER is also not added if it's disabled.
If APPEND is non-nil, HANDLER is added at the end."
  (serapeum:synchronized (hook)
    (unless (assoc handler (handlers-alist hook) :test #'equals)
      (if append
          (alexandria:appendf (symbol-value hook) (list (cons handler t)))
          (push (cons handler t) (handlers-alist hook))))
    hook))

(declaim (ftype (function ((or handler function symbol) list) (or handler function boolean)) find-handler))
(defun find-handler (handler-or-name handlers)
  "Return handler matching HANDLER-OR-NAME in HANDLERS sequence."
  (find handler-or-name handlers :test #'equals))

(defmethod remove-hook ((hook hook) handler-or-name)
  "Remove handler entry matching HANDLER-OR-NAME from handlers-alist in HOOK.
HANDLER-OR-NAME is either a handler object or a symbol.  Return HOOK's
handlers-alist."
  (serapeum:synchronized (hook)
    (let ((handler-entry (assoc handler-or-name (handlers-alist hook) :test #'equals)))
      (when handler-entry
        (setf (handlers-alist hook)
              (delete handler-entry (handlers-alist hook)))))
    (handlers-alist hook)))

(defmethod run-hook ((hook hook) &rest args)
  "Invoke all the HOOK handlers with the default `combination'.

Alternatively, use `funcall' of the hook for the same effect."
  (let ((*hook* hook))
    (apply (combination hook) hook args)))

(defmethod run-hook-with-args-until-failure ((hook hook) &rest args)
  "This is equivalent to setting the combination function to
`combine-hook-until-failure' and calling `run-hook'."
  (apply #'combine-hook-until-failure hook args))

(defmethod run-hook-with-args-until-success ((hook hook) &rest args)
  "This is equivalent to setting the combination function to
`combine-hook-until-success' and calling `run-hook'."
  (apply #'combine-hook-until-success hook args))

(defmethod disable-hook ((hook hook) &rest handlers)
  "Disable HANDLERS.
Without HANDLERS, disable all of them."
  (serapeum:synchronized (hook)
    (dolist (handler-entry (handlers-alist hook))
      (when (or (not handlers)
                (member (first handler-entry) handlers :test #'equals))
        (rplacd handler-entry nil)))))

(defmethod enable-hook ((hook hook) &rest handlers)
  "Enable HANDLERS.
Without HANDLERS, enable all of them."
  (serapeum:synchronized (hook)
    (dolist (handler-entry (handlers-alist hook))
      (when (or (not handlers)
                (member (first handler-entry) handlers :test #'equals))
        (rplacd handler-entry t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks.

;; TODO: cl-hooks uses symbol properties.  Is it a good idea?
(defvar %hook-table (make-hash-table :test #'equal)
  "Global hook table.")

(defun define-hook (hook-type name &key object handlers disabled-handlers combination)
  "Return a globally-accessible hook.
The hook can be accessed with `find-hook' at (list NAME OBJECT).
OBJECT is an arbitrary value the hook is associated to."
  (let ((hook
          (apply #'make-instance hook-type
                 :handlers handlers
                 :disabled-handlers disabled-handlers
                 (if combination
                     (list :combination combination)
                     '()))))
    (setf (gethash (list name object) %hook-table)
          hook)
    hook))

(defun find-hook (name &optional object)
  "Return the global hook with name NAME associated to OBJECT, if provided.
The following examples return different hooks:
- (find-hook 'foo-hook)
- (find-hook 'foo-hook 'bar-class)
- (find-hook 'foo-hook (make-instance 'bar-class))"
  (gethash (list name object) %hook-table))

(defmethod add-hook ((hook hook) handler &key append)
  "Add HANDLER to HOOK.  Return HOOK.
Check HANDLER's type according to the `handler-type' slot of HOOK."
  (with-simple-restart (skip "Do not add this handler.")
    (with-simple-restart (reckless-continue "Add this handler nonetheless.")
      (probe-ftype (fn handler) (handler-type hook)))
    (add-hook-internal hook handler :append append)))

(defmacro define-hook-type (name type)
  "Define hook class.
Type must be something like:

  (function (string) (values integer t))

The `handler-type' of the defined hook class has `:class' allocation
type, so that all hooks of such class have the same `handler-type'."
  (let* ((name (string name))
         (hook-class-name (intern (serapeum:concat "HOOK-" name))))
    `(defclass ,hook-class-name (hook)
       ((handler-type :initform ',type :allocation :class))
       (:metaclass closer-mop:funcallable-standard-class))))

;; TODO: Allow listing all the hooks?

(define-hook-type void (function ()))
(define-hook-type string->string (function (string) string))
(define-hook-type number->number (function (number) number))
(define-hook-type any (function (&rest t)))

(defmacro on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

ARGS can be
- A symbol if there's only one argument to the callback.
- A list of arguments.
- An empty list, if the hook handlers take no argument."
  (let ((handler-name (gensym "on-hook-handler"))
        (args (alexandria:ensure-list args)))
    `(add-hook
      ,hook (make-instance 'handler
                           :fn (lambda ,args
                                 (declare (ignorable ,@args))
                                 ,@body)
                           :name (quote ,handler-name)))))

(defmacro once-on (hook args &body body)
  "Attach a handler with ARGS and BODY to the HOOK.

Remove the handler after it fires the first time.

See `on'."
  (let ((handler-name (gensym "once-on-hook-handler"))
        (args (alexandria:ensure-list args)))
    (alexandria:once-only (hook)
      `(add-hook
        ,hook (make-instance 'handler
                             :fn (lambda ,args
                                   (declare (ignorable ,@args))
                                   (remove-hook ,hook (quote ,handler-name))
                                   ,@body)
                             :name (quote ,handler-name))))))
