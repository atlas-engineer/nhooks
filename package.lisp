(defpackage :nhooks
  (:use :common-lisp)
  (:import-from :serapeum
                #:*hook*
                #:add-hook
                #:remove-hook
                #:run-hooks
                #:run-hook
                #:run-hook-until-failure
                #:run-hook-until-success
                #:with-hook-restart)
  (:import-from #:alexandria
                #:required-argument)
  (:export
   #:*hook*
   #:add-hook
   #:remove-hook
   #:run-hooks
   #:run-hook
   #:run-hook-with-args-until-failure
   #:run-hook-with-args-until-success
   #:with-disable-handler-restart
   #:default-combine-hook
   #:combine-hook-until-failure
   #:combine-hook-until-success
   #:combine-composed-hook
   #:find-handler
   #:disable-hook
   #:enable-hook
   #:define-hook
   #:find-hook
   #:define-hook-type
   ;; Handler class:
   #:handler
   #:name
   #:fn
   #:description
   #:place
   #:value
   ;; Hook class:
   #:hook
   #:handler-type
   #:handlers-alist
   #:handlers
   #:disabled-handlers
   #:combination
   ;; Pre-generated types:
   #:hook-void
   #:hook-string->string
   #:hook-number->number
   #:hook-any
   ;; Short hook helpers
   #:on
   #:once-on))
