(var *autoform-widgets* nil)

(defmacro def-autoform-widget (args predicate &body body)
  `(= *autoform-widgets* (append *autoform-widgets*
                                 (list {:predicate  ,predicate
                                        :maker      #'(,args ,@body)}))))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "selection")]
  `(select :key        ,name
           :on-change  ,[store.write (make-object name ((_.element).get ":checked").value)]
     ,@(@ [`(option :value ,_
                    ,@(!? schema.is_required `(:required "yes"))
                    ,@(? (| (eql _ v)
                            (eql _ schema.default))
                         `(:selected "yes"))
              ,(aref schema.options _))]
          (property-names schema.options))))

(fn autoform-pattern-required (schema)
  `(,@(!? schema.pattern      `(:pattern ,!))
    ,@(!? schema.is_required  `(:required "yes"))))

(fn make-autoform-input-element (typ store name schema v)
  `(input :type  ,typ
          :key   ,name
          ,@(!? schema.size `(:size ,!))
          ,@(autoform-pattern-required schema)
          :on-change  ,[store.write (make-object name (_.element).value)]
          :value      ,(| v schema.default "")))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (in? _.type "string" "password" "email")]
  (make-autoform-input-element (? (eql schema.type "string")
                                  "text"
                                  schema.type)
                               store name schema v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "boolean")]
  `(input :type      "checkbox"
          :key       ,name
          :on-click  ,[store.write (make-object name (not v))]
          ,@(& (| v schema.default)
               '(:checked "1"))))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "text")]
  `(textarea :key  ,name
             ,@(autoform-pattern-required schema)
             :on-change  ,[store.write (make-object name (_.element).value)]
     ,(| v schema.default "")))

(def-autoform-widget (store name schema v) [eql _.type "text"]
  `(pre ,(| v schema.default "")))

(def-autoform-widget (store name schema v) [eql _.type "selection"]
  (aref schema.options v))

(def-autoform-widget (store name schema v) [identity t]
  (| v schema.default ""))

(fn set-schema-items (value what schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref (aref schema i) what) value)))

(fn make-schema-editable (schema &rest fields)
  (apply #'set-schema-items t "is_editable" schema fields))
