(var *autoform-widgets* nil)

(defmacro def-autoform-widget (args predicate &body body)
  `(= *autoform-widgets* (append *autoform-widgets*
                                 (list {:predicate  ,predicate
                                        :maker      #'(,args ,@body)}))))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "selection")]
  `(select :key        ,name
           :on-change  ,[store.write (make-object name (form-select-get-selected-option-value (_.element)))]
     ,@(@ [`(option :value ,_
                    ,@(!? schema.is_required `(:required "yes"))
                    ,@(? (eql _ v) `(:selected "yes"))
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
          :value      ,v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (in? _.type "string" "password" "email")]
  (make-autoform-input-element (? (eql schema.type "string")
                                  "text"
                                  schema.type)
                               store name schema v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "text")]
  `(textarea :key  ,name
             ,@(autoform-pattern-required schema)
             :on-change  ,[store.write (make-object name (_.element).value)]
     ,v))

(def-autoform-widget (store name schema v) [equal _.type "text"]
  `(pre ,v))

(def-autoform-widget (store name schema v) [identity t]
  v)

(fn make-fields-editable (schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref schema i).is_editable t)))
