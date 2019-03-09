(var *autoform-widgets* nil)

(defmacro def-autoform-widget (args predicate &body body)
  `(= *autoform-widgets* (append *autoform-widgets*
                                 (list {:predicate  ,predicate
                                        :maker      #'(,args ,@body)}))))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "selection")]
  `(select :key ,name
           :on-change ,[store.write (make-object name (form-select-get-selected-option-value (_.element)))]
     ,@(@ [`(option :value ,_
                    ,@(!? schema.is_required `(:required "yes"))
                    ,@(? (eql _ v) `(:selected "yes"))
              ,(aref schema.options _))]
          (property-names schema.options))))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "string")]
  `(input :type "text"
          :key ,name
          ,@(!? schema.size `(:size ,!))
          ,@(!? schema.pattern `(:pattern ,!))
          ,@(!? schema.is_required `(:required "yes"))
          :on-change ,[store.write (make-object name (_.element).value)]
          :value ,v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "password")]
  `(input :type "password"
          :key ,name
          ,@(!? schema.size `(:size ,!))
          ,@(!? schema.pattern `(:pattern ,!))
          ,@(!? schema.is_required `(:required "yes"))
          :on-change ,[store.write (make-object name (_.element).value)]
          :value ,v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "email")]
  `(input :type "email"
          :key ,name
          ,@(!? schema.size `(:size ,!))
          ,@(!? schema.is_required `(:required "yes"))
          :on-change ,[store.write (make-object name (_.element).value)]
          :value ,v))

(def-autoform-widget (store name schema v) [& _.is_editable
                                              (eql _.type "text")]
  `(textarea :key ,name
             ,@(!? schema.pattern `(:pattern ,!))
             ,@(!? schema.is_required `(:required "yes"))
             :on-change ,[store.write (make-object name (_.element).value)]
     ,v))

(def-autoform-widget (store name schema v) [equal _.type "text"]
  `(pre ,v))

(def-autoform-widget (store name schema v) [identity t]
  v)

(fn make-fields-editable (schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref schema i).is_editable t)))
