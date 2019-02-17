(fn price-string (x)
  (!= (number x)
    (+ (string (integer !))
       (lang en "."
             de ",")
       (!= (string (* ! 100))
         (subseq ! (- (length !) 2))))))

(defmacro make-autoform-widget (args predicate &body body)
  `{:predicate ,predicate
    :maker     #'(,args ,@body)})

(const *autoform-widgets*
       (list (make-autoform-widget (comp props schema name x) [& _.editable?
                                                                 (equal _.type "selection")]
               `(select :key ,name
                        :on-change ,[comp.onchange name (form-select-get-selected-option-value (_.element))]
                  ,@(@ [`(option :value ,_.name
                                 ,@(when (equal (aref props.data name) _.name)
                                     `(:selected "yes"))
                                 ,(aref _.descr "en"))]
                       schema.selection)))
             (make-autoform-widget (comp props schema name x) [& _.editable?
                                                                 (equal _.type "string")]
               `(input :type "text"
                       :key ,name
                       ,@(!? schema.size `(:size ,!))
                       ,@(!? schema.pattern `(:pattern ,!))
                       ,@(!? schema.required? `(:required "yes"))
                       :on-change ,[comp.onchange name (_.element).value]
                       :value ,x))
             (make-autoform-widget (comp props schema name x) [& _.editable?
                                                                 (equal _.type "password")]
               `(input :type "password"
                       :key ,name
                       ,@(!? schema.size `(:size ,!))
                       ,@(!? schema.pattern `(:pattern ,!))
                       ,@(!? schema.required? `(:required "yes"))
                       :on-change ,[comp.onchange name (_.element).value]
                       :value ,x))
             (make-autoform-widget (comp props schema name x) [& _.editable?
                                                                 (equal _.type "text")]
               `(textarea :key ,name
                          ,@(!? schema.pattern `(:pattern ,!))
                          ,@(!? schema.required? `(:required "yes"))
                          :on-change ,[comp.onchange name (_.element).value]
                  ,x))
             (make-autoform-widget (comp props schema name x) [equal _.type "text"]
               `(pre ,x))
             (make-autoform-widget (comp props schema name x) [identity t]
               x)))

(fn make-fields-editable (schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref schema i).editable? t)))
