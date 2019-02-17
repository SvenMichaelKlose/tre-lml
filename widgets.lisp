(fn price-string (x)
  (!= (number x)
    (+ (string (integer !))
       (lang en "."
             de ",")
       (!= (string (* ! 100))
         (subseq ! (- (length !) 2))))))

(defmacro make-schema-widget (args predicate &body body)
  `{:predicate ,predicate
    :maker     #'(,args ,@body)})

(const *widgets*
       (list (make-schema-widget (comp props schema name x) [& _.is_editable
                                                               (equal _.type "selection")]
               `(select :key ,name
                        :on-change ,[comp.onchange name (form-select-get-selected-option-value (_.element))]
                  ,@(@ [`(option :value ,_.name
                                 ,@(when (equal (aref props.data name) _.name)
                                     `(:selected "yes"))
                                 ,(aref _.descr "en"))]
                       schema.selection)))
             (make-schema-widget (comp props schema name x) [& _.is_editable
                                                               (equal _.type "string")]
               `(input :type "text"
                       :key ,name
                       ,@(!? schema.size `(:size ,!))
                       ,@(!? schema.pattern `(:pattern ,!))
                       ,@(!? schema.is_required `(:required "yes"))
                       :on-change ,[comp.onchange name (_.element).value]
                       :value ,x))
             (make-schema-widget (comp props schema name x) [& _.is_editable
                                                               (equal _.type "password")]
               `(input :type "password"
                       :key ,name
                       ,@(!? schema.size `(:size ,!))
                       ,@(!? schema.pattern `(:pattern ,!))
                       ,@(!? schema.is_required `(:required "yes"))
                       :on-change ,[comp.onchange name (_.element).value]
                       :value ,x))
             (make-schema-widget (comp props schema name x) [& _.is_editable
                                                               (equal _.type "text")]
               `(textarea :key ,name
                          ,@(!? schema.pattern `(:pattern ,!))
                          ,@(!? schema.is_required `(:required "yes"))
                          :on-change ,[comp.onchange name (_.element).value]
                  ,x))
             (make-schema-widget (comp props schema name x) [equal _.type "text"]
               `(pre ,x))
             (make-schema-widget (comp props schema name x) [identity t]
               x)))
