(fn price-string (x)
  (!= (number x)
    (+ (string (integer !))
       (lang en "."
             de ",")
       (!= (string (* ! 100))
         (subseq ! (- (length !) 2))))))

(var *autoform-widgets* nil)

(defmacro def-autoform-widget (args predicate &body body)
  `(= *autoform-widgets* (append *autoform-widgets*
                                 (list {:predicate ,predicate
                                        :maker     #'(,args ,@body)}))))

(def-autoform-widget (comp props schema name x) [& _.editable?
                                                   (equal _.type "selection")]
  `(select :key ,name
           :on-change ,[comp.onchange name (form-select-get-selected-option-value (_.element))]
     ,@(@ [with (ov   (aref props.data _)
                 txt  (aref schema.options _))
            `(option :value ,_
                     ,@(? (eql _ x)
                          `(:selected "yes"))
               ,txt)]
          (property-names schema.options))))

(def-autoform-widget (comp props schema name x) [& _.editable?
                                                    (equal _.type "string")]
  `(input :type "text"
          :key ,name
          ,@(!? schema.size `(:size ,!))
          ,@(!? schema.pattern `(:pattern ,!))
          ,@(!? schema.required? `(:required "yes"))
          :on-change ,[comp.onchange name (_.element).value]
          :value ,x))

(def-autoform-widget (comp props schema name x) [& _.editable?
                                                    (equal _.type "password")]
  `(input :type "password"
          :key ,name
          ,@(!? schema.size `(:size ,!))
          ,@(!? schema.pattern `(:pattern ,!))
          ,@(!? schema.required? `(:required "yes"))
          :on-change ,[comp.onchange name (_.element).value]
          :value ,x))

(def-autoform-widget (comp props schema name x) [& _.editable?
                                                    (equal _.type "text")]
  `(textarea :key ,name
             ,@(!? schema.pattern `(:pattern ,!))
             ,@(!? schema.required? `(:required "yes"))
             :on-change ,[comp.onchange name (_.element).value]
     ,x))

(def-autoform-widget (comp props schema name x) [equal _.type "text"]
  `(pre ,x))

(def-autoform-widget (comp props schema name x) [identity t]
  x)

(fn make-fields-editable (schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref schema i).editable? t)))
