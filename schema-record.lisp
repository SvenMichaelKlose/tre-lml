(defclass (schema-record lml-component) (init-props)
  (super init-props)
  (set-state props.data)
  this)

(defmethod schema-record onchange (k v)
  (let ndata (add-properties state k v)
    (!? props.onchange
        (funcall ! ndata)
        (print "onchange missing"))))
;        (set-state ndata))))

(defmethod schema-record _printed-value (name)
  (let v (aref state name)
    (| (!? (aref props.schema name).printer
           (funcall ! v)
           v)
       "")))

(defmethod schema-record _render-typed-field (name)
  (@ (i props.widgets)
    (!= (aref props.schema name)
      (& (funcall i.predicate !)
         (return (funcall i.maker this props ! name (_printed-value name)))))))

(defmethod schema-record _render-field (x)
  (?
    (function? x)   (funcall x state)
    (string? x)     (_render-typed-field x)
    x))

(finalize-class schema-record)
(declare-lml-component schema-record)


(defclass (schema-record-field schema-record) (init-props)
  (super init-props)
  this)

(defmethod schema-record-field render ()
  ($$ (_render-field props.field)))

(finalize-class schema-record-field)
(declare-lml-component schema-record-field)


(defclass (schema-record-row schema-record) (init-props)
  (super init-props)
  this)

(defmethod schema-record-row render ()
  ($$ `(tr ,@(@ [`(td ,@(& (string? _) `(:key ,_))
                    ,(_render-field _))]
                props.fields))))

(finalize-class schema-record-row)
(declare-lml-component schema-record-row)


(defclass (schema-record-list schema-record) (init-props)
  (super init-props)
  this)

(defmethod schema-record-list render-label (name)
  (!? (aref (aref props.schema name) "descr")
      (aref ! (downcase (symbol-name *language*)))))

(defmethod schema-record-list render ()
  ($$ `(div
         ,@(@ [`(tr
                  (td
                    ,(? (string? _)
                        (render-label _)))
                  (td ,@(& (string? _) `(:key ,_))
                    ,(_render-field _)))]
              props.fields))))

(finalize-class schema-record-list)
(declare-lml-component schema-record-list)
