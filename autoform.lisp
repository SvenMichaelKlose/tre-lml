(defclass (autoform lml-component) (init-props)
  (super (merge-properties {:widgets *autoform-widgets*} init-props))
  (set-state props.data)
  this)

(defmethod autoform onchange (k v)
  (!? props.onchange
      (funcall ! (add-properties state k v))
      (print "onchange missing")))

(defmethod autoform _printed-value (schema-item name)
  (let v (aref state name)
    (| (!? schema-item.printer
           (funcall ! v)
           v)
       "")))

(defmethod autoform _render-typed-field (schema-item name)
  (@ (i props.widgets)
    (& (funcall i.predicate schema-item)
       (return (funcall i.maker this props schema-item name (_printed-value schema-item name))))))

(defmethod autoform _render-field (schema-item x)
  (?
    (function? x)   (funcall x state)
    (string? x)     (_render-typed-field schema-item x)
    x))

(finalize-class autoform)
(declare-lml-component autoform)


(defclass (autoform-field autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-field render ()
  (_render-field props.schema props.name))

(finalize-class autoform-field)
(declare-lml-component autoform-field)


(defclass (autoform-list autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-list render ()
  ($$ `(tr ,@(@ [`(td ,@(& (string? _) `(:key ,_))
                    ,(_render-field (aref props.schema _) _))]
                props.fields))))

(finalize-class autoform-list)
(declare-lml-component autoform-list)


(defclass (autoform-panel autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-panel render-label (name)
  (!? (aref (aref props.schema name) "title")
      (aref ! (downcase (symbol-name *language*)))))

(defmethod autoform-panel render ()
  ($$ `(div
         ,@(@ [`(tr
                  (td
                    ,(? (string? _)
                        (render-label _)))
                  (td ,@(& (string? _) `(:key ,_))
                    ,(_render-field (aref props.schema _) _)))]
              props.fields))))

(finalize-class autoform-panel)
(declare-lml-component autoform-panel)
