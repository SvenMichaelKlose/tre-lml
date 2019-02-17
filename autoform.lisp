(defclass (autoform lml-component) (init-props)
  (super (merge-properties {:widgets *autoform-widgets*} init-props))
  (set-state props.data)
  this)

(defmethod autoform onchange (k v)
  (let ndata (add-properties state k v)
    (!? props.onchange
        (funcall ! ndata)
        (print "onchange missing"))))
;        (set-state ndata))))

(defmethod autoform _printed-value (name)
  (let v (aref state name)
    (| (!? (aref props.schema name).printer
           (funcall ! v)
           v)
       "")))

(defmethod autoform _render-typed-field (name)
  (@ (i props.widgets)
    (!= (aref props.schema name)
      (& (funcall i.predicate !)
         (return (funcall i.maker this props ! name (_printed-value name)))))))

(defmethod autoform _render-field (x)
  (?
    (function? x)   (funcall x state)
    (string? x)     (_render-typed-field x)
    x))

(finalize-class autoform)
(declare-lml-component autoform)


(defclass (autoform-field autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-field render ()
  ($$ (_render-field props.field)))

(finalize-class autoform-field)
(declare-lml-component autoform-field)


(defclass (autoform-list autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-list render ()
  ($$ `(tr ,@(@ [`(td ,@(& (string? _) `(:key ,_))
                    ,(_render-field _))]
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
                    ,(_render-field _)))]
              props.fields))))

(finalize-class autoform-panel)
(declare-lml-component autoform-panel)
