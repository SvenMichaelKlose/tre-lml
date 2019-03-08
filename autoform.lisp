(defclass (autoform lml-component) (init-props)
  (super (merge-properties {:widgets *autoform-widgets*} init-props))
  this)

(finalize-class autoform)
(declare-lml-component autoform)


(defclass (autoform-field autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-field _value ()
  (let v (props.store.value props.field)
    (| (!? props.schema-item.printer
           (funcall ! v)
           v)
       "")))

(defmethod autoform-field _render-typed-field ()
  (@ (widget props.widgets)
    (& (funcall widget.predicate props.schema-item)
       (return (funcall widget.maker props.store props.field props.schema-item (_value))))))

(defmethod autoform-field render ()
  ($$ (!= props.field
        (?
          (function? !)  (funcall ! props.store.data)
          (string? !)    (_render-typed-field)
          !))))

(finalize-class autoform-field)
(declare-lml-component autoform-field)


(defclass (autoform-list autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-list render ()
  ($$ `(tr ,@(@ [`(td ,@(& (string? _)
                           `(:key ,_))
                    (autoform-field :schema-item  ,(aref props.schema _)
                                    :field        ,_
                                    :store        ,props.store))]
                props.fields))))

(finalize-class autoform-list)
(declare-lml-component autoform-list)


(defclass (autoform-panel autoform) (init-props)
  (super init-props)
  this)

(defmethod autoform-panel _render-label (name)
  (!? (aref (aref props.schema name) "title")
      (aref ! (downcase (symbol-name *language*)))))

(defmethod autoform-panel render ()
  ($$ `(div
         ,@(@ [`(tr
                  (td
                    ,(? (string? _)
                        (_render-label _)))
                  (td ,@(& (string? _)
                           `(:key ,_))
                    (autoform-field :schema-item  ,(aref props.schema _)
                                    :field        ,_
                                    :store        ,props.store)))]
              props.fields))))

(finalize-class autoform-panel)
(declare-lml-component autoform-panel)
