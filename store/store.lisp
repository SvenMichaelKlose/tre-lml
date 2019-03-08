(defclass store (init-data)
  (= data (| init-data (new)))
  this)

(defmember store
    data
    _component)

(defmethod store connect (comp)
  (= _component comp))

(defmethod store child (name_)
  ; TODO: Reuse child.
  (new child-store :name    name_
                    :parent  this))

(defmethod store names ()
  (property-names data))

(defmethod store fetch ()
  data)

(defmethod store value (name)
  (aref data name))

(defmethod store _store-write (new-data)
  (= data (merge-properties data new-data)))

(defmethod store write (new-data)
  (prog1 (_store-write new-data)
    (!? _component
        (!.force-update))))

(defmethod store replace (new-data)
  (= data (copy-properties new-data)))

(defmethod store empty ()
  (= data nil))

(defmethod store commit ()
  data)

(finalize-class store)


(defclass (child-store store) (&key name parent)
  (= _name name)
  (= _parent parent)
  (super (aref _parent.data _name))
  this)

(defmember child-store
    _name
    _parent)

(defmethod child-store write (new-data)
  (prog1 (_store-write new-data)
    (_parent.write (make-object _name data))))

(defmethod child-store commit ()
  (_parent.commit))

(finalize-class child-store)
