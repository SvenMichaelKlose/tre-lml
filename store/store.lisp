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
  (prog1 (= data (merge-properties data new-data))
    (_update-component)))

(defmethod store _update-component ()
  (!? _component
      (!.force-update)))

(defmethod store write (new-data)
  (_store-write new-data))

(defmethod store replace (new-data)
  (prog1 (= data (copy-properties new-data))
    (_update-component)))

(defmethod store empty ()
  (prog1 (= data nil)
    (_update-component)))

(defmethod store commit ()
  data)

(finalize-class store)


(defclass (child-store store) (&key name parent)
  (super (copy-properties (aref parent.data name)))
  (= _name name
     _parent parent)
  this)

(defmember child-store
    _name
    _parent)

(defmethod child-store _update-parent ()
  (_parent.write (make-object _name data)))

(defmethod child-store write (new-data)
  (prog1 (_store-write new-data)
    (_update-parent)))

(defmethod child-store replace (new-data)
  (prog1 (= data (copy-properties new-data))
    (_update-parent)))

(defmethod child-store empty ()
  (prog1 (= data nil)
    (_update-parent)))

(defmethod child-store commit ()
  (_parent.commit))

(finalize-class child-store)
