(defclass store (init-data)
  (= data (| init-data (new)))
  this)

(defmember store
    data)

(defmethod store fetch ()
  data)

(defmethod store write (new-data)
  (= data (merge-properties data new-data)))

(defmethod store empty ()
  (= data nil))

(defmethod store commit ()
  data)

(finalize-class store)
