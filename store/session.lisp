(defclass (session-store store) (name)
  (super nil)
  (= _name name)
  (fetch)
  this)

(defmember session-store
    _name)

(defmethod session-store fetch ()
  (= data (| (read-json-session _name)
             (new))))

(defmethod session-store write (new-data)
  (= data (merge-properties data new-data)))

(defmethod session-store commit ()
  (write-json-session _name data)
  data)

(finalize-class session-store)
