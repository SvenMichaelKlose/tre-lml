(defclass (session-store store) (name)
  (super nil)
  (= _session name)
  (fetch)
  this)

(defmember session-store
    _session)

(defmethod session-store fetch ()
  (= data (| (read-json-session _session)
             (new))))

(defmethod session-store write (new-data)
  (_store-write new-data))

(defmethod session-store commit ()
  (write-json-session _session data)
  data)

(finalize-class session-store)
