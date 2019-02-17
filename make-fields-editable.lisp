(fn make-fields-editable (schema &rest fields)
  (@ (i (| fields (property-names schema)) schema)
    (= (aref schema i).is_editable t)))
