(defmethod staple:packages ((system (eql (asdf:find-system :tooter))))
  (list :tooter-client :tooter-objects :tooter-queries))
