(defmethod staple:packages ((system (eql (asdf:find-system "memory-regions"))))
  (list (find-package '#:org.shirakumo.memory-regions)))

(defmethod staple:subsystems ((system (eql (asdf:find-system "memory-regions"))))
  ())
