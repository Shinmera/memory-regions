(in-package #:org.shirakumo.memory-regions)

(defmethod allocate ((allocator null) size)
  (let ((ptr (cffi:foreign-funcall "calloc" :size 1 :size size :pointer)))
    (when (cffi:null-pointer-p ptr)
      (error 'out-of-memory :allocator allocator))
    (memory-region ptr size)))

(defmethod deallocate ((allocator null) (region memory-region))
  (when (memory-region-valid-p region)
    (cffi:foreign-funcall "free" :pointer (memory-region-pointer region) :void)
    (setf (memory-region-pointer region) (cffi:null-pointer))
    (setf (memory-region-size region) 0)
    region))

(defmethod reallocate ((allocator null) (region memory-region) new-size)
  (let ((ptr (cffi:foreign-funcall "realloc" :pointer (memory-region-pointer region) :size new-size :pointer)))
    (when (cffi:null-pointer-p ptr)
      (error 'out-of-memory :allocator allocator))
    (setf (memory-region-pointer region) ptr)
    (setf (memory-region-size region) new-size)
    region))

;; FIXME: Need mallinfo for total-size/free-space/etc but that requires supporting struct returns.
(defmethod total-size ((allocator null))
  (error "Not implemented"))

(defmethod used-space ((allocator null))
  (error "Not implemented"))

(defmethod minimum-block-size ((allocator null))
  (error "Not implemented"))

(defmethod maximum-block-size ((allocator null))
  (error "Not implemented"))
