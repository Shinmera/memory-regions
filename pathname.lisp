(in-package #:org.shirakumo.memory-regions)

(defstruct (pathname-memory-region
            (:include memory-region)
            (:constructor pathname-memory-region (pointer fd size)))
  (fd NIL))

(defmethod deallocate ((allocator null) (region pathname-memory-region))
  (mmap:munmap (memory-region-pointer region)
               (pathname-memory-region-fd region)
               (memory-region-size region))
  (setf (memory-region-pointer region) (cffi:null-pointer))
  (setf (memory-region-size region) 0)
  (setf (pathname-memory-region-fd region) NIL)
  region)

(defmethod to-memory-region ((pathname pathname))
  (multiple-value-bind (ptr fd size) (mmap:mmap pathname :open '(:read :write) :protection '(:read :write))
    (pathname-memory-region ptr fd size)))

(defmethod call-with-memory-region (function (data pathname) &key (offset 0))
  (mmap:with-mmap (ptr fd size data)
    (let ((region (memory-region (cffi:inc-pointer ptr offset) (- size offset))))
      (declare (dynamic-extent region))
      (funcall function region))))
