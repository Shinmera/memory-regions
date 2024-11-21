(in-package #:org.shirakumo.memory-regions)

(defstruct (mmap-memory-region
            (:include memory-region)
            (:constructor mmap-memory-region (pointer size fd)))
  (fd NIL))

(defmethod deallocate ((allocator null) (region mmap-memory-region))
  (mmap:munmap (memory-region-pointer region)
               (mmap-memory-region-fd region)
               (memory-region-size region))
  (setf (memory-region-pointer region) (cffi:null-pointer))
  (setf (memory-region-size region) 0)
  (setf (mmap-memory-region-fd region) NIL)
  region)

(defstruct (stream-memory-region
            (:include memory-region)
            (:constructor stream-memory-region (pointer size stream array)))
  (stream NIL)
  (array NIL))

(defmethod deallocate ((allocator null) (region stream-memory-region))
  (file-position (stream-memory-region-stream region) 0)
  (write-sequence (stream-memory-region-array region) (stream-memory-region-stream region))
  (static-vectors:free-static-vector (stream-memory-region-array region))
  (setf (stream-memory-region-array region) NIL)
  (close (stream-memory-region-stream region))
  (setf (stream-memory-region-stream region) NIL)
  (setf (memory-region-pointer region) (cffi:null-pointer))
  (setf (memory-region-size region) 0)
  region)

(defmethod to-memory-region ((pathname pathname))
  #+mmap
  (multiple-value-bind (ptr fd size) (mmap:mmap pathname :open '(:read :write) :protection '(:read :write))
    (mmap-memory-region ptr size fd))
  #-mmap
  (let* ((stream (open pathname :direction :io :element-type '(unsigned-byte 8)))
         (size (file-length stream))
         (array (static-vectors:make-static-vector size)))
    (declare (dynamic-extent array))
    (read-sequence array stream)
    (stream-memory-region (static-vectors:static-vector-pointer array) size stream array)))

(defmethod call-with-memory-region ((function function) (data pathname) &key (offset 0))
  #+mmap
  (mmap:with-mmap (ptr fd size data)
    (let ((region (memory-region (cffi:inc-pointer ptr offset) (- size offset))))
      (declare (dynamic-extent region))
      (funcall function region)))
  #-mmap
  (with-open-file (stream data :direction :io :element-type '(unsigned-byte 8))
    (let* ((size (- (file-length stream) offset))
           (array (make-array size :element-type '(unsigned-byte 8))))
      (declare (dynamic-extent array))
      (file-position stream offset)
      (read-sequence array stream)
      (unwind-protect (call-with-memory-region function array)
        (file-position stream offset)
        (write-sequence array stream)))))
