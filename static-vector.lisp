(in-package #:org.shirakumo.memory-regions)

(defstruct (static-vector-memory-region 
            (:include memory-region)
            (:constructor %make-static-vector-memory-region))
  (vector NIL :type T))

(declaim (inline memory-region-vector (setf memory-region-vector)))
(defun memory-region-vector (region)
  (static-vector-memory-region-vector region))

(defun (setf memory-region-vector) (vector region)
  (setf (static-vector-memory-region-vector region) vector))

(defmethod deallocate ((allocator null) (region static-vector-memory-region))
  (static-vectors:free-static-vector (memory-region-vector region))
  (setf (memory-region-vector region) NIL)
  (setf (memory-region-pointer region) (cffi:null-pointer))
  (setf (memory-region-size region) 0)
  region)

(defun static-vector-memory-region (length/vec)
  (etypecase length/vec
    (integer
     (setf length/vec (static-vectors:make-static-vector length/vec :initial-element 0)))
    (vector))
  (%make-static-vector-memory-region
   :pointer (static-vectors:static-vector-pointer length/vec)
   :size (length length/vec)
   :vector length/vec))

(defmethod to-memory-region ((vector vector))
  (check-type vector (simple-array (unsigned-byte 8) (*)))
  (static-vector-memory-region vector))
