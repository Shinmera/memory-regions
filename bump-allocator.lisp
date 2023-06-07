#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.memory-regions)

(defclass bump-allocator (memory-region-ish allocator)
  ((start :initform NIL :initarg :start :accessor start)
   (size :initform 0 :initarg :size :accessor size :reader total-size)
   (index :initform 0 :accessor index :reader used-space)))

(defmethod initialize-instance :after ((allocator bump-allocator) &key)
  (unless (start allocator)
    (setf (start allocator) (cffi:foreign-funcall "calloc" :size 1 :size (size allocator) :pointer))))

(defmethod deallocate ((allocator bump-allocator) (self null))
  (when (start allocator)
    (cffi:foreign-funcall "free" :pointer (start allocator) :void)
    (setf (start allocator) NIL)))

(defmethod clear ((allocator bump-allocator))
  (setf (index allocator) 0))

(defmethod allocate ((allocator bump-allocator) size)
  (check-type size (unsigned-byte 64))
  (let ((start (start allocator))
        (total (size allocator))
        (index (index allocator)))
    (when (< total (+ index size))
      (error 'out-of-memory :allocator allocator))
    (setf (index allocator) (+ index size))
    (memory-region (cffi:inc-pointer start index) size)))

(defmethod deallocate ((allocator bump-allocator) (region memory-region))
  (when (memory-region-valid-p region)
    (let ((start (start allocator))
          (index (index allocator)))
      (when (cffi:pointer-eq (cffi:inc-pointer start (- index (memory-region-size region)))
                             (memory-region-pointer region))
        (setf (index allocator) (- index (memory-region-size region))))
      (setf (memory-region-pointer region) (cffi:null-pointer))
      (setf (memory-region-size region) 0)
      region)))

(defmethod reallocate ((allocator bump-allocator) (region memory-region) new-size)
  (when (memory-region-valid-p region)
    (let ((start (start allocator))
          (total (size allocator))
          (index (index allocator)))
      (cond ((cffi:pointer-eq (cffi:inc-pointer start (- index (memory-region-size region)))
                              (memory-region-pointer region))
             (let ((diff (- new-size (memory-region-size region))))
               (when (< total (+ index diff))
                 (error 'out-of-memory :allocator allocator))
               (setf (index allocator) (+ index diff))
               (setf (memory-region-size region) new-size)))
            (T
             (let ((new (allocate allocator new-size)))
               (replace new region)
               (setf (memory-region-pointer region) (memory-region-pointer new))
               (setf (memory-region-size region) new-size))))
      region)))
