#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.memory-regions)

(defclass memory-region-sequence (memory-region-ish sequences:sequence)
  ((start :initarg :start :accessor start)
   (count :reader sequences:length)
   (element-type :initarg :element-type :accessor element-type)
   (element-size :initarg :element-size :accessor element-size)))

(defmethod size ((sequence memory-region-sequence))
  (* (sequences:length sequence) (element-size sequence)))

(defmethod to-sequence ((sequence memory-region-sequence))
  sequence)

(defmethod to-sequence ((memory-region memory-region) &optional (element-type :uint8))
  (let ((element-size (cffi:foreign-type-size element-type)))
    (make-instance 'memory-region-sequence :start (memory-region-pointer memory-region)
                                           :count (truncate (memory-region-size memory-region) element-size)
                                           :element-type element-type
                                           :element-size element-size)))

(defmethod to-sequence (thing &optional (element-type :uint8))
  (apply #'to-sequence (to-memory-region thing) args))

(defmethod sequences:elt ((sequence memory-region-sequence) index)
  (cffi:mem-aref (start sequence) (element-type sequence) index))

(defmethod (setf sequences:elt) (value (sequence memory-region-sequence) index)
  (setf (cffi:mem-aref (start sequence) (element-type sequence) index) value))

(defmethod sequences:subseq ((sequence memory-region-sequence) start end)
  (assert (<= end (sequences:length sequence)) (end))
  (assert (<= start end) (start))
  (let ((off (* (element-size sequence) start)))
    (make-instance 'memory-region-sequence :start (cffi:inc-pointer (start sequence) off)
                                           :count (- end start)
                                           :element-type (element-type sequence))))

(defmethod sequences:adjust-sequence ((sequence memory-region-sequence) length &key initial-contents initial-element allocator)
  
  sequence)

(defmethod sequences:make-sequence-like ((sequence memory-region-sequence) length &key initial-contents initial-element allocator)
  )
