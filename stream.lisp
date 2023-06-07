#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.memory-regions)

(defclass memory-region-stream (memory-region-ish
                                trivial-gray-streams:fundamental-binary-input-stream
                                trivial-gray-streams:fundamental-binary-output-stream)
  ((start :initarg :start :accessor start)
   (size :initarg :size :accessor size)
   (index :initarg :index :initform 0 :accessor index
          :accessor trivial-gray-streams:stream-file-position)))

(defmethod to-stream ((stream memory-region-stream))
  stream)

(defmethod to-stream ((memory-region memory-region))
  (make-instance 'memory-region-stream :start (memory-region-pointer memory-region)
                                       :size (memory-region-size memory-region)))

(defmethod to-stream (thing)
  (apply #'to-stream (to-memory-region thing)))

(defmethod trivial-gray-streams:stream-clear-input ((stream memory-region-stream))
  (setf (index stream) (size stream)))

(defmethod trivial-gray-streams:stream-clear-output ((stream memory-region-stream))
  (setf (index stream) 0))

(defmethod trivial-gray-streams:stream-read-byte ((stream memory-region-stream))
  (let ((start (start stream))
        (index (index stream))
        (size (size stream)))
    (when (<= size index)
      (error 'end-of-file :stream stream))
    (setf (index stream) (1+ index))
    (cffi:mem-aref start :uint8 index)))

(defmethod trivial-gray-streams:stream-write-byte ((stream memory-region-stream) integer)
  (let ((start (start stream))
        (index (index stream))
        (size (size stream)))
    (when (<= size index)
      (error 'end-of-file :stream stream))
    (setf (index stream) (1+ index))
    (setf (cffi:mem-aref start :uint8 index) integer)))

(defmethod trivial-gray-streams:stream-read-sequence ((stream memory-region-stream) (sequence vector) start end &key)
  (let* ((tstart (or start 0))
         (tend (or end (length sequence)))
         (start (start stream))
         (index (index stream))
         (size (size stream))
         (to-copy (min (- tend tstart) (- size index))))
    (when (< 0 to-copy)
      (with-pointer-to-vector-data (ptr sequence :direction :output)
        (cffi:foreign-funcall "memcpy" :pointer ptr :pointer (cffi:inc-pointer start index) :size to-copy)))
    to-copy))

(defmethod trivial-gray-streams:stream-write-sequence ((stream memory-region-stream) (sequence vector) start end &key)
  (let* ((tstart (or start 0))
         (tend (or end (length sequence)))
         (start (start stream))
         (index (index stream))
         (size (size stream))
         (to-copy (min (- tend tstart) (- size index))))
    (when (< 0 to-copy)
      (with-pointer-to-vector-data (ptr sequence :direction :input)
        (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer start index) :pointer ptr :size to-copy)))
    to-copy))
