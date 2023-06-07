#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.memory-regions)

(defvar *ffi-slot-info* (make-hash-table :test 'eq))

(defstruct (ffi-slot-info
            (:constructor %make-ffi-slot-info (offset reader writer))
            (:copier NIL)
            (:predicate NIL))
  (offset 0 :type (unsigned-byte 32) :read-only T)
  (reader NIL :type function :read-only T)
  (writer NIL :type function :read-only T))

(defun make-ffi-slot-info (type)
  (let ((table (make-hash-table :test 'eq))
        (foreign-type (list :struct type)))
    (handler-bind ((warning #'muffle-warning))
      (loop for slot-name in (cffi:foreign-slot-names foreign-type)
            for type = (cffi:foreign-slot-type foreign-type slot-name)
            do (setf (gethash slot-name table)
                     (%make-ffi-slot-info (cffi:foreign-slot-offset foreign-type slot-name)
                                          (compile NIL `(lambda (ptr) 
                                                          (declare (type cffi:foreign-pointer))
                                                          (declare (optimize speed (safety 0)))
                                                          (cffi:mem-ref ptr ',type)))
                                          (compile NIL `(lambda (value ptr)
                                                          (declare (type cffi:foreign-pointer))
                                                          (declare (optimize speed (safety 0)))
                                                          (setf (cffi:mem-ref ptr ',type) value)))))))
    table))

(defun ffi-slot-info (structure-type)
  (let ((type (if (listp structure-type)
                  (second structure-type)
                  structure-type)))
    (or (gethash type *ffi-slot-info*)
        (setf (gethash type *ffi-slot-info*) (make-ffi-slot-info type)))))

(defclass memory-region-object (memory-region-ish)
  ((start :initarg :start :accessor start)
   (size :initarg :size :accessor size)
   (slot-info :initarg :slot-info :accessor slot-info)))

(defmethod to-object ((object memory-region-object) structure-type)
  (declare (ignore structure-type))
  object)

(defmethod to-object ((region memory-region) structure-type)
  (let ((structure-type (if (listp structure-type) structure-type (list :struct structure-type))))
    (make-instance 'memory-region-object :start (memory-region-pointer region)
                                         :size (memory-region-size region)
                                         :slot-info (ffi-slot-info structure-type))))

(defmethod to-object (thing structure-type)
  (to-object (to-memory-region thing) structure-type))

(defmethod c2mop:slot-value-using-class (class (object memory-region-object) slot)
  (let ((slot (or (gethash slot (slot-info object))
                  (error "No such slot ~s" slot))))
    (funcall (ffi-slot-info-reader slot) (cffi:inc-pointer (start object) (ffi-slot-info-offset slot)))))

(defmethod (setf c2mop:slot-value-using-class) (value class (object memory-region-object) slot)
  (let ((slot (or (gethash slot (slot-info object))
                  (error "No such slot ~s" slot))))
    (funcall (ffi-slot-info-writer slot) value (cffi:inc-pointer (start object) (ffi-slot-info-offset slot)))))

(defmethod c2mop:slot-boundp-using-class (class (object memory-region-object) slot)
  (not (null (gethash slot (slot-info object)))))

(defmethod c2mop:slot-makunbound-using-class (class (object memory-region-object) slot)
  (error "Cannot make slots unbound on a ~s" (type-of object)))
