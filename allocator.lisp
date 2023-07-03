(in-package #:org.shirakumo.memory-regions)

(define-condition allocator-condition ()
  ((allocator :initarg :allocator :reader allocator)))

(define-condition out-of-memory (error allocator-condition)
  ()
  (:report (lambda (c s) (format s "The allocator is out of memory."))))

(define-condition block-too-big (error allocator-condition)
  ((size :initarg :size :reader size))
  (:report (lambda (c s) (format s "The requested block size of ~d octets is too big."
                                 (size c)))))

(defclass allocator () ())

(defgeneric allocate (allocator size))
(defgeneric deallocate (allocator region))
(defgeneric reallocate (allocator region new-size))
(defgeneric total-size (allocator))
(defgeneric free-space (allocator))
(defgeneric used-space (allocator))
(defgeneric minimum-block-size (allocator))
(defgeneric maximum-block-size (allocator))

(defmethod free-space ((allocator allocator))
  (- (total-size allocator) (used-space allocator)))

(defmethod minimum-block-size ((allocator allocator))
  1)

(defmethod maximum-block-size ((allocator allocator))
  (total-size allocator))

(defvar *allocator* NIL)
(defvar *standard-allocator* 'bump-allocator)

(defmacro with-arena ((size &optional (type '*standard-allocator*) &rest args) &body body)
  (let ((sizeg (gensym "SIZE"))
        (start (gensym "START")))
    `(let ((,sizeg ,size))
       (cffi:with-foreign-pointer (,start ,sizeg)
         (let ((*allocator* (make-instance ,type :start ,start :size ,sizeg ,@args)))
           ,@body)))))

(defmethod allocate ((allocator (eql T)) size)
  (allocate *allocator* size))

(defmethod deallocate ((allocator (eql T)) region)
  (deallocate *allocator* region))

(defmethod reallocate ((allocator (eql T)) region new-size)
  (reallocate *allocator* region new-size))

(defmethod total-size ((allocator (eql T)))
  (total-size *allocator*))

(defmethod free-space ((allocator (eql T)))
  (free-space *allocator*))

(defmethod used-space ((allocator (eql T)))
  (used-space *allocator*))

(defmethod minimum-block-size ((allocator (eql T)))
  (minimum-block-size *allocator*))

(defmethod maximum-block-size ((allocator (eql T)))
  (maximum-block-size *allocator*))
