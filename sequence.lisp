(in-package #:org.shirakumo.memory-regions)

(defclass memory-region-sequence (memory-region-ish org.shirakumo.trivial-extensible-sequences:sequence)
  ((region :initarg :region :reader to-memory-region)
   (start :initarg :start :accessor start)
   (count :reader org.shirakumo.trivial-extensible-sequences:length)
   (element-type :initarg :element-type :accessor element-type)
   (element-size :initarg :element-size :accessor element-size)))

(defmethod size ((sequence memory-region-sequence))
  (* (org.shirakumo.trivial-extensible-sequences:length sequence) (element-size sequence)))

(defmethod to-sequence ((sequence memory-region-sequence) &optional (element-type :uint8))
  (declare (ignore element-type))
  sequence)

(defmethod to-sequence ((memory-region memory-region) &optional (element-type :uint8))
  (let ((element-size (cffi:foreign-type-size element-type)))
    (make-instance 'memory-region-sequence :region memory-region
                                           :start (memory-region-pointer memory-region)
                                           :count (truncate (memory-region-size memory-region) element-size)
                                           :element-type element-type
                                           :element-size element-size)))

(defmethod to-sequence (thing &optional (element-type :uint8))
  (to-sequence (to-memory-region thing) element-type))

(defmethod org.shirakumo.trivial-extensible-sequences:elt ((sequence memory-region-sequence) index)
  (cffi:mem-aref (start sequence) (element-type sequence) index))

(defmethod (setf org.shirakumo.trivial-extensible-sequences:elt) (value (sequence memory-region-sequence) index)
  (setf (cffi:mem-aref (start sequence) (element-type sequence) index) value))

(defmethod org.shirakumo.trivial-extensible-sequences:subseq ((sequence memory-region-sequence) start &optional end)
  (let ((end (or end (org.shirakumo.trivial-extensible-sequences:length sequence))))
    (assert (<= end (org.shirakumo.trivial-extensible-sequences:length sequence)) (end))
    (assert (<= start end) (start))
    (let ((off (* (element-size sequence) start)))
      (make-instance 'memory-region-sequence 
                     :region (to-memory-region sequence)
                     :start (cffi:inc-pointer (start sequence) off)
                     :count (- end start)
                     :element-type (element-type sequence)))))

(defmethod org.shirakumo.trivial-extensible-sequences:adjust-sequence ((sequence memory-region-sequence) length &key initial-contents initial-element (allocator *allocator*))
  (reallocate allocator (to-memory-region sequence) (* length (element-size sequence)))
  (cond (initial-element
         (org.shirakumo.trivial-extensible-sequences:fill sequence initial-element))
        (initial-contents
         (org.shirakumo.trivial-extensible-sequences:replace sequence initial-contents)))
  sequence)

(defmethod org.shirakumo.trivial-extensible-sequences:make-sequence-like ((sequence memory-region-sequence) length &key initial-contents initial-element (allocator *allocator*))
  (let* ((region (allocate allocator (* length (element-size sequence))))
         (sequence (make-instance 'memory-region-sequence :start (memory-region-pointer region)
                                                          :count length
                                                          :element-type (element-type sequence)
                                                          :element-size (element-size sequence))))
    (cond (initial-element
           (org.shirakumo.trivial-extensible-sequences:fill sequence initial-element))
          (initial-contents
           (org.shirakumo.trivial-extensible-sequences:replace sequence initial-contents)))
    sequence))
