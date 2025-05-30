(in-package #:org.shirakumo.memory-regions)

(defvar *stack-allocation-size-limit* (* 1024 8))

(defgeneric start (region))
(defgeneric end (region))
(defgeneric size (region))
(defgeneric to-memory-region (thing))
(defgeneric call-with-memory-region (function data &key start &allow-other-keys))
(defgeneric clear (region))
(defgeneric fill (dst byte &key start end))
(defgeneric replace (dst src &key start1 end1 start2 end2))
(defgeneric subregion (region &optional start end))

(defmacro with-memory-region ((region data/size &rest args) &body body &environment env)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,region)
              (declare (type memory-region ,region))
              ,@body))
       (declare (dynamic-extent #',thunk))
       ,(if (constantp data/size env)
            `(let ((,region (make-array ,data/size :element-type '(unsigned-byte 8) :initial-element 0)))
               (declare (dynamic-extent ,region))
               (with-pointer-to-array-data (,region ,region)
                 (let ((,region (memory-region ,region ,data/size)))
                   (declare (dynamic-extent ,region))
                   (,thunk ,region))))
            `(call-with-memory-region #',thunk ,data/size ,@args)))))

(defmacro with-memory-regions (regions &body body)
  (if (null regions)
      `(progn ,@body)
      `(with-memory-region ,(first regions)
         (with-memory-regions ,(rest regions)
           ,@body))))

(declaim (inline memory-region memory-region-pointer memory-region-size))
(defstruct (memory-region
            (:constructor memory-region (pointer size)))
  (pointer NIL :type cffi:foreign-pointer)
  (size 0 :type (unsigned-byte 64)))

(defmethod print-object ((region memory-region) stream)
  (print (list 'memory-region (memory-region-pointer region) (memory-region-size region)) stream))

(defun memory-region-valid-p (region)
  (and (not (cffi:null-pointer-p (memory-region-pointer region)))
       (< 0 (memory-region-size region))))

(defmethod start ((region memory-region))
  (memory-region-pointer region))

(defmethod size ((region memory-region))
  (memory-region-size region))

(defmethod end (region)
  (cffi:inc-pointer (start region) (size region)))

(defmethod to-memory-region ((memory-region memory-region))
  memory-region)

(defmethod clear ((region memory-region))
  (cffi:foreign-funcall "memset" :pointer (memory-region-pointer region)
                                 :int 0
                                 :size (memory-region-size region)
                                 :void)
  region)

(defmethod fill ((region memory-region) byte &key (start 0) (end (memory-region-size region)))
  (check-type byte (unsigned-byte 8))
  (assert (<= 0 start end (memory-region-size region)))
  (cffi:foreign-funcall "memset" :pointer (cffi:inc-pointer (memory-region-pointer region) start)
                                 :int byte
                                 :size (- end start)
                                 :void)
  region)

(defmethod replace ((dst memory-region) (src memory-region) &key start1 end1 start2 end2)
  (let* ((start1 (or start1 0))
         (end1 (or end1 (memory-region-size dst)))
         (start2 (or start2 0))
         (end2 (or end2 (memory-region-size src)))
         (to-copy (min (- end1 start1) (- end2 start2))))
    (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer (memory-region-pointer dst) start1)
                                   :pointer (cffi:inc-pointer (memory-region-pointer src) start2)
                                   :size to-copy
                                   :void)
    dst))

(defmethod replace (dst (src memory-region) &key start1 end1 start2 end2)
  (if (typep dst 'cffi:foreign-pointer)
      (let* ((start1 (or start1 0))
             (end1 (or end1 (memory-region-size src)))
             (start2 (or start2 0))
             (end2 (or end2 (memory-region-size src)))
             (to-copy (min (- end1 start1) (- end2 start2))))
        (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer dst start1)
                                       :pointer (cffi:inc-pointer (memory-region-pointer src) start2)
                                       :size to-copy
                                       :void)
        dst)
      (call-next-method)))

(defmethod replace ((dst memory-region) src &key start1 end1 start2 end2)
  (if (typep src 'cffi:foreign-pointer)
      (let* ((start1 (or start1 0))
             (end1 (or end1 (memory-region-size dst)))
             (start2 (or start2 0))
             (end2 (or end2 (memory-region-size dst)))
             (to-copy (min (- end1 start1) (- end2 start2))))
        (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer (memory-region-pointer dst) start1)
                                       :pointer (cffi:inc-pointer src start2)
                                       :size to-copy
                                       :void)
        dst)
      (call-next-method)))

(defmethod subregion ((region memory-region) &optional start end)
  (let ((start (or start 0))
        (end (or end (memory-region-size region))))
    (assert (<= end (memory-region-size region)) (end))
    (assert (<= start end) (start))
    (memory-region (cffi:inc-pointer (memory-region-pointer region) start)
                   (- end start))))

(defmethod call-with-memory-region ((function function) (region memory-region) &key (start 0))
  (let ((region (memory-region (cffi:inc-pointer (memory-region-pointer region) start)
                               (max 0 (- (memory-region-size region) start)))))
    (declare (dynamic-extent region))
    (funcall function region)))

(defmethod call-with-memory-region ((function function) (null null) &key (start 0))
  (declare (ignore start))
  (let ((region (memory-region (cffi:null-pointer) 0)))
    (declare (dynamic-extent region))
    (funcall function region)))

(defmethod call-with-memory-region ((function function) (size integer) &key (start 0))
  (decf size start)
  (if (<= size *stack-allocation-size-limit*)
      (cffi:with-foreign-pointer (ptr size)
        (let ((region (memory-region ptr size)))
          (declare (dynamic-extent region))
          (funcall function region)))
      (let ((ptr (cffi:foreign-alloc :uint8 :count size)))
        (unwind-protect
             (let ((region (memory-region ptr size)))
               (declare (dynamic-extent region))
               (funcall function region))
          (cffi:foreign-free ptr)))))

(defmethod call-with-memory-region ((function function) pointer &key (size 0) (start 0))
  (check-type pointer cffi:foreign-pointer)
  (let ((region (memory-region (cffi:inc-pointer pointer start)
                               (max 0 (- size start)))))
    (declare (dynamic-extent region))
    (funcall function region)))

(defmethod call-with-memory-region ((function function) (data array) &key (start 0) (direction :input))
  (declare (optimize speed))
  (declare (type (unsigned-byte 32) start))
  (declare (ignore direction))
  (let* ((type (array-element-type data))
         (type-size (the (unsigned-byte 16) (element-type-size type)))
         (start (* start type-size))
         (size (- (the (unsigned-byte 32) (* (length data) type-size)) start)))
    (with-pointer-to-array-data (pointer data :direction direction)
      (let ((region (memory-region (cffi:inc-pointer pointer start) size)))
        (declare (dynamic-extent region))
        (funcall function region)))))

(defmethod subregion (region-ish &optional start end)
  (subregion (to-memory-region region-ish) start end))

(defmethod clear (dst)
  (with-memory-region (dst dst)
    (clear dst)))

(defmethod fill (dst byte &rest args &key (start 0) end)
  (cond ((not (typep dst 'cffi:foreign-pointer))
         (with-memory-region (dst dst)
           (apply #'fill dst byte args)))
        ((null end)
         (error "END has to be passed when filling a pointer."))
        (T
         (check-type byte (unsigned-byte 8))
         (cffi:foreign-funcall "memset" :pointer (cffi:inc-pointer dst start)
                                        :int byte
                                        :size (- end start)
                                        :void))))

(defmethod replace (dst src &rest args &key (start1 0) end1 (start2 0) end2)
  (cond ((and (typep dst 'cffi:foreign-pointer)
              (typep src 'cffi:foreign-pointer))
         (let ((to-copy (cond ((and end1 end2) (min (- end2 start2) (- end1 start1)))
                              (end1 (- end1 start1))
                              (end2 (- end2 start2))
                              (T (error "Either END1 or END2 have to be passed when replacing two pointers.")))))
           (cffi:foreign-funcall "memcpy" :pointer (cffi:inc-pointer dst start1)
                                          :pointer (cffi:inc-pointer src start2)
                                          :size to-copy
                                          :void)))
        ((typep dst 'cffi:foreign-pointer)
         (with-memory-region (src src)
           (apply #'replace dst src args)))
        ((typep src 'cffi:foreign-pointer)
         (with-memory-region (dst dst)
           (apply #'replace dst src args)))
        (T
         (with-memory-region (dst dst)
           (with-memory-region (src src)
             (apply #'replace dst src args))))))

(defclass memory-region-ish () ())

(defmethod to-memory-region ((region memory-region-ish))
  (memory-region (start region) (size region)))

(defmethod call-with-memory-region ((function function) (region memory-region-ish) &key (start 0))
  (let ((region (memory-region (cffi:inc-pointer (start region) start) (- (size region) start))))
    (declare (dynamic-extent region))
    (funcall function region)))
