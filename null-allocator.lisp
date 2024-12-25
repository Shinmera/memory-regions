(in-package #:org.shirakumo.memory-regions)

(defmethod allocate ((allocator null) size)
  (let ((ptr (cffi:foreign-funcall "calloc" :size 1 :size size :pointer)))
    (when (cffi:null-pointer-p ptr)
      (error 'out-of-memory :allocator allocator))
    (memory-region ptr size)))

(defmethod deallocate ((allocator null) (region memory-region))
  (when (memory-region-valid-p region)
    (cffi:foreign-funcall "free" :pointer (memory-region-pointer region) :void)
    (setf (memory-region-pointer region) (cffi:null-pointer))
    (setf (memory-region-size region) 0)
    region))

(defmethod reallocate ((allocator null) (region memory-region) new-size)
  (let ((ptr (cffi:foreign-funcall "realloc" :pointer (memory-region-pointer region) :size new-size :pointer)))
    (when (cffi:null-pointer-p ptr)
      (error 'out-of-memory :allocator allocator))
    (setf (memory-region-pointer region) ptr)
    (setf (memory-region-size region) new-size)
    region))

(defun malloc-info ()
  (let ((string (make-array (* 1024 32) :element-type '(unsigned-byte 8))))
    (with-pointer-to-array-data (ptr string :direction :output)
      (let ((file (cffi:foreign-funcall "fmemopen" :pointer ptr :size (1- (length string)) :string "w" :pointer)))
        (unless (cffi:null-pointer-p file)
          (unwind-protect
               (when (= 0 (cffi:foreign-funcall "malloc_info" :int 0 :pointer file :int))
                 (cffi:foreign-funcall "fflush" :pointer file)
                 (babel:octets-to-string string :end (cffi:foreign-funcall "ftell" :pointer file :long)))
            (cffi:foreign-funcall "fclose" :pointer file)))))))

(defun find-info-tag (tag info &optional (start 0) from-end)
  (let ((pos (search (format NIL "<~a " tag) info :start2 start :from-end from-end)))
    (when pos (+ pos 2 (length tag)))))

(defmethod total-size ((allocator null))
  (cond ((cffi:foreign-symbol-pointer "malloc_info")
         (let* ((info (malloc-info))
                (pos (find-info-tag "aspace type=\"total\"" info 0 T)))
           (parse-integer info :start (+ pos (length "size=\"")) :junk-allowed T)))
        (T
         (error "Not implemented"))))

(defmethod used-space ((allocator null))
  (cond ((cffi:foreign-symbol-pointer "malloc_info")
         (let* ((info (malloc-info))
                (pos (find-info-tag "aspace type=\"total\"" info 0 T))
                (total (parse-integer info :start (+ pos (length "size=\"")) :junk-allowed T)))
           (- total
              (loop for pos = (find-info-tag "size" info) then (find-info-tag "size" info pos)
                    while pos sum (let ((total (search "total=\"" info :start2 pos)))
                                    (parse-integer info :start (+ total (length "total=\"")) :junk-allowed T))))))
        (T
         (error "Not implemented"))))

(defmethod minimum-block-size ((allocator null))
  (cond ((cffi:foreign-symbol-pointer "malloc_good_size")
         (cffi:foreign-funcall "malloc_good_size" :size 1 :size))
        ((cffi:foreign-symbol-pointer "malloc_info")
         (loop with info = (malloc-info)
               for pos = (find-info-tag "size" info) then (find-info-tag "size" info pos)
               while pos minimize (parse-integer info :start (+ pos (length "from=\"")) :junk-allowed T)))
        (T
         1)))

(defmethod maximum-block-size ((allocator null))
  (error "Not implemented"))
