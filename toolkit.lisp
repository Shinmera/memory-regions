(in-package #:org.shirakumo.memory-regions)

;; KLUDGE: this whole thing sucks pretty bad.
(declaim (inline element-type->c-type))
(defun element-type->c-type (type)
  (case type
    (short-float :short-float)
    (single-float :float)
    (double-float :double)
    (long-float :long-double)
    (fixnum :ssize)
    (base-char :char)
    (character #+asdf-unicode :uint32 #-asdf-unicode :char)
    #+ecl (ext:byte8 :int8)
    #+ecl (ext:byte16 :int16)
    #+ecl (ext:byte32 :int32)
    #+ecl (ext:byte64 :int64)
    (T (if (listp type)
           (ecase (first type)
             (unsigned-byte
              (case (second type)
                (8 :uint8)
                (16 :uint16)
                (32 :uint32)
                (64 :uint64)
                (128 :uint128)))
             (signed-byte
              (case (second type)
                (8 :int8)
                (16 :int16)
                (32 :int32)
                (64 :int64)
                (128 :int128))))
           (error "Unknown element type: ~a" type)))))

(declaim (inline element-type-size))
(defun element-type-size (type)
  (case type
    (short-float 2)
    (single-float 4)
    (double-float 8)
    (long-float 16)
    (fixnum (cffi:foreign-type-size :ssize))
    (base-char 1)
    (character #+asdf-unicode 4 #-asdf-unicode 1)
    #+ecl (ext:byte8 1)
    #+ecl (ext:byte16 2)
    #+ecl (ext:byte32 4)
    #+ecl (ext:byte64 8)
    (T (if (listp type)
           (ecase (first type)
             ((unsigned-byte signed-byte)
              (ceiling (the (unsigned-byte 16) (second type)) 8)))
           (error "Unknown element type: ~a" type)))))

(defmacro with-pointer-to-array-data ((ptr data &key (direction :input)) &body body)
  (declare (ignorable direction))
  (let ((datag (gensym "DATA")) (thunk (gensym "THUNK"))
        (type (gensym "TYPE")) (i (gensym "I")) (dirg (gensym "DIRECTION")))
    (declare (ignorable type i dirg))
    `(let ((,datag ,data))
       (flet ((,thunk (,ptr)
                (declare (type cffi:foreign-pointer ,ptr))
                ,@body))
         (declare (dynamic-extent #',thunk))
         (typecase ,datag
           #+sbcl
           (sb-kernel:simple-unboxed-array
            (sb-sys:with-pinned-objects (,datag)
              (,thunk (sb-sys:vector-sap ,datag))))
           #+sbcl
           (array
            (let ((,ptr (sb-ext:array-storage-vector ,datag)))
              (sb-sys:with-pinned-objects (,ptr)
                (,thunk (sb-sys:vector-sap ,ptr)))))
           ;; Only use CFFI:WITH-POINTER-TO-VECTOR-DATA on impls that support
           ;; doing so for arbitrary vectors that don't require special allocation.
           ;; Meaning we exclude LispWorks, Allegro, and ABCL and force the slow
           ;; path in all cases on those impls (until they finally support lexical
           ;; pinning).
           #+(or openmcl scl clasp ecl mkcl mcl cmucl clisp)
           (vector
            (cffi:with-pointer-to-vector-data (,ptr ,datag)
              (,thunk ,ptr)))
           #-sbcl
           (array
            (let ((,type (element-type->c-type (array-element-type ,datag)))
                  (,dirg ,direction))
              (cffi:with-foreign-object (,ptr ,type (array-total-size ,datag))
                (ecase ,dirg
                  ((:input :io)
                   (dotimes (,i (array-total-size ,datag))
                     (setf (cffi:mem-aref ,ptr ,type ,i) (row-major-aref ,datag ,i))))
                  (:output))
                (unwind-protect (,thunk ,ptr)
                  (ecase ,dirg
                    (:input)
                    ((:output :io)
                     (dotimes (,i (array-total-size ,datag))
                       (setf (row-major-aref ,datag ,i) (cffi:mem-aref ,ptr ,type ,i))))))))))))))
