#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.memory-regions)

(defmethod call-with-memory-region (function (data pathname) &key (offset 0))
  (mmap:with-mmap (ptr fd size data)
    (let ((region (memory-region (cffi:inc-pointer ptr offset) (- size offset))))
      (declare (dynamic-extent region))
      (funcall function region))))
