#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.memory-regions
  (:use #:cl)
  (:shadow #:fill #:replace)
  ;; allocator.lisp
  (:export
   #:allocator-condition
   #:allocator
   #:out-of-memory
   #:block-too-big
   #:size
   #:allocator
   #:allocate
   #:deallocate
   #:reallocate
   #:total-size
   #:free-space
   #:used-space
   #:minimum-block-size
   #:maximum-block-size
   #:*allocator*
   #:*standard-allocator*
   #:with-arena)
  ;; bump-allocator.lisp
  (:export
   #:bump-allocator)
  ;; memory-region.lisp
  (:export
   #:start
   #:end
   #:size
   #:to-memory-region
   #:call-with-memory-region
   #:clear
   #:fill
   #:replace
   #:subregion
   #:with-memory-region
   #:memory-region
   #:memory-region-pointer
   #:memory-region-size
   #:memory-region-valid-p
   #:memory-region-ish)
  ;; null-allocator.lisp
  (:export)
  ;; object.lisp
  (:export
   #:memory-region-object
   #:to-object)
  ;; pathname.lisp
  (:export)
  ;; sequence.lisp
  (:export
   #:memory-region-sequence
   #:to-sequence)
  ;; stream.lisp
  (:export
   #:memory-region-stream
   #:to-stream)
  ;; toolkit.lisp
  (:export
   #:with-pointer-to-array-data))
