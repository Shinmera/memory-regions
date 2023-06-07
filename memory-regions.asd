#|
 This file is a part of memory-regions
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem memory-regions
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Implementation of a memory region abstraction"
  :homepage "https://Shirakumo.github.io/memory-regions/"
  :bug-tracker "https://github.com/Shirakumo/memory-regions/issues"
  :source-control (:git "https://github.com/Shirakumo/memory-regions.git")
  :depends-on (:memory-regions/region
               :memory-regions/allocator
               :memory-regions/sequence
               :memory-regions/stream
               :memory-regions/pathname
               :documentation-utils))

(asdf:defsystem memory-regions/region
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "memory-region")
               (:file "allocator")
               (:file "documentation"))
  :depends-on (:cffi))

(asdf:defsystem memory-regions/allocator
  :serial T
  :components ((:file "allocator")
               (:file "null-allocator")
               (:file "bump-allocator"))
  :depends-on (:memory-regions/region))

(asdf:defsystem memory-regions/sequence
  :serial T
  :components ((:file "sequence"))
  :depends-on (:memory-regions/region
               :trivial-extensible-sequences))

(asdf:defsystem memory-regions/stream
  :serial T
  :components ((:file "stream"))
  :depends-on (:memory-regions/region
               :trivial-gray-streams))

(asdf:defsystem memory-regions/pathname
  :serial T
  :components ((:file "pathname"))
  :depends-on (:memory-regions/region
               :mmap))
