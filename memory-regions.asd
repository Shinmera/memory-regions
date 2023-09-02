(asdf:defsystem memory-regions
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Implementation of a memory region abstraction"
  :homepage "https://shinmera.github.io/memory-regions/"
  :bug-tracker "https://github.com/shinmera/memory-regions/issues"
  :source-control (:git "https://github.com/shinmera/memory-regions.git")
  :depends-on (:memory-regions/region
               :memory-regions/allocator
               :memory-regions/sequence
               :memory-regions/stream
               :memory-regions/object
               :memory-regions/pathname))

(asdf:defsystem memory-regions/region
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "memory-region")
               (:file "allocator")
               (:file "documentation"))
  :depends-on (:cffi
               :documentation-utils))

(asdf:defsystem memory-regions/allocator
  :serial T
  :components ((:file "null-allocator")
               (:file "bump-allocator")
               (:file "documentation"))
  :depends-on (:memory-regions/region))

(asdf:defsystem memory-regions/sequence
  :serial T
  :components ((:file "sequence"))
  :depends-on (:memory-regions/allocator
               :trivial-extensible-sequences))

(asdf:defsystem memory-regions/stream
  :serial T
  :components ((:file "stream"))
  :depends-on (:memory-regions/region
               :trivial-gray-streams))

(asdf:defsystem memory-regions/object
  :serial T
  :components ((:file "object"))
  :depends-on (:memory-regions/region
               :closer-mop))

(asdf:defsystem memory-regions/pathname
  :serial T
  :components ((:file "pathname"))
  :depends-on (:memory-regions/region
               :static-vectors
               :mmap))
