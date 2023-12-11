(in-package #:org.shirakumo.memory-regions)

;; allocator.lisp
(docs:define-docs
  (type allocator-condition
    "Supertype for conditions related to allocators.

See ALLOCATOR")
  
  (function allocator
    "Returns the allocator related to the condition.

See ALLOCATOR-ERROR")
  
  (type out-of-memory
    "Error signalled when an allocation request cannot be fulfilled.

See ALLOCATOR-CONDITION (type)
See ALLOCATOR (type)")

  (type block-too-big
    "Error signalled when a block that is too big for the allocator is requested.

See SIZE
See ALLOCATOR-CONDITION (type)
See ALLOCATOR (type)")
  
  (type allocator
    "Representation of an allocation manager that can hand out regions.

If you plan on implementing your own allocator, you must implement the
related functions.

You may also use NIL with any of the allocation functions, in which
case the C runtime allocator (malloc, etc) is used.

When you use T with any of the allocation functions, the current value
of the *ALLOCATOR* variable is used.

In order to free the allocator itself you should pass it to DEALLOCATE
with NIL as the region.

See ALLOCATE
See DEALLOCATE
See REALLOCATE
See TOTAL-SIZE
See FREE-SPACE
See USED-SPACE
See MINIMUM-BLOCK-SIZE
See MAXIMUM-BLOCK-SIZE
See *ALLOCATOR*
See WITH-ARENA
See CLEAR")
  
  (function allocate
    "Allocate a new memory-region.

The allocator may provide a region that is bigger than the requested
size, but not smaller. If the allocator would have to shrink the
region to provide the block of memory, an error of type BLOCK-TOO-BIG
must be signalled, instead. If the allocator cannot allocate the
requested amount of memory at all, an error of type OUT-OF-MEMORY is
signalled, instead.

The data pointed to by the returned region is *NOT* guaranteed to have
been cleared out and may contain random non-zero data.

SIZE must be a positive fixnum.

See MEMORY-REGION (type)
See ALLOCATOR (type)
See BLOCK-TOO-BIG (type)
See OUT-OF-MEMORY (type)")
  
  (function deallocate
    "Free a previously allocated memory-region.

The passed memory region must have been provided by a call to ALLOCATE
of the same allocator. Passing unrelated region objects will lead to
undefined behaviour.

The returned memory region will no longer be valid. Passing in an
invalid region has no effect.

If you pass NIL as the memory region, the allocator itself will be
deallocated, after which it must not be used again.

See MEMORY-REGION (type)
See ALLOCATOR (type)
See MEMORY-REGION-VALID-P
See ALLOCATE")
  
  (function reallocate
    "Resizes a previously allocated memory-region.

The passed memory region must have been provided by a call to ALLOCATE
of the same allocator. Passing unrelated region objects will lead to
undefined behaviour.

The memory region's pointer and size are modified in-place, if the
reallocation is successful, and the data pointed to will be
preserved. If the region's size is increased, octets beyond the
previous size are *NOT* guaranteed to have been cleared and may
contain random non-zero data.

The same constraints and error behaviour as for ALLOCATE apply.

See MEMORY-REGION (type)
See ALLOCATOR (type)
See ALLOCATE")
  
  (function total-size
    "Returns the total number of octets the allocator can distribute to memory-regions.

See ALLOCATOR (type)")
  
  (function free-space
    "Returns the remaining number of octets the allocator can distribute to memory-regions.

See ALLOCATOR (type)")
  
  (function used-space
    "Returns the number of octets that are currently distributed to memory-regions.

See ALLOCATOR (type)")
  
  (function minimum-block-size
    "Returns the minimal size of an allocated memory-region.

See ALLOCATOR (type)")
  
  (function maximum-block-size
    "Returns the maximal size a memory-region can have under this allocator.

See ALLOCATOR (type)")
  
  (variable *allocator*
    "Holds the default allocator used for allocation operations.

See WITH-ARENA
See ALLOCATOR (type)")
  
  (variable *standard-allocator*
    "Holds the class name of the default allocator used for new arenas.

See WITH-ARENA
See ALLOCATOR (type)")
  
  (function with-arena
    "Executes body with an arena allocator bound.

Any memory-regions allocated by the allocator may only be referenced
within the dynamic-extent of the BODY. Attempting to do so outside of
that dynamic-extent leads to undefined behaviour.

As much as possible the arena will be stack-allocated.

See *STANDARD-ALLOCATOR*
See *ALLOCATOR*
See ALLOCATOR (type)"))

;; bump-allocator.lisp
(docs:define-docs
  (type bump-allocator
    "Implementation of a simple bump allocator.

Allocations using this will be as fast as possible. Deallocations are
only regarded if the region is the last region that was allocated. The
idea being that you typically don't deallocate individual regions, and
instead CLEAR or DEALLOCATE the allocator itself when you're done.

See ALLOCATOR (type)"))

;; memory-region.lisp
(docs:define-docs
  (function start
    "Returns a pointer to the start of the memory region.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function end
    "Returns a pointer to the end of the memory region.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function size
    "Returns the number of octets represented by the memory region.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function to-memory-region
    "Coerces the given argument to a memory region.

See MEMORY-REGION (type)
See CALL-WITH-MEMORY-REGION
See WITH-MEMORY-REGION")
  
  (function call-with-memory-region
    "Calls the function with the argument coerced to a memory region.

The passed memory region is only valid within the dynamic extent of
the function call, and may not be accessed outside of it.

You may pass OFFSET to provide a starting offset into the resulting
memory region, as well as potentially other arguments depending on the
source.

If a CFFI:FOREIGN-POINTER is passed as the source, you must also pass
a SIZE argument to provide the memory region's extents.

See MEMORY-REGION (type)
See TO-MEMORY-REGION
See WITH-MEMORY-REGION")
  
  (function clear
    "Clears the memory region and fills it with zeroes.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function fill
    "Fills the memory region with a specific octet.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function replace
    "Replaces the memory region's contents with that of another.

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function subregion
    "Returns a sub-region of the given memory-region.

The following restrictions apply:
  - START and END must both be positive fixnums
  - START must be less than or equal to END
  - END must be less than the memory region's SIZE

See MEMORY-REGION (type)
See MEMORY-REGION-ISH (type)")
  
  (function with-memory-region
    "Convenience macro to dynamically create a memory region.

If the source is a positive fixnum constant, the memory area is allocated
directly on the stack.

See CALL-WITH-MEMORY-REGION")

  (function with-memory-regions
    "Convenience macro to dynamically create multiple memory regions.

The bindings are sequential as by LET*.

See WITH-MEMORY-REGION")
  
  (type memory-region
    "Representation of a region of memory.

This is a pointer and a size.

See MEMORY-REGION-POINTER
See MEMORY-REGION-SIZE
See MEMORY-REGION-VALID-P
See MEMORY-REGION-ISH (type)
See TO-MEMORY-REGION
See CALL-WITH-MEMORY-REGION
See START
See END
See SIZE
See CLEAR
See FILL
See REPLACE
See SUBREGION")
  
  (function memory-region-pointer
    "Returns the pointer to the memory region.

See MEMORY-REGION (type)")
  
  (function memory-region-size
    "Returns the number of octets the memory region encompasses.

See MEMORY-REGION (type)")
  
  (function memory-region-valid-p
    "Returns true if the memory region is valid and may be accessed.

See MEMORY-REGION (type)")
  
  (type memory-region-ish
    "Superclass for all objects that can be coerced to a memory region.

See TO-MEMORY-REGION
See CALL-WITH-MEMORY-REGION
See MEMORY-REGION (type)
See START
See END
See SIZE
See CLEAR
See FILL
See REPLACE
See SUBREGION"))

;; null-allocator.lisp
(docs:define-docs)

;; object.lisp
(docs:define-docs
  (type memory-region-sequence
    "Representation of a memory region as an object.

You must pass a STRUCTURE-TYPE, which must be a CFFI foreign structure
type specifier.

You can then access the struct's slots via SLOT-VALUE.

See MEMORY-REGION-ISH (type)
See MEMORY-REGION (type)
See STANDARD-OBJECT (type)
See TO-OBJECT")
  
  (function to-sequence
    "Coerces the given thing to an object.

See MEMORY-REGION-OBJECT (type)"))

;; pathname.lisp
(docs:define-docs)

;; sequence.lisp
(docs:define-docs
  (type memory-region-sequence
    "Representation of a memory region as a sequence.

You must pass an ELEMENT-TYPE, which must be a CFFI foreign type
specifier.

See MEMORY-REGION-ISH (type)
See MEMORY-REGION (type)
See SEQUENCE (type)
See TO-SEQUENCE")
  
  (function to-sequence
    "Coerces the given thing to a sequence.

See MEMORY-REGION-SEQUENCE (type)"))

;; stream.lisp
(docs:define-docs
  (type memory-region-stream
    "Representation of a memory region as an octet i/o stream.

Clearing input will cause the stream to be set to the end of the
region. Clearing output will cause the stream to be set to the start
of the region.

See MEMORY-REGION-ISH (type)
See MEMORY-REGION (type)
See STREAM (type)
See TO-STREAM")
  
  (function to-stream
    "Coerces the given thing to a stream.

See MEMORY-REGION-SEQUENCE (type)"))

;; toolkit.lisp
(docs:define-docs
  (function with-pointer-to-array-data
    "Provides a foreign pointer to the array contents.

DIRECTION must be one of:
  :INPUT  --- The data is only read
  :OUTPUT --- The data is only written
  :IO     --- The data is both read and written

As much as possible the provided pointer will literally point to the
array's in-memory representation. When this is not possible, a foreign
memory area is used instead, and the data is transferred as informed
by the DIRECTION argument."))
