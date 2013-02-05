#include "modern.h"
#include "internal.h"


void modern_finalize
  (modern_library *library_in,
   void *finalizable)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct memory *memory = (struct memory *) retainable;
    
    if(memory->finalizer) memory->finalizer(library_in, retainable);
    
    library->allocator->modern_allocator_free
        (library->client_state, retainable);
}

