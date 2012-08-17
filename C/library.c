#include "modern.h"
#include "internal.h"


modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator)
{
    struct modern_library *library =
        allocator->modern_allocator_alloc(sizeof(struct modern_library));
    library->error_handler = error_handler;
    library->allocator = allocator;
    library->cache_context = NULL;
    
    library->cache_context = modern_make_initial_context(library);
    
    return (modern_library *) library;
}


struct modern_error_handler *modern_library_get_error_handler
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library_in->error_handler;
}


struct modern_allocator *modern_library_get_allocator
  (modern_library *library)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library_in->allocator;
}


void modern_library_finalize(modern_library *library)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library_in->error_handler;
}
