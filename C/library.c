#include "modern.h"
#include "internal.h"


modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator,
   void (*finalizer)(void *client_state),
   void *client_state)
{
    size_t size = sizeof(struct modern_library);
    struct modern_library *library =
        allocator->modern_allocator_alloc(client_state, size);
    if(!library) {
        error_handler->modern_error_handler_memory
            (client_state, size);
        return NULL;
    }
    
    library->error_handler = error_handler;
    library->allocator = allocator;
    library->finalizer = finalizer;
    library->client_state = client_state;
    
    return (modern_library *) library;
}


struct modern_error_handler *modern_library_get_error_handler
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->error_handler;
}


struct modern_allocator *modern_library_get_allocator
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->allocator;
}


void *modern_library_get_client_state
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->client_state;
}


void modern_library_finalize(modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    void (*finalizer)(void *client_state) = library->finalizer;
    void *client_state = library->client_state;
    
    library->allocator->modern_allocator_free(client_state, library);
    
    finalizer(client_state);
}
