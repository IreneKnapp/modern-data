#include "modern.h"
#include "internal.h"


PUBLIC modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator,
   struct modern_node_representation *node_representation,
   void (*finalizer)(void *client_state),
   void *client_state)
{
    size_t size = sizeof(struct modern_library);
    struct modern_library *library =
        allocator->alloc(client_state, size);
    if(!library) {
        error_handler->memory
            (client_state, size);
        return NULL;
    }
    
    library->error_handler = error_handler;
    library->allocator = allocator;
    library->node_representation = node_representation;
    library->finalizer = finalizer;
    library->client_state = client_state;

    return (modern_library *) library;
}


PUBLIC struct modern_error_handler *modern_library_error_handler_get
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->error_handler;
}


PUBLIC struct modern_allocator *modern_library_allocator_get
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->allocator;
}


PUBLIC struct modern_node_representation *
  modern_library_node_representation_get
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->node_representation;
}


PUBLIC void modern_library_node_set
  (modern_library *library_in,
   struct modern_node_representation *node_representation)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    if(library->node_representation) {
        library->error_handler->usage(library->client_state);
        return;
    }
    
    library->node_representation = node_representation;
}


PUBLIC void *modern_library_client_state_get
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    return library->client_state;
}


PUBLIC void modern_library_finalize(modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    void (*finalizer)(void *client_state) = library->finalizer;
    void *client_state = library->client_state;

    library->allocator->free(client_state, library);
    
    finalizer(client_state);
}
