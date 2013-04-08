#include <strings.h>
#include "modern.h"
#include "internal.h"


modern_context *modern_make_initial_context
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    struct modern_context *context =
        library->allocator->modern_allocator_alloc
            (library->client_state, sizeof(struct modern_context));
    context->memory.finalizer = internal_context_finalizer;
    context->n_values = 0;
    context->n_buckets = 1024;
    size_t hash_size = sizeof(struct modern *) * context->n_buckets;
    context->hash = library->allocator->modern_allocator_alloc
        (library->client_state, hash_size);
    bzero(context->hash, hash_size);
    
    return (modern_context *) context;
}


INTERNAL void internal_context_finalizer
  (struct modern_library *library,
   void *context_in)
{
	struct modern_context *context = (struct modern_context *) context_in;
    library->allocator->modern_allocator_free
        (library->client_state, context->hash);
    library->allocator->modern_allocator_free
        (library->client_state, context);
}


modern_context *modern_copy_context
  (modern_library *library,
   modern_context *context)
{
    // TODO
    return NULL;
}


int modern_get_in_context
  (modern_library *library,
   modern_context *context, modern *node)
{
    // TODO
    return 0;
}


void modern_add_to_context
  (modern_library *library,
   modern_context *context, modern *node)
{
    // TODO
}


modern *modern_get_from_context
  (modern_library *library,
   modern_context *context, struct modern_hash hash)
{
    // TODO
    return NULL;
}
