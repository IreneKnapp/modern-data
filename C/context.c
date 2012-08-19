#include <strings.h>
#include "modern.h"
#include "internal.h"


INTERNAL void internal_context_finalizer
  (struct modern_library *library,
   void *context);


modern_context *modern_make_initial_context
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    struct modern_context *context =
        library->allocator->modern_allocator_alloc
            (sizeof(struct modern_context));
    context->memory.retain_count = 1;
    context->memory.is_autoreleased = 0;
    context->memory.finalizer = internal_context_finalizer;
    context->n_values = 0;
    context->n_buckets = 1024;
    size_t hash_size = sizeof(struct modern *) * context->n_buckets;
    context->hash = library->allocator->modern_allocator_alloc(hash_size);
    bzero(context->hash, hash_size);
    
    return (modern_context *) context;
}


INTERNAL void internal_context_finalizer
  (struct modern_library *library,
   void *context_in)
{
	struct modern_context *context = (struct modern_context *) context_in;
    library->allocator->modern_allocator_free(context->hash);
    library->allocator->modern_allocator_free(context);
}


modern_context *modern_copy_context
  (modern_library *library,
   modern_context *context)
{
}


int modern_get_in_context
  (modern_library *library,
   modern_context *context, modern *node)
{
}


void modern_add_to_context
  (modern_library *library,
   modern_context *context, modern *node)
{
}


modern *modern_get_from_context
  (modern_library *library,
   modern_context *context, struct modern_hash *hash)
{
}
