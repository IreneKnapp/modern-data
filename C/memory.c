#include "modern.h"
#include "internal.h"


modern_autorelease_pool *modern_make_autorelease_pool
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
	size_t pool_size = sizeof(struct modern_autorelease_pool);
	struct modern_autorelease_pool *pool =
		library->allocator->modern_allocator_alloc
		    (library->client_state, pool_size);
	if(!pool) {
		library->error_handler->modern_error_handler_memory
		    (library->client_state, pool_size);
		return NULL;
	}
	
	pool->item_buffer_count = 0;
	pool->item_buffer_capacity = 128;
	size_t item_buffer_size =
		sizeof(struct memory *) * pool->item_buffer_capacity;
	pool->item_buffer = library->allocator->modern_allocator_alloc
	    (library->client_state, item_buffer_size);
	if(!pool->item_buffer) {
		library->allocator->modern_allocator_free
		    (library->client_state, pool);
		library->error_handler->modern_error_handler_memory
		    (library->client_state, item_buffer_size);
		return NULL;
	}
	
	return (modern_autorelease_pool *) pool;
}


void modern_autorelease_pool_release
  (modern_library *library_in,
   modern_autorelease_pool *pool_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    struct modern_autorelease_pool *pool =
        (struct modern_autorelease_pool *) pool_in;
	for(size_t i = 0; i < pool->item_buffer_count; pool++) {
		pool->item_buffer[i]->is_autoreleased = 0;
		modern_release(library_in, pool->item_buffer[i]);
	}
	library->allocator->modern_allocator_free
	    (library->client_state, pool->item_buffer);
	library->allocator->modern_allocator_free
	    (library->client_state, pool);
}


void modern_retain
  (modern_library *library_in, void *retainable)
{
    struct modern_library *library = (struct modern_library *) library_in;
	struct memory *memory = (struct memory *) retainable;
	if(memory->retain_count == UINT64_MAX) {
		library->error_handler->modern_error_handler_retain_count_overflow
		    (library->client_state, retainable);
	} else {
		memory->retain_count++;
	}
}


void modern_release
  (modern_library *library_in,
   void *retainable)
{
    struct modern_library *library = (struct modern_library *) library_in;
	struct memory *memory = (struct memory *) retainable;
	if(memory->retain_count == 0) {
		library->error_handler->modern_error_handler_retain_count_underflow
		    (library->client_state, retainable);
	} else if(memory->retain_count == 1) {
	    if(memory->finalizer) memory->finalizer(library_in, retainable);
	    else library->allocator->modern_allocator_free
	        (library->client_state, retainable);
	} else {
		memory->retain_count--;
	}
}


void modern_autorelease
  (modern_library *library_in,
   modern_autorelease_pool *pool_in,
   void *retainable)
{
    struct modern_library *library = (struct modern_library *) library_in;
	struct memory *memory = (struct memory *) retainable;
    struct modern_autorelease_pool *pool =
        (struct modern_autorelease_pool *) pool;
	
	if(memory->is_autoreleased) {
		library->error_handler->
		    modern_error_handler_double_autorelease
		        (library->client_state, retainable);
	} else {
		while(pool->item_buffer_count < pool->item_buffer_capacity) {
			pool->item_buffer_capacity *= 2;
			size_t item_buffer_bytes =
			    sizeof(struct memory *)
			    * pool->item_buffer_capacity;
			pool->item_buffer =
			    library->allocator->modern_allocator_realloc
                    (library->client_state, pool->item_buffer, item_buffer_bytes);
		}
		pool->item_buffer[pool->item_buffer_count] = memory;
		pool->item_buffer_count++;
		
		memory->is_autoreleased = 1;
	}
}

