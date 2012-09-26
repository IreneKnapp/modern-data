#include "modern.h"
#include "internal.h"


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
	    
	    library->allocator->modern_allocator_free
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

