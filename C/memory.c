#include "modern.h"


struct modern_autorelease_pool {
	struct modern_allocator *allocator;
	size_t item_buffer_count;
	size_t item_buffer_capacity;
	struct memory **item_buffer;
};


struct memory {
	uint64_t retain_count;
	struct modern_allocator *allocator;
	unsigned is_autoreleased : 1;
};


modern_autorelease_pool *modern_make_autorelease_pool
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator)
{
	size_t pool_size = sizeof(struct modern_autorelease_pool);
	struct modern_autorelease_pool *pool =
		allocator->modern_allocator_alloc(pool_size);
	if(!pool) {
		error_handler->modern_error_handler_memory(pool_size);
		return NULL;
	}
	
	pool->allocator = allocator;
	pool->item_buffer_count = 0;
	pool->item_buffer_capacity = 128;
	size_t item_buffer_size =
		sizeof(struct memory *) * pool->item_buffer_capacity
	pool->item_buffer = allocator->modern_allocator_alloc(item_buffer_size);
	if(!pool->item_buffer) {
		allocator->modern_allocator_free(pool);
		error_handler->modern_error_handler_memory(item_buffer_size);
		return NULL;
	}
	
	return pool;
}


void modern_autorelease_pool_release(modern_autorelease_pool *pool)
{
	for(size_t i = 0; i < pool->item_buffer_count; pool++) {
		pool->item_buffer[i]->is_autoreleased = 0;
		modern_release(pool->item_buffer[i]);
	}
	pool->allocator->modern_allocator_free(pool->item_buffer);
	pool->allocator->modern_allocator_free(pool);
}


struct modern_allocator *modern_autorelease_pool_get_allocator
  (modern_autorelease_pool *pool)
{
	return pool->allocator;
}


void modern_retain(struct modern_error_handler *error_handler, void *retainable)
{
	struct memory *memory = (struct memory *) retainable;
	if(memory->retain_count == UINT64_MAX) {
		error_handler->modern_error_handler_retain_count_overflow(retainable);
	} else {
		memory->retain_count++;
	}
}


void modern_release(struct modern_error_handler *error_handler, void *retainable)
{
	struct memory *memory = (struct memory *) retainable;
	if(memory->retain_count == 0) {
		error_handler->modern_error_handler_retain_count_underflow(retainable);
	} else if(memory->retain_count == 1) {
		memory->allocator->free(retainable);
	} else {
		memory->retain_count--;
	}
}


void modern_autorelease
  (struct modern_error_handler *error_handler,
   modern_autorelease_pool *pool,
   void *retainable)
{
	struct memory *memory = (struct memory *) retainable;
	
	if(memory->is_autoreleased) {
		error_handler->modern_error_handler_double_autorelease(retainable);
	} else {
		while(pool->item_buffer_count < pool->item_buffer_capacity) {
			pool->item_buffer_capacity *= 2;
			pool->item_buffer = pool->allocator->realloc
				(pool->item_buffer, sizeof(struct memory *) * pool->item_buffer_capacity);
		}
		pool->item_buffer[pool->item_buffer_count] = memory;
		pool->item_buffer_count++;
		
		memory->is_autoreleased = 1;
	}
}

