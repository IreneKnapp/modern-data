#include <string.h>
#include "modern.h"


static void error_memory(size_t requested_size);
static void error_retain_count_overflow(void *retainable);
static void error_retain_count_underflow(void *retainable);
static void error_double_autorelease(void *retainable);
static void error_type_mismatch(modern *expected, modern *actual);
static void error_universe_level_overflow();
static void error_buffer_index();


int main(int argc, char **argv) {
    struct modern_error_handler error_handler;
    error_handler.modern_error_handler_memory = error_memory;
    error_handler.modern_error_handler_retain_count_overflow = error_retain_count_overflow;
    error_handler.modern_error_handler_retain_count_underflow = error_retain_count_underflow;
    error_handler.modern_error_handler_double_autorelease = error_double_autorelease;
    error_handler.modern_error_handler_type_mismatch = error_type_mismatch;
    error_handler.modern_error_handler_universe_level_overflow = error_universe_level_overflow;
    error_handler.modern_error_handler_buffer_index = error_buffer_index;
    
    struct modern_allocator allocator;
    allocator.modern_allocator_alloc = malloc;
    allocator.modern_allocator_free = free;
    allocator.modern_allocator_realloc = realloc;
    
    modern_library *library =
    	modern_library_initialize(&error_handler, &allocator);
    
    modern_autorelease_pool *pool = modern_make_autorelease_pool(library);
    
    modern_context *context = modern_make_initial_context(library);
    
    float value_floats[] = {
    	0.0, -0.0,
    	1.0 / 0.0, -1.0 / 0.0,
    	1.0, -1.0,
    	0.5, -0.5,
    	0.25, -0.25,
    	0.125, -0.125,
    	1.0 - 0.5, -1.0 + 0.5,
    	1.0 - 0.25, -1.0 + 0.25,
    	1.0 - 0.125, -1.0 + 0.125,
    };
    for(int i = 0; i < 18; i++) {
		printf("%f", value_floats[i]);
		modern *value = modern_node_make_float32(library, value_floats[i]);
		
		struct modern_hash hash;
		modern_node_canonical_hash(library, value, &hash);
		
		modern_release(library, value);
	}
    
    return 0;
}


static void error_memory(size_t requested_size) {
    fprintf(stderr, "Unable to allocate %llu bytes.\n",
            (unsigned long long) requested_size);
    exit(1);
}


static void error_retain_count_overflow(void *retainable) {
    fprintf(stderr, "Retain count overflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_retain_count_underflow(void *retainable) {
    fprintf(stderr, "Retain count underflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_double_autorelease(void *retainable) {
    fprintf(stderr, "Double autorelease on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_type_mismatch(modern *expected, modern *actual) {
    fprintf(stderr, "Type mismatch.  Expected 0x%llx; got 0x%llx.\n",
            (unsigned long long) expected, (unsigned long long) actual);
    exit(1);
}


static void error_universe_level_overflow() {
    fprintf(stderr, "Universe level overflow.\n");
    exit(1);
}


static void error_buffer_index() {
    fprintf(stderr, "Buffer index out of range.\n");
    exit(1);
}
