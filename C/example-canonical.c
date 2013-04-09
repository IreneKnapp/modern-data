#include "modern.h"


static void *allocator_malloc
  (void *client_state, size_t size);
static void allocator_free
  (void *client_state, void *data);
static void *allocator_realloc
  (void *client_state, void *data, size_t size);
static void error_memory
  (void *client_state, size_t requested_size);
static void error_retain_count_overflow
  (void *client_state, void *retainable);
static void error_retain_count_underflow
  (void *client_state, void *retainable);
static void error_double_autorelease
  (void *client_state, void *retainable);
static void error_type_mismatch
  (void *client_state, modern *expected, modern *actual);
static void error_universe_level_overflow
  (void *client_state);
static void error_buffer_index
  (void *client_state);
static void error_not_applicable
  (void *client_state);
static void error_non_numeric_float
  (void *client_state);
static void library_finalizer
  (void *client_state);


int main(int argc, char **argv) {
    struct modern_error_handler error_handler;
    error_handler.modern_error_handler_memory =
        (void (*)(void *, size_t)) error_memory;
    error_handler.modern_error_handler_type_mismatch =
        (void (*)(void *, modern *, modern *)) error_type_mismatch;
    error_handler.modern_error_handler_universe_level_overflow =
        (void (*)(void *)) error_universe_level_overflow;
    error_handler.modern_error_handler_buffer_index =
        (void (*)(void *)) error_buffer_index;
    error_handler.modern_error_handler_not_applicable =
        (void (*)(void *)) error_not_applicable;
    error_handler.modern_error_handler_non_numeric_float =
        (void (*)(void *)) error_non_numeric_float;
    
    struct modern_allocator allocator;
    allocator.modern_allocator_alloc =
        (void *(*)(void *, size_t)) allocator_malloc;
    allocator.modern_allocator_free =
        (void (*)(void *, void *)) allocator_free;
    allocator.modern_allocator_realloc =
        (void *(*)(void *, void *, size_t)) allocator_realloc;
    
    struct modern_node_representation *representation =
        modern_node_representation_default_make(&allocator, NULL);
    
    modern_library *library = modern_library_initialize
        (&error_handler,
         &allocator,
         representation,
         (void (*)(void *)) library_finalizer,
         NULL);
    
    size_t all_nodes_count = 0;
    modern **all_nodes[1024];
    
    modern *float32_node =
        representation->modern_node_representation_float32_make
            (library, 12.13);
    all_nodes[all_nodes_count++] = float32_node;
    
    for(size_t i = 0; i < all_nodes_count; i++) {
        struct modern_hash hash =
            representation->modern_node_representation_canonical_hash_get
                (library, all_nodes[i]);
        printf("%016llx%016llx\n",
               (unsigned long long) hash.a,
               (unsigned long long) hash.b);
	}
	
    for(size_t i = 0; i < all_nodes_count; i++) {
		modern_finalize(library, all_nodes[i]);
	}
	
    modern_library_finalize(library);
    
    return 0;
}


static void *allocator_malloc(void *client_state, size_t size) {
    return malloc(size);
}


static void allocator_free(void *client_state, void *data) {
    free(data);
}


static void *allocator_realloc(void *client_state, void *data, size_t size) {
    return realloc(data, size);
}


static void error_memory
  (void *client_state, size_t requested_size)
{
}


static void error_retain_count_overflow
  (void *client_state, void *retainable)
{
}


static void error_retain_count_underflow
  (void *client_state, void *retainable)
{
}


static void error_double_autorelease
  (void *client_state, void *retainable)
{
}


static void error_type_mismatch
  (void *client_state, modern *expected, modern *actual)
{
}


static void error_universe_level_overflow
  (void *client_state)
{
}


static void error_buffer_index
  (void *client_state)
{
}


static void error_not_applicable
  (void *client_state)
{
}


static void error_non_numeric_float
  (void *client_state)
{
}


static void library_finalizer
  (void *client_state)
{
}
