#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


typedef void modern;
typedef void modern_context;
typedef void modern_autorelease_pool;
typedef void modern_library;
typedef float modern_float32;
typedef double modern_float64;


struct modern_hash {
    uint64_t a;
    uint64_t b;
};


struct modern_error_handler {
    void (*modern_error_handler_memory)
      (void *client_state, size_t requested_size);
    void (*modern_error_handler_retain_count_overflow)
      (void *client_state, void *retainable);
    void (*modern_error_handler_retain_count_underflow)
      (void *client_state, void *retainable);
    void (*modern_error_handler_double_autorelease)
      (void *client_state, void *retainable);
    void (*modern_error_handler_type_mismatch)
      (void *client_state, modern *expected, modern *actual);
    void (*modern_error_handler_universe_level_overflow)
      (void *client_state);
    void (*modern_error_handler_buffer_index)
      (void *client_state);
    void (*modern_error_handler_not_applicable)
      (void *client_state);
    void (*modern_error_handler_non_numeric_float)
      (void *client_state);
    void (*modern_error_handler_immutable)
      (void *client_state, modern *node);
 };


struct modern_allocator {
    void *(*modern_allocator_alloc)
      (void *client_state, size_t size);
    void (*modern_allocator_free)
      (void *client_state, void *memory);
    void *(*modern_allocator_realloc)
      (void *client_state, void *memory, size_t size);
};


struct modern_processor {
    modern_autorelease_pool *pool;
    void (*modern_processor_abort)(void *processor_state);
    void (*modern_processor_flush)(void *processor_state);
};


struct modern_stream {
    void *(*modern_stream_initialize)
      ();
    void (*modern_stream_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_magic_number)
      (void *processor_state, void *stream_state);
    void (*modern_stream_name_definition)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_type_definition_int8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int16)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat16)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_float32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_float64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_utf8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_blob)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_function_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_sigma_is_next)
      (void *processor_state, void *stream_state,
       struct modern_hash *a, struct modern_hash *b);
    void (*modern_stream_type_definition_named_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *name);
    void (*modern_stream_type_definition_universe)
      (void *processor_state, void *stream_state);
    void (*modern_stream_int8)
      (void *processor_state, void *stream_state, int8_t value);
    void (*modern_stream_int16)
      (void *processor_state, void *stream_state, int16_t value);
    void (*modern_stream_int32)
      (void *processor_state, void *stream_state, int32_t value);
    void (*modern_stream_int64)
      (void *processor_state, void *stream_state, int64_t value);
    void (*modern_stream_nat8)
      (void *processor_state, void *stream_state, uint8_t value);
    void (*modern_stream_nat16)
      (void *processor_state, void *stream_state, uint16_t value);
    void (*modern_stream_nat32)
      (void *processor_state, void *stream_state, uint32_t value);
    void (*modern_stream_nat64)
      (void *processor_state, void *stream_state, uint64_t value);
    void (*modern_stream_float32)
      (void *processor_state, void *stream_state, modern_float32 value);
    void (*modern_stream_float64)
      (void *processor_state, void *stream_state, modern_float64 value);
    void (*modern_stream_utf8_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_utf8_data)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_utf8_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_blob_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_blob_data)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_blob_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_named_value_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *name);
    void (*modern_stream_lambda_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_apply_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_family_is_next)
      (void *processor_state, void *stream_state, uint64_t n_items);
    void (*modern_stream_let_is_next)
      (void *processor_state, void *stream_state, uint64_t n_items);
    void (*modern_stream_backreference_is_next)
      (void *processor_state, void *stream_state, uint64_t index);
    void (*modern_stream_builtin_is_next)
      (void *processor_state, void *stream_state, uint16_t identifier);
    void (*modern_stream_type_as_value_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *type);
    void (*modern_stream_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_finalize)
      (void *stream_state);
};


struct modern_vfile {
    ssize_t (*modern_vfile_read)
      (void *vfile_state, uint8_t *buffer, size_t length);
    ssize_t (*modern_vfile_write)
      (void *vfile_state, uint8_t *buffer, size_t length);
};


enum modern_node_type {
    int8_value_modern_node_type = 1,
    int16_value_modern_node_type,
    int32_value_modern_node_type,
    int64_value_modern_node_type,
    nat8_value_modern_node_type,
    nat16_value_modern_node_type,
    nat32_value_modern_node_type,
    nat64_value_modern_node_type,
    float32_value_modern_node_type,
    float64_value_modern_node_type,
    utf8_value_modern_node_type,
    blob_value_modern_node_type,
    sigma_value_modern_node_type,
    named_value_modern_node_type,
    int8_type_modern_node_type,
    int16_type_modern_node_type,
    int32_type_modern_node_type,
    int64_type_modern_node_type,
    nat8_type_modern_node_type,
    nat16_type_modern_node_type,
    nat32_type_modern_node_type,
    nat64_type_modern_node_type,
    float32_type_modern_node_type,
    float64_type_modern_node_type,
    utf8_type_modern_node_type,
    blob_type_modern_node_type,
    function_type_modern_node_type,
    sigma_type_modern_node_type,
    named_type_modern_node_type,
    universe_type_modern_node_type,
    lambda_modern_node_type,
    apply_modern_node_type,
    type_family_modern_node_type,
    let_modern_node_type,
    backreference_modern_node_type,
    builtin_modern_node_type,
};


enum modern_builtin_identifier {
    plus_int8_modern_builtin_identifier = 1,
};


extern modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator,
   void (*finalizer)(void *client_state),
   void *client_state);
extern struct modern_error_handler *modern_library_get_error_handler
  (modern_library *library);
extern struct modern_allocator *modern_library_get_allocator
  (modern_library *library);
extern void *modern_library_get_client_state
  (modern_library *library);
extern void modern_library_finalize(modern_library *library);

extern modern_autorelease_pool *modern_make_autorelease_pool
  (modern_library *library);
extern void modern_autorelease_pool_release
  (modern_library *library,
   modern_autorelease_pool *pool);
extern void modern_retain
  (modern_library *library,
   void *retainable);
extern void modern_release
  (modern_library *library,
   void *retainable);
extern void modern_autorelease
  (modern_library *library,
   modern_autorelease_pool *pool,
   void *retainable);

extern modern_context *modern_make_initial_context
  (modern_library *library);
extern modern_context *modern_copy_context
  (modern_library *library,
   modern_context *context);
extern int modern_get_in_context
  (modern_library *library,
   modern_context *context, modern *node);
extern void modern_add_to_context
  (modern_library *library,
   modern_context *context, modern *node);
extern modern *modern_get_from_context
  (modern_library *library,
   modern_context *context, struct modern_hash *hash);

extern modern *modern_deserialize_memory
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   uint8_t *data, size_t length);
extern modern *modern_deserialize_file
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   FILE *file);
extern modern *modern_deserialize_fd
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   int fd);
extern modern *modern_deserialize_vfile
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);
extern modern *modern_deserialize_input_stream
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   void *processor_state, void *stream_state);

extern void modern_serialize_memory_buffer
  (modern_library *library,
   modern *value, modern_context *context, uint8_t *buffer, size_t *length);
extern uint8_t *modern_serialize_memory_allocating
  (modern_library *library,
   modern *value, modern_context *context, size_t *length);
extern void modern_serialize_file
  (modern_library *library,
   modern *value, modern_context *context, FILE *file);
extern void modern_serialize_fd
  (modern_library *library,
   modern *value, modern_context *context, int fd);
extern void modern_serialize_vfile
  (modern_library *library,
   modern *value, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);
extern modern *modern_serialize_output_stream
  (modern_library *library,
   modern *value, modern_context *context,
   struct modern_stream *stream);

extern enum modern_node_type modern_node_get_node_type
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_value_type
  (modern_library *library,
   modern *value);
extern int modern_node_get_mutable
  (modern_library *library,
   modern *value);

extern int8_t modern_node_get_int8
  (modern_library *library,
   modern *value);
extern int16_t modern_node_get_int16
  (modern_library *library,
   modern *value);
extern int32_t modern_node_get_int32
  (modern_library *library,
   modern *value);
extern int64_t modern_node_get_int64
  (modern_library *library,
   modern *value);
extern uint8_t modern_node_get_nat8
  (modern_library *library,
   modern *value);
extern uint16_t modern_node_get_nat16
  (modern_library *library,
   modern *value);
extern uint32_t modern_node_get_nat32
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_nat64
  (modern_library *library,
   modern *value);
extern modern_float32 modern_node_get_float32
  (modern_library *library,
   modern *value);
extern modern_float64 modern_node_get_float64
  (modern_library *library,
   modern *value);
extern size_t modern_node_get_utf8_bytes
  (modern_library *library,
   modern *value);
extern uint8_t *modern_node_get_utf8_data_piece
  (modern_library *library,
   modern *value, size_t offset, size_t bytes);
extern size_t modern_node_get_blob_bytes
  (modern_library *library,
   modern *value);
extern uint8_t *modern_node_get_blob_data_piece
  (modern_library *library,
   modern *value, size_t offset, size_t bytes);
extern modern *modern_node_get_sigma_field_value
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_successor
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_named_value
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_function_type_left
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_function_type_right
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_type_field_type
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_type_successor
  (modern_library *library,
   modern *value);
extern struct modern_hash *modern_node_get_named_type_name
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_named_type_content_type
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_universe_type_level
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_lambda_content
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_apply_left
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_apply_right
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_type_family_count
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_type_family_item
  (modern_library *library,
   modern *value, uint64_t index);
extern uint64_t modern_node_get_let_count
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_let_item
  (modern_library *library,
   modern *value, uint64_t index);
extern modern *modern_node_get_let_content
  (modern_library *library,
   modern *value);
extern uint16_t modern_node_get_builtin_identifier
  (modern_library *library,
   modern *value);

extern modern *modern_node_make_int8
  (modern_library *library,
   int8_t value);
extern modern *modern_node_make_int16
  (modern_library *library,
   int16_t value);
extern modern *modern_node_make_int32
  (modern_library *library,
   int32_t value);
extern modern *modern_node_make_int64
  (modern_library *library,
   int64_t value);
extern modern *modern_node_make_nat8
  (modern_library *library,
   uint8_t value);
extern modern *modern_node_make_nat16
  (modern_library *library,
   uint16_t value);
extern modern *modern_node_make_nat32
  (modern_library *library,
   uint32_t value);
extern modern *modern_node_make_nat64
  (modern_library *library,
   uint64_t value);
extern modern *modern_node_make_float32
  (modern_library *library,
   modern_float32 value);
extern modern *modern_node_make_float64
  (modern_library *library,
   modern_float64 value);
extern modern *modern_node_make_utf8
  (modern_library *library,
   uint8_t *data);
extern modern *modern_node_make_blob
  (modern_library *library,
   uint8_t *data, size_t bytes);
extern modern *modern_node_make_sigma
  (modern_library *library,
   modern *type, modern *field_value, modern *successor_value);
extern modern *modern_node_make_named_value
  (modern_library *library,
   modern *type, modern *value);

extern modern *modern_node_make_int8_type
  (modern_library *library);
extern modern *modern_node_make_int16_type
  (modern_library *library);
extern modern *modern_node_make_int32_type
  (modern_library *library);
extern modern *modern_node_make_int64_type
  (modern_library *library);
extern modern *modern_node_make_nat8_type
  (modern_library *library);
extern modern *modern_node_make_nat16_type
  (modern_library *library);
extern modern *modern_node_make_nat32_type
  (modern_library *library);
extern modern *modern_node_make_nat64_type
  (modern_library *library);
extern modern *modern_node_make_float32_type
  (modern_library *library);
extern modern *modern_node_make_float64_type
  (modern_library *library);
extern modern *modern_node_make_utf8_type
  (modern_library *library);
extern modern *modern_node_make_blob_type
  (modern_library *library);
extern modern *modern_node_make_function_type
  (modern_library *library,
   modern *left, modern *right);
extern modern *modern_node_make_sigma_type
  (modern_library *library,
   modern *field_type, modern *successor);
extern modern *modern_node_make_named_type
  (modern_library *library,
   struct modern_hash *name, modern *content_type);
extern modern *modern_node_make_universe_type
  (modern_library *library,
   uint64_t level);

extern modern *modern_node_make_lambda
  (modern_library *library,
   modern *content);
extern modern *modern_node_make_apply
  (modern_library *library,
   modern *left, modern *right);
extern modern *modern_node_make_type_family
  (modern_library *library,
   uint64_t n_items, modern **types);
extern modern *modern_node_make_let
  (modern_library *library,
   uint64_t n_items, modern **values, modern *content);
extern modern *modern_node_make_backreference
  (modern_library *library,
   uint64_t index);
extern modern *modern_node_make_builtin
  (modern_library *library,
   uint16_t identifier);

extern void modern_node_set_immutable
  (modern_library *library,
   modern *value);

extern void modern_node_set_int8
  (modern_library *library,
   modern *node,
   int8_t value);
extern void modern_node_set_int16
  (modern_library *library,
   modern *node,
   int16_t value);
extern void modern_node_set_int32
  (modern_library *library,
   modern *node,
   int32_t value);
extern void modern_node_set_int64
  (modern_library *library,
   modern *node,
   int64_t value);
extern void modern_node_set_nat8
  (modern_library *library,
   modern *node,
   uint8_t value);
extern void modern_node_set_nat16
  (modern_library *library,
   modern *node,
   uint16_t value);
extern void modern_node_set_nat32
  (modern_library *library,
   modern *node,
   uint32_t value);
extern void modern_node_set_nat64
  (modern_library *library,
   modern *node,
   uint64_t value);
extern void modern_node_set_float32
  (modern_library *library,
   modern *node,
   float value);
extern void modern_node_set_float64
  (modern_library *library,
   modern *node,
   double value);
extern void modern_node_set_utf8_data_piece
  (modern_library *library,
   modern *value,
   uint8_t *data,
   size_t offset,
   size_t old_bytes,
   size_t new_bytes);
extern void modern_node_set_blob_data_piece
  (modern_library *library,
   modern *value,
   uint8_t *data,
   size_t offset,
   size_t old_bytes,
   size_t new_bytes);
extern void modern_node_set_sigma_field_value
  (modern_library *library,
   modern *value,
   modern *field_value);
extern void modern_node_set_sigma_successor
  (modern_library *library,
   modern *value,
   modern *successor);
extern void modern_node_set_named_value
  (modern_library *library,
   modern *node,
   modern *type,
   modern *value);

extern void modern_node_set_function_type_left
  (modern_library *library,
   modern *value,
   modern *left);
extern void modern_node_set_function_type_right
  (modern_library *library,
   modern *value,
   modern *right);
extern void modern_node_set_sigma_type_field_type
  (modern_library *library,
   modern *value,
   modern *field_type);
extern void modern_node_set_sigma_type_successor
  (modern_library *library,
   modern *value,
   modern *successor);
extern void modern_node_set_named_type_name
  (modern_library *library,
   modern *value,
   struct modern_hash *name);
extern void modern_node_set_named_type_content_type
  (modern_library *library,
   modern *value,
   modern *content_type);
extern void modern_node_set_universe_type_level
  (modern_library *library,
   modern *value,
   uint64_t level);

extern void modern_node_set_lambda_content
  (modern_library *library,
   modern *value,
   modern *content);
extern void modern_node_set_apply_left
  (modern_library *library,
   modern *value,
   modern *left);
extern void modern_node_set_apply_right
  (modern_library *library,
   modern *value,
   modern *right);
extern void modern_node_set_type_family_add_item
  (modern_library *library,
   modern *value,
   modern *item,
   uint64_t index);
extern void modern_node_set_type_family_remove_item
  (modern_library *library,
   modern *value,
   uint64_t index);
extern void modern_node_set_let_add_item
  (modern_library *library,
   modern *value,
   modern *item,
   uint64_t index);
extern void modern_node_set_let_remove_item
  (modern_library *library,
   modern *value,
   uint64_t index);
extern void modern_node_set_let_content
  (modern_library *library,
   modern *value,
   modern *content);
extern void modern_node_set_builtin_identifier
  (modern_library *library,
   modern *value,
   uint16_t identifier);

extern void modern_node_canonical_hash
  (modern_library *library,
   modern *value,
   struct modern_hash *out);

extern void *modern_input_stream_memory
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   uint8_t *data, size_t length);
extern void *modern_input_stream_file
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   FILE *file);
extern void *modern_input_stream_fd
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   int fd);
extern void *modern_input_stream_vfile
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   struct modern_vfile *vfile, void *vfile_state);

extern void modern_input_stream_step
  (modern_library *library,
   struct modern_stream *stream, void *processor_state, void **stream_state);
extern void modern_input_stream_run
  (modern_library *library,
   struct modern_stream *stream, void *processor_state, void **stream_state);
extern void modern_input_stream_do_all
  (modern_library *library,
   struct modern_stream *stream, void *processor_state);

extern void modern_input_stream_finalize
  (modern_library *library,
   void *processor_state);

extern void *modern_output_stream_memory_buffer
  (modern_library *library,
   modern_autorelease_pool *pool,
   uint8_t *buffer, size_t *length);
extern void *modern_output_stream_memory_allocating
  (modern_library *library,
   modern_autorelease_pool *pool,
   size_t *length);
extern uint8_t *modern_output_stream_memory_allocating_result
  (modern_library *library,
   void *stream_state, size_t *length);
extern void *modern_output_stream_file
  (modern_library *library,
   modern_autorelease_pool *pool,
   FILE *file);
extern void *modern_output_stream_fd
  (modern_library *library,
   modern_autorelease_pool *pool,
   int fd);
extern void *modern_output_stream_vfile
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_vfile *vfile, void *vfile_state);

extern modern *modern_evaluate
  (modern_library *library, modern *node);

extern void modern_compute_hash
  (uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_compute_child_hash
  (struct modern_hash *parent,
   uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_compute_initial_namespace_hash(struct modern_hash *out);

extern struct modern_vfile *modern_make_memory_buffer_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_memory_allocating_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_file_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_fd_vfile
  (modern_library *library);

extern struct modern_stream *modern_make_explicatory_stream
  (modern_library *library);
extern struct modern_stream *modern_make_documentation_stream
  (modern_library *library);
