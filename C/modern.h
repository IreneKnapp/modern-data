#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


typedef void modern;
typedef void modern_context;
typedef void modern_autorelease_pool;


struct modern_hash {
    uint64_t a;
    uint64_t b;
};


struct modern_allocator {
    void *(*modern_allocator_alloc)(size_t size);
    void (*modern_allocator_free)(void *memory);
    void *(*modern_allocator_realloc)(void *memory, size_t size);
};


struct modern_processor {
    modern_autorelease_pool *pool;
    void (*modern_processor_abort)(void *processor);
};


struct modern_stream {
    void (*modern_stream_start)
      (void *processor, void *state);
    void (*modern_stream_name_definition)
      (void *processor, void *state, uint8_t *data, size_t length);
    void (*modern_stream_type_definition_is_next)
      (void *processor, void *state, struct modern_hash *type);
    void (*modern_stream_type_definition_int8)
      (void *processor, void *state);
    void (*modern_stream_type_definition_int16)
      (void *processor, void *state);
    void (*modern_stream_type_definition_int32)
      (void *processor, void *state);
    void (*modern_stream_type_definition_int64)
      (void *processor, void *state);
    void (*modern_stream_type_definition_nat8)
      (void *processor, void *state);
    void (*modern_stream_type_definition_nat16)
      (void *processor, void *state);
    void (*modern_stream_type_definition_nat32)
      (void *processor, void *state);
    void (*modern_stream_type_definition_nat64)
      (void *processor, void *state);
    void (*modern_stream_type_definition_float32)
      (void *processor, void *state);
    void (*modern_stream_type_definition_float64)
      (void *processor, void *state);
    void (*modern_stream_type_definition_float128)
      (void *processor, void *state);
    void (*modern_stream_type_definition_utf8)
      (void *processor, void *state);
    void (*modern_stream_type_definition_blob)
      (void *processor, void *state);
    void (*modern_stream_type_definition_array_is_next)
      (void *processor, void *state);
    void (*modern_stream_type_definition_union_start)
      (void *processor, void *state, struct modern_hash *name);
    void (*modern_stream_type_definition_union_field_is_next)
      (void *processor, void *state, struct modern_hash *field);
    void (*modern_stream_type_definition_union_end)
      (void *processor, void *state);
    void (*modern_stream_type_definition_structure_start)
      (void *processor, void *state, struct modern_hash *name);
    void (*modern_stream_type_definition_structure_field_is_next)
      (void *processor, void *state, struct modern_hash *field);
    void (*modern_stream_type_definition_structure_end)
      (void *processor, void *state);
    void (*modern_stream_type_definition_named_is_next)
      (void *processor, void *state, struct modern_hash *name);
    void (*modern_stream_type_definition_universe)
      (void *processor, void *state);
    void (*modern_stream_type)
      (void *processor, void *state, struct modern_hash *type);
    void (*modern_stream_int8)
      (void *processor, void *state, int8_t value);
    void (*modern_stream_int16)
      (void *processor, void *state, int16_t value);
    void (*modern_stream_int32)
      (void *processor, void *state, int32_t value);
    void (*modern_stream_int64)
      (void *processor, void *state, int64_t value);
    void (*modern_stream_nat8)
      (void *processor, void *state, uint8_t value);
    void (*modern_stream_float32)
      (void *processor, void *state, float value);
    void (*modern_stream_float64)
      (void *processor, void *state, double value);
    void (*modern_stream_float128)
      (void *processsor, void *state, long double value);
    void (*modern_stream_utf8_start)
      (void *processor, void *state);
    void (*modern_stream_utf8_data)
      (void *processor, void *state, uint8_t *data, size_t length);
    void (*modern_stream_utf8_end)
      (void *processor, void *state);
    void (*modern_stream_blob_start)
      (void *processor, void *state);
    void (*modern_stream_blob_data)
      (void *processor, void *state, uint8_t *data, size_t length);
    void (*modern_stream_blob_end)
      (void *processor, void *state);
    void (*modern_stream_array_start)
      (void *processor, void *state);
    void (*modern_stream_array_end)
      (void *processor, void *state);
    void (*modern_stream_union_field)
      (void *processor, void *state,
       struct modern_hash *type, struct modern_hash *field);
    void (*modern_stream_structure_start)
      (void *processor, void *state, struct modern_hash *type);
    void (*modern_stream_structure_field)
      (void *processor, void *state, struct modern_hash *field);
    void (*modern_stream_structure_end)
      (void *processor, void *state);
    void (*modern_stream_named_value_is_next)
      (void *processor, void *state, struct modern_hash *name);
    void (*modern_stream_lambda_is_next)
      (void *processor, void *state);
    void (*modern_stream_apply_is_next)
      (void *processor, void *state);
    void (*modern_stream_type_index_is_next)
      (void *processor, void *state, uint64_t index);
    void (*modern_stream_type_family_is_next)
      (void *processor, void *state, uint64_t n_items);
    void (*modern_stream_end)
      (void *processor, void *state);
};


struct modern_vfile {
    ssize_t (*modern_vfile_read)
      (void *state, uint8_t *buffer, size_t length);
    ssize_t (*modern_vfile_write)
      (void *state, uint8_t *buffer, size_t length);
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
    float128_value_modern_node_type,
    utf8_value_modern_node_type,
    blob_value_modern_node_type,
    array_value_modern_node_type,
    union_value_modern_node_type,
    structure_value_modern_node_type,
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
    float128_type_modern_node_type,
    utf8_type_modern_node_type,
    blob_type_modern_node_type,
    sigma_type_modern_node_type, // new
    array_type_modern_node_type, // goes away
    union_type_modern_node_type, // goes away
    structure_type_modern_node_type, // goes away
    named_type_modern_node_type,
    universe_type_modern_node_type,
    lambda_modern_node_type,
    apply_modern_node_type,
};


extern modern_autorelease_pool *modern_make_autorelease_pool
  (struct modern_allocator *allocator);
extern void modern_release_autorelease_pool(modern_autorelease_pool *pool);
extern void modern_retain(void *retainable);
extern void modern_release(void *retainable);
extern void modern_autorelease
  (modern_autorelease_pool *pool, void *retainable);

extern modern_context *modern_make_initial_context();
extern int modern_get_in_context(modern_context *context, modern *node);
extern void modern_add_to_context(modern_context *context, modern *node);
extern modern *modern_get_from_context
  (modern_context *context, struct modern_hash *hash);

extern modern *modern_deserialize_memory
  (modern_autorelease_pool *pool, modern_context *context,
   uint8_t *data, size_t length);
extern modern *modern_deserialize_file
  (modern_autorelease_pool *pool, modern_context *context,
   FILE *file);
extern modern *modern_deserialize_fd
  (modern_autorelease_pool *pool, modern_context *context,
   int fd);
extern modern *modern_deserialize_vfile
  (modern_autorelease_pool *pool, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);

extern void modern_stream_memory
  (modern_autorelease_pool *pool,
   struct modern_stream *stream, void *stream_state,
   uint8_t *data, size_t length);
extern void modern_stream_file
  (modern_autorelease_pool *pool,
   struct modern_stream *stream, void *stream_state,
   FILE *file);
extern void modern_stream_fd
  (modern_autorelease_pool *pool,
   struct modern_stream *stream, void *stream_state,
   int fd);
extern void modern_stream_vfile
  (modern_autorelease_pool *pool,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state);
// add an output-stream API too

extern void modern_serialize_memory_buffer
  (modern *value, modern_context *context, uint8_t *buffer, size_t *length);
extern void *modern_serialize_memory_allocating
  (modern *value, modern_context *context,
   struct modern_allocator *allocator, size_t *length);
extern void modern_serialize_file
  (modern *value, modern_context *context, FILE *file);
extern void modern_serialize_fd
  (modern *value, modern_context *context, int fd);
extern void modern_serialize_vfile
  (modern *value, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);

extern enum modern_node_type modern_node_get_node_type(modern *value);
extern modern *modern_node_get_value_type(modern *value);
extern int8_t modern_node_get_int8(modern *value);
extern int16_t modern_node_get_int16(modern *value);
extern int32_t modern_node_get_int32(modern *value);
extern int64_t modern_node_get_int64(modern *value);
extern uint8_t modern_node_get_nat8(modern *value);
extern uint16_t modern_node_get_nat16(modern *value);
extern uint32_t modern_node_get_nat32(modern *value);
extern uint64_t modern_node_get_nat64(modern *value);
extern float modern_node_get_float32(modern *value);
extern double modern_node_get_float64(modern *value);
extern long double modern_node_get_float128(modern *value);
extern size_t modern_node_get_utf8_bytes(modern *value);
extern uint8_t *modern_node_get_utf8_data_piece
  (modern *value, size_t offset, size_t bytes);
extern size_t modern_node_get_blob_bytes(modern *value);
extern uint8_t *modern_node_get_blob_data_piece
  (modern *value, size_t offset, size_t bytes);
extern uint64_t modern_node_get_array_n_items(modern *value);
extern modern *modern_node_get_array_item_node(modern *value, uint64_t index);
extern modern *modern_node_get_union_field_name(modern *value);
extern modern *modern_node_get_union_field_node
  (modern *value, struct modern_hash *field);
extern modern *modern_node_get_structure_field_node
  (modern *value, struct modern_hash *field);
extern modern *modern_node_get_named_node
  (modern *value, struct modern_hash *name);
extern modern *modern_node_get_array_type_content_type(modern *value);
extern uint64_t modern_node_get_union_type_n_fields(modern *value);
extern struct modern_hash *modern_node_get_union_type_field_name
  (modern *value, uint64_t index);
extern modern *modern_node_get_union_type_field_type
  (modern *value, uint64_t index);
extern uint64_t modern_node_get_structure_type_n_fields(modern *value);
extern struct modern_hash *modern_node_get_structure_type_field_name
  (modern *value, uint64_t index);
extern modern *modern_node_get_structure_type_field_type
  (modern *value, uint64_t index);
extern struct modern_hash *modern_node_get_named_type_name(modern *value);
extern modern *modern_node_get_named_type_content_type(modern *value);
extern modern *modern_node_get_lambda_content(modern *value);
extern modern *modern_node_get_apply_left(modern *value);
extern modern *modern_node_get_apply_right(modern *value);

extern modern *modern_node_make_int8(int8_t value);
extern modern *modern_node_make_int16(int16_t value);
extern modern *modern_node_make_int32(int32_t value);
extern modern *modern_node_make_int64(int64_t value);
extern modern *modern_node_make_nat8(uint8_t value);
extern modern *modern_node_make_nat16(uint16_t value);
extern modern *modern_node_make_nat32(uint32_t value);
extern modern *modern_node_make_nat64(uint64_t value);
extern modern *modern_node_make_float32(float value);
extern modern *modern_node_make_float64(double value);
extern modern *modern_node_make_float128(long double value);
extern modern *modern_node_make_utf8(uint8_t *data);
extern modern *modern_node_make_blob(uint8_t *data, size_t bytes);
extern modern *modern_node_make_array(uint64_t n_items, modern **values);
extern modern *modern_node_make_union
  (modern *type, struct modern_hash *field, modern *value);
extern modern *modern_node_make_structure
  (modern *type, uint64_t n_items, struct modern_hash **fields,
   modern **values);
extern modern *modern_node_make_named_value(modern *type, modern *value);

extern modern *modern_node_get_int8_type();
extern modern *modern_node_get_int16_type();
extern modern *modern_node_get_int32_type();
extern modern *modern_node_get_int64_type();
extern modern *modern_node_get_nat8_type();
extern modern *modern_node_get_nat16_type();
extern modern *modern_node_get_nat32_type();
extern modern *modern_node_get_nat64_type();
extern modern *modern_node_get_float32_type();
extern modern *modern_node_get_float64_type();
extern modern *modern_node_get_float128_type();
extern modern *modern_node_get_utf8_type();
extern modern *modern_node_get_blob_type();
extern modern *modern_node_make_array_type(modern *content_type);
extern modern *modern_node_make_union_type
  (uint64_t n_items, struct modern_hash **fields, modern **types);
extern modern *modern_node_make_structure_type
  (uint64_t n_items, struct modern_hash **fields, modern **types);
extern modern *modern_node_make_named_type
  (struct modern_hash *name, modern *content_type);
extern modern *modern_node_get_universe_type();
extern modern *modern_node_make_lambda(modern *content);
extern modern *modern_node_make_apply(modern *left, modern *right);
extern modern *modern_node_make_type_index(uint64_t index);
extern modern *modern_node_make_type_family(uint64_t n_items, modern **types);

extern void modern_node_set_int8(modern *node, int8_t value);
extern void modern_node_set_int16(modern *node, int16_t value);
extern void modern_node_set_int32(modern *node, int32_t value);
extern void modern_node_set_int64(modern *node, int64_t value);
extern void modern_node_set_nat8(modern *node, uint8_t value);
extern void modern_node_set_nat16(modern *node, uint16_t value);
extern void modern_node_set_nat32(modern *node, uint32_t value);
extern void modern_node_set_nat64(modern *node, uint64_t value);
extern void modern_node_set_float32(modern *node, float value);
extern void modern_node_set_float64(modern *node, double value);
extern void modern_node_set_float128(modern *node, long double value);
extern void modern_node_set_utf8(modern *node, uint8_t *data);
extern void modern_node_set_blob(modern *node, uint8_t *data, size_t bytes);
extern void modern_node_set_array
  (modern *node, uint64_t n_items, modern **values);
extern void modern_node_set_union
  (modern *node, modern *type, struct modern_hash *field, modern *value);
extern void modern_node_set_structure
  (modern *node, modern *type, uint64_t n_items, struct modern_hash **fields,
   modern **values);
extern void modern_node_set_named_value
  (modern *node, modern *type, modern *value);

extern void modern_compute_hash
  (uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_compute_child_hash
  (struct modern_hash *parent,
   uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_get_initial_namespace_hash(struct modern_hash *out);

extern struct modern_stream *modern_get_explicatory_stream();
extern struct modern_stream *modern_get_documentation_stream();

