#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static


struct modern_library {
    struct modern_error_handler *error_handler;
    struct modern_allocator *allocator;
    struct modern_node_representation *node_representation;
    void (*finalizer)(void *client_state);
    void *client_state;
    struct processor_explicatory_keyword_tree *processor_explicatory_keywords;
};


struct modern {
	void (*finalizer)(struct modern_library *library, struct modern *node);
    enum modern_node_type node_type;
    unsigned canonical_hash_valid : 1;
    struct modern *value_type;
    struct modern_hash canonical_hash;
    union {
        struct {
            struct modern *content_value;
        } maybe_value;
        int8_t int8_value;
        int16_t int16_value;
        int32_t int32_value;
        int64_t int64_value;
        uint8_t nat8_value;
        uint16_t nat16_value;
        uint32_t nat32_value;
        uint64_t nat64_value;
        float float32_value;
        double float64_value;
        long double float128_value;
        struct {
            size_t bytes;
            uint8_t *data;
        } utf8_value;
        struct {
            size_t bytes;
            uint8_t *data;
        } blob_value;
        struct {
            struct modern *field_value;
            struct modern *successor;
        } sigma_value;
        struct {
            struct modern_hash hash;
        } name_value;
        struct {
            struct modern *value;
        } named_value;
        struct {
            struct modern *content_type;
        } maybe_type;
        struct {
            struct modern *parameter;
            struct modern *result;
        } function_type;
        struct {
            struct modern *field_type;
            struct modern *successor;
        } sigma_type;
        struct {
            struct modern_hash name;
            struct modern *content_type;
        } named_type;
        struct {
            uint64_t level;
        } universe_type;
        struct {
            struct modern *supertype;
            struct modern *predicate;
        } satisfies_type;
        struct {
            struct modern *content;
        } lambda;
        struct {
            struct modern *function;
            struct modern *parameter;
        } apply;
        struct {
            size_t n_items;
            struct modern **members;
        } type_family;
        struct {
            size_t n_items;
            struct modern **members;
            struct modern *content;
        } let;
        struct {
            uint64_t index;
        } backreference;
        uint16_t builtin;
    } specifics;
};


struct modern_context {
	void (*finalizer)(struct modern_library *library,
                      struct modern_context *context);
    size_t n_values;
    size_t n_buckets;
    struct modern *hash;
};


struct processor_explicatory_state {
    struct modern_process process;
    struct modern_library *library;
    int started : 1;
    int ended : 1;
    int aborted : 1;
    size_t buffer_length;
    uint8_t buffer[64];
};


struct processor_explicatory_keyword_tree {
    unsigned is_leaf : 1;
    union {
        struct {
            struct processor_explicatory_keyword_tree_edge **edges;
        } internal_node;
        struct {
            void (*emit)
                 (struct processor_explicatory_state *process_state,
                  struct modern_stream *stream, void *stream_state,
                  struct modern_vfile *vfile, void *vfile_state);
        } leaf_node;
    } specifics;
};


// context.c
INTERNAL void internal_context_finalizer
  (struct modern_library *library,
   struct modern_context *context);


// memory.c
INTERNAL void default_finalize
  (modern_library *library_in,
   modern *node_in);


// node-canonical.c
INTERNAL int modern_node_canonical_hash
  (modern_library *library_in,
   modern *value_in,
   struct modern_hash *out);


// node-get.c
INTERNAL enum modern_node_type
    default_node_type_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_value_type_get
    (modern_library *library,
     void *value);
INTERNAL int
    default_mutable_get
    (modern_library *library,
     void *value);
INTERNAL int
    default_canonical_hash_valid_get
    (modern_library *library,
     modern *value);
INTERNAL struct modern_hash
    default_canonical_hash_get
    (modern_library *library,
     modern *value);
INTERNAL void *
    default_maybe_just_content_get
    (modern_library *library,
     void *value);
INTERNAL int8_t 
    default_int8_get
    (modern_library *library,
     void *value);
INTERNAL int16_t 
    default_int16_get
    (modern_library *library,
     void *value);
INTERNAL int32_t 
    default_int32_get
    (modern_library *library,
     void *value);
INTERNAL int64_t 
    default_int64_get
    (modern_library *library,
     void *value);
INTERNAL uint8_t 
    default_nat8_get
    (modern_library *library,
     void *value);
INTERNAL uint16_t 
    default_nat16_get
    (modern_library *library,
     void *value);
INTERNAL uint32_t 
    default_nat32_get
    (modern_library *library,
     void *value);
INTERNAL uint64_t 
    default_nat64_get
    (modern_library *library,
     void *value);
INTERNAL float 
    default_float32_get
    (modern_library *library,
     void *value);
INTERNAL double 
    default_float64_get
    (modern_library *library,
     void *value);
INTERNAL size_t 
    default_utf8_bytes_get
    (modern_library *library,
     void *value);
INTERNAL uint8_t *
    default_utf8_data_piece_get
    (modern_library *library,
     void *value, size_t offset, size_t bytes);
INTERNAL size_t 
    default_blob_bytes_get
    (modern_library *library,
     void *value);
INTERNAL uint8_t *
    default_blob_data_piece_get
    (modern_library *library,
     void *value, size_t offset, size_t bytes);
INTERNAL void *
    default_sigma_field_value_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_sigma_successor_get
    (modern_library *library,
     void *value);
INTERNAL struct modern_hash
    default_name_value_hash_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_named_value_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_maybe_type_content_type_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_maybe_type_content_type_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_function_type_parameter_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_function_type_result_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_sigma_type_field_type_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_sigma_type_successor_get
    (modern_library *library,
     void *value);
INTERNAL struct modern_hash
    default_named_type_name_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_named_type_content_type_get
    (modern_library *library,
     void *value);
INTERNAL uint64_t 
    default_universe_type_level_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_lambda_content_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_apply_function_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_apply_parameter_get
    (modern_library *library,
     void *value);
INTERNAL uint64_t 
    default_type_family_count_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_type_family_item_get
    (modern_library *library,
     void *value, uint64_t index);
INTERNAL uint64_t 
    default_let_count_get
    (modern_library *library,
     void *value);
INTERNAL void *
    default_let_item_get
    (modern_library *library,
     void *value, uint64_t index);
INTERNAL void *
    default_let_content_get
    (modern_library *library,
     void *value);
INTERNAL uint64_t 
    default_backreference_index_get
    (modern_library *library,
     void *value);
INTERNAL uint16_t 
    default_builtin_identifier_get
    (modern_library *library,
     void *value);


// node-make.c
INTERNAL modern *
    default_bool_false_make
    (modern_library *library);
INTERNAL modern *
    default_bool_true_make
    (modern_library *library);
INTERNAL modern *
    default_ordering_less_make
    (modern_library *library);
INTERNAL modern *
    default_ordering_equal_make
    (modern_library *library);
INTERNAL modern *
    default_ordering_greater_make
    (modern_library *library);
INTERNAL modern *
    default_maybe_nothing_make
    (modern_library *library,
     modern *type);
INTERNAL modern *
    default_maybe_just_make
    (modern_library *library,
     modern *type,
     modern *content_value);
INTERNAL modern *
    default_int8_make
    (modern_library *library,
     int8_t value);
INTERNAL modern *
    default_int16_make
    (modern_library *library,
     int16_t value);
INTERNAL modern *
    default_int32_make
    (modern_library *library,
     int32_t value);
INTERNAL modern *
    default_int64_make
    (modern_library *library,
     int64_t value);
INTERNAL modern *
    default_nat8_make
    (modern_library *library,
     uint8_t value);
INTERNAL modern *
    default_nat16_make
    (modern_library *library,
     uint16_t value);
INTERNAL modern *
    default_nat32_make
    (modern_library *library,
     uint32_t value);
INTERNAL modern *
    default_nat64_make
    (modern_library *library,
     uint64_t value);
INTERNAL modern *
    default_float32_make
    (modern_library *library,
     float value);
INTERNAL modern *
    default_float64_make
    (modern_library *library,
     double value);
INTERNAL modern *
    default_utf8_make
    (modern_library *library,
     uint8_t *data);
INTERNAL modern *
    default_blob_make
    (modern_library *library,
     uint8_t *data, size_t bytes);
INTERNAL modern *
    default_sigma_make
    (modern_library *library,
     modern *type, modern *field_value, modern *successor_value);
INTERNAL modern *
    default_name_value_make
    (modern_library *library,
     struct modern_hash name);
INTERNAL modern *
    default_named_value_make
    (modern_library *library,
     modern *type, modern *value);
INTERNAL modern *
    default_bool_type_make
    (modern_library *library);
INTERNAL modern *
    default_ordering_type_make
    (modern_library *library);
INTERNAL modern *
    default_maybe_type_make
    (modern_library *library,
     modern *content_type);
INTERNAL modern *
    default_int8_type_make
    (modern_library *library);
INTERNAL modern *
    default_int16_type_make
    (modern_library *library);
INTERNAL modern *
    default_int32_type_make
    (modern_library *library);
INTERNAL modern *
    default_int64_type_make
    (modern_library *library);
INTERNAL modern *
    default_nat8_type_make
    (modern_library *library);
INTERNAL modern *
    default_nat16_type_make
    (modern_library *library);
INTERNAL modern *
    default_nat32_type_make
    (modern_library *library);
INTERNAL modern *
    default_nat64_type_make
    (modern_library *library);
INTERNAL modern *
    default_float32_type_make
    (modern_library *library);
INTERNAL modern *
    default_float64_type_make
    (modern_library *library);
INTERNAL modern *
    default_utf8_type_make
    (modern_library *library);
INTERNAL modern *
    default_blob_type_make
    (modern_library *library);
INTERNAL modern *
    default_function_type_make
    (modern_library *library,
     modern *parameter, modern *result);
INTERNAL modern *
    default_sigma_type_make
    (modern_library *library,
     modern *field_type, modern *successor);
INTERNAL modern *
    default_name_type_make
    (modern_library *library);
INTERNAL modern *
    default_named_type_make
    (modern_library *library,
     struct modern_hash name, modern *content_type);
INTERNAL modern *
    default_universe_type_make
    (modern_library *library,
     uint64_t level);
INTERNAL modern *
    default_lambda_make
    (modern_library *library,
     modern *content);
INTERNAL modern *
    default_apply_make
    (modern_library *library,
     modern *function, modern *parameter);
INTERNAL modern *
    default_type_family_make
    (modern_library *library,
     uint64_t n_items, modern **types);
INTERNAL modern *
    default_let_make
    (modern_library *library,
     uint64_t n_items, modern **values, modern *content);
INTERNAL modern *
    default_backreference_make
    (modern_library *library,
     uint64_t index);
INTERNAL modern *
    default_builtin_make
    (modern_library *library,
     uint16_t identifier);


// node-set.c
INTERNAL void
    default_canonical_hash_set
    (modern_library *library,
     void *value,
     struct modern_hash hash);


// processor-explicatory.c
INTERNAL void
    initialize_processor_explicatory
    (struct modern_library *library);

