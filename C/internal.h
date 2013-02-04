#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static


struct modern_library {
    struct modern_error_handler *error_handler;
    struct modern_allocator *allocator;
    struct modern_node *node;
    void (*finalizer)(void *client_state);
    void *client_state;
};


struct memory {
	uint64_t retain_count;
	unsigned is_autoreleased : 1;
	void (*finalizer)(struct modern_library *library, void *retainable);
};


struct modern {
    struct memory memory;
    enum modern_node_type node_type : 6;
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
            struct modern *left;
            struct modern *right;
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
            struct modern *content;
        } lambda;
        struct {
            struct modern *left;
            struct modern *right;
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
    struct memory memory;
    size_t n_values;
    size_t n_buckets;
    struct modern *hash;
};


struct modern_autorelease_pool {
	size_t item_buffer_count;
	size_t item_buffer_capacity;
	struct memory **item_buffer;
};



// context.c
INTERNAL void internal_context_finalizer
  (struct modern_library *library,
   void *context);


// node-canonical.c
INTERNAL int modern_node_canonical_hash
  (modern_library *library_in,
   modern *value_in,
   struct modern_hash *out);


// node-get.c
INTERNAL enum modern_node_type default_modern_node_node_type_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_value_type_get
    (modern_library *library_in,
     modern *value_in);
INTERNAL int8_t default_modern_node_int8_get
    (modern_library *library_in, modern *value_in);
INTERNAL int16_t default_modern_node_int16_get
    (modern_library *library_in, modern *value_in);
INTERNAL int32_t default_modern_node_int32_get
    (modern_library *library_in, modern *value_in);
INTERNAL int64_t default_modern_node_int64_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t default_modern_node_nat8_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint16_t default_modern_node_nat16_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint32_t default_modern_node_nat32_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t default_modern_node_nat64_get
    (modern_library *library_in, modern *value_in);
INTERNAL float default_modern_node_float32_get
    (modern_library *library_in, modern *value_in);
INTERNAL double default_modern_node_float64_get
    (modern_library *library_in, modern *value_in);
INTERNAL size_t default_modern_node_utf8_bytes_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t *default_modern_node_utf8_data_piece_get
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes);
INTERNAL size_t default_modern_node_blob_bytes_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t *default_modern_node_blob_data_piece_get
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes);
INTERNAL modern *default_modern_node_sigma_field_value_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_sigma_successor_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_named_value_get
    (modern_library *library_in,
     modern *value_in);
INTERNAL modern *default_modern_node_function_type_left_get
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *default_modern_node_function_type_right_get
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *default_modern_node_sigma_type_field_type_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_sigma_type_successor_get
    (modern_library *library_in, modern *value_in);
INTERNAL struct modern_hash default_modern_node_named_type_name_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_named_type_content_type_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t default_modern_node_universe_type_level_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_lambda_content_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_apply_left_get
    (modern_library *library_in, modern *value_in);
INTERNAL modern *default_modern_node_apply_right_get
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t default_modern_node_type_family_count_get
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *default_modern_node_type_family_item_get
  (modern_library *library_in,
   modern *value_in, uint64_t index);
INTERNAL uint64_t default_modern_node_let_count_get
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *default_modern_node_let_item_get
  (modern_library *library_in,
   modern *value_in, uint64_t index);
INTERNAL modern *default_modern_node_let_content_get
  (modern_library *library_in,
   modern *value_in);
INTERNAL uint16_t default_modern_node_builtin_identifier_get
  (modern_library *library_in,
   modern *value_in);


// node-make.c
INTERNAL modern *default_modern_node_int8_make
    (modern_library *library_in,
     int8_t value);
INTERNAL modern *default_modern_node_int16_make
    (modern_library *library_in,
     int16_t value);
INTERNAL modern *default_modern_node_int32_make
    (modern_library *library_in,
     int32_t value);
INTERNAL modern *default_modern_node_int64_make
    (modern_library *library_in,
     int64_t value);
INTERNAL modern *default_modern_node_nat8_make
    (modern_library *library_in,
     uint8_t value);
INTERNAL modern *default_modern_node_nat16_make
    (modern_library *library_in,
     uint16_t value);
INTERNAL modern *default_modern_node_nat32_make
    (modern_library *library_in,
     uint32_t value);
INTERNAL modern *default_modern_node_nat64_make
    (modern_library *library_in,
     uint64_t value);
INTERNAL modern *default_modern_node_float32_make
    (modern_library *library_in,
     float value);
INTERNAL modern *default_modern_node_float64_make
    (modern_library *library_in,
     double value);
INTERNAL modern *default_modern_node_utf8_make
    (modern_library *library_in,
     uint8_t *data);
INTERNAL modern *default_modern_node_blob_make
    (modern_library *library_in,
     uint8_t *data, size_t bytes);
INTERNAL modern *default_modern_node_sigma_make
    (modern_library *library_in,
     modern *type_in,
     modern *field_value_in,
     modern *successor_in);
INTERNAL modern *default_modern_node_named_value_make
    (modern_library *library_in,
     modern *type_in, modern *value_in);
INTERNAL modern *default_modern_node_bool_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_ordering_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_maybe_type_make
    (modern_library *library_in,
     modern *content_type);
INTERNAL modern *default_modern_node_int8_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_int16_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_int32_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_int64_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_nat8_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_nat16_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_nat32_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_nat64_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_float32_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_float64_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_utf8_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_blob_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_function_type_make
    (modern_library *library_in,
     modern *left_in, modern *right_in);
INTERNAL modern *default_modern_node_sigma_type_make
    (modern_library *library_in,
     modern *field_type, modern *successor);
INTERNAL modern *default_modern_node_name_type_make
    (modern_library *library_in);
INTERNAL modern *default_modern_node_named_type_make
    (modern_library *library_in,
     struct modern_hash name, modern *content_type);
INTERNAL modern *default_modern_node_universe_type_make
    (modern_library *library_in,
     uint64_t level);
INTERNAL modern *default_modern_node_lambda_make
    (modern_library *library_in,
     modern *content_in);
INTERNAL modern *default_modern_node_apply_make
    (modern_library *library_in,
     modern *left_in, modern *right_in);
INTERNAL modern *default_modern_node_type_family_make
    (modern_library *library_in,
     uint64_t n_items, modern **types_in);
INTERNAL modern *default_modern_node_let_make
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in);
INTERNAL modern *default_modern_node_backreference_make
    (modern_library *library_in,
     uint64_t index);
INTERNAL modern *default_modern_node_builtin_make
    (modern_library *library_in,
     uint16_t identifier);


// node-set.c
INTERNAL void default_modern_node_immutable_set
    (modern_library *library,
     void *value);
INTERNAL void default_modern_node_canonical_hash_set
    (modern_library *library,
     void *value,
     struct modern_hash hash);
INTERNAL void default_modern_node_maybe_just_content_set
    (modern_library *library,
     void *value,
     void *content_value);
INTERNAL void default_modern_node_int8_set
    (modern_library *library,
     void *node,
     int8_t value);
INTERNAL void default_modern_node_int16_set
    (modern_library *library,
     void *node,
     int16_t value);
INTERNAL void default_modern_node_int32_set
    (modern_library *library,
     void *node,
     int32_t value);
INTERNAL void default_modern_node_int64_set
    (modern_library *library,
     void *node,
     int64_t value);
INTERNAL void default_modern_node_nat8_set
    (modern_library *library,
     void *node,
     uint8_t value);
INTERNAL void default_modern_node_nat16_set
    (modern_library *library,
     void *node,
     uint16_t value);
INTERNAL void default_modern_node_nat32_set
    (modern_library *library,
     void *node,
     uint32_t value);
INTERNAL void default_modern_node_nat64_set
    (modern_library *library,
     void *node,
     uint64_t value);
INTERNAL void default_modern_node_float32_set
    (modern_library *library,
     void *node,
     float value);
INTERNAL void default_modern_node_float64_set
    (modern_library *library,
     void *node,
     double value);
INTERNAL void default_modern_node_utf8_data_piece_set
    (modern_library *library,
     void *value,
     uint8_t *data,
     size_t offset,
     size_t old_bytes,
     size_t new_bytes);
INTERNAL void default_modern_node_blob_data_piece_set
    (modern_library *library,
     void *value,
     uint8_t *data,
     size_t offset,
     size_t old_bytes,
     size_t new_bytes);
INTERNAL void default_modern_node_sigma_set
    (modern_library *library,
     void *value,
     void *field_value,
     void *successor);
INTERNAL void default_modern_node_named_value_set
    (modern_library *library,
     void *node,
     void *type,
     void *value);
INTERNAL void default_modern_node_maybe_type_content_type_set
    (modern_library *library,
     void *value,
     void *content_type);
INTERNAL void default_modern_node_function_type_left_set
    (modern_library *library,
     void *value,
     void *left);
INTERNAL void default_modern_node_function_type_right_set
    (modern_library *library,
     void *value,
     void *right);
INTERNAL void default_modern_node_sigma_type_field_type_set
    (modern_library *library,
     void *value,
     void *field_type);
INTERNAL void default_modern_node_sigma_type_successor_set
    (modern_library *library,
     void *value,
     void *successor);
INTERNAL void default_modern_node_named_type_name_set
    (modern_library *library,
     void *value,
     struct modern_hash name);
INTERNAL void default_modern_node_named_type_content_type_set
    (modern_library *library,
     void *value,
     void *content_type);
INTERNAL void default_modern_node_universe_type_level_set
    (modern_library *library,
     void *value,
     uint64_t level);
INTERNAL void default_modern_node_lambda_content_set
    (modern_library *library,
     void *value,
     void *content);
INTERNAL void default_modern_node_apply_left_set
    (modern_library *library,
     void *value,
     void *left);
INTERNAL void default_modern_node_apply_right_set
    (modern_library *library,
     void *value,
     void *right);
INTERNAL void default_modern_node_type_family_item_add
    (modern_library *library,
     void *value,
     void *item,
     uint64_t index);
INTERNAL void default_modern_node_type_family_item_remove
    (modern_library *library,
     void *value,
     uint64_t index);
INTERNAL void default_modern_node_let_item_add
    (modern_library *library,
     void *value,
     void *item,
     uint64_t index);
INTERNAL void default_modern_node_let_item_remove
    (modern_library *library,
     void *value,
     uint64_t index);
INTERNAL void default_modern_node_let_content_set
    (modern_library *library,
     void *value,
     void *content);
INTERNAL void default_modern_node_backreference_index_set
    (modern_library *library,
     void *value,
     uint64_t index);
INTERNAL void default_modern_node_builtin_identifier_set
    (modern_library *library,
     void *value,
     uint16_t identifier);

