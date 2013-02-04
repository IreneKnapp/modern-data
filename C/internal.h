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
INTERNAL enum modern_node_type modern_node_get_node_type
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_value_type
    (modern_library *library_in,
     modern *value_in);
INTERNAL int8_t modern_node_get_int8
    (modern_library *library_in, modern *value_in);
INTERNAL int16_t modern_node_get_int16
    (modern_library *library_in, modern *value_in);
INTERNAL int32_t modern_node_get_int32
    (modern_library *library_in, modern *value_in);
INTERNAL int64_t modern_node_get_int64
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t modern_node_get_nat8
    (modern_library *library_in, modern *value_in);
INTERNAL uint16_t modern_node_get_nat16
    (modern_library *library_in, modern *value_in);
INTERNAL uint32_t modern_node_get_nat32
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t modern_node_get_nat64
    (modern_library *library_in, modern *value_in);
INTERNAL float modern_node_get_float32
    (modern_library *library_in, modern *value_in);
INTERNAL double modern_node_get_float64
    (modern_library *library_in, modern *value_in);
INTERNAL size_t modern_node_get_utf8_bytes
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t *modern_node_get_utf8_data_piece
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes);
INTERNAL size_t modern_node_get_blob_bytes
    (modern_library *library_in, modern *value_in);
INTERNAL uint8_t *modern_node_get_blob_data_piece
  (modern_library *library_in, modern *value_in,
   size_t offset, size_t bytes);
INTERNAL modern *modern_node_get_sigma_field_value
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_sigma_successor
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_named_value
    (modern_library *library_in,
     modern *value_in);
INTERNAL modern *modern_node_get_function_type_left
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *modern_node_get_function_type_right
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *modern_node_get_sigma_type_field_type
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_sigma_type_successor
    (modern_library *library_in, modern *value_in);
INTERNAL struct modern_hash modern_node_get_named_type_name
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_named_type_content_type
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t modern_node_get_universe_type_level
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_lambda_content
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_apply_left
    (modern_library *library_in, modern *value_in);
INTERNAL modern *modern_node_get_apply_right
    (modern_library *library_in, modern *value_in);
INTERNAL uint64_t modern_node_get_type_family_count
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *modern_node_get_type_family_item
  (modern_library *library_in,
   modern *value_in, uint64_t index);
INTERNAL uint64_t modern_node_get_let_count
  (modern_library *library_in,
   modern *value_in);
INTERNAL modern *modern_node_get_let_item
  (modern_library *library_in,
   modern *value_in, uint64_t index);
INTERNAL modern *modern_node_get_let_content
  (modern_library *library_in,
   modern *value_in);
INTERNAL uint16_t modern_node_get_builtin_identifier
  (modern_library *library_in,
   modern *value_in);


// node-make.c
INTERNAL modern *modern_node_make_int8
    (modern_library *library_in,
     int8_t value);
INTERNAL modern *modern_node_make_int16
    (modern_library *library_in,
     int16_t value);
INTERNAL modern *modern_node_make_int32
    (modern_library *library_in,
     int32_t value);
INTERNAL modern *modern_node_make_int64
    (modern_library *library_in,
     int64_t value);
INTERNAL modern *modern_node_make_nat8
    (modern_library *library_in,
     uint8_t value);
INTERNAL modern *modern_node_make_nat16
    (modern_library *library_in,
     uint16_t value);
INTERNAL modern *modern_node_make_nat32
    (modern_library *library_in,
     uint32_t value);
INTERNAL modern *modern_node_make_nat64
    (modern_library *library_in,
     uint64_t value);
INTERNAL modern *modern_node_make_float32
    (modern_library *library_in,
     float value);
INTERNAL modern *modern_node_make_float64
    (modern_library *library_in,
     double value);
INTERNAL modern *modern_node_make_utf8
    (modern_library *library_in,
     uint8_t *data);
INTERNAL modern *modern_node_make_blob
    (modern_library *library_in,
     uint8_t *data, size_t bytes);
INTERNAL modern *modern_node_make_sigma
    (modern_library *library_in,
     modern *type_in,
     modern *field_value_in,
     modern *successor_in);
INTERNAL modern *modern_node_make_named_value
    (modern_library *library_in,
     modern *type_in, modern *value_in);
INTERNAL modern *modern_node_make_bool_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_ordering_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_maybe_type
    (modern_library *library_in,
     modern *content_type);
INTERNAL modern *modern_node_make_int8_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_int16_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_int32_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_int64_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_nat8_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_nat16_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_nat32_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_nat64_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_float32_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_float64_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_utf8_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_blob_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_function_type
    (modern_library *library_in,
     modern *left_in, modern *right_in);
INTERNAL modern *modern_node_make_sigma_type
    (modern_library *library_in,
     modern *field_type, modern *successor);
INTERNAL modern *modern_node_make_name_type
    (modern_library *library_in);
INTERNAL modern *modern_node_make_named_type
    (modern_library *library_in,
     struct modern_hash name, modern *content_type);
INTERNAL modern *modern_node_make_universe_type
    (modern_library *library_in,
     uint64_t level);
INTERNAL modern *modern_node_make_lambda
    (modern_library *library_in,
     modern *content_in);
INTERNAL modern *modern_node_make_apply
    (modern_library *library_in,
     modern *left_in, modern *right_in);
INTERNAL modern *modern_node_make_type_family
    (modern_library *library_in,
     uint64_t n_items, modern **types_in);
INTERNAL modern *modern_node_make_let
    (modern_library *library_in,
     uint64_t n_items, modern **values_in, modern *content_in);
INTERNAL modern *modern_node_make_backreference
    (modern_library *library_in,
     uint64_t index);
INTERNAL modern *modern_node_make_builtin
    (modern_library *library_in,
     uint16_t identifier);

