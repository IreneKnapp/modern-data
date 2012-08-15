struct memory {
	uint64_t retain_count;
	unsigned is_autoreleased : 1;
};


struct modern {
    struct memory memory;
    enum modern_node_type node_type : 6;
    struct modern *value_type;
    union {
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
            struct modern *value;
        } named_value;
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
    } specifics;
};


struct modern_context {
    struct memory memory;
    size_t n_values;
    size_t n_buckets;
    struct {
        struct modern *value;
        unsigned is_deleted : 1;
    } *hash;
};


struct modern_autorelease_pool {
	size_t item_buffer_count;
	size_t item_buffer_capacity;
	struct memory **item_buffer;
};


struct modern_library {
    struct modern_error_handler *error_handler;
    struct modern_allocator *allocator;
    struct modern_context *cache_context;
};
