struct memory {
	uint64_t retain_count;
	unsigned is_autoreleased : 1;
};


struct modern {
    struct memory memory;
    enum modern_node_type node_type : 6;
    struct modern *value_type;
    union {
        struct {
            struct modern *field_value;
            struct modern *successor;
        } sigma_value;
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

