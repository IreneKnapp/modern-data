#include <setjmp.h>
#include <stdarg.h>
#include <string.h>
#include "modern.h"
#include "test.h"


struct allocated_data {
    void *data;
    size_t size;
    char *tag;
};


struct allocated_data_buffer {
    size_t count;
    size_t capacity;
    struct allocated_data **allocated_data;
};


enum callback_identifier {
    library_finalizer_callback_identifier = 1,
    allocator_malloc_callback_identifier,
    allocator_free_callback_identifier,
    allocator_realloc_callback_identifier,
    error_memory_callback_identifier,
    error_type_mismatch_callback_identifier,
    error_universe_level_overflow_callback_identifier,
    error_buffer_index_callback_identifier,
    error_not_applicable_callback_identifier,
    error_non_numeric_float_callback_identifier,
    stream_start_callback_identifier,
    stream_magic_number_callback_identifier,
    stream_name_definition_callback_identifier,
    stream_value_definition_is_next_callback_identifier,
    stream_type_definition_bool_callback_identifier,
    stream_type_definition_ordering_callback_identifier,
    stream_type_definition_maybe_is_next_callback_identifier,
    stream_type_definition_int8_callback_identifier,
    stream_type_definition_int16_callback_identifier,
    stream_type_definition_int32_callback_identifier,
    stream_type_definition_int64_callback_identifier,
    stream_type_definition_nat8_callback_identifier,
    stream_type_definition_nat16_callback_identifier,
    stream_type_definition_nat32_callback_identifier,
    stream_type_definition_nat64_callback_identifier,
    stream_type_definition_float32_callback_identifier,
    stream_type_definition_float64_callback_identifier,
    stream_type_definition_utf8_callback_identifier,
    stream_type_definition_blob_callback_identifier,
    stream_type_definition_function_is_next_callback_identifier,
    stream_type_definition_sigma_is_next_callback_identifier,
    stream_type_definition_named_is_next_callback_identifier,
    stream_type_definition_universe_callback_identifier,
    stream_bool_false_callback_identifier,
    stream_bool_true_callback_identifier,
    stream_ordering_less_callback_identifier,
    stream_ordering_equal_callback_identifier,
    stream_ordering_greater_callback_identifier,
    stream_maybe_nothing_callback_identifier,
    stream_maybe_just_is_next_callback_identifier,
    stream_int8_callback_identifier,
    stream_int16_callback_identifier,
    stream_int32_callback_identifier,
    stream_int64_callback_identifier,
    stream_nat8_callback_identifier,
    stream_nat16_callback_identifier,
    stream_nat32_callback_identifier,
    stream_nat64_callback_identifier,
    stream_float32_callback_identifier,
    stream_float64_callback_identifier,
    stream_utf8_start_callback_identifier,
    stream_utf8_data_callback_identifier,
    stream_utf8_end_callback_identifier,
    stream_blob_start_callback_identifier,
    stream_blob_data_callback_identifier,
    stream_blob_end_callback_identifier,
    stream_sigma_is_next_callback_identifier,
    stream_named_value_is_next_callback_identifier,
    stream_lambda_is_next_callback_identifier,
    stream_apply_is_next_callback_identifier,
    stream_type_family_is_next_callback_identifier,
    stream_let_is_next_callback_identifier,
    stream_backreference_is_next_callback_identifier,
    stream_builtin_is_next_callback_identifier,
    stream_item_from_context_is_next_callback_identifier,
    stream_end_callback_identifier,
    combinator_parallel_callback_identifier,
    combinator_sequential_callback_identifier,
};


struct callback_invocation {
    enum callback_identifier identifier;
    unsigned succeeded : 1;
    union {
        struct {
        } library_finalizer;
        struct {
            size_t size;
            void *result;
        } allocator_malloc;
        struct {
            void *data;
        } allocator_free;
        struct {
            void *data;
            size_t new_size;
            void *result;
        } allocator_realloc;
        struct {
            size_t requested_size;
        } error_memory;
        struct {
            modern *expected;
            modern *actual;
        } error_type_mismatch;
        struct {
        } error_universe_level_overflow;
        struct {
        } error_buffer_index;
        struct {
        } error_not_applicable;
        struct {
        } error_non_numeric_float;
        struct {
            struct stream_state *state;
        } stream_start;
        struct {
            struct stream_state *state;
        } stream_magic_number;
        struct {
            struct stream_state *state;
        } stream_name_definition;
        struct {
            struct stream_state *state;
        } stream_value_definition_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_bool;
        struct {
            struct stream_state *state;
        } stream_type_definition_ordering;
        struct {
            struct stream_state *state;
        } stream_type_definition_maybe_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_int8;
        struct {
            struct stream_state *state;
        } stream_type_definition_int16;
        struct {
            struct stream_state *state;
        } stream_type_definition_int32;
        struct {
            struct stream_state *state;
        } stream_type_definition_int64;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat8;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat16;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat32;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat64;
        struct {
            struct stream_state *state;
        } stream_type_definition_float32;
        struct {
            struct stream_state *state;
        } stream_type_definition_float64;
        struct {
            struct stream_state *state;
        } stream_type_definition_utf8;
        struct {
            struct stream_state *state;
        } stream_type_definition_blob;
        struct {
            struct stream_state *state;
        } stream_type_definition_function_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_sigma_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_named_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_universe;
        struct {
            struct stream_state *state;
        } stream_bool_false;
        struct {
            struct stream_state *state;
        } stream_bool_true;
        struct {
            struct stream_state *state;
        } stream_ordering_less;
        struct {
            struct stream_state *state;
        } stream_ordering_equal;
        struct {
            struct stream_state *state;
        } stream_ordering_greater;
        struct {
            struct stream_state *state;
        } stream_maybe_nothing;
        struct {
            struct stream_state *state;
        } stream_maybe_just_is_next;
        struct {
            struct stream_state *state;
        } stream_int8;
        struct {
            struct stream_state *state;
        } stream_int16;
        struct {
            struct stream_state *state;
        } stream_int32;
        struct {
            struct stream_state *state;
        } stream_int64;
        struct {
            struct stream_state *state;
        } stream_nat8;
        struct {
            struct stream_state *state;
        } stream_nat16;
        struct {
            struct stream_state *state;
        } stream_nat32;
        struct {
            struct stream_state *state;
        } stream_nat64;
        struct {
            struct stream_state *state;
        } stream_float32;
        struct {
            struct stream_state *state;
        } stream_float64;
        struct {
            struct stream_state *state;
        } stream_utf8_start;
        struct {
            struct stream_state *state;
        } stream_utf8_data;
        struct {
            struct stream_state *state;
        } stream_utf8_end;
        struct {
            struct stream_state *state;
        } stream_blob_start;
        struct {
            struct stream_state *state;
        } stream_blob_data;
        struct {
            struct stream_state *state;
        } stream_blob_end;
        struct {
            struct stream_state *state;
        } stream_sigma_is_next;
        struct {
            struct stream_state *state;
        } stream_named_value_is_next;
        struct {
            struct stream_state *state;
        } stream_lambda_is_next;
        struct {
            struct stream_state *state;
        } stream_apply_is_next;
        struct {
            struct stream_state *state;
        } stream_type_family_is_next;
        struct {
            struct stream_state *state;
        } stream_let_is_next;
        struct {
            struct stream_state *state;
        } stream_backreference_is_next;
        struct {
            struct stream_state *state;
        } stream_builtin_is_next;
        struct {
            struct stream_state *state;
        } stream_item_from_context_is_next;
        struct {
            struct stream_state *state;
        } stream_end;
    } specifics;
};


struct callback_invocation_buffer {
    size_t count;
    size_t capacity;
    struct callback_invocation **callback_invocations;
};


union callback_behavior {
    struct {
    } library_finalizer;
    struct {
        char *tag;
    } allocator_malloc;
    struct {
        char *tag;
    } allocator_free;
    struct {
        char *tag;
    } allocator_realloc;
    struct {
    } error_memory;
    struct {
    } error_type_mismatch;
    struct {
    } error_universe_level_overflow;
    struct {
    } error_buffer_index;
    struct {
    } error_not_applicable;
    struct {
    } error_non_numeric_float;
    struct {
        unsigned fail : 1;
    } stream_start;
    struct {
        unsigned fail : 1;
    } stream_magic_number;
    struct {
        unsigned fail : 1;
    } stream_name_definition;
    struct {
        unsigned fail : 1;
    } stream_value_definition_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_definition_bool;
    struct {
        unsigned fail : 1;
    } stream_type_definition_ordering;
    struct {
        unsigned fail : 1;
    } stream_type_definition_maybe_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_definition_int8;
    struct {
        unsigned fail : 1;
    } stream_type_definition_int16;
    struct {
        unsigned fail : 1;
    } stream_type_definition_int32;
    struct {
        unsigned fail : 1;
    } stream_type_definition_int64;
    struct {
        unsigned fail : 1;
    } stream_type_definition_nat8;
    struct {
        unsigned fail : 1;
    } stream_type_definition_nat16;
    struct {
        unsigned fail : 1;
    } stream_type_definition_nat32;
    struct {
        unsigned fail : 1;
    } stream_type_definition_nat64;
    struct {
        unsigned fail : 1;
    } stream_type_definition_float32;
    struct {
        unsigned fail : 1;
    } stream_type_definition_float64;
    struct {
        unsigned fail : 1;
    } stream_type_definition_utf8;
    struct {
        unsigned fail : 1;
    } stream_type_definition_blob;
    struct {
        unsigned fail : 1;
    } stream_type_definition_function_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_definition_sigma_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_definition_named_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_definition_universe;
    struct {
        unsigned fail : 1;
    } stream_bool_false;
    struct {
        unsigned fail : 1;
    } stream_bool_true;
    struct {
        unsigned fail : 1;
    } stream_ordering_less;
    struct {
        unsigned fail : 1;
    } stream_ordering_equal;
    struct {
        unsigned fail : 1;
    } stream_ordering_greater;
    struct {
        unsigned fail : 1;
    } stream_maybe_nothing;
    struct {
        unsigned fail : 1;
    } stream_maybe_just_is_next;
    struct {
        unsigned fail : 1;
    } stream_int8;
    struct {
        unsigned fail : 1;
    } stream_int16;
    struct {
        unsigned fail : 1;
    } stream_int32;
    struct {
        unsigned fail : 1;
    } stream_int64;
    struct {
        unsigned fail : 1;
    } stream_nat8;
    struct {
        unsigned fail : 1;
    } stream_nat16;
    struct {
        unsigned fail : 1;
    } stream_nat32;
    struct {
        unsigned fail : 1;
    } stream_nat64;
    struct {
        unsigned fail : 1;
    } stream_float32;
    struct {
        unsigned fail : 1;
    } stream_float64;
    struct {
        unsigned fail : 1;
    } stream_utf8_start;
    struct {
        unsigned fail : 1;
    } stream_utf8_data;
    struct {
        unsigned fail : 1;
    } stream_utf8_end;
    struct {
        unsigned fail : 1;
    } stream_blob_start;
    struct {
        unsigned fail : 1;
    } stream_blob_data;
    struct {
        unsigned fail : 1;
    } stream_blob_end;
    struct {
        unsigned fail : 1;
    } stream_sigma_is_next;
    struct {
        unsigned fail : 1;
    } stream_named_value_is_next;
    struct {
        unsigned fail : 1;
    } stream_lambda_is_next;
    struct {
        unsigned fail : 1;
    } stream_apply_is_next;
    struct {
        unsigned fail : 1;
    } stream_type_family_is_next;
    struct {
        unsigned fail : 1;
    } stream_let_is_next;
    struct {
        unsigned fail : 1;
    } stream_backreference_is_next;
    struct {
        unsigned fail : 1;
    } stream_builtin_is_next;
    struct {
        unsigned fail : 1;
    } stream_item_from_context_is_next;
    struct {
        unsigned fail : 1;
    } stream_end;
};


struct callback_invocation_pattern_buffer {
    size_t count;
    size_t capacity;
    struct callback_invocation_pattern **patterns;
};


struct callback_invocation_pattern {
    enum callback_identifier identifier;
    unsigned parameters_relevant : 1;
    unsigned should_succeed : 1;
    unsigned sticky : 1;
    union {
        struct {
        } library_finalizer;
        struct {
            size_t size;
        } allocator_malloc;
        struct {
            void *data;
            char *tag;
        } allocator_free;
        struct {
            void *data;
            size_t new_size;
            char *tag;
        } allocator_realloc;
        struct {
            size_t requested_size;
        } error_memory;
        struct {
            modern *expected;
            modern *actual;
        } error_type_mismatch;
        struct {
        } error_universe_level_overflow;
        struct {
        } error_buffer_index;
        struct {
        } error_not_applicable;
        struct {
        } error_non_numeric_float;
        struct {
            struct stream_state *state;
        } stream_start;
        struct {
            struct stream_state *state;
        } stream_magic_number;
        struct {
            struct stream_state *state;
        } stream_name_definition;
        struct {
            struct stream_state *state;
        } stream_value_definition_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_bool;
        struct {
            struct stream_state *state;
        } stream_type_definition_ordering;
        struct {
            struct stream_state *state;
        } stream_type_definition_maybe_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_int8;
        struct {
            struct stream_state *state;
        } stream_type_definition_int16;
        struct {
            struct stream_state *state;
        } stream_type_definition_int32;
        struct {
            struct stream_state *state;
        } stream_type_definition_int64;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat8;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat16;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat32;
        struct {
            struct stream_state *state;
        } stream_type_definition_nat64;
        struct {
            struct stream_state *state;
        } stream_type_definition_float32;
        struct {
            struct stream_state *state;
        } stream_type_definition_float64;
        struct {
            struct stream_state *state;
        } stream_type_definition_utf8;
        struct {
            struct stream_state *state;
        } stream_type_definition_blob;
        struct {
            struct stream_state *state;
        } stream_type_definition_function_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_sigma_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_named_is_next;
        struct {
            struct stream_state *state;
        } stream_type_definition_universe;
        struct {
            struct stream_state *state;
        } stream_bool_false;
        struct {
            struct stream_state *state;
        } stream_bool_true;
        struct {
            struct stream_state *state;
        } stream_ordering_less;
        struct {
            struct stream_state *state;
        } stream_ordering_equal;
        struct {
            struct stream_state *state;
        } stream_ordering_greater;
        struct {
            struct stream_state *state;
        } stream_maybe_nothing;
        struct {
            struct stream_state *state;
        } stream_maybe_just_is_next;
        struct {
            struct stream_state *state;
        } stream_int8;
        struct {
            struct stream_state *state;
        } stream_int16;
        struct {
            struct stream_state *state;
        } stream_int32;
        struct {
            struct stream_state *state;
        } stream_int64;
        struct {
            struct stream_state *state;
        } stream_nat8;
        struct {
            struct stream_state *state;
        } stream_nat16;
        struct {
            struct stream_state *state;
        } stream_nat32;
        struct {
            struct stream_state *state;
        } stream_nat64;
        struct {
            struct stream_state *state;
        } stream_float32;
        struct {
            struct stream_state *state;
        } stream_float64;
        struct {
            struct stream_state *state;
        } stream_utf8_start;
        struct {
            struct stream_state *state;
        } stream_utf8_data;
        struct {
            struct stream_state *state;
        } stream_utf8_end;
        struct {
            struct stream_state *state;
        } stream_blob_start;
        struct {
            struct stream_state *state;
        } stream_blob_data;
        struct {
            struct stream_state *state;
        } stream_blob_end;
        struct {
            struct stream_state *state;
        } stream_sigma_is_next;
        struct {
            struct stream_state *state;
        } stream_named_value_is_next;
        struct {
            struct stream_state *state;
        } stream_lambda_is_next;
        struct {
            struct stream_state *state;
        } stream_apply_is_next;
        struct {
            struct stream_state *state;
        } stream_type_family_is_next;
        struct {
            struct stream_state *state;
        } stream_let_is_next;
        struct {
            struct stream_state *state;
        } stream_backreference_is_next;
        struct {
            struct stream_state *state;
        } stream_builtin_is_next;
        struct {
            struct stream_state *state;
        } stream_item_from_context_is_next;
        struct {
            struct stream_state *state;
        } stream_end;
        struct {
            struct callback_invocation_pattern_buffer children;
        } combinator_parallel;
        struct {
            struct callback_invocation_pattern_buffer children;
        } combinator_sequential;
    } specifics;
    union callback_behavior behavior;
};


struct test_case {
    char *name;
    unsigned completed : 1;
    unsigned succeeded : 1;
    jmp_buf jmp_buf;
    struct callback_invocation_buffer actual_callbacks;
    struct callback_invocation_pattern_buffer normal_callback_pattern;
    struct allocated_data_buffer allocations;
};


struct test_case_buffer {
    size_t count;
    size_t capacity;
    struct test_case **test_cases;
};


struct fixtures {
    unsigned prepared : 1;
    unsigned succeeded : 1;
    jmp_buf jmp_buf;
    struct callback_invocation_buffer actual_callbacks;
    struct allocated_data_buffer allocations;
};


struct test_suite {
    struct test_case *current_test_case;
    struct fixtures *current_fixtures;
    struct callback_invocation *current_callback;
    struct callback_invocation_pattern *allocation_invocation;
    struct callback_invocation_pattern *deallocation_invocation;
    struct callback_invocation_pattern *error_invocation;
    jmp_buf jmp_buf;
    struct test_case_buffer test_cases;
    struct fixtures fixtures;
    struct callback_invocation_buffer actual_callbacks;
    struct callback_invocation_pattern *expected_callbacks;
    struct allocated_data_buffer allocations;
    unsigned output_on_header_line : 1;
};


struct stream_state {
    struct test_suite *test_suite;
};


static void *actually_malloc(size_t size);
static void actually_free(void *data);
static void *actually_realloc(void *data, size_t new_size);

static void initialize_allocated_data
  (struct allocated_data *allocated_data);
static void finalize_allocated_data
  (struct allocated_data *allocated_data);

static void initialize_allocated_data_buffer
  (struct allocated_data_buffer *buffer);
static void finalize_allocated_data_buffer
  (struct allocated_data_buffer *buffer);
static struct allocated_data *make_allocated_data_in_buffer
  (struct allocated_data_buffer *buffer);
static void remove_allocated_data_from_buffer
  (struct allocated_data_buffer *buffer,
   struct allocated_data *allocation);

static void initialize_test_suite
  (struct test_suite *test_suite);
static void finalize_test_suite
  (struct test_suite *test_suite);

static void initialize_test_case
  (struct test_case *test_case, char *name);
static void finalize_test_case
  (struct test_case *test_case);

static void initialize_test_case_buffer
  (struct test_case_buffer *buffer);
static void finalize_test_case_buffer
  (struct test_case_buffer *buffer);
static struct test_case *make_test_case_in_buffer
  (struct test_case_buffer *buffer, char *name);

static void initialize_fixtures
  (struct fixtures *fixtures);
static void finalize_fixtures
  (struct fixtures *fixtures);

static void initialize_callback_invocation
  (struct callback_invocation *callback_invocation);
static void finalize_callback_invocation
  (struct callback_invocation *callback_invocation);

static void initialize_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer);
static void finalize_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer);
static struct callback_invocation *make_callback_invocation_in_buffer
  (struct callback_invocation_buffer *buffer);
static void move_callback_invocation_pattern_into_buffer
  (struct callback_invocation_pattern_buffer *buffer,
   struct callback_invocation_pattern *pattern);
static struct callback_invocation_pattern_buffer
  *get_buffer_for_parallel_callback_invocation_pattern
  (struct test_suite *test_suite);

static void initialize_callback_invocation_pattern
  (struct callback_invocation_pattern *pattern);
static void finalize_callback_invocation_pattern
  (struct callback_invocation_pattern *patern);

static void initialize_callback_invocation_pattern_buffer
  (struct callback_invocation_pattern_buffer *buffer);
static void finalize_callback_invocation_pattern_buffer
  (struct callback_invocation_pattern_buffer *buffer);
static struct callback_invocation_pattern
  *make_callback_invocation_pattern_in_buffer
  (struct callback_invocation_pattern_buffer *buffer);
static void remove_callback_invocation_pattern_from_buffer
  (struct callback_invocation_pattern_buffer *buffer,
   struct callback_invocation_pattern *allocation);

static void copy_callback_behavior
  (enum callback_identifier identifier,
   union callback_behavior *destination,
   union callback_behavior *source);

static void print_test_case
  (struct test_case *test_case);

static void print_test_case_buffer
  (struct test_case_buffer *buffer);

static void print_callback_invocation
  (struct callback_invocation *callback_invocation);

static void print_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer);

static struct callback_invocation *begin_callback
  (struct test_suite *test_suite);

static int match_callback_invocation_against_pattern
  (struct test_suite *test_suite,
   struct callback_invocation *invocation,
   struct callback_invocation_pattern *pattern,
   struct callback_invocation_pattern_buffer **buffer_result,
   struct callback_invocation_pattern **pattern_result,
   union callback_behavior *behavior);
static int match_callback_invocation_against_pattern_helper
  (struct test_suite *test_suite,
   struct callback_invocation *invocation,
   struct callback_invocation_pattern *pattern,
   union callback_behavior *behavior);

static int callback_should_succeed
  (struct test_suite *test_suite, union callback_behavior *behavior);

static void fail
  (struct test_suite *test_suite);

static void check_for_memory_leaks
  (struct test_suite *test_suite);

static void library_finalizer
  (struct test_suite *test_suite);

static void *allocator_malloc
  (struct test_suite *test_suite, size_t size);
static void allocator_free
  (struct test_suite *test_suite, void *data);
static void *allocator_realloc
  (struct test_suite *test_suite, void *data, size_t size);

static void error_memory
  (struct test_suite *test_suite, size_t requested_size);
static void error_type_mismatch
  (struct test_suite *test_suite, modern *expected, modern *actual);
static void error_universe_level_overflow
  (struct test_suite *test_suite);
static void error_buffer_index
  (struct test_suite *test_suite);
static void error_not_applicable
  (struct test_suite *test_suite);
static void error_non_numeric_float
  (struct test_suite *test_suite);

static void stream_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_magic_number
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_name_definition
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_value_definition_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_bool
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_ordering
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_maybe_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int16
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_int64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_float32
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_float64
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_utf8
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_blob
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_function_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_definition_named_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash name);
static void stream_type_definition_universe
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_bool_false
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_bool_true
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_less
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_equal
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_ordering_greater
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_maybe_nothing
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_maybe_just_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_int8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int8_t value);
static void stream_int16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int16_t value);
static void stream_int32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int32_t value);
static void stream_int64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int64_t value);
static void stream_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t value);
static void stream_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint16_t value);
static void stream_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint32_t value);
static void stream_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint64_t value);
static void stream_float32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   float value);
static void stream_float64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   double value);
static void stream_utf8_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_utf8_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_utf8_end
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_blob_start
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_blob_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length);
static void stream_blob_end
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash *type);
static void stream_named_value_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash name);
static void stream_lambda_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_apply_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state);
static void stream_type_family_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items);
static void stream_let_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items);
static void stream_backreference_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t index);
static void stream_builtin_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint16_t identifier);
static void stream_item_from_context_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash type);
static void stream_end
  (struct modern_process *process, void *process_state,
   void *stream_state);


int main(int argc, char **argv) {
    struct modern_error_handler error_handler;
    error_handler.memory =
        (void (*)(void *, size_t)) error_memory;
    error_handler.type_mismatch =
        (void (*)(void *, modern *, modern *)) error_type_mismatch;
    error_handler.universe_level_overflow =
        (void (*)(void *)) error_universe_level_overflow;
    error_handler.buffer_index =
        (void (*)(void *)) error_buffer_index;
    error_handler.not_applicable =
        (void (*)(void *)) error_not_applicable;
    error_handler.non_numeric_float =
        (void (*)(void *)) error_non_numeric_float;
    
    struct modern_allocator allocator;
    allocator.alloc =
        (void *(*)(void *, size_t)) allocator_malloc;
    allocator.free =
        (void (*)(void *, void *)) allocator_free;
    allocator.realloc =
        (void *(*)(void *, void *, size_t)) allocator_realloc;
    
    struct test_suite *test_suite =
        actually_malloc(sizeof(struct test_suite));
    initialize_test_suite(test_suite);
    
    if(!setjmp(test_suite->jmp_buf)) {
        reset_allowances(test_suite);
        
        allow_allocation(test_suite,
            "test-main.c default node representation");
        struct modern_node_representation *node_representation =
            modern_node_representation_default_make(&allocator, test_suite);
        disallow_allocation(test_suite);
        if(!node_representation) {
            printf("\n\n"
                   "*** The testing infrastructure itself failed.\n"
                   "*** Specifically, couldn't get the default node\n"
                   "*** representation.\n");
                exit(1);
        }
        
        allow_allocation(test_suite, "test-main.c library");
        modern_library *library = modern_library_initialize
            (&error_handler,
             &allocator,
             node_representation,
             (void (*)(void *)) library_finalizer,
             test_suite);
        disallow_allocation(test_suite);
        
        if(!setjmp(test_suite->jmp_buf)) {
            test_main(test_suite, library);
            
            if(test_suite->current_test_case) {
                printf("\n\n"
                       "*** The testing infrastructure itself failed.\n"
                       "*** Specifically, missing an end_test_case().\n");
                exit(1);
            }
            
            if(test_suite->current_fixtures) {
                printf("\n\n"
                       "*** The testing infrastructure itself failed.\n"
                       "*** Specifically, missing an end_fixtures().\n");
                exit(1);
            }
            
            if(!setjmp(test_suite->jmp_buf)) {
                reset_allowances(test_suite);
                
                allow_deallocation(test_suite, "test-main.c library");
                modern_library_finalize(library);
                disallow_deallocation(test_suite);
                
                allow_deallocation(test_suite,
                    "test-main.c default node representation");
                modern_node_representation_default_finalize
                    (&allocator, test_suite, node_representation);
                disallow_deallocation(test_suite);
                
                check_for_memory_leaks(test_suite);
            } else {
                printf("\n\n"
                       "*** Unexpected behavior during library finalize.\n");
                exit(1);
            }
        } else {
            printf("\n\n"
                   "*** The testing infrastructure itself failed.\n"
                   "*** Specifically, there were Modern Data calls not "
                   "bracketed by\n"
                   "*** begin_*() / end_*().\n");
            exit(1);
        }
    } else {
        printf("\n\n"
               "*** Unexpected behavior during library initialize.\n"
               "*** That means the remainder of the test suite "
               "can't be run.\n");
        exit(1);
    }
    
    size_t pass_count = 0;
    size_t fail_count = 0;
    size_t skip_count = 0;
    for(size_t i = 0; i < test_suite->test_cases.count; i++) {
        struct test_case *test_case = test_suite->test_cases.test_cases[i];
        if(test_case->completed) {
            if(test_case->succeeded) pass_count++;
            else fail_count++;
        } else skip_count++;
    }
    
    printf("Passed %llu.  Failed %llu.  Skipped %llu.\n",
           (unsigned long long) pass_count,
           (unsigned long long) fail_count,
           (unsigned long long) skip_count);
    
    finalize_test_suite(test_suite);
    actually_free(test_suite);
    
    return 0;
}


void test_message
  (test_suite *test_suite_in,
   char *format, ...)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->output_on_header_line) {
        printf("\n");
        test_suite->output_on_header_line = 0;
    }
    
    va_list ap;
    va_start(ap, format);
    printf("  ");
    vprintf(format, ap);
    printf("\n");
    va_end(ap);
}


int begin_fixtures(test_suite *test_suite_in) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_fixtures() "
               "while already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->current_fixtures) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_fixtures() "
               "while already preparing fixtures.\n");
        exit(1);
    }
    
    if(test_suite->fixtures.prepared) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_fixtures() "
               "after already preparing fixtures.\n");
        exit(1);
    }
    
    test_suite->current_fixtures = &test_suite->fixtures;
    
    if(!setjmp(test_suite->current_fixtures->jmp_buf)) {
        return 1;
    } else {
        return 0;
    }
}


void end_fixtures(test_suite *test_suite_in) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_fixtures) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to end_fixtures() "
               "while not preparing fixtures.\n");
        exit(1);
    }
    
    test_suite->current_fixtures->prepared = 1;
    
    test_suite->current_fixtures = NULL;
}


void begin_test_case
  (test_suite *test_suite_in,
   int (*test_case)(void *test_context),
   void *test_context,
   char *format, ...)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_test_case() "
               "while already in one.\n");
        exit(1);
    }
    
    if(test_suite->current_fixtures) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_test_case() "
               "while already preparing fixtures.\n");
        exit(1);
    }
    
    va_list ap;
    
    va_start(ap, format);
    int name_size = vsnprintf(NULL, 0, format, ap) + 1;
    va_end(ap);
    
    char *name = actually_malloc(name_size);
    va_start(ap, format);
    vsnprintf(name, name_size, format, ap);
    va_end(ap);
    
    test_suite->current_test_case =
        make_test_case_in_buffer(&test_suite->test_cases, name);
    
    printf("%s...", name);
    test_suite->output_on_header_line = 1;
    
    actually_free(name);
    
    if(!test_suite->fixtures.succeeded) {
        test_suite->current_test_case = NULL;
        printf("  SKIP\n");
        test_suite->output_on_header_line = 0;
    } else {
        int jmp_result = setjmp(test_suite->current_test_case->jmp_buf);
        
        if(!jmp_result) {
            reset_allowances(test_suite);
            int succeeded = test_case(test_context);
            
            test_suite->current_test_case->completed = 1;
            test_suite->current_test_case->succeeded = succeeded;
            
            check_for_memory_leaks(test_suite);
        } else if(jmp_result == 2) {
            test_suite->current_test_case->completed = 1;
            test_suite->current_test_case->succeeded = 1;
            
            check_for_memory_leaks(test_suite);
        } else {
            test_suite->current_test_case->completed = 1;
            test_suite->current_test_case->succeeded = 0;
        }
        
        if(test_suite->current_test_case->succeeded) {
            printf("  PASS\n");
        } else {
            printf("  FAIL\n");
        }
        test_suite->output_on_header_line = 0;
        
        test_suite->current_test_case = NULL;
    }
}


void reset_allowances(test_suite *test_suite) {
    disallow_allocation(test_suite);
    disallow_deallocation(test_suite);
}


void allow_allocation(test_suite *test_suite_in, char *tag_format, ...) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->allocation_invocation) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to allow allocation when it is\n"
               "*** already allowed.\n");
        exit(1);
    }
    
    va_list ap;
    va_start(ap, tag_format);
    int tag_length = vsnprintf(NULL, 0, tag_format, ap);
    va_end(ap);
    char *tag = malloc(tag_length + 1);
    va_start(ap, tag_format);
    vsnprintf(tag, tag_length + 1, tag_format, ap);
    va_end(ap);
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = allocator_malloc_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 1;
    invocation->behavior.allocator_malloc.tag = tag;
    
    test_suite->allocation_invocation = invocation;
}


void disallow_allocation(test_suite *test_suite_in) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->allocation_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        test_suite->allocation_invocation;
    remove_callback_invocation_pattern_from_buffer(buffer, invocation);
    finalize_callback_invocation_pattern(invocation);
    actually_free(invocation);
    
    test_suite->allocation_invocation = NULL;
}


void allow_deallocation(test_suite *test_suite_in, char *tag_format, ...) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->deallocation_invocation) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to allow deallocation when it is\n"
               "*** already allowed.\n");
        exit(1);
    }

    va_list ap;
    va_start(ap, tag_format);
    int tag_length = vsnprintf(NULL, 0, tag_format, ap);
    va_end(ap);
    char *tag = malloc(tag_length + 1);
    va_start(ap, tag_format);
    vsnprintf(tag, tag_length + 1, tag_format, ap);
    va_end(ap);
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = allocator_free_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 1;
    invocation->specifics.allocator_free.tag = tag;
    
    test_suite->deallocation_invocation = invocation;
}


void disallow_deallocation(test_suite *test_suite_in) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->deallocation_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        test_suite->deallocation_invocation;
    remove_callback_invocation_pattern_from_buffer(buffer, invocation);
    finalize_callback_invocation_pattern(invocation);
    actually_free(invocation);
    
    test_suite->deallocation_invocation = NULL;
}


void expect_error_memory
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_memory_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


void expect_error_type_mismatch
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_type_mismatch_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


void expect_error_universe_level_overflow
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_universe_level_overflow_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


void expect_error_buffer_index
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_buffer_index_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


void expect_error_not_applicable
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_not_applicable_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


void expect_error_non_numeric_float
  (test_suite *test_suite_in,
   int (*test_case_helper)(void *test_context),
   void *test_context)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to expect an error "
               "while not already in a test case.\n");
        exit(1);
    }
    
    if(test_suite->error_invocation) return;
    
    struct callback_invocation_pattern_buffer *buffer =
        get_buffer_for_parallel_callback_invocation_pattern(test_suite);
    
    struct callback_invocation_pattern *invocation =
        make_callback_invocation_pattern_in_buffer(buffer);
    invocation->identifier = error_non_numeric_float_callback_identifier;
    invocation->parameters_relevant = 0;
    invocation->should_succeed = 1;
    invocation->sticky = 0;
    
    test_suite->error_invocation = invocation;
    
    int succeeded = test_case_helper(test_context);
    
    if(succeeded) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    }
}


static void *actually_malloc(size_t size) {
    void *result = malloc(size);
    if(!result) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, malloc(%llu) returned NULL.\n",
               (unsigned long long) size);
        exit(1);
    }
    return result;
}


static void actually_free(void *data) {
    free(data);
}


static void *actually_realloc(void *data, size_t new_size) {
    void *result = realloc(data, new_size);
    if(!result) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, realloc(..., %llu) returned NULL.\n",
               (unsigned long long) new_size);
        exit(1);
    }
    return result;
}


static void initialize_allocated_data
  (struct allocated_data *allocated_data)
{
    allocated_data->data = NULL;
    allocated_data->size = 0;
    allocated_data->tag = NULL;
}


static void finalize_allocated_data
  (struct allocated_data *allocated_data)
{
    if(allocated_data->data) {
        actually_free(allocated_data->data);
        allocated_data->data = NULL;            
    }

    if(allocated_data->tag) {
        actually_free(allocated_data->tag);
        allocated_data->tag = NULL;
    }
}


static void initialize_allocated_data_buffer
  (struct allocated_data_buffer *buffer)
{
    buffer->count = 0;
    buffer->capacity = 4;
    buffer->allocated_data = actually_malloc
        (sizeof(struct allocated_data *) * buffer->capacity);
}


static void finalize_allocated_data_buffer
  (struct allocated_data_buffer *buffer)
{
    for(size_t i = 0; i < buffer->count; i++) {
        finalize_allocated_data(buffer->allocated_data[i]);
        actually_free(buffer->allocated_data[i]);
    }
    actually_free(buffer->allocated_data);
}


static struct allocated_data *make_allocated_data_in_buffer
  (struct allocated_data_buffer *buffer)
{
    size_t original_capacity = buffer->capacity;
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
    }
    if(buffer->capacity != original_capacity) {
        buffer->allocated_data = actually_realloc
            (buffer->allocated_data,
             sizeof(struct allocated_data *) * buffer->capacity);
    }
    
    struct allocated_data *result =
        actually_malloc(sizeof(struct allocated_data));
    buffer->allocated_data[buffer->count] = result;
    
    buffer->count++;
    
    initialize_allocated_data(result);
    
    return result;
}


static void remove_allocated_data_from_buffer
  (struct allocated_data_buffer *buffer,
   struct allocated_data *allocation)
{
    size_t i;
    for(i = 0; i < buffer->count; i++) {
        if(buffer->allocated_data[i] == allocation) break;
    }
    if(i < buffer->count) {
        memmove(buffer->allocated_data + i,
                buffer->allocated_data + i + 1,
                sizeof(struct allocated_data *)
                * (buffer->count - (i + 1)));
        buffer->count--;
    }
}


static void initialize_test_suite
  (struct test_suite *test_suite)
{
    test_suite->current_test_case = NULL;
    test_suite->allocation_invocation = NULL;
    test_suite->deallocation_invocation = NULL;
    test_suite->error_invocation = NULL;
    initialize_test_case_buffer(&test_suite->test_cases);
    initialize_fixtures(&test_suite->fixtures);
    initialize_callback_invocation_buffer(&test_suite->actual_callbacks);
    test_suite->expected_callbacks = NULL;
    initialize_allocated_data_buffer(&test_suite->allocations);
    test_suite->output_on_header_line = 0;
}


static void finalize_test_suite
  (struct test_suite *test_suite)
{
    finalize_test_case_buffer(&test_suite->test_cases);
    finalize_fixtures(&test_suite->fixtures);
    finalize_callback_invocation_buffer(&test_suite->actual_callbacks);
    if(test_suite->expected_callbacks) {
        finalize_callback_invocation_pattern(test_suite->expected_callbacks);
    }
    finalize_allocated_data_buffer(&test_suite->allocations);
}


static void initialize_test_case
  (struct test_case *test_case, char *name)
{
    size_t name_size = strlen(name) + 1;
    test_case->name = actually_malloc(name_size + 1);
    memcpy(test_case->name, name, name_size);
    test_case->completed = 0;
    test_case->succeeded = 1;
    initialize_callback_invocation_buffer(&test_case->actual_callbacks);
    initialize_callback_invocation_pattern_buffer
        (&test_case->normal_callback_pattern);
    initialize_allocated_data_buffer(&test_case->allocations);
}


static void finalize_test_case
  (struct test_case *test_case)
{
    actually_free(test_case->name);
    finalize_callback_invocation_buffer(&test_case->actual_callbacks);
    finalize_callback_invocation_pattern_buffer
        (&test_case->normal_callback_pattern);
    finalize_allocated_data_buffer(&test_case->allocations);
}


static void initialize_test_case_buffer
  (struct test_case_buffer *buffer)
{
    buffer->count = 0;
    buffer->capacity = 128;
    buffer->test_cases = actually_malloc
        (sizeof(struct test_case *) * buffer->capacity);
}


static void finalize_test_case_buffer
  (struct test_case_buffer *buffer)
{
    for(size_t i = 0; i < buffer->count; i++) {
        finalize_test_case(buffer->test_cases[i]);
        actually_free(buffer->test_cases[i]);
    }
    actually_free(buffer->test_cases);
}


static struct test_case *make_test_case_in_buffer
  (struct test_case_buffer *buffer, char *name)
{
    size_t original_capacity = buffer->capacity;
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
    }
    if(buffer->capacity != original_capacity) {
        buffer->test_cases = actually_realloc
            (buffer->test_cases,
             sizeof(struct test_case *) * buffer->capacity);
    }
    
    struct test_case *result = actually_malloc(sizeof(struct test_case));
    buffer->test_cases[buffer->count] = result;
    
    buffer->count++;
    
    initialize_test_case(result, name);
        
    return result;
}


static void initialize_fixtures
  (struct fixtures *fixtures)
{
    fixtures->prepared = 0;
    fixtures->succeeded = 1;
    initialize_callback_invocation_buffer(&fixtures->actual_callbacks);
    initialize_allocated_data_buffer(&fixtures->allocations);
}


static void finalize_fixtures
  (struct fixtures *fixtures)
{
    finalize_callback_invocation_buffer(&fixtures->actual_callbacks);
    finalize_allocated_data_buffer(&fixtures->allocations);
}


static void initialize_callback_invocation
  (struct callback_invocation *callback_invocation)
{
    callback_invocation->identifier = 0;
    callback_invocation->succeeded = 1;
}


static void finalize_callback_invocation
  (struct callback_invocation *callback_invocation)
{
}


static void initialize_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer)
{
    buffer->count = 0;
    buffer->capacity = 8;
    buffer->callback_invocations = actually_malloc
        (sizeof(struct callback_invocation *) * buffer->capacity);
}


static void finalize_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer)
{
    for(size_t i = 0; i < buffer->count; i++) {
        finalize_callback_invocation(buffer->callback_invocations[i]);
        actually_free(buffer->callback_invocations[i]);
    }
    actually_free(buffer->callback_invocations);
}


static struct callback_invocation *make_callback_invocation_in_buffer
  (struct callback_invocation_buffer *buffer)
{
    size_t original_capacity = buffer->capacity;
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
    }
    if(buffer->capacity != original_capacity) {
        buffer->callback_invocations = actually_realloc
            (buffer->callback_invocations,
             sizeof(struct callback_invocation *) * buffer->capacity);
    }
    
    struct callback_invocation *result =
        actually_malloc(sizeof(struct callback_invocation));
    buffer->callback_invocations[buffer->count] = result;
    
    buffer->count++;
    
    initialize_callback_invocation(result);
    
    return result;
}


static void initialize_callback_invocation_pattern
  (struct callback_invocation_pattern *pattern)
{
    pattern->identifier = 0;
    pattern->parameters_relevant = 0;
    pattern->should_succeed = 1;
    pattern->sticky = 0;
}


static void finalize_callback_invocation_pattern
  (struct callback_invocation_pattern *pattern)
{
    switch(pattern->identifier) {
    case library_finalizer_callback_identifier:
        break;
    
    case allocator_malloc_callback_identifier:
        break;
    
    case allocator_free_callback_identifier:
        if(pattern->specifics.allocator_free.tag) {
            actually_free(pattern->specifics.allocator_free.tag);
            pattern->specifics.allocator_free.tag = NULL;
        }
        break;
    
    case allocator_realloc_callback_identifier:
        if(pattern->specifics.allocator_realloc.tag) {
            actually_free(pattern->specifics.allocator_realloc.tag);
            pattern->specifics.allocator_realloc.tag = NULL;
        }
        break;
    
    case error_memory_callback_identifier:
        break;
    
    case error_type_mismatch_callback_identifier:
        break;
    
    case error_universe_level_overflow_callback_identifier:
        break;
    
    case error_buffer_index_callback_identifier:
        break;
    
    case error_not_applicable_callback_identifier:
        break;
    
    case error_non_numeric_float_callback_identifier:
        break;
    
    case stream_start_callback_identifier:
        break;
    
    case stream_magic_number_callback_identifier:
        break;
    
    case stream_name_definition_callback_identifier:
        break;
    
    case stream_value_definition_is_next_callback_identifier:
        break;
    
    case stream_type_definition_bool_callback_identifier:
        break;
    
    case stream_type_definition_ordering_callback_identifier:
        break;
    
    case stream_type_definition_maybe_is_next_callback_identifier:
        break;
    
    case stream_type_definition_int8_callback_identifier:
        break;
    
    case stream_type_definition_int16_callback_identifier:
        break;
    
    case stream_type_definition_int32_callback_identifier:
        break;
    
    case stream_type_definition_int64_callback_identifier:
        break;
    
    case stream_type_definition_nat8_callback_identifier:
        break;
    
    case stream_type_definition_nat16_callback_identifier:
        break;
    
    case stream_type_definition_nat32_callback_identifier:
        break;
    
    case stream_type_definition_nat64_callback_identifier:
        break;
    
    case stream_type_definition_float32_callback_identifier:
        break;
    
    case stream_type_definition_float64_callback_identifier:
        break;
    
    case stream_type_definition_utf8_callback_identifier:
        break;
    
    case stream_type_definition_blob_callback_identifier:
        break;
    
    case stream_type_definition_function_is_next_callback_identifier:
        break;
    
    case stream_type_definition_sigma_is_next_callback_identifier:
        break;
    
    case stream_type_definition_named_is_next_callback_identifier:
        break;
    
    case stream_type_definition_universe_callback_identifier:
        break;
    
    case stream_bool_false_callback_identifier:
        break;
    
    case stream_bool_true_callback_identifier:
        break;
    
    case stream_ordering_less_callback_identifier:
        break;
    
    case stream_ordering_equal_callback_identifier:
        break;
    
    case stream_ordering_greater_callback_identifier:
        break;
    
    case stream_maybe_nothing_callback_identifier:
        break;
    
    case stream_maybe_just_is_next_callback_identifier:
        break;
    
    case stream_int8_callback_identifier:
        break;
    
    case stream_int16_callback_identifier:
        break;
    
    case stream_int32_callback_identifier:
        break;
    
    case stream_int64_callback_identifier:
        break;
    
    case stream_nat8_callback_identifier:
        break;
    
    case stream_nat16_callback_identifier:
        break;
    
    case stream_nat32_callback_identifier:
        break;
    
    case stream_nat64_callback_identifier:
        break;
    
    case stream_float32_callback_identifier:
        break;
    
    case stream_float64_callback_identifier:
        break;
    
    case stream_utf8_start_callback_identifier:
        break;
    
    case stream_utf8_data_callback_identifier:
        break;
    
    case stream_utf8_end_callback_identifier:
        break;
    
    case stream_blob_start_callback_identifier:
        break;
    
    case stream_blob_data_callback_identifier:
        break;
    
    case stream_blob_end_callback_identifier:
        break;
    
    case stream_sigma_is_next_callback_identifier:
        break;
    
    case stream_named_value_is_next_callback_identifier:
        break;
    
    case stream_lambda_is_next_callback_identifier:
        break;
    
    case stream_apply_is_next_callback_identifier:
        break;
    
    case stream_type_family_is_next_callback_identifier:
        break;
    
    case stream_let_is_next_callback_identifier:
        break;
    
    case stream_backreference_is_next_callback_identifier:
        break;
    
    case stream_builtin_is_next_callback_identifier:
        break;
    
    case stream_item_from_context_is_next_callback_identifier:
        break;
    
    case stream_end_callback_identifier:
        break;
    
    case combinator_parallel_callback_identifier:
        finalize_callback_invocation_pattern_buffer
            (&pattern->specifics.combinator_parallel.children);
        break;
    
    case combinator_sequential_callback_identifier:
        finalize_callback_invocation_pattern_buffer
            (&pattern->specifics.combinator_sequential.children);
        break;
    
    }
}


static void initialize_callback_invocation_pattern_buffer
  (struct callback_invocation_pattern_buffer *buffer)
{
    buffer->count = 0;
    buffer->capacity = 8;
    buffer->patterns = actually_malloc
        (sizeof(struct callback_invocation_pattern *) * buffer->capacity);
}


static void finalize_callback_invocation_pattern_buffer
  (struct callback_invocation_pattern_buffer *buffer)
{
    for(size_t i = 0; i < buffer->count; i++) {
        finalize_callback_invocation_pattern(buffer->patterns[i]);
        actually_free(buffer->patterns[i]);
    }
    actually_free(buffer->patterns);
}


static struct callback_invocation_pattern
  *make_callback_invocation_pattern_in_buffer
  (struct callback_invocation_pattern_buffer *buffer)
{
    size_t original_capacity = buffer->capacity;
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
    }
    if(buffer->capacity != original_capacity) {
        buffer->patterns = actually_realloc
            (buffer->patterns,
             sizeof(struct callback_invocation_pattern *) * buffer->capacity);
    }
    
    struct callback_invocation_pattern *result =
        actually_malloc(sizeof(struct callback_invocation_pattern));
    buffer->patterns[buffer->count] = result;
    
    buffer->count++;
    
    initialize_callback_invocation_pattern(result);
    
    return result;
}


static void move_callback_invocation_pattern_into_buffer
  (struct callback_invocation_pattern_buffer *buffer,
   struct callback_invocation_pattern *pattern)
{
    size_t original_capacity = buffer->capacity;
    while(buffer->count + 1 >= buffer->capacity) {
        buffer->capacity *= 2;
    }
    if(buffer->capacity != original_capacity) {
        buffer->patterns = actually_realloc
            (buffer->patterns,
             sizeof(struct callback_invocation_pattern *) * buffer->capacity);
    }
    
    buffer->patterns[buffer->count] = pattern;
    
    buffer->count++;
}


static struct callback_invocation_pattern_buffer
  *get_buffer_for_parallel_callback_invocation_pattern
  (struct test_suite *test_suite)
{
    if(!test_suite->expected_callbacks) {
        test_suite->expected_callbacks =
            actually_malloc(sizeof(struct callback_invocation_pattern));
        initialize_callback_invocation_pattern(test_suite->expected_callbacks);
        test_suite->expected_callbacks->identifier =
            combinator_parallel_callback_identifier;
        initialize_callback_invocation_pattern_buffer
            (&test_suite->expected_callbacks->specifics.combinator_parallel
             .children);
        return &test_suite->expected_callbacks->specifics.combinator_parallel
               .children;
    } else switch(test_suite->expected_callbacks->identifier) {
    case combinator_parallel_callback_identifier:
        return &test_suite->expected_callbacks->specifics.combinator_parallel
               .children;
        break;
    
    default:
    {
        struct callback_invocation_pattern *temporary =
            test_suite->expected_callbacks;
        test_suite->expected_callbacks =
            actually_malloc(sizeof(struct callback_invocation_pattern));
        initialize_callback_invocation_pattern(test_suite->expected_callbacks);
        test_suite->expected_callbacks->identifier =
            combinator_parallel_callback_identifier;
        initialize_callback_invocation_pattern_buffer
            (&test_suite->expected_callbacks->specifics.combinator_parallel
             .children);
        move_callback_invocation_pattern_into_buffer
            (&test_suite->expected_callbacks->specifics.combinator_parallel
             .children,
             temporary);
        return &test_suite->expected_callbacks->specifics.combinator_parallel
               .children;
        break;
    }
    }
}


static void remove_callback_invocation_pattern_from_buffer
  (struct callback_invocation_pattern_buffer *buffer,
   struct callback_invocation_pattern *allocation)
{
    size_t i;
    for(i = 0; i < buffer->count; i++) {
        if(buffer->patterns[i] == allocation) break;
    }
    if(i < buffer->count) {
        memmove(buffer->patterns + i,
                buffer->patterns + i + 1,
                sizeof(struct callback_invocation_pattern *)
                * (buffer->count - (i + 1)));
        buffer->count--;
    }
}


static void copy_callback_behavior
  (enum callback_identifier identifier,
   union callback_behavior *destination,
   union callback_behavior *source)
{
    switch(identifier) {
    case library_finalizer_callback_identifier:
        break;
    
    case allocator_malloc_callback_identifier:
        if(source->allocator_malloc.tag) {
            destination->allocator_malloc.tag =
                strdup(source->allocator_malloc.tag);
        } else {
            destination->allocator_malloc.tag = NULL;
        }
        break;
    
    case allocator_free_callback_identifier:
        if(source->allocator_free.tag) {
            destination->allocator_free.tag =
                strdup(source->allocator_free.tag);
        } else {
            destination->allocator_free.tag = NULL;
        }
        break;
    
    case allocator_realloc_callback_identifier:
        if(source->allocator_realloc.tag) {
            destination->allocator_realloc.tag =
                strdup(source->allocator_realloc.tag);
        } else {
            destination->allocator_realloc.tag = NULL;
        }
        break;
    
    case error_memory_callback_identifier:
        break;
    
    case error_type_mismatch_callback_identifier:
        break;
    
    case error_universe_level_overflow_callback_identifier:
        break;
    
    case error_buffer_index_callback_identifier:
        break;
    
    case error_not_applicable_callback_identifier:
        break;
    
    case error_non_numeric_float_callback_identifier:
        break;
    
    case stream_start_callback_identifier:
        destination->stream_start.fail =
            source->stream_start.fail;
        break;
    
    case stream_magic_number_callback_identifier:
        destination->stream_magic_number.fail =
            source->stream_magic_number.fail;
        break;
    
    case stream_name_definition_callback_identifier:
        destination->stream_name_definition.fail =
            source->stream_name_definition.fail;
        break;
    
    case stream_value_definition_is_next_callback_identifier:
        destination->stream_value_definition_is_next.fail =
            source->stream_value_definition_is_next.fail;
        break;
    
    case stream_type_definition_bool_callback_identifier:
        destination->stream_type_definition_bool.fail =
            source->stream_type_definition_bool.fail;
        break;
    
    case stream_type_definition_ordering_callback_identifier:
        destination->stream_type_definition_ordering.fail =
            source->stream_type_definition_ordering.fail;
        break;
    
    case stream_type_definition_maybe_is_next_callback_identifier:
        destination->stream_type_definition_maybe_is_next.fail =
            source->stream_type_definition_maybe_is_next.fail;
        break;
    
    case stream_type_definition_int8_callback_identifier:
        destination->stream_type_definition_int8.fail =
            source->stream_type_definition_int8.fail;
        break;
    
    case stream_type_definition_int16_callback_identifier:
        destination->stream_type_definition_int16.fail =
            source->stream_type_definition_int16.fail;
        break;
    
    case stream_type_definition_int32_callback_identifier:
        destination->stream_type_definition_int32.fail =
            source->stream_type_definition_int32.fail;
        break;
    
    case stream_type_definition_int64_callback_identifier:
        destination->stream_type_definition_int64.fail =
            source->stream_type_definition_int64.fail;
        break;
    
    case stream_type_definition_nat8_callback_identifier:
        destination->stream_type_definition_nat8.fail =
            source->stream_type_definition_nat8.fail;
        break;
    
    case stream_type_definition_nat16_callback_identifier:
        destination->stream_type_definition_nat16.fail =
            source->stream_type_definition_nat16.fail;
        break;
    
    case stream_type_definition_nat32_callback_identifier:
        destination->stream_type_definition_nat32.fail =
            source->stream_type_definition_nat32.fail;
        break;
    
    case stream_type_definition_nat64_callback_identifier:
        destination->stream_type_definition_nat64.fail =
            source->stream_type_definition_nat64.fail;
        break;
    
    case stream_type_definition_float32_callback_identifier:
        destination->stream_type_definition_float32.fail =
            source->stream_type_definition_float32.fail;
        break;
    
    case stream_type_definition_float64_callback_identifier:
        destination->stream_type_definition_float64.fail =
            source->stream_type_definition_float64.fail;
        break;
    
    case stream_type_definition_utf8_callback_identifier:
        destination->stream_type_definition_utf8.fail =
            source->stream_type_definition_utf8.fail;
        break;
    
    case stream_type_definition_blob_callback_identifier:
        destination->stream_type_definition_blob.fail =
            source->stream_type_definition_blob.fail;
        break;
    
    case stream_type_definition_function_is_next_callback_identifier:
        destination->stream_type_definition_function_is_next.fail =
            source->stream_type_definition_function_is_next.fail;
        break;
    
    case stream_type_definition_sigma_is_next_callback_identifier:
        destination->stream_type_definition_sigma_is_next.fail =
            source->stream_type_definition_sigma_is_next.fail;
        break;
    
    case stream_type_definition_named_is_next_callback_identifier:
        destination->stream_type_definition_named_is_next.fail =
            source->stream_type_definition_named_is_next.fail;
        break;
    
    case stream_type_definition_universe_callback_identifier:
        destination->stream_type_definition_universe.fail =
            source->stream_type_definition_universe.fail;
        break;
    
    case stream_bool_false_callback_identifier:
        destination->stream_bool_false.fail =
            source->stream_bool_false.fail;
        break;
    
    case stream_bool_true_callback_identifier:
        destination->stream_bool_true.fail =
            source->stream_bool_true.fail;
        break;
    
    case stream_ordering_less_callback_identifier:
        destination->stream_ordering_less.fail =
            source->stream_ordering_less.fail;
        break;
    
    case stream_ordering_equal_callback_identifier:
        destination->stream_ordering_equal.fail =
            source->stream_ordering_equal.fail;
        break;
    
    case stream_ordering_greater_callback_identifier:
        destination->stream_ordering_greater.fail =
            source->stream_ordering_greater.fail;
        break;
    
    case stream_maybe_nothing_callback_identifier:
        destination->stream_maybe_nothing.fail =
            source->stream_maybe_nothing.fail;
        break;
    
    case stream_maybe_just_is_next_callback_identifier:
        destination->stream_maybe_just_is_next.fail =
            source->stream_maybe_just_is_next.fail;
        break;
    
    case stream_int8_callback_identifier:
        destination->stream_int8.fail =
            source->stream_int8.fail;
        break;
    
    case stream_int16_callback_identifier:
        destination->stream_int16.fail =
            source->stream_int16.fail;
        break;
    
    case stream_int32_callback_identifier:
        destination->stream_int32.fail =
            source->stream_int32.fail;
        break;
    
    case stream_int64_callback_identifier:
        destination->stream_int64.fail =
            source->stream_int64.fail;
        break;
    
    case stream_nat8_callback_identifier:
        destination->stream_nat8.fail =
            source->stream_nat8.fail;
        break;
    
    case stream_nat16_callback_identifier:
        destination->stream_nat16.fail =
            source->stream_nat16.fail;
        break;
    
    case stream_nat32_callback_identifier:
        destination->stream_nat32.fail =
            source->stream_nat32.fail;
        break;
    
    case stream_nat64_callback_identifier:
        destination->stream_nat64.fail =
            source->stream_nat64.fail;
        break;
    
    case stream_float32_callback_identifier:
        destination->stream_float32.fail =
            source->stream_float32.fail;
        break;
    
    case stream_float64_callback_identifier:
        destination->stream_float64.fail =
            source->stream_float64.fail;
        break;
    
    case stream_utf8_start_callback_identifier:
        destination->stream_utf8_start.fail =
            source->stream_utf8_start.fail;
        break;
    
    case stream_utf8_data_callback_identifier:
        destination->stream_utf8_data.fail =
            source->stream_utf8_data.fail;
        break;
    
    case stream_utf8_end_callback_identifier:
        destination->stream_utf8_end.fail =
            source->stream_utf8_end.fail;
        break;
    
    case stream_blob_start_callback_identifier:
        destination->stream_blob_start.fail =
            source->stream_blob_start.fail;
        break;
    
    case stream_blob_data_callback_identifier:
        destination->stream_blob_data.fail =
            source->stream_blob_data.fail;
        break;
    
    case stream_blob_end_callback_identifier:
        destination->stream_blob_end.fail =
            source->stream_blob_end.fail;
        break;
    
    case stream_sigma_is_next_callback_identifier:
        destination->stream_sigma_is_next.fail =
            source->stream_sigma_is_next.fail;
        break;
    
    case stream_named_value_is_next_callback_identifier:
        destination->stream_named_value_is_next.fail =
            source->stream_named_value_is_next.fail;
        break;
    
    case stream_lambda_is_next_callback_identifier:
        destination->stream_lambda_is_next.fail =
            source->stream_lambda_is_next.fail;
        break;
    
    case stream_apply_is_next_callback_identifier:
        destination->stream_apply_is_next.fail =
            source->stream_apply_is_next.fail;
        break;
    
    case stream_type_family_is_next_callback_identifier:
        destination->stream_type_family_is_next.fail =
            source->stream_type_family_is_next.fail;
        break;
    
    case stream_let_is_next_callback_identifier:
        destination->stream_let_is_next.fail =
            source->stream_let_is_next.fail;
        break;
    
    case stream_backreference_is_next_callback_identifier:
        destination->stream_backreference_is_next.fail =
            source->stream_backreference_is_next.fail;
        break;
    
    case stream_builtin_is_next_callback_identifier:
        destination->stream_builtin_is_next.fail =
            source->stream_builtin_is_next.fail;
        break;
    
    case stream_item_from_context_is_next_callback_identifier:
        destination->stream_item_from_context_is_next.fail =
            source->stream_item_from_context_is_next.fail;
        break;
    
    case stream_end_callback_identifier:
        destination->stream_end.fail =
            source->stream_end.fail;
        break;
    
    case combinator_parallel_callback_identifier:
        break;
    
    case combinator_sequential_callback_identifier:
        break;
    
    }
}


static void print_test_case
  (struct test_case *test_case)
{
    // TODO
}


static void print_test_case_buffer
  (struct test_case_buffer *buffer)
{
    // TODO
}


static void print_callback_invocation
  (struct callback_invocation *invocation)
{
    switch(invocation->identifier) {
    case library_finalizer_callback_identifier:
        printf("modern_library_finalize()\n");
        break;
    
    case allocator_malloc_callback_identifier:
        printf("modern_allocator_malloc(%llu)\n",
               (unsigned long long)
               invocation->specifics.allocator_malloc.size);
        break;
    
    case allocator_free_callback_identifier:
        printf("modern_allocator_free(0x%llx)\n",
               (unsigned long long)
               invocation->specifics.allocator_free.data);
        break;
    
    case allocator_realloc_callback_identifier:
        printf("modern_allocator_realloc(0x%llx, %llu)\n",
               (unsigned long long)
               invocation->specifics.allocator_realloc.data,
               (unsigned long long)
               invocation->specifics.allocator_realloc.new_size);
        break;
    
    case error_memory_callback_identifier:
        printf("modern_error_handler_memory(%llu)\n",
               (unsigned long long)
               invocation->specifics.error_memory.requested_size);
        break;
    
    case error_type_mismatch_callback_identifier:
        printf("modern_error_handler_type_mismatch(0x%llx, 0x%llx)\n",
               (unsigned long long)
               invocation->specifics.error_type_mismatch.expected,
               (unsigned long long)
               invocation->specifics.error_type_mismatch.actual);
        break;
    
    case error_universe_level_overflow_callback_identifier:
        printf("modern_error_handler_universe_level_overflow()\n");
        break;
    
    case error_buffer_index_callback_identifier:
        printf("modern_error_handler_buffer_index()\n");
        break;
    
    case error_not_applicable_callback_identifier:
        printf("modern_error_handler_not_applicable()\n");
        break;
    
    case error_non_numeric_float_callback_identifier:
        printf("modern_error_handler_non_numeric_float()\n");
        break;
    
    case stream_start_callback_identifier:
        printf("stream_start_callback_identifier()\n");
        break;
    
    case stream_magic_number_callback_identifier:
        printf("stream_magic_number_callback_identifier()\n");
        break;
    
    case stream_name_definition_callback_identifier:
        printf("stream_name_definition_callback_identifier()\n");
        break;
    
    case stream_value_definition_is_next_callback_identifier:
        printf("stream_value_definition_is_next_callback_identifier()\n");
        break;
    
    case stream_type_definition_bool_callback_identifier:
        printf("stream_type_definition_bool_callback_identifier()\n");
        break;
    
    case stream_type_definition_ordering_callback_identifier:
        printf("stream_type_definition_ordering_callback_identifier()\n");
        break;
    
    case stream_type_definition_maybe_is_next_callback_identifier:
        printf("stream_type_definition_maybe_is_next_callback_identifier()\n");
        break;
    
    case stream_type_definition_int8_callback_identifier:
        printf("stream_type_definition_int8_callback_identifier()\n");
        break;
    
    case stream_type_definition_int16_callback_identifier:
        printf("stream_type_definition_int16_callback_identifier()\n");
        break;
    
    case stream_type_definition_int32_callback_identifier:
        printf("stream_type_definition_int32_callback_identifier()\n");
        break;
    
    case stream_type_definition_int64_callback_identifier:
        printf("stream_type_definition_int64_callback_identifier()\n");
        break;
    
    case stream_type_definition_nat8_callback_identifier:
        printf("stream_type_definition_nat8_callback_identifier()\n");
        break;
    
    case stream_type_definition_nat16_callback_identifier:
        printf("stream_type_definition_nat16_callback_identifier()\n");
        break;
    
    case stream_type_definition_nat32_callback_identifier:
        printf("stream_type_definition_nat32_callback_identifier()\n");
        break;
    
    case stream_type_definition_nat64_callback_identifier:
        printf("stream_type_definition_nat64_callback_identifier()\n");
        break;
    
    case stream_type_definition_float32_callback_identifier:
        printf("stream_type_definition_float32_callback_identifier()\n");
        break;
    
    case stream_type_definition_float64_callback_identifier:
        printf("stream_type_definition_float64_callback_identifier()\n");
        break;
    
    case stream_type_definition_utf8_callback_identifier:
        printf("stream_type_definition_utf8_callback_identifier()\n");
        break;
    
    case stream_type_definition_blob_callback_identifier:
        printf("stream_type_definition_blob_callback_identifier()\n");
        break;
    
    case stream_type_definition_function_is_next_callback_identifier:
        printf("stream_type_definition_function_is_next_callback_identifier()\n");
        break;
    
    case stream_type_definition_sigma_is_next_callback_identifier:
        printf("stream_type_definition_sigma_is_next_callback_identifier()\n");
        break;
    
    case stream_type_definition_named_is_next_callback_identifier:
        printf("stream_type_definition_named_is_next_callback_identifier()\n");
        break;
    
    case stream_type_definition_universe_callback_identifier:
        printf("stream_type_definition_universe_callback_identifier()\n");
        break;
    
    case stream_bool_false_callback_identifier:
        printf("stream_bool_false_callback_identifier()\n");
        break;
    
    case stream_bool_true_callback_identifier:
        printf("stream_bool_true_callback_identifier()\n");
        break;
    
    case stream_ordering_less_callback_identifier:
        printf("stream_ordering_less_callback_identifier()\n");
        break;
    
    case stream_ordering_equal_callback_identifier:
        printf("stream_ordering_equal_callback_identifier()\n");
        break;
    
    case stream_ordering_greater_callback_identifier:
        printf("stream_ordering_greater_callback_identifier()\n");
        break;
    
    case stream_maybe_nothing_callback_identifier:
        printf("stream_maybe_nothing_callback_identifier()\n");
        break;
    
    case stream_maybe_just_is_next_callback_identifier:
        printf("stream_maybe_just_is_next_callback_identifier()\n");
        break;
    
    case stream_int8_callback_identifier:
        printf("stream_int8_callback_identifier()\n");
        break;
    
    case stream_int16_callback_identifier:
        printf("stream_int16_callback_identifier()\n");
        break;
    
    case stream_int32_callback_identifier:
        printf("stream_int32_callback_identifier()\n");
        break;
    
    case stream_int64_callback_identifier:
        printf("stream_int64_callback_identifier()\n");
        break;
    
    case stream_nat8_callback_identifier:
        printf("stream_nat8_callback_identifier()\n");
        break;
    
    case stream_nat16_callback_identifier:
        printf("stream_nat16_callback_identifier()\n");
        break;
    
    case stream_nat32_callback_identifier:
        printf("stream_nat32_callback_identifier()\n");
        break;
    
    case stream_nat64_callback_identifier:
        printf("stream_nat64_callback_identifier()\n");
        break;
    
    case stream_float32_callback_identifier:
        printf("stream_float32_callback_identifier()\n");
        break;
    
    case stream_float64_callback_identifier:
        printf("stream_float64_callback_identifier()\n");
        break;
    
    case stream_utf8_start_callback_identifier:
        printf("stream_utf8_start_callback_identifier()\n");
        break;
    
    case stream_utf8_data_callback_identifier:
        printf("stream_utf8_data_callback_identifier()\n");
        break;
    
    case stream_utf8_end_callback_identifier:
        printf("stream_utf8_end_callback_identifier()\n");
        break;
    
    case stream_blob_start_callback_identifier:
        printf("stream_blob_start_callback_identifier()\n");
        break;
    
    case stream_blob_data_callback_identifier:
        printf("stream_blob_data_callback_identifier()\n");
        break;
    
    case stream_blob_end_callback_identifier:
        printf("stream_blob_end_callback_identifier()\n");
        break;
    
    case stream_sigma_is_next_callback_identifier:
        printf("stream_sigma_is_next_callback_identifier()\n");
        break;
    
    case stream_named_value_is_next_callback_identifier:
        printf("stream_named_value_is_next_callback_identifier()\n");
        break;
    
    case stream_lambda_is_next_callback_identifier:
        printf("stream_lambda_is_next_callback_identifier()\n");
        break;
    
    case stream_apply_is_next_callback_identifier:
        printf("stream_apply_is_next_callback_identifier()\n");
        break;
    
    case stream_type_family_is_next_callback_identifier:
        printf("stream_type_family_is_next_callback_identifier()\n");
        break;
    
    case stream_let_is_next_callback_identifier:
        printf("stream_let_is_next_callback_identifier()\n");
        break;
    
    case stream_backreference_is_next_callback_identifier:
        printf("stream_backreference_is_next_callback_identifier()\n");
        break;
    
    case stream_builtin_is_next_callback_identifier:
        printf("stream_builtin_is_next_callback_identifier()\n");
        break;
    
    case stream_item_from_context_is_next_callback_identifier:
        printf("stream_item_from_context_is_next_callback_identifier()\n");
        break;
    
    case stream_end_callback_identifier:
        printf("stream_end_callback_identifier()\n");
        break;
    
    case combinator_parallel_callback_identifier:
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to describe a callback invocation, "
               "but it wasn't a real one, it was combinator_parallel.\n");
        exit(1);
        break;
    
    case combinator_sequential_callback_identifier:
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to describe a callback invocation, "
               "but it wasn't a real one, it was combinator_sequential.\n");
        exit(1);
        break;
    
    }
}


static void print_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer)
{
    // TODO
}


static struct callback_invocation *begin_callback
  (struct test_suite *test_suite)
{
    struct callback_invocation *result;
    if(test_suite->current_test_case) {
        result = make_callback_invocation_in_buffer
            (&test_suite->current_test_case->actual_callbacks);
    } else if(test_suite->current_fixtures) {
        result = make_callback_invocation_in_buffer
            (&test_suite->current_fixtures->actual_callbacks);
    } else {
        result = make_callback_invocation_in_buffer
            (&test_suite->actual_callbacks);
    }
    
    test_suite->current_callback = result;
    
    return result;
}


static int match_callback_invocation_against_pattern
  (struct test_suite *test_suite,
   struct callback_invocation *invocation,
   struct callback_invocation_pattern *pattern,
   struct callback_invocation_pattern_buffer **buffer_result,
   struct callback_invocation_pattern **pattern_result,
   union callback_behavior *behavior)
{
    switch(invocation->identifier) {
    case combinator_parallel_callback_identifier:
    {
        struct callback_invocation_pattern_buffer *buffer =
            &pattern->specifics.combinator_parallel.children;
        size_t expectation_index;
        for(expectation_index = 0;
            expectation_index < buffer->count;
            expectation_index++)
        {
            struct callback_invocation_pattern *expected =
                buffer->patterns[expectation_index];
            int matches =
                match_callback_invocation_against_pattern_helper
                    (test_suite, invocation, expected, behavior);
            if(matches) {
                *buffer_result = buffer;
                *pattern_result = expected;
                return 1;
            }
        }
        
        return 0;
    }
    
    case combinator_sequential_callback_identifier:
    {
        struct callback_invocation_pattern_buffer *buffer =
            &pattern->specifics.combinator_sequential.children;
        if(buffer->count < 1) return 0;
        struct callback_invocation_pattern *expected = buffer->patterns[0];
        int matches =
            match_callback_invocation_against_pattern_helper
                (test_suite, invocation, expected, behavior);
        if(matches) {
            *buffer_result = buffer;
            *pattern_result = expected;
            return 1;
        } else return 0;
    }
    
    default:
    {
        int matches =
            match_callback_invocation_against_pattern_helper
                (test_suite, invocation, pattern, behavior);
        if(matches) {
            *buffer_result = NULL;
            *pattern_result = pattern;
            return 1;
        } else return 0;
    }
    }
}  


static int match_callback_invocation_against_pattern_helper
  (struct test_suite *test_suite,
   struct callback_invocation *invocation,
   struct callback_invocation_pattern *pattern,
   union callback_behavior *behavior)
{
    if(pattern->identifier != invocation->identifier) return 0;
    
    int matches = 0;
    switch(pattern->identifier) {
    case library_finalizer_callback_identifier:
        matches = 1;
        break;

    case allocator_malloc_callback_identifier:
        matches = 1;
        break;

    case allocator_free_callback_identifier:
    {
        struct allocated_data_buffer *buffer;
        if(test_suite->current_test_case) {
            buffer = &test_suite->current_test_case->allocations;
        } else if(test_suite->current_fixtures) {
            buffer = &test_suite->current_fixtures->allocations;
        } else {
            buffer = &test_suite->allocations;
        }
        
        struct allocated_data *allocation = NULL;
        for(size_t i = 0; i < buffer->count; i++) {
            if(buffer->allocated_data[i]->data
               == invocation->specifics.allocator_free.data)
            {
                allocation = buffer->allocated_data[i];
                break;
            }
        }
        
        if(!allocation) {
            matches = 0;
        } else {
            matches = 1;
            if(pattern->specifics.allocator_free.tag) {
                if(strcmp(allocation->tag,
                          pattern->specifics.allocator_free.tag))
                {
                    matches = 0;
                }
            }
        }
        break;
    }
    
    case allocator_realloc_callback_identifier:
    {
        struct allocated_data_buffer *buffer;
        if(test_suite->current_test_case) {
            buffer = &test_suite->current_test_case->allocations;
        } else if(test_suite->current_fixtures) {
            buffer = &test_suite->current_fixtures->allocations;
        } else {
            buffer = &test_suite->allocations;
        }
        
        struct allocated_data *allocation = NULL;
        for(size_t i = 0; i < buffer->count; i++) {
            if(buffer->allocated_data[i]->data ==
               invocation->specifics.allocator_free.data)
            {
                allocation = buffer->allocated_data[i];
                break;
            }
        }
        
        if(!allocation) {
            matches = 0;
        } else {
            matches = 1;
            if(pattern->specifics.allocator_realloc.tag) {
                if(strcmp(allocation->tag,
                          pattern->specifics.allocator_realloc.tag))
                {
                    matches = 0;
                }
            }
        }
        break;
    }

    case error_memory_callback_identifier:
        matches = 1;
        break;

    case error_type_mismatch_callback_identifier:
        matches = 1;
        break;

    case error_universe_level_overflow_callback_identifier:
        matches = 1;
        break;

    case error_buffer_index_callback_identifier:
        matches = 1;
        break;

    case error_not_applicable_callback_identifier:
        matches = 1;
        break;

    case error_non_numeric_float_callback_identifier:
        matches = 1;
        break;

    case stream_start_callback_identifier:
        matches = 1;
        break;

    case stream_magic_number_callback_identifier:
        matches = 1;
        break;

    case stream_name_definition_callback_identifier:
        matches = 1;
        break;

    case stream_value_definition_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_bool_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_ordering_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_maybe_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_int8_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_int16_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_int32_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_int64_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_nat8_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_nat16_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_nat32_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_nat64_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_float32_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_float64_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_utf8_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_blob_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_function_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_sigma_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_named_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_definition_universe_callback_identifier:
        matches = 1;
        break;

    case stream_bool_false_callback_identifier:
        matches = 1;
        break;

    case stream_bool_true_callback_identifier:
        matches = 1;
        break;

    case stream_ordering_less_callback_identifier:
        matches = 1;
        break;

    case stream_ordering_equal_callback_identifier:
        matches = 1;
        break;

    case stream_ordering_greater_callback_identifier:
        matches = 1;
        break;

    case stream_maybe_nothing_callback_identifier:
        matches = 1;
        break;

    case stream_maybe_just_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_int8_callback_identifier:
        matches = 1;
        break;

    case stream_int16_callback_identifier:
        matches = 1;
        break;

    case stream_int32_callback_identifier:
        matches = 1;
        break;

    case stream_int64_callback_identifier:
        matches = 1;
        break;

    case stream_nat8_callback_identifier:
        matches = 1;
        break;

    case stream_nat16_callback_identifier:
        matches = 1;
        break;

    case stream_nat32_callback_identifier:
        matches = 1;
        break;

    case stream_nat64_callback_identifier:
        matches = 1;
        break;

    case stream_float32_callback_identifier:
        matches = 1;
        break;

    case stream_float64_callback_identifier:
        matches = 1;
        break;

    case stream_utf8_start_callback_identifier:
        matches = 1;
        break;

    case stream_utf8_data_callback_identifier:
        matches = 1;
        break;

    case stream_utf8_end_callback_identifier:
        matches = 1;
        break;

    case stream_blob_start_callback_identifier:
        matches = 1;
        break;

    case stream_blob_data_callback_identifier:
        matches = 1;
        break;

    case stream_blob_end_callback_identifier:
        matches = 1;
        break;

    case stream_sigma_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_named_value_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_lambda_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_apply_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_type_family_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_let_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_backreference_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_builtin_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_item_from_context_is_next_callback_identifier:
        matches = 1;
        break;

    case stream_end_callback_identifier:
        matches = 1;
        break;
    
    case combinator_parallel_callback_identifier:
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to do a low-level comparison against "
               "a callback invocation pattern, but it wasn't a real one, it "
               "was combinator_parallel.\n");
        exit(1);
        break;
    
    case combinator_sequential_callback_identifier:
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to do a low-level comparison against "
               "a callback invocation pattern, but it wasn't a real one, it "
               "was combinator_sequential.\n");
        exit(1);
        break;
    }
    
    return matches;
}


static int callback_should_succeed
  (struct test_suite *test_suite, union callback_behavior *behavior)
{
    struct callback_invocation *actual = test_suite->current_callback;
    
    test_suite->current_callback = NULL;
    
    struct callback_invocation_pattern_buffer *buffer = NULL;
    struct callback_invocation_pattern *expected = NULL;
    int matches = 0;
    if(!test_suite->expected_callbacks) matches = 0;
    else matches = match_callback_invocation_against_pattern
        (test_suite,
         actual,
         test_suite->expected_callbacks,
         &buffer,
         &expected,
         behavior);
    if(matches) {
        int should_succeed = expected->should_succeed;
        
        if(behavior) {
            memcpy(behavior, &expected->behavior,
                sizeof(union callback_behavior));
        }
        
        if(!expected->sticky) {
            if(!buffer) {
                finalize_callback_invocation_pattern
                    (test_suite->expected_callbacks);
                actually_free(test_suite->expected_callbacks);
                test_suite->expected_callbacks = NULL;
            } else {
                remove_callback_invocation_pattern_from_buffer
                    (buffer, expected);
                finalize_callback_invocation_pattern(expected);
                actually_free(expected);
            }
        }
        
        return should_succeed;
    } else {
        if(test_suite->output_on_header_line) {
            printf("\n");
            test_suite->output_on_header_line = 0;
        }
        printf("  Unexpected: ");
        print_callback_invocation(actual);
        
        fail(test_suite); // Never returns.
        return 0;
    }
}


static void fail
  (struct test_suite *test_suite)
{
    if(test_suite->current_test_case) {
        test_suite->current_test_case->succeeded = 0;
        longjmp(test_suite->current_test_case->jmp_buf, 1);
    } else if(test_suite->current_fixtures) {
        test_suite->current_fixtures->succeeded = 0;
        longjmp(test_suite->current_fixtures->jmp_buf, 1);
    } else {
        longjmp(test_suite->jmp_buf, 1);
    }
}


static void check_for_memory_leaks
  (struct test_suite *test_suite)
{
    struct allocated_data_buffer *buffer;
    if(test_suite->current_test_case) {
        buffer = &test_suite->current_test_case->allocations;
    } else if(test_suite->current_fixtures) {
        buffer = &test_suite->current_fixtures->allocations;
    } else {
        buffer = &test_suite->allocations;
    }
    
    if(buffer->count > 0) {
        size_t leak_count = 0;
        size_t leak_size = 0;
        
        for(size_t i = 0; i < buffer->count; i++) {
            struct allocated_data *allocation = buffer->allocated_data[i];
            
            leak_count++;
            leak_size += allocation->size;

            if(test_suite->output_on_header_line) {
                printf("\n");
                test_suite->output_on_header_line = 0;
            }
            printf("  Leak of %llu bytes in item tagged \"%s\"\n",
                   (unsigned long long) allocation->size,
                   allocation->tag);
            
            finalize_allocated_data(allocation);
            actually_free(allocation);
        }
        
        buffer->count = 0;
        
        if(test_suite->output_on_header_line) {
            printf("\n");
            test_suite->output_on_header_line = 0;
        }
        printf("  Memory leak: %llu bytes in %llu items\n",
               (unsigned long long) leak_size,
               (unsigned long long) leak_count);
        
        fail(test_suite); // Never returns.
    }
}


void flush_expectations(test_suite *test_suite_in)
{
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->expected_callbacks) {
        test_message(test_suite, "Missing expected calls.");
        fail(test_suite); // Never returns.
    }
}


static void library_finalizer
  (struct test_suite *test_suite)
{
}


static void *allocator_malloc(struct test_suite *test_suite, size_t size) {
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = allocator_malloc_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.allocator_malloc.size = size;
    
    union callback_behavior behavior;
    if(callback_should_succeed(test_suite, &behavior)) {
        void *data = actually_malloc(size);
                
        struct allocated_data *allocation;
        if(test_suite->current_test_case) {
            allocation = make_allocated_data_in_buffer
                (&test_suite->current_test_case->allocations);
        } else if(test_suite->current_fixtures) {
            allocation = make_allocated_data_in_buffer
                (&test_suite->current_fixtures->allocations);
        } else {
            allocation = make_allocated_data_in_buffer
                (&test_suite->allocations);
        }
        
        allocation->data = data;
        allocation->size = size;
        allocation->tag = behavior.allocator_malloc.tag;
        
        return data;
    } else {
        return NULL;
    }
}


static void allocator_free(struct test_suite *test_suite, void *data) {
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = allocator_free_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.allocator_free.data = data;
    
    union callback_behavior behavior;
    if(callback_should_succeed(test_suite, &behavior)) {
        struct allocated_data_buffer *buffer;
        if(test_suite->current_test_case) {
            buffer = &test_suite->current_test_case->allocations;
        } else if(test_suite->current_fixtures) {
            buffer = &test_suite->current_fixtures->allocations;
        } else {
            buffer = &test_suite->allocations;
        }
        
        struct allocated_data *allocation = NULL;
        for(size_t i = 0; i < buffer->count; i++) {
            if(buffer->allocated_data[i]->data == data) {
                allocation = buffer->allocated_data[i];
                break;
            }
        }
        
        if(!allocation) {
            printf("\n\n"
                   "*** The testing infrastructure itself failed.\n"
                   "*** Specifically, got as far as trying to perform a free, "
                   "but no allocation information for the memory in question could be found.\n");
            exit(1);
        }
        
        remove_allocated_data_from_buffer(buffer, allocation);
        finalize_allocated_data(allocation);
        actually_free(allocation);
    }
}


static void *allocator_realloc
  (struct test_suite *test_suite, void *data, size_t size)
{
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = allocator_free_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.allocator_realloc.data = data;
    invocation->specifics.allocator_realloc.new_size = size;
    
    if(callback_should_succeed(test_suite, NULL))
        return actually_realloc(data, size);
    else return NULL;
}


static void error_memory
  (struct test_suite *test_suite, size_t requested_size)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_memory_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.error_memory.requested_size = requested_size;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


static void error_type_mismatch
  (struct test_suite *test_suite, modern *expected, modern *actual)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_type_mismatch_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.error_type_mismatch.expected = expected;
    invocation->specifics.error_type_mismatch.actual = actual;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


static void error_universe_level_overflow
  (struct test_suite *test_suite)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_universe_level_overflow_callback_identifier;
    invocation->succeeded = 0;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


static void error_buffer_index
  (struct test_suite *test_suite)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_buffer_index_callback_identifier;
    invocation->succeeded = 0;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


static void error_not_applicable
  (struct test_suite *test_suite)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_not_applicable_callback_identifier;
    invocation->succeeded = 0;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


static void error_non_numeric_float
  (struct test_suite *test_suite)
{
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, received an error callback "
               "while not already in a test case.\n");
        exit(1);
    }
    
    test_suite->error_invocation = NULL;
    
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = error_non_numeric_float_callback_identifier;
    invocation->succeeded = 0;
    
    if(callback_should_succeed(test_suite, NULL)) {
        longjmp(test_suite->current_test_case->jmp_buf, 2);
    } else {
        return;
    }
}


struct modern_stream *test_stream_make(test_suite *test_suite)
{
    struct modern_stream *stream = malloc(sizeof(struct modern_stream));
    if(!stream) return NULL;
    
    stream->start = stream_start;
    stream->magic_number = stream_magic_number;
    stream->name_definition = stream_name_definition;
    stream->value_definition_is_next = stream_value_definition_is_next;
    stream->type_definition_bool = stream_type_definition_bool;
    stream->type_definition_ordering = stream_type_definition_ordering;
    stream->type_definition_maybe_is_next =
      stream_type_definition_maybe_is_next;
    stream->type_definition_int8 = stream_type_definition_int8;
    stream->type_definition_int16 = stream_type_definition_int16;
    stream->type_definition_int32 = stream_type_definition_int32;
    stream->type_definition_int64 = stream_type_definition_int64;
    stream->type_definition_nat8 = stream_type_definition_nat8;
    stream->type_definition_nat16 = stream_type_definition_nat16;
    stream->type_definition_nat32 = stream_type_definition_nat32;
    stream->type_definition_nat64 = stream_type_definition_nat64;
    stream->type_definition_float32 = stream_type_definition_float32;
    stream->type_definition_float64 = stream_type_definition_float64;
    stream->type_definition_utf8 = stream_type_definition_utf8;
    stream->type_definition_blob = stream_type_definition_blob;
    stream->type_definition_function_is_next =
      stream_type_definition_function_is_next;
    stream->type_definition_sigma_is_next =
      stream_type_definition_sigma_is_next;
    stream->type_definition_named_is_next =
      stream_type_definition_named_is_next;
    stream->type_definition_universe = stream_type_definition_universe;
    stream->bool_false = stream_bool_false;
    stream->bool_true = stream_bool_true;
    stream->ordering_less = stream_ordering_less;
    stream->ordering_equal = stream_ordering_equal;
    stream->ordering_greater = stream_ordering_greater;
    stream->maybe_nothing = stream_maybe_nothing;
    stream->maybe_just_is_next = stream_maybe_just_is_next;
    stream->int8 = stream_int8;
    stream->int16 = stream_int16;
    stream->int32 = stream_int32;
    stream->int64 = stream_int64;
    stream->nat8 = stream_nat8;
    stream->nat16 = stream_nat16;
    stream->nat32 = stream_nat32;
    stream->nat64 = stream_nat64;
    stream->float32 = stream_float32;
    stream->float64 = stream_float64;
    stream->utf8_start = stream_utf8_start;
    stream->utf8_data = stream_utf8_data;
    stream->utf8_end = stream_utf8_end;
    stream->blob_start = stream_blob_start;
    stream->blob_data = stream_blob_data;
    stream->blob_end = stream_blob_end;
    stream->sigma_is_next = stream_sigma_is_next;
    stream->named_value_is_next = stream_named_value_is_next;
    stream->lambda_is_next = stream_lambda_is_next;
    stream->apply_is_next = stream_apply_is_next;
    stream->type_family_is_next = stream_type_family_is_next;
    stream->let_is_next = stream_let_is_next;
    stream->backreference_is_next = stream_backreference_is_next;
    stream->builtin_is_next = stream_builtin_is_next;
    stream->item_from_context_is_next =
      stream_item_from_context_is_next;
    stream->end = stream_end;
    
    return stream;
}


void *test_stream_initialize(test_suite *test_suite)
{
    struct stream_state *stream_state = malloc(sizeof(struct stream_state));
    if(!stream_state) return NULL;
    
    stream_state->test_suite = test_suite;
    
    return stream_state;
}

static void stream_start
  (struct modern_process *process, void *process_state,
   void *stream_state_in)
{
    struct stream_state *stream_state =
      (struct stream_state *) stream_state_in;
    struct test_suite *test_suite = stream_state->test_suite;
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    invocation->identifier = stream_start_callback_identifier;
    invocation->succeeded = 0;
    invocation->specifics.stream_start.state = stream_state_in;
    
    union callback_behavior behavior;
    if(callback_should_succeed(test_suite, &behavior)) {
        return;
    } else {
        return;
    }
}


static void stream_magic_number
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_name_definition
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


static void stream_value_definition_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_bool
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_ordering
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_maybe_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_int8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_int16
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_int32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_int64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_float32
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_float64
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_utf8
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_blob
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_function_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_definition_named_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash name)
{
}


static void stream_type_definition_universe
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_bool_false
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_bool_true
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_ordering_less
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_ordering_equal
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_ordering_greater
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_maybe_nothing
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_maybe_just_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_int8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int8_t value)
{
}


static void stream_int16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int16_t value)
{
}


static void stream_int32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int32_t value)
{
}


static void stream_int64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   int64_t value)
{
}


static void stream_nat8
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t value)
{
}


static void stream_nat16
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint16_t value)
{
}


static void stream_nat32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint32_t value)
{
}


static void stream_nat64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint64_t value)
{
}


static void stream_float32
  (struct modern_process *process, void *process_state,
   void *stream_state,
   float value)
{
}


static void stream_float64
  (struct modern_process *process, void *process_state,
   void *stream_state,
   double value)
{
}


static void stream_utf8_start
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_utf8_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


static void stream_utf8_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_blob_start
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_blob_data
  (struct modern_process *process, void *process_state,
   void *stream_state,
   uint8_t *data, size_t length)
{
}


static void stream_blob_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_sigma_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state,
   struct modern_hash *type)
{
}


static void stream_named_value_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash name)
{
}


static void stream_lambda_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_apply_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}


static void stream_type_family_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items)
{
}


static void stream_let_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t n_items)
{
}


static void stream_backreference_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint64_t index)
{
}


static void stream_builtin_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, uint16_t identifier)
{
}


static void stream_item_from_context_is_next
  (struct modern_process *process, void *process_state,
   void *stream_state, struct modern_hash type)
{
}


static void stream_end
  (struct modern_process *process, void *process_state,
   void *stream_state)
{
}
