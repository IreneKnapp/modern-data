#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static

typedef void test_suite;

extern void test_main
  (test_suite *test_suite, modern_library *library);

extern void test_message
  (test_suite *test_suite,
   char *format, ...);

extern int begin_fixtures(test_suite *test_suite);
extern void end_fixtures(test_suite *test_suite);

extern void begin_test_case
  (test_suite *test_suite,
   int (*test_case)(void *test_context),
   void *test_context,
   char *format, ...);

extern void reset_allowances(test_suite *test_suite);

extern void allow_allocation(test_suite *test_suite, char *tag_format, ...);
extern void disallow_allocation(test_suite *test_suite);

extern void allow_deallocation(test_suite *test_suite, char *tag_format, ...);
extern void disallow_deallocation(test_suite *test_suite);

extern void expect_error_memory
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_retain_count_overflow
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_retain_count_underflow
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_double_autorelease
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_type_mismatch
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_universe_level_overflow
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_buffer_index
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_not_applicable
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);
extern void expect_error_non_numeric_float
  (test_suite *test_suite,
   int (*test_case_helper)(void *test_context),
   void *test_context);

extern void expect_stream_start
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_magic_number
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_name_definition
  (test_suite *test_suite,
   void *stream_state,
   uint8_t *data, size_t length);
extern void expect_stream_value_definition_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_bool
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_ordering
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_maybe_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_int8
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_int16
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_int32
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_int64
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_nat8
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_nat16
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_nat32
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_nat64
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_float32
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_float64
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_utf8
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_blob
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_function_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_sigma_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_definition_named_is_next
  (test_suite *test_suite,
   void *stream_state,
   struct modern_hash name);
extern void expect_stream_type_definition_universe
  (test_suite *test_suite,
   void *stream_state,
   uint64_t level);
extern void expect_stream_type_definition_satisfies_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_bool_false
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_bool_true
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_ordering_less
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_ordering_equal
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_ordering_greater
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_maybe_nothing
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_maybe_just_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_int8
  (test_suite *test_suite,
   void *stream_state,
   int8_t value);
extern void expect_stream_int16
  (test_suite *test_suite,
   void *stream_state,
   int16_t value);
extern void expect_stream_int32
  (test_suite *test_suite,
   void *stream_state,
   int32_t value);
extern void expect_stream_int64
  (test_suite *test_suite,
   void *stream_state,
   int64_t value);
extern void expect_stream_nat8
  (test_suite *test_suite,
   void *stream_state,
   uint8_t value);
extern void expect_stream_nat16
  (test_suite *test_suite,
   void *stream_state,
   uint16_t value);
extern void expect_stream_nat32
  (test_suite *test_suite,
   void *stream_state,
   uint32_t value);
extern void expect_stream_nat64
  (test_suite *test_suite,
   void *stream_state,
   uint64_t value);
extern void expect_stream_float32
  (test_suite *test_suite,
   void *stream_state,
   float value);
extern void expect_stream_float64
  (test_suite *test_suite,
   void *stream_state,
   double value);
extern void expect_stream_utf8_start
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_utf8_data
  (test_suite *test_suite,
   void *stream_state,
   uint8_t *data, size_t length);
extern void expect_stream_utf8_end
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_blob_start
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_blob_data
  (test_suite *test_suite,
   void *stream_state,
   uint8_t *data, size_t length);
extern void expect_stream_blob_end
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_sigma_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_named_value_is_next
  (test_suite *test_suite,
   void *stream_state,
   struct modern_hash *name);
extern void expect_stream_lambda_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_apply_is_next
  (test_suite *test_suite,
   void *stream_state);
extern void expect_stream_type_family_is_next
  (test_suite *test_suite,
   void *stream_state, uint64_t n_items);
extern void expect_stream_let_is_next
  (test_suite *test_suite,
   void *stream_state, uint64_t n_items);
extern void expect_stream_backreference
  (test_suite *test_suite,
   void *stream_state, uint64_t index);
extern void expect_stream_builtin
  (test_suite *test_suite,
   void *stream_state, uint16_t identifier);
extern void expect_stream_item_from_context
  (test_suite *test_suite,
   void *stream_state, struct modern_hash *hash);
extern void expect_stream_end
  (test_suite *test_suite,
   void *stream_state);

extern void flush_expectations(test_suite *test_suite);

extern struct modern_stream *test_stream_make(test_suite *test_suite);
extern void *test_stream_initialize(test_suite *test_suite);
