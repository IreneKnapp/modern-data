#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static

typedef void test_suite;

extern void test_main
  (test_suite *test_suite, modern_library *library);

extern int begin_fixtures(test_suite *test_suite);
extern void end_fixtures(test_suite *test_suite);

extern void begin_test_case
  (test_suite *test_suite,
   int (*test_case)(void *test_context),
   void *test_context,
   char *format, ...);

extern void reset_allowances(test_suite *test_suite);

extern void allow_allocation(test_suite *test_suite);
extern void disallow_allocation(test_suite *test_suite);

extern void allow_deallocation(test_suite *test_suite);
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
