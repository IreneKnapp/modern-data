#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static

typedef void *test_suite;


extern int begin_fixtures(test_suite test_suite);
extern void end_fixtures(test_suite test_suite);

extern void begin_test_case
  (test_suite test_suite,
   int (*test_case)(void *test_context),
   void *test_context,
   char *format, ...);

extern void reset_allowances(test_suite test_suite);

extern void allow_allocation(test_suite test_suite);
extern void disallow_allocation(test_suite test_suite);

extern void allow_deallocation(test_suite test_suite);
extern void disallow_deallocation(test_suite test_suite);
