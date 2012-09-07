#define INTERNAL __attribute__ ((visibility ("hidden")))
#define HELPER static

typedef void *test_suite;


extern int begin_test_case
  (test_suite test_suite, char *format, ...);
extern void end_test_case
  (test_suite test_suite, int succeeded);
