#include <setjmp.h>
#include <stdarg.h>
#include <string.h>
#include "modern.h"
#include "test.h"

enum callback_identifier {
    allocator_malloc_callback_identifier = 1,
    allocator_free_callback_identifier,
    allocator_realloc_callback_identifier,
    error_memory_callback_identifier,
    error_retain_count_overflow_callback_identifier,
    error_retain_count_underflow_callback_identifier,
    error_double_autorelease_callback_identifier,
    error_type_mismatch_callback_identifier,
    error_universe_level_overflow_callback_identifier,
    error_buffer_index_callback_identifier,
    error_not_applicable_callback_identifier,
    error_non_numeric_float_callback_identifier,
    library_finalizer_callback_identifier,
};


struct callback_invocation {
    enum callback_identifier identifier;
    unsigned completed : 1;
    unsigned succeeded : 1;
    unsigned expected : 1;
    union {
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
            void *retainable;
        } error_retain_count_overflow;
        struct {
            void *retainable;
        } error_retain_count_underflow;
        struct {
            void *retainable;
        } error_double_autorelease;
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
        } library_finalizer;
    } specifics;
};


struct callback_invocation_buffer {
    size_t count;
    size_t capacity;
    struct callback_invocation **callback_invocations;
};


struct test_case {
    char *name;
    unsigned completed : 1;
    unsigned succeeded : 1;
    struct callback_invocation_buffer actual_callbacks;
    struct callback_invocation_buffer normal_callback_pattern;
};


struct test_case_buffer {
    size_t count;
    size_t capacity;
    struct test_case **test_cases;
};


struct test_suite {
    struct test_case *current_test_case;
    struct callback_invocation *current_callback;
    jmp_buf jmp_buf;
    struct test_case_buffer test_cases;
    struct callback_invocation_buffer actual_callbacks;
    struct callback_invocation_buffer expected_callbacks;
};


extern void test_main
  (test_suite test_suite, modern_library *library);

static void *actually_malloc(size_t size);
static void actually_free(void *data);
static void *actually_realloc(void *data, size_t new_size);

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

static void print_test_case
  (struct test_case *test_case);

static void print_test_case_buffer
  (struct test_case_buffer *buffer);

static void print_callback_invocation
  (struct callback_invocation *callback_invocation);

static void print_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer);

static void *allocator_malloc
  (struct test_suite *test_suite, size_t size);
static void allocator_free
  (struct test_suite *test_suite, void *data);
static void *allocator_realloc
  (struct test_suite *test_suite, void *data, size_t size);
static void error_memory
  (struct test_suite *test_suite, size_t requested_size);
static void error_retain_count_overflow
  (struct test_suite *test_suite, void *retainable);
static void error_retain_count_underflow
  (struct test_suite *test_suite, void *retainable);
static void error_double_autorelease
  (struct test_suite *test_suite, void *retainable);
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
static void library_finalizer
  (struct test_suite *test_suite);


int main(int argc, char **argv) {
    struct modern_error_handler error_handler;
    error_handler.modern_error_handler_memory =
        (void (*)(void *, size_t)) error_memory;
    error_handler.modern_error_handler_retain_count_overflow =
        (void (*)(void *, void *)) error_retain_count_overflow;
    error_handler.modern_error_handler_retain_count_underflow =
        (void (*)(void *, void *)) error_retain_count_underflow;
    error_handler.modern_error_handler_double_autorelease =
        (void (*)(void *, void *)) error_double_autorelease;
    error_handler.modern_error_handler_type_mismatch =
        (void (*)(void *, modern *, modern *)) error_type_mismatch;
    error_handler.modern_error_handler_universe_level_overflow =
        (void (*)(void *)) error_universe_level_overflow;
    error_handler.modern_error_handler_buffer_index =
        (void (*)(void *)) error_buffer_index;
    error_handler.modern_error_handler_not_applicable =
        (void (*)(void *)) error_not_applicable;
    error_handler.modern_error_handler_non_numeric_float =
        (void (*)(void *)) error_non_numeric_float;
    
    struct modern_allocator allocator;
    allocator.modern_allocator_alloc =
        (void *(*)(void *, size_t)) allocator_malloc;
    allocator.modern_allocator_free =
        (void (*)(void *, void *)) allocator_free;
    allocator.modern_allocator_realloc =
        (void *(*)(void *, void *, size_t)) allocator_realloc;
    
    struct test_suite *test_suite =
        actually_malloc(sizeof(struct test_suite));
    initialize_test_suite(test_suite);
    
    if(!setjmp(test_suite->jmp_buf)) {
        modern_library *library = modern_library_initialize
            (&error_handler,
             &allocator,
             (void (*)(void *)) library_finalizer,
             test_suite);
        
        if(!setjmp(test_suite->jmp_buf)) {
            test_main(test_suite, library);
            
            if(test_suite->current_test_case) {
                printf("\n\n"
                       "*** The testing infrastructure itself failed.\n"
                       "*** Specifically, missing an end_test_case().\n");
                exit(1);
            }
            
            if(!setjmp(test_suite->jmp_buf)) {
                modern_library_finalize(library);
            } else {
                printf("\n\n"
                       "*** Unexpected behavior during library finalize.\n");
                exit(1);
            }
        } else {
            printf("\n\n"
                   "*** The testing infrastructure itself failed.\n"
                   "*** Specifically, there were Modern Data calls not bracketed by\n"
                   "*** begin_test_case() / end_test_case().\n");
            exit(1);
        }
    } else {
        printf("\n\n"
               "*** Unexpected behavior during library initialize.\n"
               "*** That means the remainder of the test suite can't be run.\n");
        exit(1);
    }
        
    finalize_test_suite(test_suite);
    actually_free(test_suite);
    
    return 0;
}


int begin_test_case(test_suite test_suite_in, char *format, ...) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to begin_test_case() "
               "when already in one.\n");
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
    
    actually_free(name);
    
    if(!setjmp(test_suite->jmp_buf)) {
        return 1;
    } else {
        return 0;
    }
}


void end_test_case(test_suite test_suite_in, int succeeded) {
    struct test_suite *test_suite = (struct test_suite *) test_suite_in;
    
    if(!test_suite->current_test_case) {
        printf("\n\n"
               "*** The testing infrastructure itself failed.\n"
               "*** Specifically, tried to end_test_case() "
               "when not in one.\n");
        exit(1);
    }
    
    if(test_suite->current_test_case->succeeded) {
        test_suite->current_test_case->succeeded = succeeded;
    }
    
    test_suite->current_test_case->completed = 1;
    
    test_suite->current_test_case = NULL;
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


static void initialize_test_suite
  (struct test_suite *test_suite)
{
    test_suite->current_test_case = NULL;
    initialize_test_case_buffer(&test_suite->test_cases);
    initialize_callback_invocation_buffer(&test_suite->actual_callbacks);
    initialize_callback_invocation_buffer(&test_suite->expected_callbacks);
}


static void finalize_test_suite
  (struct test_suite *test_suite)
{
    finalize_test_case_buffer(&test_suite->test_cases);
    finalize_callback_invocation_buffer(&test_suite->actual_callbacks);
    finalize_callback_invocation_buffer(&test_suite->expected_callbacks);
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
    initialize_callback_invocation_buffer(&test_case->normal_callback_pattern);
}


static void finalize_test_case
  (struct test_case *test_case)
{
    actually_free(test_case->name);
    finalize_callback_invocation_buffer(&test_case->actual_callbacks);
    finalize_callback_invocation_buffer(&test_case->normal_callback_pattern);
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


static void initialize_callback_invocation
  (struct callback_invocation *callback_invocation)
{
    callback_invocation->identifier = 0;
    callback_invocation->completed = 0;
    callback_invocation->succeeded = 1;
    callback_invocation->expected = 0;
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
  (struct callback_invocation *callback_invocation)
{
    // TODO
}


static void print_callback_invocation_buffer
  (struct callback_invocation_buffer *buffer)
{
    // TODO
}


static struct callback_invocation *begin_callback
  (struct test_suite *test_suite)
{
    // TODO
    // insert this callback into the appropriate list of actual callbacks
    return NULL;
}


static int callback_should_succeed
  (struct test_suite *test_suite)
{
    struct callback_invocation *actual = test_suite->current_callback;
    
    size_t expectation_index;
    for(expectation_index = 0;
        expectation_index < test_suite->expected_callbacks.count;
        expectation_index++)
    {
        struct callback_invocation *expected =
            test_suite->expected_callbacks.callback_invocations
                [expectation_index];
        if(expected->identifier == actual->identifier)
        {
            int matches = 0;
            switch(expected->identifier) {
            default: matches = 1;
            }
            if(matches) break;
        }
    }
    int expectation_found =
        expectation_index < test_suite->expected_callbacks.count;
    
    test_suite->current_callback = NULL;
    
    if(expectation_found) return 1;
    else longjmp(test_suite->jmp_buf, 1);
}


static void *allocator_malloc(struct test_suite *test_suite, size_t size) {
    struct callback_invocation *invocation = begin_callback(test_suite);
    
    // TODO
    // fill out the parameters in the given invocation
    
    if(callback_should_succeed(test_suite)) return actually_malloc(size);
    else return NULL;
}


static void allocator_free(struct test_suite *test_suite, void *data) {
}


static void *allocator_realloc(struct test_suite *test_suite, void *data, size_t size) {
    return NULL;
}


static void error_memory
  (struct test_suite *test_suite, size_t requested_size)
{
    fprintf(stderr, "Unable to allocate %llu bytes.\n",
            (unsigned long long) requested_size);
    exit(1);
}


static void error_retain_count_overflow
  (struct test_suite *test_suite, void *retainable)
{
    fprintf(stderr, "Retain count overflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_retain_count_underflow
  (struct test_suite *test_suite, void *retainable)
{
    fprintf(stderr, "Retain count underflow on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_double_autorelease
  (struct test_suite *test_suite, void *retainable)
{
    fprintf(stderr, "Double autorelease on object at 0x%llx.\n",
            (unsigned long long) retainable);
    exit(1);
}


static void error_type_mismatch
  (struct test_suite *test_suite, modern *expected, modern *actual)
{
    fprintf(stderr, "Type mismatch.  Expected 0x%llx; got 0x%llx.\n",
            (unsigned long long) expected, (unsigned long long) actual);
    exit(1);
}


static void error_universe_level_overflow
  (struct test_suite *test_suite)
{
    fprintf(stderr, "Universe level overflow.\n");
    exit(1);
}


static void error_buffer_index
  (struct test_suite *test_suite)
{
    fprintf(stderr, "Buffer index out of range.\n");
    exit(1);
}


static void error_not_applicable
  (struct test_suite *test_suite)
{
    fprintf(stderr, "Not applicable.\n");
    exit(1);
}


static void error_non_numeric_float
  (struct test_suite *test_suite)
{
    fprintf(stderr, "Non-numeric float.\n");
    exit(1);
}


static void library_finalizer
  (struct test_suite *test_suite)
{
}
