#include "modern.h"
#include "internal.h"


struct processor_explicatory_state {
    struct modern_library *library;
};


HELPER void *processor_explicatory_initialize(modern_library *library);
HELPER void processor_explicatory_finalize(void *process_state);
HELPER void processor_explicatory_step
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state);
HELPER void processor_explicatory_run
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state);


struct modern_processor *modern_processor_explicatory_make
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct modern_processor);
    struct modern_processor *processor =
        library->allocator->alloc(library->client_state, size);
    if(!processor) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }
    
    processor->initialize = processor_explicatory_initialize;
    processor->finalize = processor_explicatory_finalize;
    processor->step = processor_explicatory_step;
    processor->run = processor_explicatory_run;
    
    return processor;
}


HELPER void *processor_explicatory_initialize
  (modern_library *library_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    size_t size = sizeof(struct processor_explicatory_state);
    struct processor_explicatory_state *state =
        library->allocator->alloc(library->client_state, size);
    if(!state) {
        library->error_handler->memory(library->client_state, size);
        return NULL;
    }

    state->library = library;
    
    return state;
}


HELPER void processor_explicatory_finalize(void *process_state_in)
{
    struct processor_explicatory_state *state =
        (struct processor_explicatory_state *) process_state_in;
    struct modern_library *library = state->library;
    
    library->allocator->free(library->client_state, state);
}


HELPER void processor_explicatory_step
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void processor_explicatory_run
  (void *process_state,
   struct modern_stream *stream, void *stream_state,
   struct modern_vfile *vfile, void *vfile_state)
{
}

