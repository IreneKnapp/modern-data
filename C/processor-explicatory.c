#include <ctype.h>
#include <string.h>
#include "modern.h"
#include "internal.h"


struct raw_keyword {
    uint8_t *text;
    void (*emit)
         (struct processor_explicatory_state *process_state,
          struct modern_stream *stream, void *stream_state,
          struct modern_vfile *vfile, void *vfile_state);
};


HELPER void debug_trie
    (int level, struct processor_explicatory_keyword_tree *node);
HELPER int keyword_insert
    (struct modern_library *library,
     uint8_t *text,
     void (*emit)
          (struct processor_explicatory_state *process_state,
           struct modern_stream *stream, void *stream_state,
           struct modern_vfile *vfile, void *vfile_state));
HELPER struct processor_explicatory_keyword_tree *keyword_lookup_node
    (uint8_t **text, struct processor_explicatory_keyword_tree ***link);
HELPER void *processor_explicatory_initialize(modern_library *library);
HELPER void processor_explicatory_finalize
    (modern_library *library_in, void *process_state);
HELPER void processor_explicatory_abort(void *process_state);
HELPER void processor_explicatory_flush(void *process_state);
HELPER void processor_explicatory_step
    (void *process_state,
     struct modern_stream *stream, void *stream_state,
     struct modern_vfile *vfile, void *vfile_state);
HELPER void processor_explicatory_run
    (void *process_state,
     struct modern_stream *stream, void *stream_state,
     struct modern_vfile *vfile, void *vfile_state);
HELPER void processor_explicatory_extend_buffer
    (struct processor_explicatory_state *process_state,
     size_t desired_size);
HELPER void emit_name_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_value_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_bool
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_ordering
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_maybe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_utf8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_blob
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_function
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_named
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_universe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_definition_satisfies
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_bool_false
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_bool_true
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_ordering_less
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_ordering_equal
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_ordering_greater
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_maybe_nothing
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_maybe_just
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_utf8_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_utf8_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_utf8_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_blob_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_blob_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_blob_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_named_value
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_lambda
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_apply
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_type_family
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_let
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_backreference
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_builtin
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);
HELPER void emit_item_from_context
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state);


INTERNAL void
    initialize_processor_explicatory
    (struct modern_library *library)
{
    library->processor_explicatory_keywords = NULL;
    
    struct raw_keyword raw_keywords[] = {
        { (uint8_t *) "name_definition",
          emit_name_definition },
        { (uint8_t *) "value_definition",
          emit_value_definition },
        { (uint8_t *) "type_definition_bool",
          emit_type_definition_bool },
        { (uint8_t *) "type_definition_ordering",
          emit_type_definition_ordering },
        { (uint8_t *) "type_definition_maybe",
          emit_type_definition_maybe },
        { (uint8_t *) "type_definition_int8",
          emit_type_definition_int8 },
        { (uint8_t *) "type_definition_int16",
          emit_type_definition_int16 },
        { (uint8_t *) "type_definition_int32",
          emit_type_definition_int32 },
        { (uint8_t *) "type_definition_int64",
          emit_type_definition_int64 },
        { (uint8_t *) "type_definition_nat8",
          emit_type_definition_nat8 },
        { (uint8_t *) "type_definition_nat16",
          emit_type_definition_nat16 },
        { (uint8_t *) "type_definition_nat32",
          emit_type_definition_nat32 },
        { (uint8_t *) "type_definition_nat64",
          emit_type_definition_nat64 },
        { (uint8_t *) "type_definition_float32",
          emit_type_definition_float32 },
        { (uint8_t *) "type_definition_float64",
          emit_type_definition_float64 },
        { (uint8_t *) "type_definition_utf8",
          emit_type_definition_utf8 },
        { (uint8_t *) "type_definition_blob",
          emit_type_definition_blob },
        { (uint8_t *) "type_definition_function",
          emit_type_definition_function },
        { (uint8_t *) "type_definition_sigma",
          emit_type_definition_sigma },
        { (uint8_t *) "type_definition_named",
          emit_type_definition_named },
        { (uint8_t *) "type_definition_universe",
          emit_type_definition_universe },
        { (uint8_t *) "type_definition_satisfies",
          emit_type_definition_satisfies },
        { (uint8_t *) "bool_false",
          emit_bool_false },
        { (uint8_t *) "bool_true",
          emit_bool_true },
        { (uint8_t *) "ordering_less",
          emit_ordering_less },
        { (uint8_t *) "ordering_equal",
          emit_ordering_equal },
        { (uint8_t *) "ordering_greater",
          emit_ordering_greater },
        { (uint8_t *) "maybe_nothing",
          emit_maybe_nothing },
        { (uint8_t *) "maybe_just",
          emit_maybe_just },
        { (uint8_t *) "int8",
          emit_int8 },
        { (uint8_t *) "int16",
          emit_int16 },
        { (uint8_t *) "int32",
          emit_int32 },
        { (uint8_t *) "int64",
          emit_int64 },
        { (uint8_t *) "nat8",
          emit_nat8 },
        { (uint8_t *) "nat16",
          emit_nat16 },
        { (uint8_t *) "nat32",
          emit_nat32 },
        { (uint8_t *) "nat64",
          emit_nat64 },
        { (uint8_t *) "float32",
          emit_float32 },
        { (uint8_t *) "float64",
          emit_float64 },
        { (uint8_t *) "utf8_start",
          emit_utf8_start },
        { (uint8_t *) "utf8_data",
          emit_utf8_data },
        { (uint8_t *) "utf8_end",
          emit_utf8_end },
        { (uint8_t *) "blob_start",
          emit_blob_start },
        { (uint8_t *) "blob_data",
          emit_blob_data },
        { (uint8_t *) "blob_end",
          emit_blob_end },
        { (uint8_t *) "sigma",
          emit_sigma },
        { (uint8_t *) "named_value",
          emit_named_value },
        { (uint8_t *) "lambda",
          emit_lambda },
        { (uint8_t *) "apply",
          emit_apply },
        { (uint8_t *) "type_family",
          emit_type_family },
        { (uint8_t *) "let",
          emit_let },
        { (uint8_t *) "backreference",
          emit_backreference },
        { (uint8_t *) "builtin",
          emit_builtin },
        { (uint8_t *) "item_from_context",
          emit_item_from_context },
        { NULL, NULL },
    };
    
    for(struct raw_keyword *raw_keyword = raw_keywords;
        raw_keyword->text;
        raw_keyword++)
    {
        if(!keyword_insert(library, raw_keyword->text, raw_keyword->emit))
            return;
    }
    debug_trie(0, library->processor_explicatory_keywords);
}


HELPER void debug_trie
    (int level, struct processor_explicatory_keyword_tree *node)
{
    if(!node) {
        for(int i = 0; i < level; i++) printf("    ");
        printf("NULL\n");
        return;
    } if(node->is_leaf) {
        for(int i = 0; i < level; i++) printf("    ");
        printf("0x%016llx\n",
               (unsigned long long) node->specifics.leaf_node.emit);
    } else {
        for(uint8_t i = 0; i < node->specifics.internal_node.edge_count; i++) {
            for(int i = 0; i < level; i++) printf("    ");
            printf("%s\n", node->specifics.internal_node.edges[i].text);
            debug_trie(level + 1, node->specifics.internal_node.edges[i].node);
        }
    }
}


INTERNAL void
    finalize_processor_explicatory
    (struct modern_library *library)
{
    // TODO
}


HELPER int keyword_insert
    (struct modern_library *library,
     uint8_t *text,
     void (*emit)
          (struct processor_explicatory_state *process_state,
           struct modern_stream *stream, void *stream_state,
           struct modern_vfile *vfile, void *vfile_state))
{
    struct processor_explicatory_keyword_tree **link =
        &library->processor_explicatory_keywords;
    keyword_lookup_node(&text, &link);
    if(*link && (*link)->is_leaf) return 1;
    
    if(*link
       && ((*link)->specifics.internal_node.edge_count
           >= PROCESSOR_EXPLICATORY_KEYWORD_TREE_EDGES_PER_NODE))
    {
       printf("OOPS!\n");
       return 0;
    }
    
    struct processor_explicatory_keyword_tree *leaf;
    {
        size_t size = sizeof(struct processor_explicatory_keyword_tree);
        leaf = library->allocator->alloc(library->client_state, size);
        if(!leaf) {
            library->error_handler->memory(library->client_state, size);
            return 0;
        }
    }
    
    leaf->is_leaf = 1;
    leaf->specifics.leaf_node.emit = emit;
    
    if(!*link) {
        struct processor_explicatory_keyword_tree *parent;
        {
            size_t size = sizeof(struct processor_explicatory_keyword_tree);
            parent = library->allocator->alloc(library->client_state, size);
            if(!parent) {
                library->allocator->free(library->client_state, leaf);
                library->error_handler->memory(library->client_state, size);
                return 0;
            }
        }
        
        parent->is_leaf = 0;
        parent->specifics.internal_node.edge_count = 0;
        
        *link = parent;
    }
    
    struct processor_explicatory_keyword_tree_edge *edge = NULL;
    for(uint8_t i = 0;
        i < (*link)->specifics.internal_node.edge_count;
        i++)
    {
        if(text[0] == (*link)->specifics.internal_node.edges[i].text[0]) {
            edge = &(*link)->specifics.internal_node.edges[i];
            
            for(uint8_t j = 0;
                text[j] && edge->text[j] && (text[j] == edge->text[j]);
                j++);
            if(edge->text[j]) {
                size_t size = strlen(edge->text + j) + 1;
                // TODO
            }
            
            break;
        }
    }
    if(!edge) {
        edge = &(*link)->specifics.internal_node.edges
            [(*link)->specifics.internal_node.edge_count];
        (*link)->specifics.internal_node.edge_count++;
        
        edge->text = text;
        edge->node = leaf;
    }
    
    return 1;
}


HELPER struct processor_explicatory_keyword_tree *keyword_lookup_node
    (uint8_t **text_inout,
     struct processor_explicatory_keyword_tree ***link_inout)
{
    uint8_t *text = *text_inout;
    struct processor_explicatory_keyword_tree **link = *link_inout;
    while(*link) {
        if((*link)->is_leaf) break;
        
        struct processor_explicatory_keyword_tree_edge *edge = NULL;
        for(uint8_t i = 0;
            i < (*link)->specifics.internal_node.edge_count;
            i++)
        {
            if(!strcmp
                ((char *) text,
                 (char *) (*link)->specifics.internal_node.edges[i].text))
            {
                edge = &(*link)->specifics.internal_node.edges[i];
                break;
            }
        }
        if(!edge) break;
        
        text += strlen((char *) edge->text);
        link = &edge->node;
    }
    *text_inout = text;
    *link_inout = link;
    return *link;
}


PUBLIC struct modern_processor *modern_processor_explicatory_make
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
    
    state->process.abort = processor_explicatory_abort;
    state->process.flush = processor_explicatory_flush;
    state->library = library;
    state->started = 0;
    state->ended = 0;
    state->aborted = 0;
    
    return state;
}


HELPER void processor_explicatory_finalize
    (modern_library *library_in, void *process_state_in)
{
    struct modern_library *library = (struct modern_library *) library_in;
    
    library->allocator->free(library->client_state, process_state_in);
}


HELPER void processor_explicatory_abort(void *process_state)
{
}


HELPER void processor_explicatory_flush(void *process_state)
{
}


HELPER void processor_explicatory_step
    (void *process_state_in,
     struct modern_stream *stream, void *stream_state,
     struct modern_vfile *vfile, void *vfile_state)
{
    struct processor_explicatory_state *process_state =
        (struct processor_explicatory_state *) process_state_in;
    struct modern_library *library = process_state->library;
    
    process_state->aborted = 0;
    
    if(!process_state->started) {
        process_state->started = 1;
        process_state->buffer_length = 0;
        
        stream->start(&process_state->process, process_state_in, stream_state);
    } else {
        while(1) {
            if(!process_state->buffer_length) {
                size_t bytes_read = vfile->read
                    (vfile_state,
                     process_state->buffer, sizeof(process_state->buffer));
                
                if(!bytes_read) {
                    if(!process_state->ended) {
                        process_state->ended = 1;
                        
                        stream->end(&process_state->process, process_state_in,
                                    stream_state);
                    }
                    
                    break;
                } else {
                    process_state->buffer_length += bytes_read;
                }
            }
            
            if(process_state->buffer_length) {
                uint8_t c = process_state->buffer[0];
                if(isspace(c)) {
                    process_state->buffer_length--;
                    if(process_state->buffer_length) {
                        memmove(&process_state->buffer[0],
                                &process_state->buffer[1],
                                process_state->buffer_length);
                    }
                } else if(isalpha(c) || isdigit(c) || (c == '_')) {
                    size_t token_length = 1;
                    while(token_length < process_state->buffer_length) {
                        c = process_state->buffer[token_length];
                        printf("'%c'\n", c);
                        if(!(isalpha(c) || isdigit(c) || (c == '_')))
                            break;
                        token_length++;
                    }
                }
            }
        }
    }
}


HELPER void processor_explicatory_run
    (void *process_state_in,
     struct modern_stream *stream, void *stream_state,
     struct modern_vfile *vfile, void *vfile_state)
{
    struct processor_explicatory_state *process_state =
        (struct processor_explicatory_state *) process_state_in;
    
    process_state->aborted = 0;
    
    while(!process_state->ended && !process_state->aborted)
    {
        processor_explicatory_step
            (process_state_in, stream, stream_state, vfile, vfile_state);
    }
}


HELPER void processor_explicatory_extend_buffer
    (struct processor_explicatory_state *process_state,
     size_t desired_size)
{
}


HELPER void emit_name_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_value_definition
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_bool
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_ordering
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_maybe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_utf8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_blob
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_function
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_named
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_universe
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_definition_satisfies
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_bool_false
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_bool_true
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_less
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_equal
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_ordering_greater
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_maybe_nothing
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_maybe_just
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_int64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat8
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat16
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_nat64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_float32
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_float64
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_utf8_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_start
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_data
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_blob_end
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_sigma
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_named_value
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_lambda
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_apply
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_type_family
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_let
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_backreference
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_builtin
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}


HELPER void emit_item_from_context
     (struct processor_explicatory_state *process_state,
      struct modern_stream *stream, void *stream_state,
      struct modern_vfile *vfile, void *vfile_state)
{
}

