#define KEYWORD_COUNT 54
#define KEYWORD_MODULUS 357


struct keyword {
    uint8_t *text;
    void (*emit)
         (struct processor_explicatory_state *process_state,
          struct modern_stream *stream, void *stream_state,
          struct modern_vfile *vfile, void *vfile_state);
};


int8_t keyword_table1[KEYWORD_MODULUS] = {
    -1, -1, -1, -1, -1, -1, 52, -1, -1, -1, 21, 45, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 34, -1, -1, -1,
    -1, -1, 3, 27, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, 10, -1, -1, 32, -1, 53, -1, 17, -1, -1, -1,
    -1, -1, -1, -1, 46, -1, -1, -1, -1, -1, 31, -1, 47, 24, -1, 6,
    -1, -1, -1, -1, -1, -1, 29, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, 25, -1, -1, 42, -1, -1, -1, -1, -1, -1, 8, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, 15, 40, -1, -1, 49, -1, -1, 30, -1, -1, -1, 7,
    -1, -1, 11, -1, -1, -1, -1, -1, 5, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 2, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 50, -1, -1, 22, -1, -1, -1, -1, -1,
    -1, -1, 36, -1, -1, -1, -1, -1, -1, -1, -1, 14, -1, -1, -1, 18,
    -1, -1, -1, -1, 39, -1, 4, -1, -1, -1, -1, -1, -1, 12, -1, 1,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, 37, -1, -1, -1, 48, -1, -1, 51, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, -1, 44, 16, -1, -1, -1, -1, -1, 23, -1,
    35, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1, -1, 38, -1, -1, -1, -1, -1, -1, -1, 28, 9,
    -1, -1, -1, -1, -1, 13, 33, 41, -1, -1, 43, -1, -1, -1, 26, 20,
    -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 19, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1,
};


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


struct keyword keyword_table2[KEYWORD_COUNT] = {
    { (uint8_t *) "name_definition", emit_name_definition },
    { (uint8_t *) "value_definition", emit_value_definition },
    { (uint8_t *) "type_definition_bool", emit_type_definition_bool },
    { (uint8_t *) "type_definition_ordering", emit_type_definition_ordering },
    { (uint8_t *) "type_definition_maybe", emit_type_definition_maybe },
    { (uint8_t *) "type_definition_int8", emit_type_definition_int8 },
    { (uint8_t *) "type_definition_int16", emit_type_definition_int16 },
    { (uint8_t *) "type_definition_int32", emit_type_definition_int32 },
    { (uint8_t *) "type_definition_int64", emit_type_definition_int64 },
    { (uint8_t *) "type_definition_nat8", emit_type_definition_nat8 },
    { (uint8_t *) "type_definition_nat16", emit_type_definition_nat16 },
    { (uint8_t *) "type_definition_nat32", emit_type_definition_nat32 },
    { (uint8_t *) "type_definition_nat64", emit_type_definition_nat64 },
    { (uint8_t *) "type_definition_float32", emit_type_definition_float32 },
    { (uint8_t *) "type_definition_float64", emit_type_definition_float64 },
    { (uint8_t *) "type_definition_utf8", emit_type_definition_utf8 },
    { (uint8_t *) "type_definition_blob", emit_type_definition_blob },
    { (uint8_t *) "type_definition_function", emit_type_definition_function },
    { (uint8_t *) "type_definition_sigma", emit_type_definition_sigma },
    { (uint8_t *) "type_definition_named", emit_type_definition_named },
    { (uint8_t *) "type_definition_universe", emit_type_definition_universe },
    { (uint8_t *) "type_definition_satisfies", emit_type_definition_satisfies },
    { (uint8_t *) "bool_false", emit_bool_false },
    { (uint8_t *) "bool_true", emit_bool_true },
    { (uint8_t *) "ordering_less", emit_ordering_less },
    { (uint8_t *) "ordering_equal", emit_ordering_equal },
    { (uint8_t *) "ordering_greater", emit_ordering_greater },
    { (uint8_t *) "maybe_nothing", emit_maybe_nothing },
    { (uint8_t *) "maybe_just", emit_maybe_just },
    { (uint8_t *) "int8", emit_int8 },
    { (uint8_t *) "int16", emit_int16 },
    { (uint8_t *) "int32", emit_int32 },
    { (uint8_t *) "int64", emit_int64 },
    { (uint8_t *) "nat8", emit_nat8 },
    { (uint8_t *) "nat16", emit_nat16 },
    { (uint8_t *) "nat32", emit_nat32 },
    { (uint8_t *) "nat64", emit_nat64 },
    { (uint8_t *) "float32", emit_float32 },
    { (uint8_t *) "float64", emit_float64 },
    { (uint8_t *) "utf8_start", emit_utf8_start },
    { (uint8_t *) "utf8_data", emit_utf8_data },
    { (uint8_t *) "utf8_end", emit_utf8_end },
    { (uint8_t *) "blob_start", emit_blob_start },
    { (uint8_t *) "blob_data", emit_blob_data },
    { (uint8_t *) "blob_end", emit_blob_end },
    { (uint8_t *) "sigma", emit_sigma },
    { (uint8_t *) "named_value", emit_named_value },
    { (uint8_t *) "lambda", emit_lambda },
    { (uint8_t *) "apply", emit_apply },
    { (uint8_t *) "type_family", emit_type_family },
    { (uint8_t *) "let", emit_let },
    { (uint8_t *) "backreference", emit_backreference },
    { (uint8_t *) "builtin", emit_builtin },
    { (uint8_t *) "item_from_context", emit_item_from_context },
};


HELPER void (*get_keyword(uint8_t *text, size_t length))
    (struct processor_explicatory_state *process_state,
     struct modern_stream *stream, void *stream_state,
     struct modern_vfile *vfile, void *vfile_state)
{
    struct modern_hash hash;
    modern_hash_compute(text, length, &hash);
    size_t bucket = keyword_table1[hash.a % KEYWORD_MODULUS];
    if(bucket == -1) return NULL;
    struct keyword *keyword = &keyword_table2[bucket];
    if(strncmp((char *) keyword->text, (char *) text, length))
        return NULL;
    return keyword->emit;
}
