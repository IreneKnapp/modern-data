#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "modern.h"


uint8_t *raw_keywords[] = {
    (uint8_t *) "name_definition",
    (uint8_t *) "value_definition",
    (uint8_t *) "type_definition_bool",
    (uint8_t *) "type_definition_ordering",
    (uint8_t *) "type_definition_maybe",
    (uint8_t *) "type_definition_int8",
    (uint8_t *) "type_definition_int16",
    (uint8_t *) "type_definition_int32",
    (uint8_t *) "type_definition_int64",
    (uint8_t *) "type_definition_nat8",
    (uint8_t *) "type_definition_nat16",
    (uint8_t *) "type_definition_nat32",
    (uint8_t *) "type_definition_nat64",
    (uint8_t *) "type_definition_float32",
    (uint8_t *) "type_definition_float64",
    (uint8_t *) "type_definition_utf8",
    (uint8_t *) "type_definition_blob",
    (uint8_t *) "type_definition_function",
    (uint8_t *) "type_definition_sigma",
    (uint8_t *) "type_definition_named",
    (uint8_t *) "type_definition_universe",
    (uint8_t *) "type_definition_satisfies",
    (uint8_t *) "bool_false",
    (uint8_t *) "bool_true",
    (uint8_t *) "ordering_less",
    (uint8_t *) "ordering_equal",
    (uint8_t *) "ordering_greater",
    (uint8_t *) "maybe_nothing",
    (uint8_t *) "maybe_just",
    (uint8_t *) "int8",
    (uint8_t *) "int16",
    (uint8_t *) "int32",
    (uint8_t *) "int64",
    (uint8_t *) "nat8",
    (uint8_t *) "nat16",
    (uint8_t *) "nat32",
    (uint8_t *) "nat64",
    (uint8_t *) "float32",
    (uint8_t *) "float64",
    (uint8_t *) "utf8_start",
    (uint8_t *) "utf8_data",
    (uint8_t *) "utf8_end",
    (uint8_t *) "blob_start",
    (uint8_t *) "blob_data",
    (uint8_t *) "blob_end",
    (uint8_t *) "sigma",
    (uint8_t *) "named_value",
    (uint8_t *) "lambda",
    (uint8_t *) "apply",
    (uint8_t *) "type_family",
    (uint8_t *) "let",
    (uint8_t *) "backreference",
    (uint8_t *) "builtin",
    (uint8_t *) "item_from_context",
    NULL
};


struct keyword {
    size_t index;
    uint8_t *text;
};


struct bucket {
    size_t count;
    struct keyword **keywords;
};


int main(int argc, char **argv) {
    if(argc != 1) {
        fprintf(stderr, "Usage: make-keywords\n");
        return 1;
    }
    
    size_t keyword_count = 0;
    for(uint8_t **point = raw_keywords; *point; point++) keyword_count++;
    
    struct keyword **keywords =
        malloc(sizeof(struct keyword *) * keyword_count);
    for(size_t i = 0; i < keyword_count; i++) {
        keywords[i] = malloc(sizeof(struct keyword));
        keywords[i]->text = raw_keywords[i];
        keywords[i]->index = i;
    }
    
    size_t bucket_count;
    struct bucket **buckets;
    for(bucket_count = keyword_count;
        bucket_count < 2048;
        bucket_count++)
    {
        buckets = malloc(sizeof(struct bucket *) * bucket_count);
        for(size_t i = 0; i < bucket_count; i++) {
            buckets[i] = malloc(sizeof(struct bucket));
            buckets[i]->count = 0;
            buckets[i]->keywords = NULL;
        }
        
        for(size_t i = 0; i < keyword_count; i++) {
            struct modern_hash hash;
            modern_hash_compute
                (keywords[i]->text, strlen((char *) keywords[i]->text), &hash);
            size_t j = hash.a % bucket_count;
            buckets[j]->count++;
            buckets[j]->keywords = realloc
                (buckets[j]->keywords,
                 sizeof(struct keyword *) * buckets[j]->count);
            buckets[j]->keywords[buckets[j]->count - 1] = keywords[i];
        }

        size_t i;
        for(i = 0; i < bucket_count; i++) {
            if(buckets[i]->count > 1) break;
        }
        if(i == bucket_count) break;

        for(size_t i = 0; i < bucket_count; i++) {
            free(buckets[i]->keywords);
        }
        free(buckets);
    }
    
    printf("#define KEYWORD_COUNT %i\n", (int) keyword_count);
    printf("#define KEYWORD_MODULUS %i\n", (int) bucket_count);
    printf("\n\n");
    printf("struct keyword {\n");
    printf("    uint8_t *text;\n");
    printf("    void (*emit)\n");
    printf("         (struct processor_explicatory_state *process_state,\n");
    printf("          struct modern_stream *stream, void *stream_state,\n");
    printf("          struct modern_vfile *vfile, void *vfile_state);\n");
    printf("};\n");
    printf("\n\n");
    printf("int8_t keyword_table1[KEYWORD_MODULUS] = {\n");
    for(size_t i = 0; i < bucket_count; i++) {
        if(i % 16 == 0) printf("    ");
        else printf(" ");
        printf("%i,",
               buckets[i]->count ? (int) buckets[i]->keywords[0]->index : -1);
        if((i % 16 == 15) || (i + 1 == bucket_count)) printf("\n");
    }
    printf("};\n");
    printf("\n\n");
    for(size_t i = 0; i < keyword_count; i++) {
        printf("HELPER void emit_%s\n", (char *) keywords[i]->text);
        printf("     (struct processor_explicatory_state *process_state,\n");
        printf("      struct modern_stream *stream, void *stream_state,\n");
        printf("      struct modern_vfile *vfile, void *vfile_state);\n");
    }
    printf("\n\n");
    printf("struct keyword keyword_table2[KEYWORD_COUNT] = {\n");
    for(size_t i = 0; i < keyword_count; i++) {
        printf("    { (uint8_t *) \"%s\", emit_%s },\n",
               (char *) keywords[i]->text, (char *) keywords[i]->text);
    }
    printf("};\n");
    printf("\n\n");
    printf("HELPER void (*get_keyword(uint8_t *text, size_t length))\n");
    printf("    (struct processor_explicatory_state *process_state,\n");
    printf("     struct modern_stream *stream, void *stream_state,\n");
    printf("     struct modern_vfile *vfile, void *vfile_state)\n");
    printf("{\n");
    printf("    struct modern_hash hash;\n");
    printf("    modern_hash_compute(text, length, &hash);\n");
    printf("    size_t bucket = keyword_table1[hash.a %% KEYWORD_MODULUS];\n");
    printf("    if(bucket == -1) return NULL;\n");
    printf("    struct keyword *keyword = &keyword_table2[bucket];\n");
    printf("    if(strncmp((char *) keyword->text, (char *) text, length))\n");
    printf("        return NULL;\n");
    printf("    return keyword->emit;\n");
    printf("}\n");
    
    return 0;
}

