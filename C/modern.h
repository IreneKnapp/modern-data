#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


typedef void modern;
typedef void modern_context;
typedef void modern_autorelease_pool;
typedef void modern_library;
typedef float modern_float32;
typedef double modern_float64;


struct modern_hash {
    uint64_t a;
    uint64_t b;
};


struct modern_error_handler {
    void (*modern_error_handler_memory)
      (void *client_state, size_t requested_size);
    void (*modern_error_handler_retain_count_overflow)
      (void *client_state, void *retainable);
    void (*modern_error_handler_retain_count_underflow)
      (void *client_state, void *retainable);
    void (*modern_error_handler_double_autorelease)
      (void *client_state, void *retainable);
    void (*modern_error_handler_type_mismatch)
      (void *client_state, modern *expected, modern *actual);
    void (*modern_error_handler_universe_level_overflow)
      (void *client_state);
    void (*modern_error_handler_buffer_index)
      (void *client_state);
    void (*modern_error_handler_not_applicable)
      (void *client_state);
    void (*modern_error_handler_non_numeric_float)
      (void *client_state);
    void (*modern_error_handler_immutable)
      (void *client_state, modern *node);
    void (*modern_error_handler_builtin_identifier)
      (void *client_state, uint16_t identifier);
 };


struct modern_allocator {
    void *(*modern_allocator_alloc)
      (void *client_state, size_t size);
    void (*modern_allocator_free)
      (void *client_state, void *memory);
    void *(*modern_allocator_realloc)
      (void *client_state, void *memory, size_t size);
};


struct modern_processor {
    modern_autorelease_pool *pool;
    void (*modern_processor_abort)(void *processor_state);
    void (*modern_processor_flush)(void *processor_state);
};


struct modern_stream {
    void *(*modern_stream_initialize)
      ();
    void (*modern_stream_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_magic_number)
      (void *processor_state, void *stream_state);
    void (*modern_stream_name_definition)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_value_definition_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_bool)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_maybe_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int16)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_int64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat16)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_nat64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_float32)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_float64)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_utf8)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_blob)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_function_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_definition_sigma_is_next)
      (void *processor_state, void *stream_state,
       struct modern_hash *a, struct modern_hash *b);
    void (*modern_stream_type_definition_named_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *name);
    void (*modern_stream_type_definition_universe)
      (void *processor_state, void *stream_state);
    void (*modern_stream_false)
      (void *processor_state, void *stream_state);
    void (*modern_stream_true)
      (void *processor_state, void *stream_state);
    void (*modern_stream_nothing)
      (void *processor_state, void *stream_state);
    void (*modern_stream_just_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_int8)
      (void *processor_state, void *stream_state, int8_t value);
    void (*modern_stream_int16)
      (void *processor_state, void *stream_state, int16_t value);
    void (*modern_stream_int32)
      (void *processor_state, void *stream_state, int32_t value);
    void (*modern_stream_int64)
      (void *processor_state, void *stream_state, int64_t value);
    void (*modern_stream_nat8)
      (void *processor_state, void *stream_state, uint8_t value);
    void (*modern_stream_nat16)
      (void *processor_state, void *stream_state, uint16_t value);
    void (*modern_stream_nat32)
      (void *processor_state, void *stream_state, uint32_t value);
    void (*modern_stream_nat64)
      (void *processor_state, void *stream_state, uint64_t value);
    void (*modern_stream_float32)
      (void *processor_state, void *stream_state, modern_float32 value);
    void (*modern_stream_float64)
      (void *processor_state, void *stream_state, modern_float64 value);
    void (*modern_stream_utf8_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_utf8_data)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_utf8_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_blob_start)
      (void *processor_state, void *stream_state);
    void (*modern_stream_blob_data)
      (void *processor_state, void *stream_state,
       uint8_t *data, size_t length);
    void (*modern_stream_blob_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_named_value_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *name);
    void (*modern_stream_lambda_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_apply_is_next)
      (void *processor_state, void *stream_state);
    void (*modern_stream_type_family_is_next)
      (void *processor_state, void *stream_state, uint64_t n_items);
    void (*modern_stream_let_is_next)
      (void *processor_state, void *stream_state, uint64_t n_items);
    void (*modern_stream_backreference_is_next)
      (void *processor_state, void *stream_state, uint64_t index);
    void (*modern_stream_builtin_is_next)
      (void *processor_state, void *stream_state, uint16_t identifier);
    void (*modern_stream_item_from_context_is_next)
      (void *processor_state, void *stream_state, struct modern_hash *type);
    void (*modern_stream_end)
      (void *processor_state, void *stream_state);
    void (*modern_stream_finalize)
      (void *stream_state);
};


struct modern_vfile {
    ssize_t (*modern_vfile_read)
      (void *vfile_state, uint8_t *buffer, size_t length);
    ssize_t (*modern_vfile_write)
      (void *vfile_state, uint8_t *buffer, size_t length);
};


enum modern_node_type {
    bool_value_false_modern_node_type = 1,
    bool_value_true_modern_node_type,
    maybe_value_nothing_modern_node_type,
    maybe_value_just_modern_node_type,
    int8_value_modern_node_type,
    int16_value_modern_node_type,
    int32_value_modern_node_type,
    int64_value_modern_node_type,
    nat8_value_modern_node_type,
    nat16_value_modern_node_type,
    nat32_value_modern_node_type,
    nat64_value_modern_node_type,
    float32_value_modern_node_type,
    float64_value_modern_node_type,
    utf8_value_modern_node_type,
    blob_value_modern_node_type,
    sigma_value_modern_node_type,
    name_value_modern_node_type,
    named_value_modern_node_type,
    bool_type_modern_node_type,
    maybe_type_modern_node_type,
    int8_type_modern_node_type,
    int16_type_modern_node_type,
    int32_type_modern_node_type,
    int64_type_modern_node_type,
    nat8_type_modern_node_type,
    nat16_type_modern_node_type,
    nat32_type_modern_node_type,
    nat64_type_modern_node_type,
    float32_type_modern_node_type,
    float64_type_modern_node_type,
    utf8_type_modern_node_type,
    blob_type_modern_node_type,
    function_type_modern_node_type,
    sigma_type_modern_node_type,
    name_type_modern_node_type,
    named_type_modern_node_type,
    universe_type_modern_node_type,
    lambda_modern_node_type,
    apply_modern_node_type,
    type_family_modern_node_type,
    let_modern_node_type,
    backreference_modern_node_type,
    builtin_modern_node_type,
};


enum modern_builtin_identifier {
    if_bool_modern_builtin_identifier = 0,
    and_bool_modern_builtin_identifier = 32,
    or_bool_modern_builtin_identifier = 64,
    not_bool_modern_builtin_identifier = 96,
    equal_to_bool_modern_builtin_identifier = 128,
    equal_to_int8_modern_builtin_identifier = 129,
    equal_to_int16_modern_builtin_identifier = 130,
    equal_to_int32_modern_builtin_identifier = 131,
    equal_to_int64_modern_builtin_identifier = 132,
    equal_to_nat8_modern_builtin_identifier = 133,
    equal_to_nat16_modern_builtin_identifier = 134,
    equal_to_nat32_modern_builtin_identifier = 135,
    equal_to_nat64_modern_builtin_identifier = 136,
    equal_to_float32_modern_builtin_identifier = 137,
    equal_to_float64_modern_builtin_identifier = 138,
    equal_to_name_modern_builtin_identifier = 139,
    equal_to_utf8_modern_builtin_identifier = 140,
    equal_to_blob_modern_builtin_identifier = 150,
    less_than_int8_modern_builtin_identifier = 160,
    less_than_int16_modern_builtin_identifier = 161,
    less_than_int32_modern_builtin_identifier = 162,
    less_than_int64_modern_builtin_identifier = 163,
    less_than_nat8_modern_builtin_identifier = 164,
    less_than_nat16_modern_builtin_identifier = 165,
    less_than_nat32_modern_builtin_identifier = 166,
    less_than_nat64_modern_builtin_identifier = 167,
    less_than_float32_modern_builtin_identifier = 168,
    less_than_float64_modern_builtin_identifier = 169,
    add_int8_modern_builtin_identifier = 192,
    add_int16_modern_builtin_identifier = 193,
    add_int32_modern_builtin_identifier = 194,
    add_int64_modern_builtin_identifier = 195,
    add_nat8_modern_builtin_identifier = 196,
    add_nat16_modern_builtin_identifier = 197,
    add_nat32_modern_builtin_identifier = 198,
    add_nat64_modern_builtin_identifier = 199,
    add_float32_modern_builtin_identifier = 200,
    add_float64_modern_builtin_identifier = 201,
    subtract_int8_modern_builtin_identifier = 224,
    subtract_int16_modern_builtin_identifier = 225,
    subtract_int32_modern_builtin_identifier = 226,
    subtract_int64_modern_builtin_identifier = 227,
    subtract_nat8_modern_builtin_identifier = 228,
    subtract_nat16_modern_builtin_identifier = 229,
    subtract_nat32_modern_builtin_identifier = 230,
    subtract_nat64_modern_builtin_identifier = 231,
    subtract_float32_modern_builtin_identifier = 232,
    subtract_float64_modern_builtin_identifier = 233,
    multiply_int8_modern_builtin_identifier = 256,
    multiply_int16_modern_builtin_identifier = 257,
    multiply_int32_modern_builtin_identifier = 258,
    multiply_int64_modern_builtin_identifier = 259,
    multiply_nat8_modern_builtin_identifier = 260,
    multiply_nat16_modern_builtin_identifier = 261,
    multiply_nat32_modern_builtin_identifier = 262,
    multiply_nat64_modern_builtin_identifier = 263,
    multiply_float32_modern_builtin_identifier = 264,
    multiply_float64_modern_builtin_identifier = 265,
    divide_towards_zero_int8_modern_builtin_identifier = 288,
    divide_towards_zero_int16_modern_builtin_identifier = 289,
    divide_towards_zero_int32_modern_builtin_identifier = 290,
    divide_towards_zero_int64_modern_builtin_identifier = 291,
    divide_towards_zero_nat8_modern_builtin_identifier = 292,
    divide_towards_zero_nat16_modern_builtin_identifier = 293,
    divide_towards_zero_nat32_modern_builtin_identifier = 294,
    divide_towards_zero_nat64_modern_builtin_identifier = 295,
    divide_towards_negative_infinity_int8_modern_builtin_identifier = 320,
    divide_towards_negative_infinity_int16_modern_builtin_identifier = 321,
    divide_towards_negative_infinity_int32_modern_builtin_identifier = 322,
    divide_towards_negative_infinity_int64_modern_builtin_identifier = 323,
    divide_towards_negative_infinity_nat8_modern_builtin_identifier = 324,
    divide_towards_negative_infinity_nat16_modern_builtin_identifier = 325,
    divide_towards_negative_infinity_nat32_modern_builtin_identifier = 326,
    divide_towards_negative_infinity_nat64_modern_builtin_identifier = 327,
    divide_float32_modern_builtin_identifier = 352,
    divide_float64_modern_builtin_identifier = 353,
    modulus_towards_zero_int8_modern_builtin_identifier = 384,
    modulus_towards_zero_int16_modern_builtin_identifier = 385,
    modulus_towards_zero_int32_modern_builtin_identifier = 386,
    modulus_towards_zero_int64_modern_builtin_identifier = 387,
    modulus_towards_zero_nat8_modern_builtin_identifier = 388,
    modulus_towards_zero_nat16_modern_builtin_identifier = 389,
    modulus_towards_zero_nat32_modern_builtin_identifier = 390,
    modulus_towards_zero_nat64_modern_builtin_identifier = 391,
    modulus_towards_negative_infinity_int8_modern_builtin_identifier = 416,
    modulus_towards_negative_infinity_int16_modern_builtin_identifier = 417,
    modulus_towards_negative_infinity_int32_modern_builtin_identifier = 418,
    modulus_towards_negative_infinity_int64_modern_builtin_identifier = 419,
    modulus_towards_negative_infinity_nat8_modern_builtin_identifier = 420,
    modulus_towards_negative_infinity_nat16_modern_builtin_identifier = 421,
    modulus_towards_negative_infinity_nat32_modern_builtin_identifier = 422,
    modulus_towards_negative_infinity_nat64_modern_builtin_identifier = 423,
    negate_int8_modern_builtin_identifier = 448,
    negate_int16_modern_builtin_identifier = 449,
    negate_int32_modern_builtin_identifier = 450,
    negate_int64_modern_builtin_identifier = 451,
    negate_float32_modern_builtin_identifier = 452,
    negate_float64_modern_builtin_identifier = 453,
    absolute_value_int8_modern_builtin_identifier = 480,
    absolute_value_int16_modern_builtin_identifier = 481,
    absolute_value_int32_modern_builtin_identifier = 482,
    absolute_value_int64_modern_builtin_identifier = 483,
    absolute_value_float32_modern_builtin_identifier = 484,
    absolute_value_float64_modern_builtin_identifier = 485,
    sign_int8_modern_builtin_identifier = 512,
    sign_int16_modern_builtin_identifier = 513,
    sign_int32_modern_builtin_identifier = 514,
    sign_int64_modern_builtin_identifier = 515,
    sign_float32_modern_builtin_identifier = 516,
    sign_float64_modern_builtin_identifier = 517,
    pi_float32_modern_builtin_identifier = 544,
    pi_float64_modern_builtin_identifier = 545,
    square_root_float32_modern_builtin_identifier = 576,
    square_root_float64_modern_builtin_identifier = 577,
    natural_logarithm_float32_modern_builtin_identifier = 608,
    natural_logarithm_float64_modern_builtin_identifier = 609,
    e_to_the_x_float32_modern_builtin_identifier = 640,
    e_to_the_x_float64_modern_builtin_identifier = 641,
    two_to_the_x_float32_modern_builtin_identifier = 672,
    two_to_the_x_float64_modern_builtin_identifier = 673,
    x_to_the_y_float32_modern_builtin_identifier = 704,
    x_to_the_y_float64_modern_builtin_identifier = 705,
    logarithm_base_x_float32_modern_builtin_identifier = 736,
    logarithm_base_x_float64_modern_builtin_identifier = 737,
    sine_float32_modern_builtin_identifier = 768,
    sine_float64_modern_builtin_identifier = 769,
    cosine_float32_modern_builtin_identifier = 800,
    cosine_float64_modern_builtin_identifier = 801,
    tangent_float32_modern_builtin_identifier = 832,
    tangent_float64_modern_builtin_identifier = 833,
    arcsine_float32_modern_builtin_identifier = 864,
    arcsine_float64_modern_builtin_identifier = 865,
    arccosine_float32_modern_builtin_identifier = 896,
    arccosine_float64_modern_builtin_identifier = 897,
    arctangent_float32_modern_builtin_identifier = 928,
    arctangent_float64_modern_builtin_identifier = 929,
    arctangent_fraction_float32_modern_builtin_identifier = 960,
    arctangent_fraction_float64_modern_builtin_identifier = 961,
    hyperbolic_sine_float32_modern_builtin_identifier = 992,
    hyperbolic_sine_float64_modern_builtin_identifier = 993,
    hyperbolic_cosine_float32_modern_builtin_identifier = 1024,
    hyperbolic_cosine_float64_modern_builtin_identifier = 1025,
    hyperbolic_tangent_float32_modern_builtin_identifier = 1056,
    hyperbolic_tangent_float64_modern_builtin_identifier = 1057,
    hyperbolic_arcsine_float32_modern_builtin_identifier = 1088,
    hyperbolic_arcsine_float64_modern_builtin_identifier = 1089,
    hyperbolic_arccosine_float32_modern_builtin_identifier = 1120,
    hyperbolic_arccosine_float64_modern_builtin_identifier = 1121,
    hyperbolic_arctangent_float32_modern_builtin_identifier = 1152,
    hyperbolic_arctangent_float64_modern_builtin_identifier = 1153,
    round_towards_zero_float32_int8_modern_builtin_identifier = 1184,
    round_towards_zero_float64_int8_modern_builtin_identifier = 1185,
    round_towards_zero_float32_int16_modern_builtin_identifier = 1186,
    round_towards_zero_float64_int16_modern_builtin_identifier = 1187,
    round_towards_zero_float32_int32_modern_builtin_identifier = 1188,
    round_towards_zero_float64_int32_modern_builtin_identifier = 1189,
    round_towards_zero_float32_int64_modern_builtin_identifier = 1190,
    round_towards_zero_float64_int64_modern_builtin_identifier = 1191,
    round_towards_zero_float32_nat8_modern_builtin_identifier = 1192,
    round_towards_zero_float64_nat8_modern_builtin_identifier = 1193,
    round_towards_zero_float32_nat16_modern_builtin_identifier = 1194,
    round_towards_zero_float64_nat16_modern_builtin_identifier = 1195,
    round_towards_zero_float32_nat32_modern_builtin_identifier = 1196,
    round_towards_zero_float64_nat32_modern_builtin_identifier = 1197,
    round_towards_zero_float32_nat64_modern_builtin_identifier = 1198,
    round_towards_zero_float64_nat64_modern_builtin_identifier = 1199,
    round_towards_zero_float32_float32_modern_builtin_identifier = 1200,
    round_towards_zero_float64_float64_modern_builtin_identifier = 1201,
    round_away_from_zero_float32_int8_modern_builtin_identifier = 1216,
    round_away_from_zero_float64_int8_modern_builtin_identifier = 1217,
    round_away_from_zero_float32_int16_modern_builtin_identifier = 1218,
    round_away_from_zero_float64_int16_modern_builtin_identifier = 1219,
    round_away_from_zero_float32_int32_modern_builtin_identifier = 1220,
    round_away_from_zero_float64_int32_modern_builtin_identifier = 1221,
    round_away_from_zero_float32_int64_modern_builtin_identifier = 1222,
    round_away_from_zero_float64_int64_modern_builtin_identifier = 1223,
    round_away_from_zero_float32_nat8_modern_builtin_identifier = 1224,
    round_away_from_zero_float64_nat8_modern_builtin_identifier = 1225,
    round_away_from_zero_float32_nat16_modern_builtin_identifier = 1226,
    round_away_from_zero_float64_nat16_modern_builtin_identifier = 1227,
    round_away_from_zero_float32_nat32_modern_builtin_identifier = 1228,
    round_away_from_zero_float64_nat32_modern_builtin_identifier = 1229,
    round_away_from_zero_float32_nat64_modern_builtin_identifier = 1230,
    round_away_from_zero_float64_nat64_modern_builtin_identifier = 1231,
    round_away_from_zero_float32_float32_modern_builtin_identifier = 1232,
    round_away_from_zero_float64_float64_modern_builtin_identifier = 1233,
    round_towards_even_float32_int8_modern_builtin_identifier = 1248,
    round_towards_even_float64_int8_modern_builtin_identifier = 1249,
    round_towards_even_float32_int16_modern_builtin_identifier = 1250,
    round_towards_even_float64_int16_modern_builtin_identifier = 1251,
    round_towards_even_float32_int32_modern_builtin_identifier = 1252,
    round_towards_even_float64_int32_modern_builtin_identifier = 1253,
    round_towards_even_float32_int64_modern_builtin_identifier = 1254,
    round_towards_even_float64_int64_modern_builtin_identifier = 1255,
    round_towards_even_float32_nat8_modern_builtin_identifier = 1256,
    round_towards_even_float64_nat8_modern_builtin_identifier = 1257,
    round_towards_even_float32_nat16_modern_builtin_identifier = 1258,
    round_towards_even_float64_nat16_modern_builtin_identifier = 1259,
    round_towards_even_float32_nat32_modern_builtin_identifier = 1260,
    round_towards_even_float64_nat32_modern_builtin_identifier = 1261,
    round_towards_even_float32_nat64_modern_builtin_identifier = 1262,
    round_towards_even_float64_nat64_modern_builtin_identifier = 1263,
    round_towards_even_float32_float32_modern_builtin_identifier = 1264,
    round_towards_even_float64_float64_modern_builtin_identifier = 1265,
    round_towards_odd_float32_int8_modern_builtin_identifier = 1280,
    round_towards_odd_float64_int8_modern_builtin_identifier = 1281,
    round_towards_odd_float32_int16_modern_builtin_identifier = 1282,
    round_towards_odd_float64_int16_modern_builtin_identifier = 1283,
    round_towards_odd_float32_int32_modern_builtin_identifier = 1284,
    round_towards_odd_float64_int32_modern_builtin_identifier = 1285,
    round_towards_odd_float32_int64_modern_builtin_identifier = 1286,
    round_towards_odd_float64_int64_modern_builtin_identifier = 1287,
    round_towards_odd_float32_nat8_modern_builtin_identifier = 1288,
    round_towards_odd_float64_nat8_modern_builtin_identifier = 1289,
    round_towards_odd_float32_nat16_modern_builtin_identifier = 1290,
    round_towards_odd_float64_nat16_modern_builtin_identifier = 1291,
    round_towards_odd_float32_nat32_modern_builtin_identifier = 1292,
    round_towards_odd_float64_nat32_modern_builtin_identifier = 1293,
    round_towards_odd_float32_nat64_modern_builtin_identifier = 1294,
    round_towards_odd_float64_nat64_modern_builtin_identifier = 1295,
    round_towards_odd_float32_float32_modern_builtin_identifier = 1296,
    round_towards_odd_float64_float64_modern_builtin_identifier = 1297,
    ceiling_float32_int8_modern_builtin_identifier = 1312,
    ceiling_float64_int8_modern_builtin_identifier = 1313,
    ceiling_float32_int16_modern_builtin_identifier = 1314,
    ceiling_float64_int16_modern_builtin_identifier = 1315,
    ceiling_float32_int32_modern_builtin_identifier = 1316,
    ceiling_float64_int32_modern_builtin_identifier = 1317,
    ceiling_float32_int64_modern_builtin_identifier = 1318,
    ceiling_float64_int64_modern_builtin_identifier = 1319,
    ceiling_float32_nat8_modern_builtin_identifier = 1320,
    ceiling_float64_nat8_modern_builtin_identifier = 1321,
    ceiling_float32_nat16_modern_builtin_identifier = 1322,
    ceiling_float64_nat16_modern_builtin_identifier = 1323,
    ceiling_float32_nat32_modern_builtin_identifier = 1324,
    ceiling_float64_nat32_modern_builtin_identifier = 1325,
    ceiling_float32_nat64_modern_builtin_identifier = 1326,
    ceiling_float64_nat64_modern_builtin_identifier = 1327,
    ceiling_float32_float32_modern_builtin_identifier = 1328,
    ceiling_float64_float64_modern_builtin_identifier = 1329,
    floor_float32_int8_modern_builtin_identifier = 1344,
    floor_float64_int8_modern_builtin_identifier = 1345,
    floor_float32_int16_modern_builtin_identifier = 1346,
    floor_float64_int16_modern_builtin_identifier = 1347,
    floor_float32_int32_modern_builtin_identifier = 1348,
    floor_float64_int32_modern_builtin_identifier = 1349,
    floor_float32_int64_modern_builtin_identifier = 1350,
    floor_float64_int64_modern_builtin_identifier = 1351,
    floor_float32_nat8_modern_builtin_identifier = 1352,
    floor_float64_nat8_modern_builtin_identifier = 1353,
    floor_float32_nat16_modern_builtin_identifier = 1354,
    floor_float64_nat16_modern_builtin_identifier = 1355,
    floor_float32_nat32_modern_builtin_identifier = 1356,
    floor_float64_nat32_modern_builtin_identifier = 1357,
    floor_float32_nat64_modern_builtin_identifier = 1358,
    floor_float64_nat64_modern_builtin_identifier = 1359,
    floor_float32_float32_modern_builtin_identifier = 1360,
    floor_float64_float64_modern_builtin_identifier = 1361,
    minimum_bound_int8_modern_builtin_identifier = 1376,
    minimum_bound_int16_modern_builtin_identifier = 1377,
    minimum_bound_int32_modern_builtin_identifier = 1378,
    minimum_bound_int64_modern_builtin_identifier = 1379,
    minimum_bound_nat8_modern_builtin_identifier = 1380,
    minimum_bound_nat16_modern_builtin_identifier = 1381,
    minimum_bound_nat32_modern_builtin_identifier = 1382,
    minimum_bound_nat64_modern_builtin_identifier = 1383,
    maximum_bound_int8_modern_builtin_identifier = 1408,
    maximum_bound_int16_modern_builtin_identifier = 1409,
    maximum_bound_int32_modern_builtin_identifier = 1410,
    maximum_bound_int64_modern_builtin_identifier = 1411,
    maximum_bound_nat8_modern_builtin_identifier = 1412,
    maximum_bound_nat16_modern_builtin_identifier = 1413,
    maximum_bound_nat32_modern_builtin_identifier = 1414,
    maximum_bound_nat64_modern_builtin_identifier = 1415,
    shift_left_int8_modern_builtin_identifier = 1440,
    shift_left_int16_modern_builtin_identifier = 1441,
    shift_left_int32_modern_builtin_identifier = 1442,
    shift_left_int64_modern_builtin_identifier = 1443,
    shift_left_nat8_modern_builtin_identifier = 1444,
    shift_left_nat16_modern_builtin_identifier = 1445,
    shift_left_nat32_modern_builtin_identifier = 1456,
    shift_left_nat64_modern_builtin_identifier = 1457,
    shift_right_int8_modern_builtin_identifier = 1472,
    shift_right_int16_modern_builtin_identifier = 1473,
    shift_right_int32_modern_builtin_identifier = 1474,
    shift_right_int64_modern_builtin_identifier = 1475,
    shift_right_nat8_modern_builtin_identifier = 1476,
    shift_right_nat16_modern_builtin_identifier = 1477,
    shift_right_nat32_modern_builtin_identifier = 1478,
    shift_right_nat64_modern_builtin_identifier = 1479,
    rotate_left_int8_modern_builtin_identifier = 1504,
    rotate_left_int16_modern_builtin_identifier = 1505,
    rotate_left_int32_modern_builtin_identifier = 1506,
    rotate_left_int64_modern_builtin_identifier = 1507,
    rotate_left_nat8_modern_builtin_identifier = 1508,
    rotate_left_nat16_modern_builtin_identifier = 1509,
    rotate_left_nat32_modern_builtin_identifier = 1510,
    rotate_left_nat64_modern_builtin_identifier = 1511,
    rotate_right_int8_modern_builtin_identifier = 1536,
    rotate_right_int16_modern_builtin_identifier = 1537,
    rotate_right_int32_modern_builtin_identifier = 1538,
    rotate_right_int64_modern_builtin_identifier = 1539,
    rotate_right_nat8_modern_builtin_identifier = 1540,
    rotate_right_nat16_modern_builtin_identifier = 1541,
    rotate_right_nat32_modern_builtin_identifier = 1542,
    rotate_right_nat64_modern_builtin_identifier = 1543,
    bit_and_int8_modern_builtin_identifier = 1568,
    bit_and_int16_modern_builtin_identifier = 1569,
    bit_and_int32_modern_builtin_identifier = 1570,
    bit_and_int64_modern_builtin_identifier = 1571,
    bit_and_nat8_modern_builtin_identifier = 1572,
    bit_and_nat16_modern_builtin_identifier = 1573,
    bit_and_nat32_modern_builtin_identifier = 1574,
    bit_and_nat64_modern_builtin_identifier = 1575,
    bit_or_int8_modern_builtin_identifier = 1600,
    bit_or_int16_modern_builtin_identifier = 1601,
    bit_or_int32_modern_builtin_identifier = 1602,
    bit_or_int64_modern_builtin_identifier = 1603,
    bit_or_nat8_modern_builtin_identifier = 1604,
    bit_or_nat16_modern_builtin_identifier = 1605,
    bit_or_nat32_modern_builtin_identifier = 1606,
    bit_or_nat64_modern_builtin_identifier = 1607,
    bit_xor_int8_modern_builtin_identifier = 1632,
    bit_xor_int16_modern_builtin_identifier = 1633,
    bit_xor_int32_modern_builtin_identifier = 1634,
    bit_xor_int64_modern_builtin_identifier = 1635,
    bit_xor_nat8_modern_builtin_identifier = 1636,
    bit_xor_nat16_modern_builtin_identifier = 1637,
    bit_xor_nat32_modern_builtin_identifier = 1638,
    bit_xor_nat64_modern_builtin_identifier = 1639,
    bit_not_int8_modern_builtin_identifier = 1664,
    bit_not_int16_modern_builtin_identifier = 1665,
    bit_not_int32_modern_builtin_identifier = 1666,
    bit_not_int64_modern_builtin_identifier = 1667,
    bit_not_nat8_modern_builtin_identifier = 1668,
    bit_not_nat16_modern_builtin_identifier = 1669,
    bit_not_nat32_modern_builtin_identifier = 1670,
    bit_not_nat64_modern_builtin_identifier = 1671,
    decode_utf8_modern_builtin_identifier = 1696,
    encode_utf8_modern_builtin_identifier = 1728,
    character_offset_to_byte_offset_utf8_modern_builtin_identifier = 1760,
    length_bytes_utf8_modern_builtin_identifier = 1792,
    length_bytes_blob_modern_builtin_identifier = 1793,
    get_byte_blob_modern_builtin_identifier = 1824,
    replace_byte_blob_modern_builtin_identifier = 1856,
    get_data_piece_utf8_modern_builtin_identifier = 1888,
    get_data_piece_blob_modern_builtin_identifier = 1889,
    replace_data_piece_utf8_modern_builtin_identifier = 1920,
    replace_data_piece_blob_modern_builtin_identifier = 1921,
    empty_utf8_modern_builtin_identifier = 1952,
    empty_blob_modern_builtin_identifier = 1953,
    get_sigma_field_value_modern_builtin_identifier = 1984,
    get_sigma_successor_modern_builtin_identifier = 2016,
    get_named_value_modern_builtin_identifier = 2048,
    get_function_type_left_modern_builtin_identifier = 2080,
    get_function_type_right_modern_builtin_identifier = 2112,
    get_sigma_type_field_type_modern_builtin_identifier = 2144,
    get_sigma_type_successor_modern_builtin_identifier = 2176,
    get_named_type_content_type_modern_builtin_identifier = 2208,
    get_universe_type_level_modern_builtin_identifier = 2240,
    make_sigma_modern_builtin_identifier = 2272,
    make_name_modern_builtin_identifier = 2304,
    make_named_value_modern_builtin_identifier = 2336,
    make_function_type_modern_builtin_identifier = 2368,
    make_sigma_type_modern_builtin_identifier = 2400,
    make_named_type_modern_builtin_identifier = 2432,
    make_universe_type_modern_builtin_identifier = 2464,
    make_maybe_type_modern_builtin_identifier = 2496,
    get_maybe_type_content_type_modern_builtin_identifier = 2528,
    maybe_is_just_modern_builtin_identifier = 2560,
    fmap_maybe_modern_builtin_identifier = 2592,
    from_maybe_modern_builtin_identifier = 2624,
    cast_int8_int16_modern_builtin_identifier = 10240,
    cast_int8_int32_modern_builtin_identifier = 10241,
    cast_int8_int64_modern_builtin_identifier = 10242,
    cast_int8_nat8_modern_builtin_identifier = 10243,
    cast_int8_nat16_modern_builtin_identifier = 10244,
    cast_int8_nat32_modern_builtin_identifier = 10245,
    cast_int8_nat64_modern_builtin_identifier = 10246,
    cast_int16_int8_modern_builtin_identifier = 10247,
    cast_int16_int32_modern_builtin_identifier = 10248,
    cast_int16_int64_modern_builtin_identifier = 10249,
    cast_int16_nat8_modern_builtin_identifier = 10250,
    cast_int16_nat16_modern_builtin_identifier = 10251,
    cast_int16_nat32_modern_builtin_identifier = 10252,
    cast_int16_nat64_modern_builtin_identifier = 10253,
    cast_int32_int8_modern_builtin_identifier = 10254,
    cast_int32_int16_modern_builtin_identifier = 10255,
    cast_int32_int64_modern_builtin_identifier = 10256,
    cast_int32_nat8_modern_builtin_identifier = 10257,
    cast_int32_nat16_modern_builtin_identifier = 10258,
    cast_int32_nat32_modern_builtin_identifier = 10259,
    cast_int32_nat64_modern_builtin_identifier = 10260,
    cast_int64_int8_modern_builtin_identifier = 10261,
    cast_int64_int16_modern_builtin_identifier = 10262,
    cast_int64_int32_modern_builtin_identifier = 10263,
    cast_int64_nat8_modern_builtin_identifier = 10264,
    cast_int64_nat16_modern_builtin_identifier = 10265,
    cast_int64_nat32_modern_builtin_identifier = 10266,
    cast_int64_nat64_modern_builtin_identifier = 10267,
    cast_nat8_int8_modern_builtin_identifier = 10268,
    cast_nat8_int16_modern_builtin_identifier = 10269,
    cast_nat8_int32_modern_builtin_identifier = 10270,
    cast_nat8_int64_modern_builtin_identifier = 10271,
    cast_nat8_nat16_modern_builtin_identifier = 10272,
    cast_nat8_nat32_modern_builtin_identifier = 10273,
    cast_nat8_nat64_modern_builtin_identifier = 10274,
    cast_nat16_int8_modern_builtin_identifier = 10275,
    cast_nat16_int16_modern_builtin_identifier = 10276,
    cast_nat16_int32_modern_builtin_identifier = 10277,
    cast_nat16_int64_modern_builtin_identifier = 10278,
    cast_nat16_nat8_modern_builtin_identifier = 10279,
    cast_nat16_nat32_modern_builtin_identifier = 10280,
    cast_nat16_nat64_modern_builtin_identifier = 10281,
    cast_nat32_int8_modern_builtin_identifier = 10282,
    cast_nat32_int16_modern_builtin_identifier = 10283,
    cast_nat32_int32_modern_builtin_identifier = 10284,
    cast_nat32_int64_modern_builtin_identifier = 10285,
    cast_nat32_nat8_modern_builtin_identifier = 10286,
    cast_nat32_nat16_modern_builtin_identifier = 10287,
    cast_nat32_nat64_modern_builtin_identifier = 10288,
    cast_nat64_int8_modern_builtin_identifier = 10289,
    cast_nat64_int16_modern_builtin_identifier = 10290,
    cast_nat64_int32_modern_builtin_identifier = 10291,
    cast_nat64_int64_modern_builtin_identifier = 10292,
    cast_nat64_nat8_modern_builtin_identifier = 10293,
    cast_nat64_nat16_modern_builtin_identifier = 10294,
    cast_nat64_nat32_modern_builtin_identifier = 10295,
    cast_utf8_blob_modern_builtin_identifier = 10296,
    cast_blob_utf8_modern_builtin_identifier = 10297,
    cast_name_blob_modern_builtin_identifier = 10298,
    cast_blob_name_modern_builtin_identifier = 10299,
    cast_float32_float64_modern_builtin_identifier = 10300,
    cast_float64_float32_modern_builtin_identifier = 10301,
    cast_blob_int8_modern_builtin_identifier = 10302,
    cast_blob_int16_modern_builtin_identifier = 10303,
    cast_blob_int32_modern_builtin_identifier = 10304,
    cast_blob_int64_modern_builtin_identifier = 10305,
    cast_blob_nat8_modern_builtin_identifier = 10306,
    cast_blob_nat16_modern_builtin_identifier = 10307,
    cast_blob_nat32_modern_builtin_identifier = 10308,
    cast_blob_nat64_modern_builtin_identifier = 10309,
    cast_blob_float32_modern_builtin_identifier = 10310,
    cast_blob_float64_modern_builtin_identifier = 10311,
    cast_int8_blob_modern_builtin_identifier = 10312,
    cast_int16_blob_modern_builtin_identifier = 10313,
    cast_int32_blob_modern_builtin_identifier = 10314,
    cast_int64_blob_modern_builtin_identifier = 10315,
    cast_nat8_blob_modern_builtin_identifier = 10316,
    cast_nat16_blob_modern_builtin_identifier = 10317,
    cast_nat32_blob_modern_builtin_identifier = 10318,
    cast_nat64_blob_modern_builtin_identifier = 10319,
    cast_float32_blob_modern_builtin_identifier = 10320,
    cast_float64_blob_modern_builtin_identifier = 10321,
};


extern modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator,
   void (*finalizer)(void *client_state),
   void *client_state);
extern struct modern_error_handler *modern_library_get_error_handler
  (modern_library *library);
extern struct modern_allocator *modern_library_get_allocator
  (modern_library *library);
extern void *modern_library_get_client_state
  (modern_library *library);
extern void modern_library_finalize(modern_library *library);

extern modern_autorelease_pool *modern_make_autorelease_pool
  (modern_library *library);
extern void modern_autorelease_pool_release
  (modern_library *library,
   modern_autorelease_pool *pool);
extern void modern_retain
  (modern_library *library,
   void *retainable);
extern void modern_release
  (modern_library *library,
   void *retainable);
extern void modern_autorelease
  (modern_library *library,
   modern_autorelease_pool *pool,
   void *retainable);

extern modern_context *modern_make_initial_context
  (modern_library *library);
extern modern_context *modern_copy_context
  (modern_library *library,
   modern_context *context);
extern int modern_get_in_context
  (modern_library *library,
   modern_context *context, modern *node);
extern void modern_add_to_context
  (modern_library *library,
   modern_context *context, modern *node);
extern modern *modern_get_from_context
  (modern_library *library,
   modern_context *context, struct modern_hash *hash);

extern modern *modern_deserialize_memory
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   uint8_t *data, size_t length);
extern modern *modern_deserialize_file
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   FILE *file);
extern modern *modern_deserialize_fd
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   int fd);
extern modern *modern_deserialize_vfile
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);
extern modern *modern_deserialize_input_stream
  (modern_library *library,
   modern_autorelease_pool *pool, modern_context *context,
   void *processor_state, void *stream_state);

extern void modern_serialize_memory_buffer
  (modern_library *library,
   modern *value, modern_context *context, uint8_t *buffer, size_t *length);
extern uint8_t *modern_serialize_memory_allocating
  (modern_library *library,
   modern *value, modern_context *context, size_t *length);
extern void modern_serialize_file
  (modern_library *library,
   modern *value, modern_context *context, FILE *file);
extern void modern_serialize_fd
  (modern_library *library,
   modern *value, modern_context *context, int fd);
extern void modern_serialize_vfile
  (modern_library *library,
   modern *value, modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);
extern modern *modern_serialize_output_stream
  (modern_library *library,
   modern *value, modern_context *context,
   struct modern_stream *stream);

extern enum modern_node_type modern_node_get_node_type
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_value_type
  (modern_library *library,
   modern *value);
extern int modern_node_get_mutable
  (modern_library *library,
   modern *value);
extern modern *modern_node_copy
  (modern_library *library,
   modern *value);

extern modern *modern_node_get_maybe_just_content
  (modern_library *library,
   modern *value);
extern int8_t modern_node_get_int8
  (modern_library *library,
   modern *value);
extern int16_t modern_node_get_int16
  (modern_library *library,
   modern *value);
extern int32_t modern_node_get_int32
  (modern_library *library,
   modern *value);
extern int64_t modern_node_get_int64
  (modern_library *library,
   modern *value);
extern uint8_t modern_node_get_nat8
  (modern_library *library,
   modern *value);
extern uint16_t modern_node_get_nat16
  (modern_library *library,
   modern *value);
extern uint32_t modern_node_get_nat32
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_nat64
  (modern_library *library,
   modern *value);
extern modern_float32 modern_node_get_float32
  (modern_library *library,
   modern *value);
extern modern_float64 modern_node_get_float64
  (modern_library *library,
   modern *value);
extern size_t modern_node_get_utf8_bytes
  (modern_library *library,
   modern *value);
extern uint8_t *modern_node_get_utf8_data_piece
  (modern_library *library,
   modern *value, size_t offset, size_t bytes);
extern size_t modern_node_get_blob_bytes
  (modern_library *library,
   modern *value);
extern uint8_t *modern_node_get_blob_data_piece
  (modern_library *library,
   modern *value, size_t offset, size_t bytes);
extern modern *modern_node_get_sigma_field_value
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_successor
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_named_value
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_maybe_type_content_type
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_function_type_left
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_function_type_right
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_type_field_type
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_sigma_type_successor
  (modern_library *library,
   modern *value);
extern struct modern_hash *modern_node_get_named_type_name
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_named_type_content_type
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_universe_type_level
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_lambda_content
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_apply_left
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_apply_right
  (modern_library *library,
   modern *value);
extern uint64_t modern_node_get_type_family_count
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_type_family_item
  (modern_library *library,
   modern *value, uint64_t index);
extern uint64_t modern_node_get_let_count
  (modern_library *library,
   modern *value);
extern modern *modern_node_get_let_item
  (modern_library *library,
   modern *value, uint64_t index);
extern modern *modern_node_get_let_content
  (modern_library *library,
   modern *value);
extern uint16_t modern_node_get_builtin_identifier
  (modern_library *library,
   modern *value);

extern modern *modern_node_make_bool_false
  (modern_library *library);
extern modern *modern_node_make_bool_true
  (modern_library *library);
extern modern *modern_node_make_maybe_nothing
  (modern_library *library,
   modern *type);
extern modern *modern_node_make_maybe_just
  (modern_library *library,
   modern *type,
   modern *content_value);
extern modern *modern_node_make_int8
  (modern_library *library,
   int8_t value);
extern modern *modern_node_make_int16
  (modern_library *library,
   int16_t value);
extern modern *modern_node_make_int32
  (modern_library *library,
   int32_t value);
extern modern *modern_node_make_int64
  (modern_library *library,
   int64_t value);
extern modern *modern_node_make_nat8
  (modern_library *library,
   uint8_t value);
extern modern *modern_node_make_nat16
  (modern_library *library,
   uint16_t value);
extern modern *modern_node_make_nat32
  (modern_library *library,
   uint32_t value);
extern modern *modern_node_make_nat64
  (modern_library *library,
   uint64_t value);
extern modern *modern_node_make_float32
  (modern_library *library,
   modern_float32 value);
extern modern *modern_node_make_float64
  (modern_library *library,
   modern_float64 value);
extern modern *modern_node_make_utf8
  (modern_library *library,
   uint8_t *data);
extern modern *modern_node_make_blob
  (modern_library *library,
   uint8_t *data, size_t bytes);
extern modern *modern_node_make_sigma
  (modern_library *library,
   modern *type, modern *field_value, modern *successor_value);
extern modern *modern_node_make_name
  (modern_library *library,
   struct modern_hash *name);
extern modern *modern_node_make_named_value
  (modern_library *library,
   modern *type, modern *value);

extern modern *modern_node_make_bool_type
  (modern_library *library);
extern modern *modern_node_make_maybe_type
  (modern_library *library,
   modern *content_type);
extern modern *modern_node_make_int8_type
  (modern_library *library);
extern modern *modern_node_make_int16_type
  (modern_library *library);
extern modern *modern_node_make_int32_type
  (modern_library *library);
extern modern *modern_node_make_int64_type
  (modern_library *library);
extern modern *modern_node_make_nat8_type
  (modern_library *library);
extern modern *modern_node_make_nat16_type
  (modern_library *library);
extern modern *modern_node_make_nat32_type
  (modern_library *library);
extern modern *modern_node_make_nat64_type
  (modern_library *library);
extern modern *modern_node_make_float32_type
  (modern_library *library);
extern modern *modern_node_make_float64_type
  (modern_library *library);
extern modern *modern_node_make_utf8_type
  (modern_library *library);
extern modern *modern_node_make_blob_type
  (modern_library *library);
extern modern *modern_node_make_function_type
  (modern_library *library,
   modern *left, modern *right);
extern modern *modern_node_make_sigma_type
  (modern_library *library,
   modern *field_type, modern *successor);
extern modern *modern_node_make_name_type
  (modern_library *library,
   struct modern_hash *name);
extern modern *modern_node_make_named_type
  (modern_library *library,
   struct modern_hash *name, modern *content_type);
extern modern *modern_node_make_universe_type
  (modern_library *library,
   uint64_t level);

extern modern *modern_node_make_lambda
  (modern_library *library,
   modern *content);
extern modern *modern_node_make_apply
  (modern_library *library,
   modern *left, modern *right);
extern modern *modern_node_make_type_family
  (modern_library *library,
   uint64_t n_items, modern **types);
extern modern *modern_node_make_let
  (modern_library *library,
   uint64_t n_items, modern **values, modern *content);
extern modern *modern_node_make_backreference
  (modern_library *library,
   uint64_t index);
extern modern *modern_node_make_builtin
  (modern_library *library,
   uint16_t identifier);

extern void modern_node_set_immutable
  (modern_library *library,
   modern *value);

extern void modern_node_set_maybe_just_content
  (modern_library *library,
   modern *value,
   modern *content_value);
extern void modern_node_set_int8
  (modern_library *library,
   modern *node,
   int8_t value);
extern void modern_node_set_int16
  (modern_library *library,
   modern *node,
   int16_t value);
extern void modern_node_set_int32
  (modern_library *library,
   modern *node,
   int32_t value);
extern void modern_node_set_int64
  (modern_library *library,
   modern *node,
   int64_t value);
extern void modern_node_set_nat8
  (modern_library *library,
   modern *node,
   uint8_t value);
extern void modern_node_set_nat16
  (modern_library *library,
   modern *node,
   uint16_t value);
extern void modern_node_set_nat32
  (modern_library *library,
   modern *node,
   uint32_t value);
extern void modern_node_set_nat64
  (modern_library *library,
   modern *node,
   uint64_t value);
extern void modern_node_set_float32
  (modern_library *library,
   modern *node,
   float value);
extern void modern_node_set_float64
  (modern_library *library,
   modern *node,
   double value);
extern void modern_node_set_utf8_data_piece
  (modern_library *library,
   modern *value,
   uint8_t *data,
   size_t offset,
   size_t old_bytes,
   size_t new_bytes);
extern void modern_node_set_blob_data_piece
  (modern_library *library,
   modern *value,
   uint8_t *data,
   size_t offset,
   size_t old_bytes,
   size_t new_bytes);
extern void modern_node_set_sigma_field_value
  (modern_library *library,
   modern *value,
   modern *field_value);
extern void modern_node_set_sigma_successor
  (modern_library *library,
   modern *value,
   modern *successor);
extern void modern_node_set_named_value
  (modern_library *library,
   modern *node,
   modern *type,
   modern *value);

extern void modern_node_set_maybe_type_content_type
  (modern_library *library,
   modern *value,
   modern *content_type);
extern void modern_node_set_function_type_left
  (modern_library *library,
   modern *value,
   modern *left);
extern void modern_node_set_function_type_right
  (modern_library *library,
   modern *value,
   modern *right);
extern void modern_node_set_sigma_type_field_type
  (modern_library *library,
   modern *value,
   modern *field_type);
extern void modern_node_set_sigma_type_successor
  (modern_library *library,
   modern *value,
   modern *successor);
extern void modern_node_set_named_type_name
  (modern_library *library,
   modern *value,
   struct modern_hash *name);
extern void modern_node_set_named_type_content_type
  (modern_library *library,
   modern *value,
   modern *content_type);
extern void modern_node_set_universe_type_level
  (modern_library *library,
   modern *value,
   uint64_t level);

extern void modern_node_set_lambda_content
  (modern_library *library,
   modern *value,
   modern *content);
extern void modern_node_set_apply_left
  (modern_library *library,
   modern *value,
   modern *left);
extern void modern_node_set_apply_right
  (modern_library *library,
   modern *value,
   modern *right);
extern void modern_node_set_type_family_add_item
  (modern_library *library,
   modern *value,
   modern *item,
   uint64_t index);
extern void modern_node_set_type_family_remove_item
  (modern_library *library,
   modern *value,
   uint64_t index);
extern void modern_node_set_let_add_item
  (modern_library *library,
   modern *value,
   modern *item,
   uint64_t index);
extern void modern_node_set_let_remove_item
  (modern_library *library,
   modern *value,
   uint64_t index);
extern void modern_node_set_let_content
  (modern_library *library,
   modern *value,
   modern *content);
extern void modern_node_set_builtin_identifier
  (modern_library *library,
   modern *value,
   uint16_t identifier);

extern void modern_node_canonical_hash
  (modern_library *library,
   modern *value,
   struct modern_hash *out);

extern void *modern_input_stream_memory
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   uint8_t *data, size_t length);
extern void *modern_input_stream_file
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   FILE *file);
extern void *modern_input_stream_fd
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   int fd);
extern void *modern_input_stream_vfile
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_stream *stream,
   struct modern_vfile *vfile, void *vfile_state);

extern void modern_input_stream_step
  (modern_library *library,
   struct modern_stream *stream, void *processor_state, void **stream_state);
extern void modern_input_stream_run
  (modern_library *library,
   struct modern_stream *stream, void *processor_state, void **stream_state);
extern void modern_input_stream_do_all
  (modern_library *library,
   struct modern_stream *stream, void *processor_state);

extern void modern_input_stream_finalize
  (modern_library *library,
   void *processor_state);

extern void *modern_output_stream_memory_buffer
  (modern_library *library,
   modern_autorelease_pool *pool,
   uint8_t *buffer, size_t *length);
extern void *modern_output_stream_memory_allocating
  (modern_library *library,
   modern_autorelease_pool *pool,
   size_t *length);
extern uint8_t *modern_output_stream_memory_allocating_result
  (modern_library *library,
   void *stream_state, size_t *length);
extern void *modern_output_stream_file
  (modern_library *library,
   modern_autorelease_pool *pool,
   FILE *file);
extern void *modern_output_stream_fd
  (modern_library *library,
   modern_autorelease_pool *pool,
   int fd);
extern void *modern_output_stream_vfile
  (modern_library *library,
   modern_autorelease_pool *pool,
   struct modern_vfile *vfile, void *vfile_state);

extern modern *modern_evaluate
  (modern_library *library, modern *node);

extern void modern_compute_hash
  (uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_compute_child_hash
  (struct modern_hash *parent,
   uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_compute_initial_namespace_hash(struct modern_hash *out);

extern struct modern_vfile *modern_make_memory_buffer_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_memory_allocating_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_file_vfile
  (modern_library *library);
extern struct modern_vfile *modern_make_fd_vfile
  (modern_library *library);

extern struct modern_stream *modern_make_explicatory_stream
  (modern_library *library);
extern struct modern_stream *modern_make_documentation_stream
  (modern_library *library);
