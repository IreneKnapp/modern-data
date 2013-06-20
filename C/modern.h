#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>


typedef void modern;
typedef void modern_context;
typedef void modern_library;


struct modern_hash {
    uint64_t a;
    uint64_t b;
};


struct modern_error_handler {
    void (*memory)
      (void *client_state, size_t requested_size);
    void (*type_mismatch)
      (void *client_state, modern *expected, modern *actual);
    void (*universe_level_overflow)
      (void *client_state);
    void (*buffer_index)
      (void *client_state);
    void (*not_applicable)
      (void *client_state);
    void (*non_numeric_float)
      (void *client_state);
    void (*immutable)
      (void *client_state, modern *node);
    void (*builtin_identifier)
      (void *client_state, uint16_t identifier);
    void (*usage)
      (void *client_state);
 };


struct modern_allocator {
    void *(*alloc)
      (void *client_state, size_t size);
    void (*free)
      (void *client_state, void *memory);
    void *(*realloc)
      (void *client_state, void *memory, size_t size);
};


struct modern_process {
    void (*abort)(void *process_state);
    void (*flush)(void *process_state);
};


struct modern_stream {
    void (*start)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*magic_number)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*name_definition)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint8_t *data, size_t length);
    void (*value_definition_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_bool)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_ordering)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_maybe_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_int8)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_int16)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_int32)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_int64)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_nat8)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_nat16)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_nat32)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_nat64)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_float32)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_float64)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_utf8)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_blob)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_function_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_sigma_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_definition_named_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       struct modern_hash name);
    void (*type_definition_universe)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*bool_false)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*bool_true)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*ordering_less)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*ordering_equal)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*ordering_greater)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*maybe_nothing)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*maybe_just_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*int8)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       int8_t value);
    void (*int16)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       int16_t value);
    void (*int32)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       int32_t value);
    void (*int64)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       int64_t value);
    void (*nat8)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint8_t value);
    void (*nat16)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint16_t value);
    void (*nat32)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint32_t value);
    void (*nat64)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint64_t value);
    void (*float32)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       float value);
    void (*float64)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       double value);
    void (*utf8_start)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*utf8_data)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint8_t *data, size_t length);
    void (*utf8_end)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*blob_start)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*blob_data)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       uint8_t *data, size_t length);
    void (*blob_end)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*sigma_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state,
       struct modern_hash *type);
    void (*named_value_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, struct modern_hash name);
    void (*lambda_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*apply_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state);
    void (*type_family_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, uint64_t n_items);
    void (*let_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, uint64_t n_items);
    void (*backreference_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, uint64_t index);
    void (*builtin_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, uint16_t identifier);
    void (*item_from_context_is_next)
      (struct modern_process *process, void *process_state,
       void *stream_state, struct modern_hash type);
    void (*end)
      (struct modern_process *process, void *process_state,
       void *stream_state);
};


struct modern_vfile {
    ssize_t (*read)
      (void *vfile_state, uint8_t *buffer, size_t length);
    ssize_t (*write)
      (void *vfile_state, uint8_t *buffer, size_t length);
};


struct modern_processor {
    void *(*initialize)
      (modern_library *library);
    void (*finalize)
      (void *process_state);
    void (*step)
      (void *process_state,
       struct modern_stream *stream, void *stream_state,
       struct modern_vfile *vfile, void *vfile_state);
    void (*run)
      (void *process_state,
       struct modern_stream *stream, void *stream_state,
       struct modern_vfile *vfile, void *vfile_state);
};


enum modern_node_type {
    modern_node_type_bool_value_false = 1,
    modern_node_type_bool_value_true = 2,
    modern_node_type_ordering_value_less = 3,
    modern_node_type_ordering_value_equal = 4,
    modern_node_type_ordering_value_greater = 5,
    modern_node_type_maybe_value_nothing = 6,
    modern_node_type_maybe_value_just = 7,
    modern_node_type_int8_value = 8,
    modern_node_type_int16_value = 9,
    modern_node_type_int32_value = 10,
    modern_node_type_int64_value = 11,
    modern_node_type_nat8_value = 12,
    modern_node_type_nat16_value = 13,
    modern_node_type_nat32_value = 14,
    modern_node_type_nat64_value = 15,
    modern_node_type_float32_value = 16,
    modern_node_type_float64_value = 17,
    modern_node_type_utf8_value = 18,
    modern_node_type_blob_value = 19,
    modern_node_type_sigma_value = 20,
    modern_node_type_name_value = 21,
    modern_node_type_named_value = 22,
    modern_node_type_bool_type = 23,
    modern_node_type_ordering_type = 24,
    modern_node_type_maybe_type = 25,
    modern_node_type_int8_type = 26,
    modern_node_type_int16_type = 27,
    modern_node_type_int32_type = 28,
    modern_node_type_int64_type = 29,
    modern_node_type_nat8_type = 30,
    modern_node_type_nat16_type = 31,
    modern_node_type_nat32_type = 32,
    modern_node_type_nat64_type = 33,
    modern_node_type_float32_type = 34,
    modern_node_type_float64_type = 35,
    modern_node_type_utf8_type = 36,
    modern_node_type_blob_type = 37,
    modern_node_type_function_type = 38,
    modern_node_type_sigma_type = 39,
    modern_node_type_name_type = 40,
    modern_node_type_named_type = 41,
    modern_node_type_universe_type = 42,
    modern_node_type_satisfies_type = 43,
    modern_node_type_lambda = 44,
    modern_node_type_apply = 45,
    modern_node_type_type_family = 46,
    modern_node_type_let = 47,
    modern_node_type_backreference = 48,
    modern_node_type_builtin = 49,
};


enum modern_builtin_identifier {
    modern_builtin_identifier_if_bool = 0,
    modern_builtin_identifier_and_bool = 32,
    modern_builtin_identifier_or_bool = 64,
    modern_builtin_identifier_not_bool = 96,
    modern_builtin_identifier_equal_to_bool = 128,
    modern_builtin_identifier_equal_to_int8 = 129,
    modern_builtin_identifier_equal_to_int16 = 130,
    modern_builtin_identifier_equal_to_int32 = 131,
    modern_builtin_identifier_equal_to_int64 = 132,
    modern_builtin_identifier_equal_to_nat8 = 133,
    modern_builtin_identifier_equal_to_nat16 = 134,
    modern_builtin_identifier_equal_to_nat32 = 135,
    modern_builtin_identifier_equal_to_nat64 = 136,
    modern_builtin_identifier_equal_to_float32 = 137,
    modern_builtin_identifier_equal_to_float64 = 138,
    modern_builtin_identifier_equal_to_name = 139,
    modern_builtin_identifier_equal_to_utf8 = 140,
    modern_builtin_identifier_equal_to_blob = 150,
    modern_builtin_identifier_equal_to_ordering = 151,
    modern_builtin_identifier_compare_int8 = 160,
    modern_builtin_identifier_compare_int16 = 161,
    modern_builtin_identifier_compare_int32 = 162,
    modern_builtin_identifier_compare_int64 = 163,
    modern_builtin_identifier_compare_nat8 = 164,
    modern_builtin_identifier_compare_nat16 = 165,
    modern_builtin_identifier_compare_nat32 = 166,
    modern_builtin_identifier_compare_nat64 = 167,
    modern_builtin_identifier_compare_float32 = 168,
    modern_builtin_identifier_compare_float64 = 169,
    modern_builtin_identifier_add_int8 = 192,
    modern_builtin_identifier_add_int16 = 193,
    modern_builtin_identifier_add_int32 = 194,
    modern_builtin_identifier_add_int64 = 195,
    modern_builtin_identifier_add_nat8 = 196,
    modern_builtin_identifier_add_nat16 = 197,
    modern_builtin_identifier_add_nat32 = 198,
    modern_builtin_identifier_add_nat64 = 199,
    modern_builtin_identifier_add_float32 = 200,
    modern_builtin_identifier_add_float64 = 201,
    modern_builtin_identifier_subtract_int8 = 224,
    modern_builtin_identifier_subtract_int16 = 225,
    modern_builtin_identifier_subtract_int32 = 226,
    modern_builtin_identifier_subtract_int64 = 227,
    modern_builtin_identifier_subtract_nat8 = 228,
    modern_builtin_identifier_subtract_nat16 = 229,
    modern_builtin_identifier_subtract_nat32 = 230,
    modern_builtin_identifier_subtract_nat64 = 231,
    modern_builtin_identifier_subtract_float32 = 232,
    modern_builtin_identifier_subtract_float64 = 233,
    modern_builtin_identifier_multiply_int8 = 256,
    modern_builtin_identifier_multiply_int16 = 257,
    modern_builtin_identifier_multiply_int32 = 258,
    modern_builtin_identifier_multiply_int64 = 259,
    modern_builtin_identifier_multiply_nat8 = 260,
    modern_builtin_identifier_multiply_nat16 = 261,
    modern_builtin_identifier_multiply_nat32 = 262,
    modern_builtin_identifier_multiply_nat64 = 263,
    modern_builtin_identifier_multiply_float32 = 264,
    modern_builtin_identifier_multiply_float64 = 265,
    modern_builtin_identifier_divide_towards_zero_int8 = 288,
    modern_builtin_identifier_divide_towards_zero_int16 = 289,
    modern_builtin_identifier_divide_towards_zero_int32 = 290,
    modern_builtin_identifier_divide_towards_zero_int64 = 291,
    modern_builtin_identifier_divide_towards_zero_nat8 = 292,
    modern_builtin_identifier_divide_towards_zero_nat16 = 293,
    modern_builtin_identifier_divide_towards_zero_nat32 = 294,
    modern_builtin_identifier_divide_towards_zero_nat64 = 295,
    modern_builtin_identifier_divide_towards_negative_infinity_int8 = 320,
    modern_builtin_identifier_divide_towards_negative_infinity_int16 = 321,
    modern_builtin_identifier_divide_towards_negative_infinity_int32 = 322,
    modern_builtin_identifier_divide_towards_negative_infinity_int64 = 323,
    modern_builtin_identifier_divide_float32 = 352,
    modern_builtin_identifier_divide_float64 = 353,
    modern_builtin_identifier_modulus_towards_zero_int8 = 384,
    modern_builtin_identifier_modulus_towards_zero_int16 = 385,
    modern_builtin_identifier_modulus_towards_zero_int32 = 386,
    modern_builtin_identifier_modulus_towards_zero_int64 = 387,
    modern_builtin_identifier_modulus_towards_zero_nat8 = 388,
    modern_builtin_identifier_modulus_towards_zero_nat16 = 389,
    modern_builtin_identifier_modulus_towards_zero_nat32 = 390,
    modern_builtin_identifier_modulus_towards_zero_nat64 = 391,
    modern_builtin_identifier_modulus_towards_negative_infinity_int8 = 416,
    modern_builtin_identifier_modulus_towards_negative_infinity_int16 = 417,
    modern_builtin_identifier_modulus_towards_negative_infinity_int32 = 418,
    modern_builtin_identifier_modulus_towards_negative_infinity_int64 = 419,
    modern_builtin_identifier_negate_int8 = 448,
    modern_builtin_identifier_negate_int16 = 449,
    modern_builtin_identifier_negate_int32 = 450,
    modern_builtin_identifier_negate_int64 = 451,
    modern_builtin_identifier_negate_float32 = 452,
    modern_builtin_identifier_negate_float64 = 453,
    modern_builtin_identifier_absolute_value_int8 = 480,
    modern_builtin_identifier_absolute_value_int16 = 481,
    modern_builtin_identifier_absolute_value_int32 = 482,
    modern_builtin_identifier_absolute_value_int64 = 483,
    modern_builtin_identifier_absolute_value_float32 = 484,
    modern_builtin_identifier_absolute_value_float64 = 485,
    modern_builtin_identifier_sign_int8 = 512,
    modern_builtin_identifier_sign_int16 = 513,
    modern_builtin_identifier_sign_int32 = 514,
    modern_builtin_identifier_sign_int64 = 515,
    modern_builtin_identifier_sign_float32 = 516,
    modern_builtin_identifier_sign_float64 = 517,
    modern_builtin_identifier_pi_float32 = 544,
    modern_builtin_identifier_pi_float64 = 545,
    modern_builtin_identifier_square_root_float32 = 576,
    modern_builtin_identifier_square_root_float64 = 577,
    modern_builtin_identifier_natural_logarithm_float32 = 608,
    modern_builtin_identifier_natural_logarithm_float64 = 609,
    modern_builtin_identifier_e_to_the_x_float32 = 640,
    modern_builtin_identifier_e_to_the_x_float64 = 641,
    modern_builtin_identifier_two_to_the_x_float32 = 672,
    modern_builtin_identifier_two_to_the_x_float64 = 673,
    modern_builtin_identifier_x_to_the_y_float32 = 704,
    modern_builtin_identifier_x_to_the_y_float64 = 705,
    modern_builtin_identifier_logarithm_base_x_float32 = 736,
    modern_builtin_identifier_logarithm_base_x_float64 = 737,
    modern_builtin_identifier_sine_float32 = 768,
    modern_builtin_identifier_sine_float64 = 769,
    modern_builtin_identifier_cosine_float32 = 800,
    modern_builtin_identifier_cosine_float64 = 801,
    modern_builtin_identifier_tangent_float32 = 832,
    modern_builtin_identifier_tangent_float64 = 833,
    modern_builtin_identifier_arcsine_float32 = 864,
    modern_builtin_identifier_arcsine_float64 = 865,
    modern_builtin_identifier_arccosine_float32 = 896,
    modern_builtin_identifier_arccosine_float64 = 897,
    modern_builtin_identifier_arctangent_float32 = 928,
    modern_builtin_identifier_arctangent_float64 = 929,
    modern_builtin_identifier_arctangent_fraction_float32 = 960,
    modern_builtin_identifier_arctangent_fraction_float64 = 961,
    modern_builtin_identifier_hyperbolic_sine_float32 = 992,
    modern_builtin_identifier_hyperbolic_sine_float64 = 993,
    modern_builtin_identifier_hyperbolic_cosine_float32 = 1024,
    modern_builtin_identifier_hyperbolic_cosine_float64 = 1025,
    modern_builtin_identifier_hyperbolic_tangent_float32 = 1056,
    modern_builtin_identifier_hyperbolic_tangent_float64 = 1057,
    modern_builtin_identifier_hyperbolic_arcsine_float32 = 1088,
    modern_builtin_identifier_hyperbolic_arcsine_float64 = 1089,
    modern_builtin_identifier_hyperbolic_arccosine_float32 = 1120,
    modern_builtin_identifier_hyperbolic_arccosine_float64 = 1121,
    modern_builtin_identifier_hyperbolic_arctangent_float32 = 1152,
    modern_builtin_identifier_hyperbolic_arctangent_float64 = 1153,
    modern_builtin_identifier_round_towards_zero_float32_int8 = 1184,
    modern_builtin_identifier_round_towards_zero_float64_int8 = 1185,
    modern_builtin_identifier_round_towards_zero_float32_int16 = 1186,
    modern_builtin_identifier_round_towards_zero_float64_int16 = 1187,
    modern_builtin_identifier_round_towards_zero_float32_int32 = 1188,
    modern_builtin_identifier_round_towards_zero_float64_int32 = 1189,
    modern_builtin_identifier_round_towards_zero_float32_int64 = 1190,
    modern_builtin_identifier_round_towards_zero_float64_int64 = 1191,
    modern_builtin_identifier_round_towards_zero_float32_nat8 = 1192,
    modern_builtin_identifier_round_towards_zero_float64_nat8 = 1193,
    modern_builtin_identifier_round_towards_zero_float32_nat16 = 1194,
    modern_builtin_identifier_round_towards_zero_float64_nat16 = 1195,
    modern_builtin_identifier_round_towards_zero_float32_nat32 = 1196,
    modern_builtin_identifier_round_towards_zero_float64_nat32 = 1197,
    modern_builtin_identifier_round_towards_zero_float32_nat64 = 1198,
    modern_builtin_identifier_round_towards_zero_float64_nat64 = 1199,
    modern_builtin_identifier_round_towards_zero_float32_float32 = 1200,
    modern_builtin_identifier_round_towards_zero_float64_float64 = 1201,
    modern_builtin_identifier_round_away_from_zero_float32_int8 = 1216,
    modern_builtin_identifier_round_away_from_zero_float64_int8 = 1217,
    modern_builtin_identifier_round_away_from_zero_float32_int16 = 1218,
    modern_builtin_identifier_round_away_from_zero_float64_int16 = 1219,
    modern_builtin_identifier_round_away_from_zero_float32_int32 = 1220,
    modern_builtin_identifier_round_away_from_zero_float64_int32 = 1221,
    modern_builtin_identifier_round_away_from_zero_float32_int64 = 1222,
    modern_builtin_identifier_round_away_from_zero_float64_int64 = 1223,
    modern_builtin_identifier_round_away_from_zero_float32_nat8 = 1224,
    modern_builtin_identifier_round_away_from_zero_float64_nat8 = 1225,
    modern_builtin_identifier_round_away_from_zero_float32_nat16 = 1226,
    modern_builtin_identifier_round_away_from_zero_float64_nat16 = 1227,
    modern_builtin_identifier_round_away_from_zero_float32_nat32 = 1228,
    modern_builtin_identifier_round_away_from_zero_float64_nat32 = 1229,
    modern_builtin_identifier_round_away_from_zero_float32_nat64 = 1230,
    modern_builtin_identifier_round_away_from_zero_float64_nat64 = 1231,
    modern_builtin_identifier_round_away_from_zero_float32_float32 = 1232,
    modern_builtin_identifier_round_away_from_zero_float64_float64 = 1233,
    modern_builtin_identifier_round_towards_even_float32_int8 = 1248,
    modern_builtin_identifier_round_towards_even_float64_int8 = 1249,
    modern_builtin_identifier_round_towards_even_float32_int16 = 1250,
    modern_builtin_identifier_round_towards_even_float64_int16 = 1251,
    modern_builtin_identifier_round_towards_even_float32_int32 = 1252,
    modern_builtin_identifier_round_towards_even_float64_int32 = 1253,
    modern_builtin_identifier_round_towards_even_float32_int64 = 1254,
    modern_builtin_identifier_round_towards_even_float64_int64 = 1255,
    modern_builtin_identifier_round_towards_even_float32_nat8 = 1256,
    modern_builtin_identifier_round_towards_even_float64_nat8 = 1257,
    modern_builtin_identifier_round_towards_even_float32_nat16 = 1258,
    modern_builtin_identifier_round_towards_even_float64_nat16 = 1259,
    modern_builtin_identifier_round_towards_even_float32_nat32 = 1260,
    modern_builtin_identifier_round_towards_even_float64_nat32 = 1261,
    modern_builtin_identifier_round_towards_even_float32_nat64 = 1262,
    modern_builtin_identifier_round_towards_even_float64_nat64 = 1263,
    modern_builtin_identifier_round_towards_even_float32_float32 = 1264,
    modern_builtin_identifier_round_towards_even_float64_float64 = 1265,
    modern_builtin_identifier_round_towards_odd_float32_int8 = 1280,
    modern_builtin_identifier_round_towards_odd_float64_int8 = 1281,
    modern_builtin_identifier_round_towards_odd_float32_int16 = 1282,
    modern_builtin_identifier_round_towards_odd_float64_int16 = 1283,
    modern_builtin_identifier_round_towards_odd_float32_int32 = 1284,
    modern_builtin_identifier_round_towards_odd_float64_int32 = 1285,
    modern_builtin_identifier_round_towards_odd_float32_int64 = 1286,
    modern_builtin_identifier_round_towards_odd_float64_int64 = 1287,
    modern_builtin_identifier_round_towards_odd_float32_nat8 = 1288,
    modern_builtin_identifier_round_towards_odd_float64_nat8 = 1289,
    modern_builtin_identifier_round_towards_odd_float32_nat16 = 1290,
    modern_builtin_identifier_round_towards_odd_float64_nat16 = 1291,
    modern_builtin_identifier_round_towards_odd_float32_nat32 = 1292,
    modern_builtin_identifier_round_towards_odd_float64_nat32 = 1293,
    modern_builtin_identifier_round_towards_odd_float32_nat64 = 1294,
    modern_builtin_identifier_round_towards_odd_float64_nat64 = 1295,
    modern_builtin_identifier_round_towards_odd_float32_float32 = 1296,
    modern_builtin_identifier_round_towards_odd_float64_float64 = 1297,
    modern_builtin_identifier_ceiling_float32_int8 = 1312,
    modern_builtin_identifier_ceiling_float64_int8 = 1313,
    modern_builtin_identifier_ceiling_float32_int16 = 1314,
    modern_builtin_identifier_ceiling_float64_int16 = 1315,
    modern_builtin_identifier_ceiling_float32_int32 = 1316,
    modern_builtin_identifier_ceiling_float64_int32 = 1317,
    modern_builtin_identifier_ceiling_float32_int64 = 1318,
    modern_builtin_identifier_ceiling_float64_int64 = 1319,
    modern_builtin_identifier_ceiling_float32_nat8 = 1320,
    modern_builtin_identifier_ceiling_float64_nat8 = 1321,
    modern_builtin_identifier_ceiling_float32_nat16 = 1322,
    modern_builtin_identifier_ceiling_float64_nat16 = 1323,
    modern_builtin_identifier_ceiling_float32_nat32 = 1324,
    modern_builtin_identifier_ceiling_float64_nat32 = 1325,
    modern_builtin_identifier_ceiling_float32_nat64 = 1326,
    modern_builtin_identifier_ceiling_float64_nat64 = 1327,
    modern_builtin_identifier_ceiling_float32_float32 = 1328,
    modern_builtin_identifier_ceiling_float64_float64 = 1329,
    modern_builtin_identifier_floor_float32_int8 = 1344,
    modern_builtin_identifier_floor_float64_int8 = 1345,
    modern_builtin_identifier_floor_float32_int16 = 1346,
    modern_builtin_identifier_floor_float64_int16 = 1347,
    modern_builtin_identifier_floor_float32_int32 = 1348,
    modern_builtin_identifier_floor_float64_int32 = 1349,
    modern_builtin_identifier_floor_float32_int64 = 1350,
    modern_builtin_identifier_floor_float64_int64 = 1351,
    modern_builtin_identifier_floor_float32_nat8 = 1352,
    modern_builtin_identifier_floor_float64_nat8 = 1353,
    modern_builtin_identifier_floor_float32_nat16 = 1354,
    modern_builtin_identifier_floor_float64_nat16 = 1355,
    modern_builtin_identifier_floor_float32_nat32 = 1356,
    modern_builtin_identifier_floor_float64_nat32 = 1357,
    modern_builtin_identifier_floor_float32_nat64 = 1358,
    modern_builtin_identifier_floor_float64_nat64 = 1359,
    modern_builtin_identifier_floor_float32_float32 = 1360,
    modern_builtin_identifier_floor_float64_float64 = 1361,
    modern_builtin_identifier_minimum_bound_int8 = 1376,
    modern_builtin_identifier_minimum_bound_int16 = 1377,
    modern_builtin_identifier_minimum_bound_int32 = 1378,
    modern_builtin_identifier_minimum_bound_int64 = 1379,
    modern_builtin_identifier_minimum_bound_nat8 = 1380,
    modern_builtin_identifier_minimum_bound_nat16 = 1381,
    modern_builtin_identifier_minimum_bound_nat32 = 1382,
    modern_builtin_identifier_minimum_bound_nat64 = 1383,
    modern_builtin_identifier_maximum_bound_int8 = 1408,
    modern_builtin_identifier_maximum_bound_int16 = 1409,
    modern_builtin_identifier_maximum_bound_int32 = 1410,
    modern_builtin_identifier_maximum_bound_int64 = 1411,
    modern_builtin_identifier_maximum_bound_nat8 = 1412,
    modern_builtin_identifier_maximum_bound_nat16 = 1413,
    modern_builtin_identifier_maximum_bound_nat32 = 1414,
    modern_builtin_identifier_maximum_bound_nat64 = 1415,
    modern_builtin_identifier_shift_left_int8 = 1440,
    modern_builtin_identifier_shift_left_int16 = 1441,
    modern_builtin_identifier_shift_left_int32 = 1442,
    modern_builtin_identifier_shift_left_int64 = 1443,
    modern_builtin_identifier_shift_left_nat8 = 1444,
    modern_builtin_identifier_shift_left_nat16 = 1445,
    modern_builtin_identifier_shift_left_nat32 = 1456,
    modern_builtin_identifier_shift_left_nat64 = 1457,
    modern_builtin_identifier_shift_right_int8 = 1472,
    modern_builtin_identifier_shift_right_int16 = 1473,
    modern_builtin_identifier_shift_right_int32 = 1474,
    modern_builtin_identifier_shift_right_int64 = 1475,
    modern_builtin_identifier_shift_right_nat8 = 1476,
    modern_builtin_identifier_shift_right_nat16 = 1477,
    modern_builtin_identifier_shift_right_nat32 = 1478,
    modern_builtin_identifier_shift_right_nat64 = 1479,
    modern_builtin_identifier_rotate_left_int8 = 1504,
    modern_builtin_identifier_rotate_left_int16 = 1505,
    modern_builtin_identifier_rotate_left_int32 = 1506,
    modern_builtin_identifier_rotate_left_int64 = 1507,
    modern_builtin_identifier_rotate_left_nat8 = 1508,
    modern_builtin_identifier_rotate_left_nat16 = 1509,
    modern_builtin_identifier_rotate_left_nat32 = 1510,
    modern_builtin_identifier_rotate_left_nat64 = 1511,
    modern_builtin_identifier_rotate_right_int8 = 1536,
    modern_builtin_identifier_rotate_right_int16 = 1537,
    modern_builtin_identifier_rotate_right_int32 = 1538,
    modern_builtin_identifier_rotate_right_int64 = 1539,
    modern_builtin_identifier_rotate_right_nat8 = 1540,
    modern_builtin_identifier_rotate_right_nat16 = 1541,
    modern_builtin_identifier_rotate_right_nat32 = 1542,
    modern_builtin_identifier_rotate_right_nat64 = 1543,
    modern_builtin_identifier_bit_and_int8 = 1568,
    modern_builtin_identifier_bit_and_int16 = 1569,
    modern_builtin_identifier_bit_and_int32 = 1570,
    modern_builtin_identifier_bit_and_int64 = 1571,
    modern_builtin_identifier_bit_and_nat8 = 1572,
    modern_builtin_identifier_bit_and_nat16 = 1573,
    modern_builtin_identifier_bit_and_nat32 = 1574,
    modern_builtin_identifier_bit_and_nat64 = 1575,
    modern_builtin_identifier_bit_or_int8 = 1600,
    modern_builtin_identifier_bit_or_int16 = 1601,
    modern_builtin_identifier_bit_or_int32 = 1602,
    modern_builtin_identifier_bit_or_int64 = 1603,
    modern_builtin_identifier_bit_or_nat8 = 1604,
    modern_builtin_identifier_bit_or_nat16 = 1605,
    modern_builtin_identifier_bit_or_nat32 = 1606,
    modern_builtin_identifier_bit_or_nat64 = 1607,
    modern_builtin_identifier_bit_xor_int8 = 1632,
    modern_builtin_identifier_bit_xor_int16 = 1633,
    modern_builtin_identifier_bit_xor_int32 = 1634,
    modern_builtin_identifier_bit_xor_int64 = 1635,
    modern_builtin_identifier_bit_xor_nat8 = 1636,
    modern_builtin_identifier_bit_xor_nat16 = 1637,
    modern_builtin_identifier_bit_xor_nat32 = 1638,
    modern_builtin_identifier_bit_xor_nat64 = 1639,
    modern_builtin_identifier_bit_not_int8 = 1664,
    modern_builtin_identifier_bit_not_int16 = 1665,
    modern_builtin_identifier_bit_not_int32 = 1666,
    modern_builtin_identifier_bit_not_int64 = 1667,
    modern_builtin_identifier_bit_not_nat8 = 1668,
    modern_builtin_identifier_bit_not_nat16 = 1669,
    modern_builtin_identifier_bit_not_nat32 = 1670,
    modern_builtin_identifier_bit_not_nat64 = 1671,
    modern_builtin_identifier_decode_utf8 = 1696,
    modern_builtin_identifier_encode_utf8 = 1728,
    modern_builtin_identifier_character_offset_to_byte_offset_utf8 = 1760,
    modern_builtin_identifier_length_bytes_utf8 = 1792,
    modern_builtin_identifier_length_bytes_blob = 1793,
    modern_builtin_identifier_get_byte_blob = 1824,
    modern_builtin_identifier_replace_byte_blob = 1856,
    modern_builtin_identifier_get_data_piece_utf8 = 1888,
    modern_builtin_identifier_get_data_piece_blob = 1889,
    modern_builtin_identifier_replace_data_piece_utf8 = 1920,
    modern_builtin_identifier_replace_data_piece_blob = 1921,
    modern_builtin_identifier_empty_utf8 = 1952,
    modern_builtin_identifier_empty_blob = 1953,
    modern_builtin_identifier_get_sigma_field_value = 1984,
    modern_builtin_identifier_get_sigma_successor = 2016,
    modern_builtin_identifier_get_named_value = 2048,
    modern_builtin_identifier_get_function_type_parameter = 2080,
    modern_builtin_identifier_get_function_type_result = 2112,
    modern_builtin_identifier_get_sigma_type_field_type = 2144,
    modern_builtin_identifier_get_sigma_type_successor = 2176,
    modern_builtin_identifier_get_named_type_content_type = 2208,
    modern_builtin_identifier_get_universe_type_level = 2240,
    modern_builtin_identifier_get_satisfies_type_supertype = 2256,
    modern_builtin_identifier_get_satisfies_type_predicate = 2257,
    modern_builtin_identifier_make_sigma = 2272,
    modern_builtin_identifier_make_name = 2304,
    modern_builtin_identifier_make_named_value = 2336,
    modern_builtin_identifier_make_function_type = 2368,
    modern_builtin_identifier_make_sigma_type = 2400,
    modern_builtin_identifier_make_named_type = 2432,
    modern_builtin_identifier_make_universe_type = 2464,
    modern_builtin_identifier_make_satisfies_type = 2480,
    modern_builtin_identifier_make_maybe_type = 2496,
    modern_builtin_identifier_get_maybe_type_content_type = 2528,
    modern_builtin_identifier_maybe_is_just = 2560,
    modern_builtin_identifier_fmap_maybe = 2592,
    modern_builtin_identifier_from_maybe = 2624,
    modern_builtin_identifier_cast_int8_int16 = 10240,
    modern_builtin_identifier_cast_int8_int32 = 10241,
    modern_builtin_identifier_cast_int8_int64 = 10242,
    modern_builtin_identifier_cast_int8_nat8 = 10243,
    modern_builtin_identifier_cast_int8_nat16 = 10244,
    modern_builtin_identifier_cast_int8_nat32 = 10245,
    modern_builtin_identifier_cast_int8_nat64 = 10246,
    modern_builtin_identifier_cast_int16_int8 = 10247,
    modern_builtin_identifier_cast_int16_int32 = 10248,
    modern_builtin_identifier_cast_int16_int64 = 10249,
    modern_builtin_identifier_cast_int16_nat8 = 10250,
    modern_builtin_identifier_cast_int16_nat16 = 10251,
    modern_builtin_identifier_cast_int16_nat32 = 10252,
    modern_builtin_identifier_cast_int16_nat64 = 10253,
    modern_builtin_identifier_cast_int32_int8 = 10254,
    modern_builtin_identifier_cast_int32_int16 = 10255,
    modern_builtin_identifier_cast_int32_int64 = 10256,
    modern_builtin_identifier_cast_int32_nat8 = 10257,
    modern_builtin_identifier_cast_int32_nat16 = 10258,
    modern_builtin_identifier_cast_int32_nat32 = 10259,
    modern_builtin_identifier_cast_int32_nat64 = 10260,
    modern_builtin_identifier_cast_int64_int8 = 10261,
    modern_builtin_identifier_cast_int64_int16 = 10262,
    modern_builtin_identifier_cast_int64_int32 = 10263,
    modern_builtin_identifier_cast_int64_nat8 = 10264,
    modern_builtin_identifier_cast_int64_nat16 = 10265,
    modern_builtin_identifier_cast_int64_nat32 = 10266,
    modern_builtin_identifier_cast_int64_nat64 = 10267,
    modern_builtin_identifier_cast_nat8_int8 = 10268,
    modern_builtin_identifier_cast_nat8_int16 = 10269,
    modern_builtin_identifier_cast_nat8_int32 = 10270,
    modern_builtin_identifier_cast_nat8_int64 = 10271,
    modern_builtin_identifier_cast_nat8_nat16 = 10272,
    modern_builtin_identifier_cast_nat8_nat32 = 10273,
    modern_builtin_identifier_cast_nat8_nat64 = 10274,
    modern_builtin_identifier_cast_nat16_int8 = 10275,
    modern_builtin_identifier_cast_nat16_int16 = 10276,
    modern_builtin_identifier_cast_nat16_int32 = 10277,
    modern_builtin_identifier_cast_nat16_int64 = 10278,
    modern_builtin_identifier_cast_nat16_nat8 = 10279,
    modern_builtin_identifier_cast_nat16_nat32 = 10280,
    modern_builtin_identifier_cast_nat16_nat64 = 10281,
    modern_builtin_identifier_cast_nat32_int8 = 10282,
    modern_builtin_identifier_cast_nat32_int16 = 10283,
    modern_builtin_identifier_cast_nat32_int32 = 10284,
    modern_builtin_identifier_cast_nat32_int64 = 10285,
    modern_builtin_identifier_cast_nat32_nat8 = 10286,
    modern_builtin_identifier_cast_nat32_nat16 = 10287,
    modern_builtin_identifier_cast_nat32_nat64 = 10288,
    modern_builtin_identifier_cast_nat64_int8 = 10289,
    modern_builtin_identifier_cast_nat64_int16 = 10290,
    modern_builtin_identifier_cast_nat64_int32 = 10291,
    modern_builtin_identifier_cast_nat64_int64 = 10292,
    modern_builtin_identifier_cast_nat64_nat8 = 10293,
    modern_builtin_identifier_cast_nat64_nat16 = 10294,
    modern_builtin_identifier_cast_nat64_nat32 = 10295,
    modern_builtin_identifier_cast_utf8_blob = 10296,
    modern_builtin_identifier_cast_blob_utf8 = 10297,
    modern_builtin_identifier_cast_name_blob = 10298,
    modern_builtin_identifier_cast_blob_name = 10299,
    modern_builtin_identifier_cast_float32_float64 = 10300,
    modern_builtin_identifier_cast_float64_float32 = 10301,
    modern_builtin_identifier_cast_blob_int8 = 10302,
    modern_builtin_identifier_cast_blob_int16 = 10303,
    modern_builtin_identifier_cast_blob_int32 = 10304,
    modern_builtin_identifier_cast_blob_int64 = 10305,
    modern_builtin_identifier_cast_blob_nat8 = 10306,
    modern_builtin_identifier_cast_blob_nat16 = 10307,
    modern_builtin_identifier_cast_blob_nat32 = 10308,
    modern_builtin_identifier_cast_blob_nat64 = 10309,
    modern_builtin_identifier_cast_blob_float32 = 10310,
    modern_builtin_identifier_cast_blob_float64 = 10311,
    modern_builtin_identifier_cast_int8_blob = 10312,
    modern_builtin_identifier_cast_int16_blob = 10313,
    modern_builtin_identifier_cast_int32_blob = 10314,
    modern_builtin_identifier_cast_int64_blob = 10315,
    modern_builtin_identifier_cast_nat8_blob = 10316,
    modern_builtin_identifier_cast_nat16_blob = 10317,
    modern_builtin_identifier_cast_nat32_blob = 10318,
    modern_builtin_identifier_cast_nat64_blob = 10319,
    modern_builtin_identifier_cast_float32_blob = 10320,
    modern_builtin_identifier_cast_float64_blob = 10321,
};


struct modern_node_representation {
    enum modern_node_type
      (*node_type_get)
      (modern_library *library,
       void *value);
    void *
      (*value_type_get)
      (modern_library *library,
       void *value);
    int
      (*mutable_get)
      (modern_library *library,
       void *value);
    int
      (*canonical_hash_valid_get)
      (modern_library *library,
       modern *value);
    struct modern_hash
      (*canonical_hash_get)
      (modern_library *library,
       modern *value);
    void *
      (*maybe_just_content_get)
      (modern_library *library,
       void *value);
    int8_t 
      (*int8_get)
      (modern_library *library,
       void *value);
    int16_t 
      (*int16_get)
      (modern_library *library,
       void *value);
    int32_t 
      (*int32_get)
      (modern_library *library,
       void *value);
    int64_t 
      (*int64_get)
      (modern_library *library,
       void *value);
    uint8_t 
      (*nat8_get)
      (modern_library *library,
       void *value);
    uint16_t 
      (*nat16_get)
      (modern_library *library,
       void *value);
    uint32_t 
      (*nat32_get)
      (modern_library *library,
       void *value);
    uint64_t 
      (*nat64_get)
      (modern_library *library,
       void *value);
    float 
      (*float32_get)
      (modern_library *library,
       void *value);
    double 
      (*float64_get)
      (modern_library *library,
       void *value);
    size_t 
      (*utf8_bytes_get)
      (modern_library *library,
       void *value);
    uint8_t *
      (*utf8_data_piece_get)
      (modern_library *library,
       void *value, size_t offset, size_t bytes);
    size_t 
      (*blob_bytes_get)
      (modern_library *library,
       void *value);
    uint8_t *
      (*blob_data_piece_get)
      (modern_library *library,
       void *value, size_t offset, size_t bytes);
    void *
      (*sigma_field_value_get)
      (modern_library *library,
       void *value);
    void *
      (*sigma_successor_get)
      (modern_library *library,
       void *value);
    struct modern_hash
      (*name_value_hash_get)
      (modern_library *library,
       void *value);
    void *
      (*named_value_get)
      (modern_library *library,
       void *value);
    void *
      (*maybe_type_content_type_get)
      (modern_library *library,
       void *value);
    void *
      (*function_type_parameter_get)
      (modern_library *library,
       void *value);
    void *
      (*function_type_result_get)
      (modern_library *library,
       void *value);
    void *
      (*sigma_type_field_type_get)
      (modern_library *library,
       void *value);
    void *
      (*sigma_type_successor_get)
      (modern_library *library,
       void *value);
    struct modern_hash
      (*named_type_name_get)
      (modern_library *library,
       void *value);
    void *
      (*named_type_content_type_get)
      (modern_library *library,
       void *value);
    uint64_t 
      (*universe_type_level_get)
      (modern_library *library,
       void *value);
    void *
      (*satisfies_type_supertype_get)
      (modern_library *library,
       void *value);
    void *
      (*satisfies_type_predicate_get)
      (modern_library *library,
       void *value);
    void *
      (*lambda_content_get)
      (modern_library *library,
       void *value);
    void *
      (*apply_function_get)
      (modern_library *library,
       void *value);
    void *
      (*apply_parameter_get)
      (modern_library *library,
       void *value);
    uint64_t 
      (*type_family_count_get)
      (modern_library *library,
       void *value);
    void *
      (*type_family_item_get)
      (modern_library *library,
       void *value, uint64_t index);
    uint64_t 
      (*let_count_get)
      (modern_library *library,
       void *value);
    void *
      (*let_item_get)
      (modern_library *library,
       void *value, uint64_t index);
    void *
      (*let_content_get)
      (modern_library *library,
       void *value);
    uint64_t 
      (*backreference_index_get)
      (modern_library *library,
       void *value);
    uint16_t 
      (*builtin_identifier_get)
      (modern_library *library,
       void *value);
    void *
      (*bool_false_make)
      (modern_library *library);
    void *
      (*bool_true_make)
      (modern_library *library);
    void *
      (*ordering_less_make)
      (modern_library *library);
    void *
      (*ordering_equal_make)
      (modern_library *library);
    void *
      (*ordering_greater_make)
      (modern_library *library);
    void *
      (*maybe_nothing_make)
      (modern_library *library,
       void *type);
    void *
      (*maybe_just_make)
      (modern_library *library,
       void *type,
       void *content_value);
    void *
      (*int8_make)
      (modern_library *library,
       int8_t value);
    void *
      (*int16_make)
      (modern_library *library,
       int16_t value);
    void *
      (*int32_make)
      (modern_library *library,
       int32_t value);
    void *
      (*int64_make)
      (modern_library *library,
       int64_t value);
    void *
      (*nat8_make)
      (modern_library *library,
       uint8_t value);
    void *
      (*nat16_make)
      (modern_library *library,
       uint16_t value);
    void *
      (*nat32_make)
      (modern_library *library,
       uint32_t value);
    void *
      (*nat64_make)
      (modern_library *library,
       uint64_t value);
    void *
      (*float32_make)
      (modern_library *library,
       float value);
    void *
      (*float64_make)
      (modern_library *library,
       double value);
    void *
      (*utf8_make)
      (modern_library *library,
       uint8_t *data);
    void *
      (*blob_make)
      (modern_library *library,
       uint8_t *data, size_t bytes);
    void *
      (*sigma_make)
      (modern_library *library,
       void *type, void *field_value, void *successor_value);
    void *
      (*name_value_make)
      (modern_library *library,
       struct modern_hash name);
    void *
      (*named_value_make)
      (modern_library *library,
       void *type, void *value);
    void *
      (*bool_type_make)
      (modern_library *library);
    void *
      (*ordering_type_make)
      (modern_library *library);
    void *
      (*maybe_type_make)
      (modern_library *library,
       void *content_type);
    void *
      (*int8_type_make)
      (modern_library *library);
    void *
      (*int16_type_make)
      (modern_library *library);
    void *
      (*int32_type_make)
      (modern_library *library);
    void *
      (*int64_type_make)
      (modern_library *library);
    void *
      (*nat8_type_make)
      (modern_library *library);
    void *
      (*nat16_type_make)
      (modern_library *library);
    void *
      (*nat32_type_make)
      (modern_library *library);
    void *
      (*nat64_type_make)
      (modern_library *library);
    void *
      (*float32_type_make)
      (modern_library *library);
    void *
      (*float64_type_make)
      (modern_library *library);
    void *
      (*utf8_type_make)
      (modern_library *library);
    void *
      (*blob_type_make)
      (modern_library *library);
    void *
      (*function_type_make)
      (modern_library *library,
       void *parameter, void *result);
    void *
      (*sigma_type_make)
      (modern_library *library,
       void *field_type, void *successor);
    void *
      (*name_type_make)
      (modern_library *library);
    void *
      (*named_type_make)
      (modern_library *library,
       struct modern_hash name, void *content_type);
    void *
      (*universe_type_make)
      (modern_library *library,
       uint64_t level);
    void *
      (*satisfies_type_make)
      (modern_library *library,
       void *supertype, void *predicate);
    void *
      (*lambda_make)
      (modern_library *library,
       void *content);
    void *
      (*apply_make)
      (modern_library *library,
       void *function, void *parameter);
    void *
      (*type_family_make)
      (modern_library *library,
       uint64_t n_items, void **types);
    void *
      (*let_make)
      (modern_library *library,
       uint64_t n_items, void **values, void *content);
    void *
      (*backreference_make)
      (modern_library *library,
       uint64_t index);
    void *
      (*builtin_make)
      (modern_library *library,
       uint16_t identifier);
    void 
      (*immutable_set)
      (modern_library *library,
       void *value);
    void
      (*canonical_hash_set)
      (modern_library *library,
       void *value,
       struct modern_hash hash);
    void 
      (*maybe_just_content_set)
      (modern_library *library,
       void *value,
       void *content_value);
    void 
      (*int8_set)
      (modern_library *library,
       void *node,
       int8_t value);
    void 
      (*int16_set)
      (modern_library *library,
       void *node,
       int16_t value);
    void 
      (*int32_set)
      (modern_library *library,
       void *node,
       int32_t value);
    void 
      (*int64_set)
      (modern_library *library,
       void *node,
       int64_t value);
    void 
      (*nat8_set)
      (modern_library *library,
       void *node,
       uint8_t value);
    void 
      (*nat16_set)
      (modern_library *library,
       void *node,
       uint16_t value);
    void 
      (*nat32_set)
      (modern_library *library,
       void *node,
       uint32_t value);
    void 
      (*nat64_set)
      (modern_library *library,
       void *node,
       uint64_t value);
    void 
      (*float32_set)
      (modern_library *library,
       void *node,
       float value);
    void 
      (*float64_set)
      (modern_library *library,
       void *node,
       double value);
    void 
      (*utf8_data_piece_set)
      (modern_library *library,
       void *value,
       uint8_t *data,
       size_t offset,
       size_t old_bytes,
       size_t new_bytes);
    void 
      (*blob_data_piece_set)
      (modern_library *library,
       void *value,
       uint8_t *data,
       size_t offset,
       size_t old_bytes,
       size_t new_bytes);
    void 
      (*sigma_set)
      (modern_library *library,
       void *value,
       void *field_value,
       void *successor);
    void 
      (*named_value_set)
      (modern_library *library,
       void *node,
       void *type,
       void *value);
    void 
      (*maybe_type_content_type_set)
      (modern_library *library,
       void *value,
       void *content_type);
    void 
      (*function_type_parameter_set)
      (modern_library *library,
       void *value,
       void *parameter);
    void 
      (*function_type_result_set)
      (modern_library *library,
       void *value,
       void *result);
    void 
      (*sigma_type_field_type_set)
      (modern_library *library,
       void *value,
       void *field_type);
    void 
      (*sigma_type_successor_set)
      (modern_library *library,
       void *value,
       void *successor);
    void 
      (*named_type_name_set)
      (modern_library *library,
       void *value,
       struct modern_hash name);
    void 
      (*named_type_content_type_set)
      (modern_library *library,
       void *value,
       void *content_type);
    void 
      (*universe_type_level_set)
      (modern_library *library,
       void *value,
       uint64_t level);
    void
      (*satisfies_type_supertype_set)
      (modern_library *library,
       void *value,
       void *supertype);
    void
      (*satisfies_type_predicate_set)
      (modern_library *library,
       void *value,
       void *predicate);
    void 
      (*lambda_content_set)
      (modern_library *library,
       void *value,
       void *content);
    void 
      (*apply_function_set)
      (modern_library *library,
       void *value,
       void *function);
    void 
      (*apply_parameter_set)
      (modern_library *library,
       void *value,
       void *function);
    void 
      (*type_family_item_add)
      (modern_library *library,
       void *value,
       void *item,
       uint64_t index);
    void 
      (*type_family_item_remove)
      (modern_library *library,
       void *value,
       uint64_t index);
    void 
      (*let_item_add)
      (modern_library *library,
       void *value,
       void *item,
       uint64_t index);
    void 
      (*let_item_remove)
      (modern_library *library,
       void *value,
       uint64_t index);
    void 
      (*let_content_set)
      (modern_library *library,
       void *value,
       void *content);
    void 
      (*backreference_index_set)
      (modern_library *library,
       void *value,
       uint64_t index);
    void 
      (*builtin_identifier_set)
      (modern_library *library,
       void *value,
       uint16_t identifier);
};


extern modern_library *modern_library_initialize
  (struct modern_error_handler *error_handler,
   struct modern_allocator *allocator,
   struct modern_node_representation *node_representation,
   void (*finalizer)(void *client_state),
   void *client_state);
extern struct modern_error_handler *modern_library_error_handler_get
  (modern_library *library);
extern struct modern_allocator *modern_library_allocator_get
  (modern_library *library);
extern struct modern_node_representation *modern_library_node_representation_get
  (modern_library *library);
extern void *modern_library_client_state_get
  (modern_library *library);
extern void modern_library_finalize(modern_library *library);

extern void modern_finalize
  (modern_library *library,
   void *finalizable);

extern modern_context *modern_initial_context_make
  (modern_library *library);
extern modern_context *modern_context_copy
  (modern_library *library,
   modern_context *context);
extern int modern_in_context_get
  (modern_library *library,
   modern_context *context, modern *node);
extern void modern_to_context_add
  (modern_library *library,
   modern_context *context, modern *node);
extern modern *modern_from_context_get
  (modern_library *library,
   modern_context *context, struct modern_hash hash);

extern modern *modern_deserialize_memory
  (modern_library *library,
   modern_context *context,
   uint8_t *data, size_t length);
extern modern *modern_deserialize_file
  (modern_library *library,
   modern_context *context,
   FILE *file);
extern modern *modern_deserialize_fd
  (modern_library *library,
   modern_context *context,
   int fd);
extern modern *modern_deserialize_vfile
  (modern_library *library,
   modern_context *context,
   struct modern_vfile *vfile, void *vfile_state);
extern modern *modern_deserialize_input_stream
  (modern_library *library,
   modern_context *context,
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

extern int modern_node_canonical_hash_compute
  (modern_library *library,
   modern *value,
   struct modern_hash *out);

extern modern *modern_evaluate
  (modern_library *library, modern *node);
extern modern *modern_resolve_backreferences
  (modern_library *library, modern *node);

extern void modern_hash_compute
  (uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_hash_child_compute
  (struct modern_hash *parent,
   uint8_t *data, size_t length, struct modern_hash *out);
extern void modern_hash_initial_namespace_compute(struct modern_hash *out);

extern struct modern_vfile *modern_vfile_memory_buffer_make
  (modern_library *library);
extern void *modern_vfile_memory_buffer_initialize
  (modern_library *library,
   uint8_t *data,
   size_t length);
extern void modern_vfile_memory_buffer_finalize
  (modern_library *library,
   void *vfile_state);

extern struct modern_vfile *modern_vfile_memory_allocating_make
  (modern_library *library);
extern void *modern_vfile_memory_allocating_initialize
  (modern_library *library);
extern void modern_vfile_memory_allocating_finalize
  (modern_library *library,
   void *vfile_state);
extern size_t modern_vfile_memory_allocating_bytes_get
  (modern_library *library,
   void *vfile_state);
extern uint8_t *modern_vfile_memory_allocating_data_piece_get
  (modern_library *library,
   void *vfile_state,
   size_t offset, size_t bytes);

extern struct modern_vfile *modern_vfile_stdio_make
  (modern_library *library);
extern void *modern_vfile_stdio_initialize
  (modern_library *library,
   FILE *stream);
extern void modern_vfile_stdio_finalize
  (modern_library *library,
   void *vfile_state);

extern struct modern_vfile *modern_vfile_fd_make
  (modern_library *library);
extern void *modern_vfile_fd_initialize
  (modern_library *library,
   int fd);
extern void modern_vfile_fd_finalize
  (modern_library *library,
   void *vfile_state);

extern struct modern_processor *modern_processor_input_make
  (modern_library *library);
extern void modern_processor_input_finalize
  (modern_library *library,
   struct modern_processor *processor);

extern struct modern_processor *modern_processor_explicatory_make
  (modern_library *library);
extern void modern_processor_explicatory_finalize
  (modern_library *library,
   struct modern_processor *processor);

extern struct modern_stream *modern_stream_output_make
  (modern_library *library);
extern void *modern_stream_output_initialize
  (modern_library *library);
extern void modern_stream_output_finalize
  (modern_library *library,
   void *stream_state);

extern struct modern_stream *modern_stream_explicatory_make
  (modern_library *library);
extern void *modern_stream_explicatory_initialize
  (modern_library *library);
extern void modern_stream_explicatory_finalize
  (modern_library *library,
   void *stream_state);

extern struct modern_node_representation
  *modern_node_representation_default_make
  (struct modern_allocator *allocator,
   void *client_state);
extern void
  modern_node_representation_default_finalize
  (struct modern_allocator *allocator,
   void *client_state,
   struct modern_node_representation *node_representation);

