window.BENCHMARK_DATA = {
  "lastUpdate": 1606134643315,
  "repoUrl": "https://github.com/schungx/rhai",
  "entries": {
    "Rust Benchmark": [
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "21b989afd53c65fc74783aff2e19525b44165881",
          "message": "Refine function call parameters size.",
          "timestamp": "2020-10-12T17:00:58+08:00",
          "tree_id": "498f413e7e6efaa08931b95a2f8693e557308ead",
          "url": "https://github.com/schungx/rhai/commit/21b989afd53c65fc74783aff2e19525b44165881"
        },
        "date": 1602495067895,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118283,
            "range": "± 23362",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 274,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2385,
            "range": "± 956",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2453,
            "range": "± 1121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7655846,
            "range": "± 977796",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 634,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 689,
            "range": "± 213",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14883,
            "range": "± 2349",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12857,
            "range": "± 2087",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 355,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 694,
            "range": "± 180",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 65,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 58,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2274855,
            "range": "± 182980",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3238572,
            "range": "± 615962",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2709023,
            "range": "± 482147",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2359,
            "range": "± 418",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2398,
            "range": "± 461",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 444,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 493,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1120,
            "range": "± 285",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2140,
            "range": "± 230",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 977,
            "range": "± 323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 956,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 420,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 376,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 261,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 351,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 713,
            "range": "± 226",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 465,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 584576,
            "range": "± 109836",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29264778,
            "range": "± 4194651",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2963,
            "range": "± 436",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11835,
            "range": "± 2409",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3809,
            "range": "± 725",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14343,
            "range": "± 1083",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14715,
            "range": "± 3368",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33699,
            "range": "± 2616",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2693,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 352,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2043861,
            "range": "± 267220",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6d0851de4448278f04585421a808d7c12cb934ce",
          "message": "Reverse function call parameter change.",
          "timestamp": "2020-10-12T19:36:34+08:00",
          "tree_id": "9fefa38fc0978bb25b87fd0e3692968ac11fb139",
          "url": "https://github.com/schungx/rhai/commit/6d0851de4448278f04585421a808d7c12cb934ce"
        },
        "date": 1602503095633,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116161,
            "range": "± 4543",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 79,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 270,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2257,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2103,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7388947,
            "range": "± 304217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 634,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 698,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15326,
            "range": "± 1009",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13203,
            "range": "± 1080",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 342,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 608,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 60,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 60,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 60,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2240621,
            "range": "± 162038",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2994937,
            "range": "± 216442",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2538204,
            "range": "± 155194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2291,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2282,
            "range": "± 211",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 432,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 490,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 935,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1910,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 813,
            "range": "± 176",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 894,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 347,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 361,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 213,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 332,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 692,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 406,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 522914,
            "range": "± 46212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25282503,
            "range": "± 1684449",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2586,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11667,
            "range": "± 1019",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3569,
            "range": "± 269",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13848,
            "range": "± 1156",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14029,
            "range": "± 1093",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 32776,
            "range": "± 2907",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2558,
            "range": "± 251",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 332,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1878569,
            "range": "± 182548",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "747fda1ec7435ca0e5aa3e3cbc1ace97f502031a",
          "message": "Add filter, map, reduce to Array.",
          "timestamp": "2020-10-12T22:49:51+08:00",
          "tree_id": "2bd28689eb44de787b8ed63c71dbafa9a6cabdf1",
          "url": "https://github.com/schungx/rhai/commit/747fda1ec7435ca0e5aa3e3cbc1ace97f502031a"
        },
        "date": 1602514842113,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 95522,
            "range": "± 10968",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 72,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 65,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 224,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1959,
            "range": "± 321",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1974,
            "range": "± 259",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6471943,
            "range": "± 867138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 569,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 659,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14408,
            "range": "± 1705",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12675,
            "range": "± 967",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 328,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 600,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 60,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 59,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 59,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2262479,
            "range": "± 147893",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2839038,
            "range": "± 270290",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2379490,
            "range": "± 327636",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1965,
            "range": "± 207",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1969,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 378,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 402,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 797,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1633,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 729,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 772,
            "range": "± 98",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 302,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 296,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 200,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 314,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 641,
            "range": "± 98",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 403,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 426206,
            "range": "± 83591",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 23883002,
            "range": "± 3704068",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2489,
            "range": "± 555",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11206,
            "range": "± 1266",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3277,
            "range": "± 564",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14614,
            "range": "± 1557",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13763,
            "range": "± 2535",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 32264,
            "range": "± 3750",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2411,
            "range": "± 547",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 280,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1648242,
            "range": "± 202554",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "e6667a3996ab70e8b82ddd5844cd953e1184a0d5",
          "message": "Revise docs.",
          "timestamp": "2020-10-12T23:17:22+08:00",
          "tree_id": "90eeda6e3d0fdaf66b237562b568a9e5957593ee",
          "url": "https://github.com/schungx/rhai/commit/e6667a3996ab70e8b82ddd5844cd953e1184a0d5"
        },
        "date": 1602516076429,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 115478,
            "range": "± 689",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 263,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2315,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2163,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7508900,
            "range": "± 583517",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 650,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 699,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15304,
            "range": "± 639",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13420,
            "range": "± 749",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 351,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 633,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 62,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 62,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2347431,
            "range": "± 70212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3198106,
            "range": "± 66683",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2709278,
            "range": "± 48346",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2404,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2435,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 464,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 505,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1003,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1997,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 880,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 961,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 376,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 374,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 222,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 346,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 702,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 415,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 530706,
            "range": "± 30750",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26105285,
            "range": "± 728506",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2690,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11837,
            "range": "± 529",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3826,
            "range": "± 519",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14264,
            "range": "± 1820",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13982,
            "range": "± 1783",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34587,
            "range": "± 2319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2732,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 345,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2012806,
            "range": "± 165288",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "1c86c66f1a687728f793ac478356cffbc59924fc",
          "message": "Add more functions to arrays.",
          "timestamp": "2020-10-13T10:57:29+08:00",
          "tree_id": "b360c378f40e6e5a091de47df454ca3064c319b3",
          "url": "https://github.com/schungx/rhai/commit/1c86c66f1a687728f793ac478356cffbc59924fc"
        },
        "date": 1602558420252,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 121162,
            "range": "± 28906",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 90,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 73,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 306,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2341,
            "range": "± 408",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2187,
            "range": "± 516",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7745785,
            "range": "± 1464455",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 638,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 671,
            "range": "± 190",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14966,
            "range": "± 5484",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13173,
            "range": "± 5002",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 356,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 630,
            "range": "± 217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 75,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 75,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 67,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2250181,
            "range": "± 480746",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3258148,
            "range": "± 427054",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2725211,
            "range": "± 422504",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2624,
            "range": "± 1092",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2689,
            "range": "± 840",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 494,
            "range": "± 217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 548,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 994,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1647,
            "range": "± 442",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 977,
            "range": "± 311",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 950,
            "range": "± 221",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 379,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 374,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 252,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 388,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 713,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 414,
            "range": "± 98",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 588255,
            "range": "± 199906",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 30332510,
            "range": "± 3897487",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3048,
            "range": "± 1494",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13340,
            "range": "± 1529",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3782,
            "range": "± 785",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14490,
            "range": "± 6297",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14646,
            "range": "± 1848",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33787,
            "range": "± 8370",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2812,
            "range": "± 519",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 395,
            "range": "± 185",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2237480,
            "range": "± 316609",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "13c4d0bbb35b42db21ee07269841ca19c3c60c71",
          "message": "Adjust precedence of in.",
          "timestamp": "2020-10-13T16:01:42+08:00",
          "tree_id": "7e462c326149cec8d83b49be2a6ba20168fffeeb",
          "url": "https://github.com/schungx/rhai/commit/13c4d0bbb35b42db21ee07269841ca19c3c60c71"
        },
        "date": 1602579809489,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118900,
            "range": "± 1256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 78,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 266,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2326,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2171,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7496444,
            "range": "± 6578",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 648,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 693,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 342,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 620,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 64,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2329028,
            "range": "± 2768",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3142060,
            "range": "± 3913",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2686140,
            "range": "± 4060",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "3df8d6c398e1b9ad84136c99cd9ba859ef5ea8ba",
          "message": "Fix typo.",
          "timestamp": "2020-10-13T17:16:19+08:00",
          "tree_id": "e9a280320a433abee0b147fcf2807cb767105f96",
          "url": "https://github.com/schungx/rhai/commit/3df8d6c398e1b9ad84136c99cd9ba859ef5ea8ba"
        },
        "date": 1602580941971,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116615,
            "range": "± 11020",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 80,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 74,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 277,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2379,
            "range": "± 279",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2154,
            "range": "± 201",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7588053,
            "range": "± 785660",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 629,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 671,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14288,
            "range": "± 1456",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12727,
            "range": "± 1617",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 337,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 618,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 58,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 60,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 59,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2296435,
            "range": "± 280062",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3189591,
            "range": "± 268506",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2680337,
            "range": "± 215168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2348,
            "range": "± 282",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2403,
            "range": "± 302",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 437,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 492,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 995,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1707,
            "range": "± 185",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 900,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1046,
            "range": "± 160",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 424,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 390,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 231,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 720,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 417,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 547432,
            "range": "± 73670",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29961730,
            "range": "± 2050476",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2796,
            "range": "± 406",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11777,
            "range": "± 1470",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3740,
            "range": "± 399",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14451,
            "range": "± 1101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15067,
            "range": "± 1509",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34472,
            "range": "± 4204",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2671,
            "range": "± 254",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 367,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2092412,
            "range": "± 198138",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "707ece7e80dad9f67e29d15c4e77bec83f1961c6",
          "message": "Refactor iterators API.",
          "timestamp": "2020-10-14T23:22:10+08:00",
          "tree_id": "372a5b7037afef1966e71fcb120d6ae8d326efad",
          "url": "https://github.com/schungx/rhai/commit/707ece7e80dad9f67e29d15c4e77bec83f1961c6"
        },
        "date": 1602690081072,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 125885,
            "range": "± 14301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 84,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 81,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 289,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2345,
            "range": "± 273",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2235,
            "range": "± 222",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7702076,
            "range": "± 770090",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 651,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 683,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14731,
            "range": "± 2144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12794,
            "range": "± 2089",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 385,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 650,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 60,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 59,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 59,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2450975,
            "range": "± 221327",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3418316,
            "range": "± 460973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2748019,
            "range": "± 279083",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2347,
            "range": "± 349",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2503,
            "range": "± 244",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 464,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 500,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1051,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1716,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 925,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 961,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 394,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 240,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 369,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 762,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 434,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 551877,
            "range": "± 37108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29816979,
            "range": "± 2492406",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2743,
            "range": "± 317",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12224,
            "range": "± 1455",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3879,
            "range": "± 503",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15135,
            "range": "± 2162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15088,
            "range": "± 2838",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33914,
            "range": "± 3888",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2730,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 388,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2042888,
            "range": "± 291256",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "3c9250b0bfd90bd9059e5ddc483a6b5d60aaac13",
          "message": "Skip wrapping if function returns Dynamicc.",
          "timestamp": "2020-10-15T13:28:22+08:00",
          "tree_id": "1b1b241871976ba7d7ba9f05aaa10c25f4ad011a",
          "url": "https://github.com/schungx/rhai/commit/3c9250b0bfd90bd9059e5ddc483a6b5d60aaac13"
        },
        "date": 1602741629745,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 120433,
            "range": "± 4475",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 81,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 261,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2301,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2100,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7540312,
            "range": "± 351502",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 642,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 674,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15468,
            "range": "± 737",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13434,
            "range": "± 662",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 334,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 611,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2266052,
            "range": "± 101983",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3141720,
            "range": "± 122493",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2636500,
            "range": "± 118823",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2403,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2465,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 451,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 495,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 986,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1656,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 869,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 951,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 374,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 359,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 225,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 332,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 708,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 417,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 537951,
            "range": "± 35369",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26811215,
            "range": "± 867696",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2758,
            "range": "± 146",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12092,
            "range": "± 1005",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3801,
            "range": "± 306",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14766,
            "range": "± 1083",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14512,
            "range": "± 606",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34569,
            "range": "± 3164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2672,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 349,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1996654,
            "range": "± 145259",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "8abb3c52037099cce5268714709a19d3e0c89ad2",
          "message": "Fix ui tests.",
          "timestamp": "2020-10-15T14:06:54+08:00",
          "tree_id": "a9351541e68b2922c1eee2740ea4f3465ef4060d",
          "url": "https://github.com/schungx/rhai/commit/8abb3c52037099cce5268714709a19d3e0c89ad2"
        },
        "date": 1602742237097,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 119770,
            "range": "± 3903",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 69,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 261,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2066,
            "range": "± 205",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2099,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7714124,
            "range": "± 196892",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 647,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 690,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 13415,
            "range": "± 1615",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12962,
            "range": "± 1578",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 351,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 629,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 62,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 62,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2349607,
            "range": "± 69206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2950721,
            "range": "± 195392",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2548406,
            "range": "± 236004",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2409,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2451,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 460,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 507,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1049,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1649,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 775,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 898,
            "range": "± 148",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 368,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 370,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 221,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 345,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 721,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 419,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 540824,
            "range": "± 28799",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26959760,
            "range": "± 1206692",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2691,
            "range": "± 239",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11737,
            "range": "± 2402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3757,
            "range": "± 368",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14966,
            "range": "± 1165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15095,
            "range": "± 766",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35451,
            "range": "± 1195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2724,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 351,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1966873,
            "range": "± 301402",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "a6fa94d946391d6e47a4f729971202b93208352d",
          "message": "Rhai function names with $ and . no longer fail.",
          "timestamp": "2020-10-15T14:36:10+08:00",
          "tree_id": "d98032d9bf18fef048adc139209b1fb9d33a4598",
          "url": "https://github.com/schungx/rhai/commit/a6fa94d946391d6e47a4f729971202b93208352d"
        },
        "date": 1602744083886,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 107599,
            "range": "± 15238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 70,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 65,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 234,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2030,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1859,
            "range": "± 217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7130574,
            "range": "± 1119462",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 565,
            "range": "± 124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 578,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12841,
            "range": "± 3489",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11000,
            "range": "± 1247",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 311,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 553,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 52,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 53,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 51,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1927165,
            "range": "± 218252",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2741133,
            "range": "± 410009",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2288826,
            "range": "± 376174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2094,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2104,
            "range": "± 312",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 414,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 424,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 854,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1456,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 751,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 827,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 340,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 334,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 205,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 286,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 608,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 362,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 457346,
            "range": "± 86610",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25464410,
            "range": "± 2508596",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2281,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 10630,
            "range": "± 2228",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3328,
            "range": "± 739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 12493,
            "range": "± 1438",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 12406,
            "range": "± 1744",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 29331,
            "range": "± 7134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2410,
            "range": "± 432",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 314,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1726019,
            "range": "± 221443",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "1e21a7f7e7147f8848381b45b1f8edf213d2c318",
          "message": "Introduce IndexChainValue.",
          "timestamp": "2020-10-15T23:30:30+08:00",
          "tree_id": "db79430a78f195f7aa1e1e8022ed4256c527c84e",
          "url": "https://github.com/schungx/rhai/commit/1e21a7f7e7147f8848381b45b1f8edf213d2c318"
        },
        "date": 1602776287077,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113899,
            "range": "± 7522",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 78,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 73,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 246,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2301,
            "range": "± 169",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2143,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7056960,
            "range": "± 346498",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 670,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 731,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15391,
            "range": "± 4086",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12945,
            "range": "± 1002",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 339,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 605,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 60,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 60,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 60,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2354026,
            "range": "± 41960",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3120103,
            "range": "± 56065",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2635778,
            "range": "± 71574",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2511,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2560,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 473,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 525,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 970,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1629,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 864,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 899,
            "range": "± 91",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 372,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 369,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 242,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 325,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 735,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 376,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 493150,
            "range": "± 54456",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25789158,
            "range": "± 1206291",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2657,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11578,
            "range": "± 983",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3729,
            "range": "± 291",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13963,
            "range": "± 973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13837,
            "range": "± 1666",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33731,
            "range": "± 2162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2588,
            "range": "± 368",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 326,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1997434,
            "range": "± 112148",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "54d5b293908cf73fadf398a6411ebb11d53c358d",
          "message": "Remove clone.",
          "timestamp": "2020-10-15T23:44:05+08:00",
          "tree_id": "2c57fca587d56c0f4acfaebbce7c51d4ecc7c314",
          "url": "https://github.com/schungx/rhai/commit/54d5b293908cf73fadf398a6411ebb11d53c358d"
        },
        "date": 1602840771516,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 136580,
            "range": "± 26658",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 86,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 262,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2433,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2275,
            "range": "± 332",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7499713,
            "range": "± 123358",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 673,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 712,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15261,
            "range": "± 166",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13451,
            "range": "± 172",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 345,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 621,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 64,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2325468,
            "range": "± 1582",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3127280,
            "range": "± 4795",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2661504,
            "range": "± 3098",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2542,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2566,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 472,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 527,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 996,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1739,
            "range": "± 272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 867,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 951,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 374,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 371,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 243,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 323,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 757,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 393,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 541509,
            "range": "± 2161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27009791,
            "range": "± 94835",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2788,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12318,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3834,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14911,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15119,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35891,
            "range": "± 276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2691,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 353,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2135923,
            "range": "± 5146",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "849aec06212c1de1a98fd57551e2e9444c5a3c05",
          "message": "Set version to 0.19.1.",
          "timestamp": "2020-10-16T17:32:26+08:00",
          "tree_id": "d57d2c80b2b32d87ba992903ad422b0bf7b65d64",
          "url": "https://github.com/schungx/rhai/commit/849aec06212c1de1a98fd57551e2e9444c5a3c05"
        },
        "date": 1602841109735,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118098,
            "range": "± 6686",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 84,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 256,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2412,
            "range": "± 260",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2248,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7366114,
            "range": "± 343081",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 659,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 691,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14888,
            "range": "± 674",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13046,
            "range": "± 507",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 343,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 611,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2331198,
            "range": "± 66998",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3129974,
            "range": "± 174744",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2644583,
            "range": "± 92496",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2419,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2517,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 458,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 523,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 993,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1656,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 864,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 936,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 382,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 372,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 243,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 321,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 754,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 392,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 544799,
            "range": "± 174486",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26641450,
            "range": "± 473270",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2735,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12255,
            "range": "± 1119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3828,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14506,
            "range": "± 905",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15038,
            "range": "± 1122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34883,
            "range": "± 5543",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2665,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 352,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2093981,
            "range": "± 127531",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f995d09d8b1377c6f86318a3d8671a4010061dc5",
          "message": "Fix bug with calling scripted function.",
          "timestamp": "2020-10-16T21:16:06+08:00",
          "tree_id": "7c80d56fa2466b13f94664c4224f3a72ba239b51",
          "url": "https://github.com/schungx/rhai/commit/f995d09d8b1377c6f86318a3d8671a4010061dc5"
        },
        "date": 1602854561744,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 119092,
            "range": "± 1631",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 81,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 270,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2456,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2313,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7504155,
            "range": "± 14178",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 671,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 713,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15219,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13436,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 345,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 620,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 64,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 64,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2323445,
            "range": "± 1796",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3134423,
            "range": "± 7973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2640336,
            "range": "± 15542",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2476,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2531,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 477,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 527,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1034,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2028,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 861,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 946,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 373,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 370,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 246,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 325,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 754,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 393,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 536603,
            "range": "± 80168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27071310,
            "range": "± 117288",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2768,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12335,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3875,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15057,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15249,
            "range": "± 119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 36040,
            "range": "± 317",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2717,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 356,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2126982,
            "range": "± 3341",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6f134368c3cdb84feb25515430542e5f2b601d5e",
          "message": "Bump version.",
          "timestamp": "2020-10-16T21:19:24+08:00",
          "tree_id": "df1c703802c1e3a167f5f9151cca089ff8eeede4",
          "url": "https://github.com/schungx/rhai/commit/6f134368c3cdb84feb25515430542e5f2b601d5e"
        },
        "date": 1602854801946,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 125953,
            "range": "± 1165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 87,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 80,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 275,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2617,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2383,
            "range": "± 764",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 8159520,
            "range": "± 941478",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 746,
            "range": "± 213",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 750,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16037,
            "range": "± 176",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13910,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 364,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 659,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 66,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 66,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 66,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2471869,
            "range": "± 1861",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3385853,
            "range": "± 4559",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2905073,
            "range": "± 6049",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2617,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2662,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 493,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 547,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1024,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2231,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 917,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 999,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 395,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 393,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 263,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 355,
            "range": "± 264",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 797,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 424,
            "range": "± 389",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 570595,
            "range": "± 178898",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29898291,
            "range": "± 2278742",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2962,
            "range": "± 371",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13101,
            "range": "± 1081",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4052,
            "range": "± 313",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15334,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15443,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 37091,
            "range": "± 294",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2851,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 370,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2225446,
            "range": "± 5691",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "d88adfd73dfae7bee78771ff0e97d8314657e6a0",
          "message": "Fix test.",
          "timestamp": "2020-10-16T21:26:38+08:00",
          "tree_id": "ce437a753bd83e180bf0aedc5e804659cde044a1",
          "url": "https://github.com/schungx/rhai/commit/d88adfd73dfae7bee78771ff0e97d8314657e6a0"
        },
        "date": 1602855214262,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 119597,
            "range": "± 54707",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 82,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 83,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 282,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2455,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2304,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7399169,
            "range": "± 747741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 656,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 693,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14997,
            "range": "± 1484",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13203,
            "range": "± 793",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 341,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 618,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 58,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 58,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 58,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2279707,
            "range": "± 168201",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3221239,
            "range": "± 501439",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2737238,
            "range": "± 520587",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2461,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2515,
            "range": "± 119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 483,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 527,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1001,
            "range": "± 231",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2029,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 900,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1003,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 378,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 373,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 249,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 320,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 751,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 392,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 525331,
            "range": "± 163689",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29121682,
            "range": "± 1759037",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2722,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11965,
            "range": "± 976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3802,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14578,
            "range": "± 1482",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14634,
            "range": "± 874",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33718,
            "range": "± 3918",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2699,
            "range": "± 546",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 353,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2084789,
            "range": "± 154385",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "39474d642001f9438c3239fd542c6f28329f94f8",
          "message": "Streamline function pointers and currying.",
          "timestamp": "2020-10-17T13:49:16+08:00",
          "tree_id": "ebd2c7b96ccfd07fea8f8dad1c6c2c3ab004b6ae",
          "url": "https://github.com/schungx/rhai/commit/39474d642001f9438c3239fd542c6f28329f94f8"
        },
        "date": 1602914732022,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 121062,
            "range": "± 1112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 84,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 76,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 263,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2424,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2263,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7411431,
            "range": "± 6163",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 676,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 718,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15477,
            "range": "± 255",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13492,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 351,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 628,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 64,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2352663,
            "range": "± 1672",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3216214,
            "range": "± 4310",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2728094,
            "range": "± 1487",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2507,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2560,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 480,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 539,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 972,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2048,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 862,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 947,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 373,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 370,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 242,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 324,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 754,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 393,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 539616,
            "range": "± 35823",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26948744,
            "range": "± 109521",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2790,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12377,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3878,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14927,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15034,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35841,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2782,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 353,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2112565,
            "range": "± 12560",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "8eb6c821d4687ed40fda31cf9125ebaa02e9ce91",
          "message": "Fix tests.",
          "timestamp": "2020-10-17T14:08:59+08:00",
          "tree_id": "d4188df611ce8057d78c3648281ac2666571c4b5",
          "url": "https://github.com/schungx/rhai/commit/8eb6c821d4687ed40fda31cf9125ebaa02e9ce91"
        },
        "date": 1602915554312,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117590,
            "range": "± 6790",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 82,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 258,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2303,
            "range": "± 198",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2163,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7619028,
            "range": "± 418272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 660,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 708,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14869,
            "range": "± 989",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13164,
            "range": "± 1402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 346,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 623,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2345948,
            "range": "± 117775",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3129205,
            "range": "± 242918",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2712560,
            "range": "± 206933",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2502,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2564,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 482,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 541,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 977,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2002,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 860,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 943,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 372,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 374,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 239,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 310,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 716,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 384,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 531049,
            "range": "± 28319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26460434,
            "range": "± 1250271",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2788,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12462,
            "range": "± 551",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3895,
            "range": "± 561",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14984,
            "range": "± 262",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15086,
            "range": "± 1319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35431,
            "range": "± 3186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2760,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 358,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2129072,
            "range": "± 32008",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f903eda8ab3ef6e3dfbaa0c6b274bad2fb35596f",
          "message": "Catch Fn and eval in method call at parse time.",
          "timestamp": "2020-10-17T18:18:29+08:00",
          "tree_id": "b569ce89d0bec0c78777d8f893d41685b7c62845",
          "url": "https://github.com/schungx/rhai/commit/f903eda8ab3ef6e3dfbaa0c6b274bad2fb35596f"
        },
        "date": 1602935257099,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 120192,
            "range": "± 1800",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 78,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 258,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2439,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2271,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7401103,
            "range": "± 11794",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 675,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 721,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15626,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13526,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 352,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 649,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 62,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 62,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2363168,
            "range": "± 6104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3206347,
            "range": "± 8275",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2725257,
            "range": "± 11957",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2539,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2618,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 477,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 534,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 983,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2006,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 868,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 948,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 383,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 381,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 257,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 342,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 767,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 410,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 540801,
            "range": "± 196764",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26923858,
            "range": "± 129741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2758,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12423,
            "range": "± 138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3964,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15118,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15173,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35374,
            "range": "± 311",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2792,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 358,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2144455,
            "range": "± 11945",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "aa6d00f25370b07029b5af5368532264d8343391",
          "message": "Fix bug.",
          "timestamp": "2020-10-17T20:01:31+08:00",
          "tree_id": "df0fa4ae4d85860c403e3b5fec58988bdf454324",
          "url": "https://github.com/schungx/rhai/commit/aa6d00f25370b07029b5af5368532264d8343391"
        },
        "date": 1602936419716,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 126063,
            "range": "± 10594",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 87,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 80,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 271,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2614,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2465,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7726772,
            "range": "± 6233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 700,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 749,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15997,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13942,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 367,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 663,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 67,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 67,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 67,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2460514,
            "range": "± 3789",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3362543,
            "range": "± 1830",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2867756,
            "range": "± 6560",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2596,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2653,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 500,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 559,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1033,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2170,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 906,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 996,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 390,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 387,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 256,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 338,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 786,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 409,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 559533,
            "range": "± 1975",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27974054,
            "range": "± 129614",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2899,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12602,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4025,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15478,
            "range": "± 103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15606,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 37148,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2841,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 375,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2294841,
            "range": "± 71461",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "82e6dd446ad30bf470af99a281632a175bddc8d1",
          "message": "Encapsulate register_fn_raw parameters into NativeCallContext.",
          "timestamp": "2020-10-18T17:02:17+08:00",
          "tree_id": "4a36b9c08566abeb0458fa20d9d9a3f01cb95253",
          "url": "https://github.com/schungx/rhai/commit/82e6dd446ad30bf470af99a281632a175bddc8d1"
        },
        "date": 1603012062554,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 121467,
            "range": "± 846",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 262,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2302,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2116,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7498950,
            "range": "± 9825",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 670,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 713,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15691,
            "range": "± 1732",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13753,
            "range": "± 1174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 352,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 636,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 64,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2413502,
            "range": "± 1069016",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2978478,
            "range": "± 4004",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2680450,
            "range": "± 5025",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2408,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2460,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 473,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 526,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 992,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 2053,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 846,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 956,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 387,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 384,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 240,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 327,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 741,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 392,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 536370,
            "range": "± 48151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26621935,
            "range": "± 1037828",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2776,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12321,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3904,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15048,
            "range": "± 180",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15077,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35445,
            "range": "± 324",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2788,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 351,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2152166,
            "range": "± 63990",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "232ff91957bcfaea97db07a3f0e4d600ee1b29d3",
          "message": "Fix doc test.",
          "timestamp": "2020-10-18T17:08:57+08:00",
          "tree_id": "643b04ab9cdbf9a2307519004a272cd02bd38c34",
          "url": "https://github.com/schungx/rhai/commit/232ff91957bcfaea97db07a3f0e4d600ee1b29d3"
        },
        "date": 1603012413386,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 120952,
            "range": "± 6739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 92,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 262,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2306,
            "range": "± 160",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2187,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7580186,
            "range": "± 299958",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 677,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 683,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15225,
            "range": "± 600",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13670,
            "range": "± 419",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 351,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 634,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2365985,
            "range": "± 174523",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2954245,
            "range": "± 216822",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2629789,
            "range": "± 252011",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2186,
            "range": "± 727",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2444,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 481,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 532,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1005,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1884,
            "range": "± 272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 846,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 956,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 368,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 376,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 239,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 311,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 712,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 360,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 530372,
            "range": "± 41285",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25381873,
            "range": "± 2218272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2731,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11719,
            "range": "± 1197",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3842,
            "range": "± 543",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14891,
            "range": "± 1059",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14424,
            "range": "± 1152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35659,
            "range": "± 3557",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2625,
            "range": "± 310",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 351,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2045109,
            "range": "± 208372",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "46b92c9d1f22970a84c574c3200f9b3d94e5e5a2",
          "message": "Allow NativeCallContext in function arguments.",
          "timestamp": "2020-10-18T21:47:34+08:00",
          "tree_id": "ebebfbe6087963726fc3d9b86cdf3f8571fe5f5e",
          "url": "https://github.com/schungx/rhai/commit/46b92c9d1f22970a84c574c3200f9b3d94e5e5a2"
        },
        "date": 1603030066380,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118637,
            "range": "± 15776",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 262,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2252,
            "range": "± 410",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2054,
            "range": "± 321",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7113489,
            "range": "± 894631",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 633,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 698,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15405,
            "range": "± 2192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13083,
            "range": "± 2543",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 347,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 610,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 57,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 58,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 61,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2241771,
            "range": "± 348750",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2852009,
            "range": "± 467758",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2552407,
            "range": "± 503592",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2183,
            "range": "± 358",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2293,
            "range": "± 400",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 443,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 505,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 973,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1936,
            "range": "± 369",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 830,
            "range": "± 169",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 926,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 384,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 375,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 247,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 328,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 764,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 389,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 500184,
            "range": "± 70924",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28302312,
            "range": "± 3070702",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2599,
            "range": "± 452",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11588,
            "range": "± 2144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3774,
            "range": "± 586",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14096,
            "range": "± 3135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13971,
            "range": "± 2887",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 32030,
            "range": "± 4854",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2588,
            "range": "± 404",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 344,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2029973,
            "range": "± 387258",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6e5c903241e5d0753f2978fbf4bafa80bfc8a000",
          "message": "Fix no_module build.",
          "timestamp": "2020-10-18T22:10:08+08:00",
          "tree_id": "d3d8f1b8da03fe9a3b58b37b0b9e270f5dfc326a",
          "url": "https://github.com/schungx/rhai/commit/6e5c903241e5d0753f2978fbf4bafa80bfc8a000"
        },
        "date": 1603030725045,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 122350,
            "range": "± 22318",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 84,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 76,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 256,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2291,
            "range": "± 153",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2097,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7345074,
            "range": "± 341623",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 658,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 691,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15925,
            "range": "± 1305",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14775,
            "range": "± 2596",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 349,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 634,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 62,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 62,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2331810,
            "range": "± 146920",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2865173,
            "range": "± 212880",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2462631,
            "range": "± 316625",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2409,
            "range": "± 163",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2317,
            "range": "± 285",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 463,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 499,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 980,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1998,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 843,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 951,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 370,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 364,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 245,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 323,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 741,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 388,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 518569,
            "range": "± 36898",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26529055,
            "range": "± 1847596",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2845,
            "range": "± 516",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12375,
            "range": "± 2821",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3854,
            "range": "± 3415",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15457,
            "range": "± 2615",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15390,
            "range": "± 3048",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33648,
            "range": "± 3350",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2635,
            "range": "± 248",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 349,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2014239,
            "range": "± 265464",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "ea814779bf270258ee2dbe231bab0b215ea3a859",
          "message": "Fix test.",
          "timestamp": "2020-10-18T22:24:01+08:00",
          "tree_id": "71c2704e821abcccccbfe4d7e742a07a7ab10535",
          "url": "https://github.com/schungx/rhai/commit/ea814779bf270258ee2dbe231bab0b215ea3a859"
        },
        "date": 1603031373845,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117193,
            "range": "± 12931",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 90,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 74,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 252,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2260,
            "range": "± 251",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2079,
            "range": "± 322",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7461435,
            "range": "± 639950",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 666,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 705,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14852,
            "range": "± 3716",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13559,
            "range": "± 3824",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 348,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 665,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2336801,
            "range": "± 270855",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2963463,
            "range": "± 641535",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2632369,
            "range": "± 245319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2365,
            "range": "± 349",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2432,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 460,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 506,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1002,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1906,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 827,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 949,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 372,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 365,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 238,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 318,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 738,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 388,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 533063,
            "range": "± 253809",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26324484,
            "range": "± 1119024",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2732,
            "range": "± 187",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12186,
            "range": "± 621",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3823,
            "range": "± 291",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14332,
            "range": "± 1010",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14873,
            "range": "± 1207",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35378,
            "range": "± 2591",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2661,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 351,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2054746,
            "range": "± 145502",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "d68c9517953ac1de6472ccb03b547aacacedf53f",
          "message": "Pad string to exact length even when padding string is not multiple.",
          "timestamp": "2020-10-18T22:47:58+08:00",
          "tree_id": "e1333a24466b00b8b1ff2433eed52feb59ecbd00",
          "url": "https://github.com/schungx/rhai/commit/d68c9517953ac1de6472ccb03b547aacacedf53f"
        },
        "date": 1603033045930,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 122109,
            "range": "± 23740",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 85,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 75,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 268,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2294,
            "range": "± 424",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2163,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7380169,
            "range": "± 761322",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 658,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 710,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14378,
            "range": "± 1852",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12577,
            "range": "± 2055",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 359,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 652,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 59,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 59,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 59,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2275089,
            "range": "± 456597",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2834943,
            "range": "± 482012",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2501841,
            "range": "± 332291",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2177,
            "range": "± 445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2274,
            "range": "± 422",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 424,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 489,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 938,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1936,
            "range": "± 397",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 776,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 891,
            "range": "± 175",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 354,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 348,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 220,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 309,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 728,
            "range": "± 171",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 370,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 510958,
            "range": "± 89850",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28173130,
            "range": "± 4593772",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2635,
            "range": "± 341",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11654,
            "range": "± 2972",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3727,
            "range": "± 921",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13770,
            "range": "± 2362",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14044,
            "range": "± 1860",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 32551,
            "range": "± 6134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2848,
            "range": "± 414",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 358,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2080313,
            "range": "± 269402",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "a9fd0ff4de7b6827afd58ba595dc959e4d9ef3f9",
          "message": "Encapsulate scope into EvalContext;\nFix bug with custom syntax delta.",
          "timestamp": "2020-10-19T19:11:55+08:00",
          "tree_id": "629f1e46d64da813547fcf62813255cc4aa626e0",
          "url": "https://github.com/schungx/rhai/commit/a9fd0ff4de7b6827afd58ba595dc959e4d9ef3f9"
        },
        "date": 1603106384964,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 102957,
            "range": "± 17472",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 76,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 63,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 210,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1958,
            "range": "± 994",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1770,
            "range": "± 266",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6082129,
            "range": "± 808521",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 530,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 553,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12584,
            "range": "± 1807",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11100,
            "range": "± 1859",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 280,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 485,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 45,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 45,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 46,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1830749,
            "range": "± 440827",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2535267,
            "range": "± 534450",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2399205,
            "range": "± 467967",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2003,
            "range": "± 374",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2007,
            "range": "± 429",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 366,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 417,
            "range": "± 165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 816,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1689,
            "range": "± 379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 743,
            "range": "± 253",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 847,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 313,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 325,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 202,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 277,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 616,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 321,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 430753,
            "range": "± 85109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 24667642,
            "range": "± 4338124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2489,
            "range": "± 629",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 10970,
            "range": "± 1734",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3322,
            "range": "± 456",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 11790,
            "range": "± 1848",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 11673,
            "range": "± 1874",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 26388,
            "range": "± 5189",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2162,
            "range": "± 276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 332,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1766299,
            "range": "± 503062",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "ccba5f218875df9809fcfa3d8bdf52e33e5d5bc2",
          "message": "Move custom syntax into separate function.",
          "timestamp": "2020-10-19T19:21:40+08:00",
          "tree_id": "3d690e445f146b3f19eff08d3dcd08985e671f0a",
          "url": "https://github.com/schungx/rhai/commit/ccba5f218875df9809fcfa3d8bdf52e33e5d5bc2"
        },
        "date": 1603113356493,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 106882,
            "range": "± 26404",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 72,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 70,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 228,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1835,
            "range": "± 390",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1813,
            "range": "± 582",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6032490,
            "range": "± 973976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 534,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 589,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 11677,
            "range": "± 1927",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 10008,
            "range": "± 1952",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 313,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 559,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 53,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 55,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 52,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1854261,
            "range": "± 387606",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2410729,
            "range": "± 431560",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2180352,
            "range": "± 359024",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1954,
            "range": "± 511",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1862,
            "range": "± 319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 362,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 404,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 774,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1630,
            "range": "± 543",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 705,
            "range": "± 292",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 828,
            "range": "± 159",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 332,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 313,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 201,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 261,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 579,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 318,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 430165,
            "range": "± 91796",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25982111,
            "range": "± 4560582",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2245,
            "range": "± 717",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 9444,
            "range": "± 1972",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 2891,
            "range": "± 525",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 11235,
            "range": "± 1761",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 11553,
            "range": "± 2187",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 26952,
            "range": "± 4290",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2385,
            "range": "± 534",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 289,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1773399,
            "range": "± 262166",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "92ba7b42d5b3b34f10e17b9c9c00fe5e5f46fdd0",
          "message": "Implement namespaces chain.",
          "timestamp": "2020-10-20T10:54:32+08:00",
          "tree_id": "41c4e2971d8c7484c93b7eeae85a44d2b33822df",
          "url": "https://github.com/schungx/rhai/commit/92ba7b42d5b3b34f10e17b9c9c00fe5e5f46fdd0"
        },
        "date": 1603163369632,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 89194,
            "range": "± 22320",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 67,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 72,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 202,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1841,
            "range": "± 693",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1683,
            "range": "± 485",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6058869,
            "range": "± 918713",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 557,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 599,
            "range": "± 215",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 11824,
            "range": "± 3994",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12037,
            "range": "± 2000",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 347,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 587,
            "range": "± 234",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 71,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 75,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 72,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2258106,
            "range": "± 878190",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2444762,
            "range": "± 611398",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2283983,
            "range": "± 788864",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2173,
            "range": "± 422",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2090,
            "range": "± 566",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 443,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 473,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 864,
            "range": "± 166",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1683,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 863,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 981,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 382,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 246,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 329,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 773,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 400,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 548269,
            "range": "± 26951",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27373101,
            "range": "± 1357844",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2778,
            "range": "± 168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12354,
            "range": "± 585",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3996,
            "range": "± 733",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15145,
            "range": "± 503",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15190,
            "range": "± 660",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35990,
            "range": "± 1297",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2745,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 384,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2182033,
            "range": "± 36320",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "09f8b13f2d409256f9952547447dadd0a14080cf",
          "message": "Fix test output.",
          "timestamp": "2020-10-20T11:10:46+08:00",
          "tree_id": "b3b923fd536744b8212ac130ea5ede46052d3a8a",
          "url": "https://github.com/schungx/rhai/commit/09f8b13f2d409256f9952547447dadd0a14080cf"
        },
        "date": 1603163918331,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 98319,
            "range": "± 33786",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 68,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 67,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 218,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1944,
            "range": "± 487",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1764,
            "range": "± 307",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6711694,
            "range": "± 1423905",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 580,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 665,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12850,
            "range": "± 4066",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11329,
            "range": "± 2596",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 273,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 542,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 62,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 68,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 63,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1940436,
            "range": "± 386093",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2375186,
            "range": "± 360741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2146514,
            "range": "± 377124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1976,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1814,
            "range": "± 661",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 357,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 447,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 822,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1353,
            "range": "± 345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 679,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 787,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 308,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 306,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 211,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 248,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 628,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 341,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 461850,
            "range": "± 80662",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 21983094,
            "range": "± 2461580",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2229,
            "range": "± 356",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 10028,
            "range": "± 1759",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3163,
            "range": "± 1157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 11866,
            "range": "± 1862",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 11880,
            "range": "± 2442",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 29585,
            "range": "± 4589",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2174,
            "range": "± 469",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 281,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1709659,
            "range": "± 230437",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "5ee9dfc5cda7094ae2f0dee31970e18d931670a3",
          "message": "1) Change namespaces to iter_namespaces\n2) throw can throw any value",
          "timestamp": "2020-10-20T18:09:26+08:00",
          "tree_id": "f17f43fe4e524f0c8afa19556953b24eb3cae709",
          "url": "https://github.com/schungx/rhai/commit/5ee9dfc5cda7094ae2f0dee31970e18d931670a3"
        },
        "date": 1603189444549,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116787,
            "range": "± 10007",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 77,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 274,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2399,
            "range": "± 284",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2253,
            "range": "± 210",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7680799,
            "range": "± 607430",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 682,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 747,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14890,
            "range": "± 2452",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13086,
            "range": "± 1265",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 370,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 657,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 74,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 75,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 75,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2345090,
            "range": "± 146233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3035645,
            "range": "± 227714",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2667563,
            "range": "± 177215",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2392,
            "range": "± 294",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2462,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 478,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 535,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1017,
            "range": "± 103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1664,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 859,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 977,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 394,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 392,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 267,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 350,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 787,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 421,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 557948,
            "range": "± 69762",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29950801,
            "range": "± 2424569",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2729,
            "range": "± 464",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12074,
            "range": "± 1003",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3739,
            "range": "± 440",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14407,
            "range": "± 3035",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14567,
            "range": "± 1150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34118,
            "range": "± 3690",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2649,
            "range": "± 298",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 365,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2158505,
            "range": "± 230321",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "07bdb824fe8cdb1987582e2c857efee27a24ba40",
          "message": "Add try-catch.",
          "timestamp": "2020-10-20T23:16:03+08:00",
          "tree_id": "4187af29555d24d2362af4f750caf7536e53ea68",
          "url": "https://github.com/schungx/rhai/commit/07bdb824fe8cdb1987582e2c857efee27a24ba40"
        },
        "date": 1603207392243,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 89471,
            "range": "± 22721",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 64,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 58,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 209,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1894,
            "range": "± 391",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1628,
            "range": "± 522",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 5961908,
            "range": "± 777983",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 542,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 572,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 11641,
            "range": "± 1981",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 10258,
            "range": "± 3280",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 279,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 495,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 59,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 64,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1823749,
            "range": "± 389179",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2335001,
            "range": "± 416315",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2152892,
            "range": "± 289532",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1870,
            "range": "± 597",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1979,
            "range": "± 348",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 365,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 429,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 753,
            "range": "± 184",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1327,
            "range": "± 239",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 697,
            "range": "± 220",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 747,
            "range": "± 134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 315,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 299,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 194,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 240,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 594,
            "range": "± 162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 313,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 446799,
            "range": "± 69188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 22103321,
            "range": "± 3087267",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2156,
            "range": "± 704",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 9588,
            "range": "± 1450",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 2984,
            "range": "± 547",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 11794,
            "range": "± 3922",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 11901,
            "range": "± 2735",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 29860,
            "range": "± 7012",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2167,
            "range": "± 537",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 300,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1697624,
            "range": "± 343739",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "ce03a4fda5d600a8a649085d1d31cc8d88832343",
          "message": "Fix throw test.",
          "timestamp": "2020-10-20T23:30:01+08:00",
          "tree_id": "907aeb5826b7451d90d2101773e5aa586d776eff",
          "url": "https://github.com/schungx/rhai/commit/ce03a4fda5d600a8a649085d1d31cc8d88832343"
        },
        "date": 1603208414286,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117326,
            "range": "± 951",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 83,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 76,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 261,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2338,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2179,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7636502,
            "range": "± 13129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 681,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 739,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15506,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13355,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 367,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 653,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 78,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 78,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 78,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2425746,
            "range": "± 11528",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3001943,
            "range": "± 6821",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2753374,
            "range": "± 3870",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2482,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2497,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 480,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 537,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1014,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1722,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 886,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 976,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 381,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 332,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 764,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 405,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 544857,
            "range": "± 1942",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27156294,
            "range": "± 76447",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2814,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12101,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3921,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14810,
            "range": "± 172",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15123,
            "range": "± 145",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35573,
            "range": "± 243",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2716,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 364,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2170800,
            "range": "± 10305",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "ad60db5bae786d1217ddce364c85065f856418b3",
          "message": "Reduce max fn call level.",
          "timestamp": "2020-10-21T10:10:46+08:00",
          "tree_id": "d4008920f83588af70f3104e059e42b272eb55fb",
          "url": "https://github.com/schungx/rhai/commit/ad60db5bae786d1217ddce364c85065f856418b3"
        },
        "date": 1603246571303,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 105046,
            "range": "± 10987",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 73,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 69,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 236,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2090,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1901,
            "range": "± 263",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6839480,
            "range": "± 1103571",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 582,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 640,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12740,
            "range": "± 2280",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11570,
            "range": "± 1413",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 315,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 581,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 67,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 70,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 69,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2148952,
            "range": "± 158542",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2633909,
            "range": "± 322379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2382412,
            "range": "± 297196",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2209,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2211,
            "range": "± 336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 440,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 483,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 924,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1534,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 777,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 884,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 354,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 356,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 225,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 300,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 663,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 360,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 503702,
            "range": "± 57635",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 24259358,
            "range": "± 3845376",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2464,
            "range": "± 348",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 10973,
            "range": "± 1289",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3453,
            "range": "± 386",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13444,
            "range": "± 1306",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13518,
            "range": "± 1445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 31076,
            "range": "± 4822",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2480,
            "range": "± 176",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 321,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1967667,
            "range": "± 154252",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "2c7c719cd557b96fa2dc055879084889769dbae6",
          "message": "Minor cleanup before release.",
          "timestamp": "2020-10-22T12:26:44+08:00",
          "tree_id": "1a6503c74b849e15f859aa559fe9b26d2ecac809",
          "url": "https://github.com/schungx/rhai/commit/2c7c719cd557b96fa2dc055879084889769dbae6"
        },
        "date": 1603341259619,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 108341,
            "range": "± 18355",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 63,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 60,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 208,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1839,
            "range": "± 402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1661,
            "range": "± 592",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6009027,
            "range": "± 1280745",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 529,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 564,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12089,
            "range": "± 1260",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 10221,
            "range": "± 1026",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 284,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 497,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 60,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 60,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 59,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1828734,
            "range": "± 179122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2308293,
            "range": "± 223958",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2102667,
            "range": "± 314995",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1699,
            "range": "± 599",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1736,
            "range": "± 512",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 375,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 428,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 844,
            "range": "± 313",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1360,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 683,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 769,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 298,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 298,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 189,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 278,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 580,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 299,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 415415,
            "range": "± 52205",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 20827651,
            "range": "± 1777623",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2108,
            "range": "± 209",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 9757,
            "range": "± 1996",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3171,
            "range": "± 673",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 12715,
            "range": "± 2699",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13030,
            "range": "± 2783",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 31441,
            "range": "± 4142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2222,
            "range": "± 415",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 285,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1736107,
            "range": "± 326445",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b607a3a9ba6ab8235d25d43987c2a9f6a98683ce",
          "message": "Add raw API for custom syntax.",
          "timestamp": "2020-10-25T21:57:18+08:00",
          "tree_id": "ef74a7c36ddf7b8b8fcb533f450f89d1e5a27e18",
          "url": "https://github.com/schungx/rhai/commit/b607a3a9ba6ab8235d25d43987c2a9f6a98683ce"
        },
        "date": 1603634755494,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114916,
            "range": "± 7529",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 111,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 296,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2441,
            "range": "± 179",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2209,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7401413,
            "range": "± 672058",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 594,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 660,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14524,
            "range": "± 2230",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12903,
            "range": "± 2088",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 335,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 651,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 69,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 68,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 69,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2084188,
            "range": "± 430498",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2906892,
            "range": "± 324722",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2622967,
            "range": "± 195450",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2365,
            "range": "± 658",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2533,
            "range": "± 537",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 490,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 535,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 969,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1505,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 826,
            "range": "± 147",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 883,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 361,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 333,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 231,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 324,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 750,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 394,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 515163,
            "range": "± 91420",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 24756853,
            "range": "± 3905833",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2955,
            "range": "± 325",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12449,
            "range": "± 1510",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4190,
            "range": "± 876",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14391,
            "range": "± 2568",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14074,
            "range": "± 2578",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 33675,
            "range": "± 5339",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2559,
            "range": "± 459",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 350,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2194786,
            "range": "± 77027",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b467b187225ec222abfe03e55c15d8b69a6ffe51",
          "message": "Fix no_std build.",
          "timestamp": "2020-10-25T22:08:02+08:00",
          "tree_id": "8d9c1afca3fed7fc50fb2556baeb54fcb44980d9",
          "url": "https://github.com/schungx/rhai/commit/b467b187225ec222abfe03e55c15d8b69a6ffe51"
        },
        "date": 1603635271661,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113886,
            "range": "± 10703",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 120,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 294,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2345,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2152,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7596487,
            "range": "± 236108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 677,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 724,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16374,
            "range": "± 568",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14462,
            "range": "± 479",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 371,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 662,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 74,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2351161,
            "range": "± 107377",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2915545,
            "range": "± 152502",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2681865,
            "range": "± 102041",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2461,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2515,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 485,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 534,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1013,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1701,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 855,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 954,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 371,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 338,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 771,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 408,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 542008,
            "range": "± 16533",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26240925,
            "range": "± 952236",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3143,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12835,
            "range": "± 668",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4485,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15673,
            "range": "± 930",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16029,
            "range": "± 607",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39130,
            "range": "± 2735",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2822,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 348,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2102265,
            "range": "± 97457",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "7496c77ac917f52b4a7cd2c84c446304c75ddd7c",
          "message": "Custom syntax parser function takes &[String].",
          "timestamp": "2020-10-26T19:46:58+08:00",
          "tree_id": "6719eceb3b7cb75ace26ec65e8e78fa7452ecedf",
          "url": "https://github.com/schungx/rhai/commit/7496c77ac917f52b4a7cd2c84c446304c75ddd7c"
        },
        "date": 1603713005812,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 121636,
            "range": "± 2363",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 116,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 306,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2451,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2272,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 8012429,
            "range": "± 8117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 714,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 771,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16961,
            "range": "± 178",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 15215,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 386,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 689,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 80,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 80,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 80,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2491769,
            "range": "± 4635",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3098376,
            "range": "± 4915",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2828088,
            "range": "± 20393",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2572,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2643,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 506,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 566,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1061,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1780,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 909,
            "range": "± 203",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1030,
            "range": "± 443",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 397,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 398,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 257,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 788,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 424,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 568915,
            "range": "± 2290",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28365617,
            "range": "± 2182315",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3309,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13542,
            "range": "± 190",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4673,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16472,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16706,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 40757,
            "range": "± 316",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3031,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 369,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2265532,
            "range": "± 6811",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "54d68c106167b85f506c4d243c7088e94a963096",
          "message": "Fix bug.",
          "timestamp": "2020-10-26T21:49:49+08:00",
          "tree_id": "6fcfbe7a8dcbdd407fc05f617a1e7c48c04949f8",
          "url": "https://github.com/schungx/rhai/commit/54d68c106167b85f506c4d243c7088e94a963096"
        },
        "date": 1603720709567,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 102303,
            "range": "± 13741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 112,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 103,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 266,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2109,
            "range": "± 294",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1887,
            "range": "± 251",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6731888,
            "range": "± 810605",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 587,
            "range": "± 197",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 646,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15286,
            "range": "± 2743",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 12814,
            "range": "± 1701",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 347,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 595,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 70,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 69,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 70,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2195579,
            "range": "± 295042",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2694909,
            "range": "± 333467",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2421831,
            "range": "± 380928",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2229,
            "range": "± 328",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2305,
            "range": "± 354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 451,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 506,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 907,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1535,
            "range": "± 203",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 815,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 870,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 345,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 334,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 231,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 309,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 754,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 366,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 471249,
            "range": "± 68247",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 23998852,
            "range": "± 2900450",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2788,
            "range": "± 401",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11702,
            "range": "± 1706",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3949,
            "range": "± 520",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13925,
            "range": "± 2233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14016,
            "range": "± 1586",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34567,
            "range": "± 4333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2599,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 330,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1973593,
            "range": "± 273193",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "01663a6581f82c41b37a51371fe4edc513a49b46",
          "message": "Refine docs.",
          "timestamp": "2020-10-27T11:30:38+08:00",
          "tree_id": "0c54878783394ffe0276a7d549ba72b0f9742c0e",
          "url": "https://github.com/schungx/rhai/commit/01663a6581f82c41b37a51371fe4edc513a49b46"
        },
        "date": 1603769964615,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114397,
            "range": "± 14375",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 112,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 96,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 276,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2102,
            "range": "± 307",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1952,
            "range": "± 199",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6661803,
            "range": "± 572322",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 615,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 665,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15500,
            "range": "± 2237",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13682,
            "range": "± 1634",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 364,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 596,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 67,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 66,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 67,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2205146,
            "range": "± 453388",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2765519,
            "range": "± 325601",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2550466,
            "range": "± 437497",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2126,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2173,
            "range": "± 364",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 419,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 495,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 912,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1526,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 774,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 894,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 346,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 343,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 232,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 299,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 674,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 362,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 490667,
            "range": "± 64351",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26078910,
            "range": "± 2604281",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2776,
            "range": "± 414",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11281,
            "range": "± 1306",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3943,
            "range": "± 765",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14288,
            "range": "± 3100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14228,
            "range": "± 2691",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 35991,
            "range": "± 3784",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2635,
            "range": "± 248",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 316,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1920487,
            "range": "± 202907",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "93b5df6b3c05697a0b5cf330aed90984b2a3749c",
          "message": "Pack Stmt structure.",
          "timestamp": "2020-10-27T18:18:19+08:00",
          "tree_id": "be0d77ff4c024e8bdbd518d2f9e5389e60ace26e",
          "url": "https://github.com/schungx/rhai/commit/93b5df6b3c05697a0b5cf330aed90984b2a3749c"
        },
        "date": 1603794271805,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 140022,
            "range": "± 36026",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 132,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 122,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 344,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2496,
            "range": "± 323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2368,
            "range": "± 550",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 8427185,
            "range": "± 1397459",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 746,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 800,
            "range": "± 176",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 19144,
            "range": "± 5122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16836,
            "range": "± 4353",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 409,
            "range": "± 91",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 767,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 88,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 80,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 83,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2515475,
            "range": "± 374976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 3284480,
            "range": "± 743412",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2957691,
            "range": "± 487808",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2643,
            "range": "± 396",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2772,
            "range": "± 696",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 496,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 562,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1117,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1999,
            "range": "± 402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 970,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1187,
            "range": "± 283",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 455,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 447,
            "range": "± 119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 298,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 358,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 841,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 443,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 592033,
            "range": "± 111226",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32951548,
            "range": "± 3444139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3484,
            "range": "± 471",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14351,
            "range": "± 3641",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5143,
            "range": "± 942",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18091,
            "range": "± 6825",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18527,
            "range": "± 3332",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 47220,
            "range": "± 10326",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3258,
            "range": "± 705",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 432,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2430971,
            "range": "± 342021",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f62f7908ab3bd9d4e892b08eeb4793221441e62f",
          "message": "Pack Stmt tighter.",
          "timestamp": "2020-10-27T19:23:43+08:00",
          "tree_id": "c111fb532464fb90198d3e6d4fea7a497f8affda",
          "url": "https://github.com/schungx/rhai/commit/f62f7908ab3bd9d4e892b08eeb4793221441e62f"
        },
        "date": 1603798320649,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 99670,
            "range": "± 19884",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 99,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 92,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 240,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1972,
            "range": "± 460",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1952,
            "range": "± 323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6282682,
            "range": "± 1266224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 549,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 573,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 13068,
            "range": "± 1965",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11536,
            "range": "± 2402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 293,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 516,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 64,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 63,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 65,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2039326,
            "range": "± 415506",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2381517,
            "range": "± 477256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2184977,
            "range": "± 481884",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1985,
            "range": "± 343",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1965,
            "range": "± 495",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 402,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 455,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 844,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1344,
            "range": "± 307",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 699,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 787,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 303,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 312,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 204,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 265,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 622,
            "range": "± 138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 329,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 449295,
            "range": "± 102848",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 21869991,
            "range": "± 3926383",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2529,
            "range": "± 563",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 10845,
            "range": "± 1845",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3598,
            "range": "± 710",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 12746,
            "range": "± 2645",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 12906,
            "range": "± 2489",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 31431,
            "range": "± 6739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2323,
            "range": "± 518",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 274,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1798268,
            "range": "± 412534",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "fd2ba54b49fa1768ce7bb731b70143c809ba93e7",
          "message": "Introduce BinaryExpr.",
          "timestamp": "2020-10-27T23:00:05+08:00",
          "tree_id": "e4e5a5ce3d917903510fc98055ea77c3cc58718d",
          "url": "https://github.com/schungx/rhai/commit/fd2ba54b49fa1768ce7bb731b70143c809ba93e7"
        },
        "date": 1603811573471,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114713,
            "range": "± 18295",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 116,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 103,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 319,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2292,
            "range": "± 367",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2374,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7291399,
            "range": "± 672788",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 658,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 703,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15711,
            "range": "± 2744",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14142,
            "range": "± 1300",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 374,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 658,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 71,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 72,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 71,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2188191,
            "range": "± 236881",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2970795,
            "range": "± 195637",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2602572,
            "range": "± 242719",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2456,
            "range": "± 280",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2502,
            "range": "± 209",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 474,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 547,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1045,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1712,
            "range": "± 505",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 849,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 971,
            "range": "± 127",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 384,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 383,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 257,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 331,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 739,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 400,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 555301,
            "range": "± 65369",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 31841831,
            "range": "± 6298165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3209,
            "range": "± 362",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13611,
            "range": "± 4308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4723,
            "range": "± 1354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16235,
            "range": "± 5623",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16614,
            "range": "± 1838",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38110,
            "range": "± 6585",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2841,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 365,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2195868,
            "range": "± 175988",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "51fca1e75722d3c81d5fc3fd44a5759007ccf2fc",
          "message": "Move Assignment to Stmt.",
          "timestamp": "2020-10-27T23:21:20+08:00",
          "tree_id": "ab1279355493d6bd4248852fa8cf12097efe98e0",
          "url": "https://github.com/schungx/rhai/commit/51fca1e75722d3c81d5fc3fd44a5759007ccf2fc"
        },
        "date": 1603812544363,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116044,
            "range": "± 10992",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 129,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 109,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 293,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2352,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2384,
            "range": "± 169",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7115896,
            "range": "± 590611",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 644,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 679,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15661,
            "range": "± 970",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14445,
            "range": "± 1394",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 354,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 636,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 74,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 76,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2075689,
            "range": "± 77847",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2682797,
            "range": "± 210462",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2381858,
            "range": "± 136698",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2370,
            "range": "± 294",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2411,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 467,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 491,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1016,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1638,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 837,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 971,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 392,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 386,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 248,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 331,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 755,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 393,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 510985,
            "range": "± 36914",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25907916,
            "range": "± 1912032",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3053,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12778,
            "range": "± 959",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4398,
            "range": "± 397",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15693,
            "range": "± 989",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15328,
            "range": "± 922",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38242,
            "range": "± 2622",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2906,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 343,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2003472,
            "range": "± 110817",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "4b087d0e695340960234f108d5c0a3f01f80166e",
          "message": "Fix test.",
          "timestamp": "2020-10-27T23:45:04+08:00",
          "tree_id": "e5cb834e243cbe0c7fa20782cc2b5cec9c4431f7",
          "url": "https://github.com/schungx/rhai/commit/4b087d0e695340960234f108d5c0a3f01f80166e"
        },
        "date": 1603813752424,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116926,
            "range": "± 6072",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 296,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2351,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2379,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7324766,
            "range": "± 65678",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 666,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 697,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16263,
            "range": "± 173",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14475,
            "range": "± 223",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 371,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 661,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 78,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2105867,
            "range": "± 24642",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2723814,
            "range": "± 133070",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2460426,
            "range": "± 36430",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2430,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2457,
            "range": "± 223",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 486,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 516,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 999,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1666,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 858,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 968,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 379,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 373,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 247,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 331,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 757,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 400,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 517117,
            "range": "± 22503",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27007308,
            "range": "± 509086",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3131,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13136,
            "range": "± 1415",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4475,
            "range": "± 162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15788,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15774,
            "range": "± 462",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39495,
            "range": "± 1217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2938,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 351,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2011500,
            "range": "± 90615",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "30e11f137b722976b92fdf7f509e7aa8f487c4e8",
          "message": "Move ErrorAssignmentToUnknownLHS to ParseError.",
          "timestamp": "2020-10-28T10:26:36+08:00",
          "tree_id": "1b134fa4380ac896df9d562d653e4b59ec071217",
          "url": "https://github.com/schungx/rhai/commit/30e11f137b722976b92fdf7f509e7aa8f487c4e8"
        },
        "date": 1603852339455,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113423,
            "range": "± 12858",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 117,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 113,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 300,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2347,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2422,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7296056,
            "range": "± 99314",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 670,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 700,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16064,
            "range": "± 208",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14406,
            "range": "± 189",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 369,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 654,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2113774,
            "range": "± 38393",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2685870,
            "range": "± 68523",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2435052,
            "range": "± 7998",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2463,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2478,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 485,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 517,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1014,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1694,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 873,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 992,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 391,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 392,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 252,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 336,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 772,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 405,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 509505,
            "range": "± 26713",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26892509,
            "range": "± 441901",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3127,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12747,
            "range": "± 1669",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4431,
            "range": "± 286",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15608,
            "range": "± 1039",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15802,
            "range": "± 353",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38646,
            "range": "± 2630",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2850,
            "range": "± 225",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 338,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2065701,
            "range": "± 38133",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "00b1051732e96a413fabae8ab4299c04ae8ec669",
          "message": "Optimize Stmt/Expr reflections.",
          "timestamp": "2020-10-28T14:10:48+08:00",
          "tree_id": "6da7933cee5caee8abe9944558646674c29b89bf",
          "url": "https://github.com/schungx/rhai/commit/00b1051732e96a413fabae8ab4299c04ae8ec669"
        },
        "date": 1603866077214,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118236,
            "range": "± 1628",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 302,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2352,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2428,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7292974,
            "range": "± 49777",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 677,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 707,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16190,
            "range": "± 463",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14283,
            "range": "± 258",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 366,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 663,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2114173,
            "range": "± 27256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2642249,
            "range": "± 46069",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2399116,
            "range": "± 32338",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2492,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2496,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 485,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 517,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1021,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1682,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 858,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 986,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 397,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 394,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 252,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 331,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 771,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 400,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 515985,
            "range": "± 14610",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26967269,
            "range": "± 235643",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3109,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13092,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4423,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15778,
            "range": "± 151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16060,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43982,
            "range": "± 6131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2876,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 339,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2039310,
            "range": "± 10180",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f56c3fe9cb85029aee78bbe347014356aaabda79",
          "message": "Add Ident/IdentX to AST.",
          "timestamp": "2020-10-28T19:11:17+08:00",
          "tree_id": "b46031a5db4e4e7d8613a7ea797870b082351e92",
          "url": "https://github.com/schungx/rhai/commit/f56c3fe9cb85029aee78bbe347014356aaabda79"
        },
        "date": 1603884070413,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117343,
            "range": "± 3729",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 129,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 294,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2347,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2214,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7290770,
            "range": "± 224789",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 667,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 704,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16265,
            "range": "± 1355",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14420,
            "range": "± 1173",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 364,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 652,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2205317,
            "range": "± 46965",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2761146,
            "range": "± 54669",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2515682,
            "range": "± 48963",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2453,
            "range": "± 165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2486,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 494,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 526,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1003,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1706,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 864,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 979,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 376,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 378,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 246,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 334,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 758,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 407,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 511924,
            "range": "± 28740",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27076225,
            "range": "± 375425",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3140,
            "range": "± 193",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13178,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4472,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16101,
            "range": "± 1071",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16393,
            "range": "± 1734",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 42338,
            "range": "± 526",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2898,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 349,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2041045,
            "range": "± 26348",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f5ffbfbe061bca2c5127a09948a541a96620793e",
          "message": "Code structure cleanup.",
          "timestamp": "2020-10-28T22:18:44+08:00",
          "tree_id": "99214fdfd3857b401e0fafcd7676e39d05c83c10",
          "url": "https://github.com/schungx/rhai/commit/f5ffbfbe061bca2c5127a09948a541a96620793e"
        },
        "date": 1603895278677,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 127077,
            "range": "± 18115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 137,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 120,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 342,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2471,
            "range": "± 409",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2291,
            "range": "± 513",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7908534,
            "range": "± 1093488",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 732,
            "range": "± 193",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 757,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16974,
            "range": "± 4423",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 15237,
            "range": "± 2853",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 407,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 720,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 78,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 76,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 76,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2179928,
            "range": "± 446256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2893797,
            "range": "± 564683",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2620724,
            "range": "± 397272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2528,
            "range": "± 1851",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2671,
            "range": "± 448",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 519,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 557,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1099,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1799,
            "range": "± 470",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 944,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1036,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 431,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 413,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 278,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 372,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 858,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 450,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 577555,
            "range": "± 115706",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 33145999,
            "range": "± 3892577",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3621,
            "range": "± 962",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16556,
            "range": "± 15006",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5388,
            "range": "± 632",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18812,
            "range": "± 5416",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18664,
            "range": "± 2526",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 49319,
            "range": "± 10274",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3689,
            "range": "± 636",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 387,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2212309,
            "range": "± 352027",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "cbd7ed2ca7c96f64290b1da213c9b7e630d6ba25",
          "message": "Fix test.",
          "timestamp": "2020-10-28T22:30:35+08:00",
          "tree_id": "930b8463eb7240c3f9334d630e6bc033a3dc312c",
          "url": "https://github.com/schungx/rhai/commit/cbd7ed2ca7c96f64290b1da213c9b7e630d6ba25"
        },
        "date": 1603895896958,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114199,
            "range": "± 2231",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 281,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2351,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2145,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7173005,
            "range": "± 52235",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 685,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 714,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16152,
            "range": "± 165",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14100,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 358,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 637,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2008503,
            "range": "± 3599",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2583248,
            "range": "± 5668",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2333271,
            "range": "± 5097",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2479,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2510,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 492,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 523,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 964,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1675,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 830,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 936,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 366,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 365,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 238,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 318,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 787,
            "range": "± 196",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 423,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 490812,
            "range": "± 4001",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26330365,
            "range": "± 104580",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3159,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13265,
            "range": "± 298",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4495,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15811,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16197,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39729,
            "range": "± 301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2947,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 337,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1976207,
            "range": "± 13501",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "4e115d2bc2241f4f5be41aa1cad0fc88d519216e",
          "message": "Code structure refactor.",
          "timestamp": "2020-10-29T11:37:51+08:00",
          "tree_id": "39e91ecb1deb6bb0eac31c45f61c96002532b282",
          "url": "https://github.com/schungx/rhai/commit/4e115d2bc2241f4f5be41aa1cad0fc88d519216e"
        },
        "date": 1603943117280,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114103,
            "range": "± 2849",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 109,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 273,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2290,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2129,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6932714,
            "range": "± 308971",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 655,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 707,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16030,
            "range": "± 580",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13907,
            "range": "± 419",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 349,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 624,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1983073,
            "range": "± 79335",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2547540,
            "range": "± 81183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2301629,
            "range": "± 76683",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2463,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2463,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 491,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 520,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 964,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1624,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 818,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 928,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 365,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 358,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 237,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 310,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 723,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 378,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 479463,
            "range": "± 21727",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25547596,
            "range": "± 742992",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3102,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13007,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4455,
            "range": "± 375",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15500,
            "range": "± 588",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15798,
            "range": "± 915",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38658,
            "range": "± 1412",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2872,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 335,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1972518,
            "range": "± 104864",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "a73584cd362397f4294e718d80a4fd918bc8704a",
          "message": "Fix no_std.",
          "timestamp": "2020-10-29T12:00:02+08:00",
          "tree_id": "fef5a884fff81f1cd8139f0bf7de98597e4b00fd",
          "url": "https://github.com/schungx/rhai/commit/a73584cd362397f4294e718d80a4fd918bc8704a"
        },
        "date": 1603944416909,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114628,
            "range": "± 16964",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 119,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 266,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2229,
            "range": "± 345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2135,
            "range": "± 293",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7214091,
            "range": "± 1212493",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 673,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 686,
            "range": "± 171",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15748,
            "range": "± 2896",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13791,
            "range": "± 2087",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 349,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 624,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 78,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 76,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 76,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1993902,
            "range": "± 218059",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2594051,
            "range": "± 172610",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2330361,
            "range": "± 300743",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2448,
            "range": "± 308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2496,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 484,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 507,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 940,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1647,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 833,
            "range": "± 211",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 936,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 371,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 358,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 237,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 319,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 716,
            "range": "± 162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 388,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 497286,
            "range": "± 71069",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26900295,
            "range": "± 2416822",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3182,
            "range": "± 274",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13281,
            "range": "± 4296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4487,
            "range": "± 1380",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15478,
            "range": "± 3350",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16173,
            "range": "± 2699",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 41929,
            "range": "± 3549",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2775,
            "range": "± 337",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 347,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1967512,
            "range": "± 289935",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "1e16cb03cea86840e3478e04d7ae20dd4e68628d",
          "message": "Add content to no-std buld.",
          "timestamp": "2020-10-31T11:52:42+08:00",
          "tree_id": "962dc88b184f9ae2a9d00b76010fa9908893b8b3",
          "url": "https://github.com/schungx/rhai/commit/1e16cb03cea86840e3478e04d7ae20dd4e68628d"
        },
        "date": 1604116886505,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 99036,
            "range": "± 13217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 102,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 100,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 274,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2105,
            "range": "± 298",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1974,
            "range": "± 337",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6318590,
            "range": "± 1071028",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 586,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 632,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14612,
            "range": "± 3795",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13269,
            "range": "± 2208",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 302,
            "range": "± 98",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 566,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 66,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 64,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 66,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1721266,
            "range": "± 357323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2272886,
            "range": "± 401410",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2076761,
            "range": "± 369403",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2163,
            "range": "± 353",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2208,
            "range": "± 336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 426,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 467,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 942,
            "range": "± 245",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1484,
            "range": "± 268",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 763,
            "range": "± 161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 846,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 354,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 352,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 213,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 298,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 673,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 352,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 432589,
            "range": "± 62364",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25777450,
            "range": "± 2940718",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2817,
            "range": "± 383",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12802,
            "range": "± 1794",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4125,
            "range": "± 821",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14663,
            "range": "± 2123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15303,
            "range": "± 2845",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44209,
            "range": "± 9295",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2841,
            "range": "± 460",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 317,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1824651,
            "range": "± 392538",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "89811c8dfcb4261cdad56780a5c21c01376616f0",
          "message": "Pack Stmt and Expr some more.",
          "timestamp": "2020-10-31T14:13:45+08:00",
          "tree_id": "5e0f9ec7b06fbb2f6ebf55c4b68c39e426b3051c",
          "url": "https://github.com/schungx/rhai/commit/89811c8dfcb4261cdad56780a5c21c01376616f0"
        },
        "date": 1604125398767,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 130268,
            "range": "± 25187",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 137,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 119,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 324,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2660,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2850,
            "range": "± 373",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 8064788,
            "range": "± 1023364",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 737,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 808,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17721,
            "range": "± 2484",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 15617,
            "range": "± 3103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 384,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 715,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 81,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 81,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 80,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2126511,
            "range": "± 178741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2794693,
            "range": "± 203239",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2595347,
            "range": "± 278409",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2750,
            "range": "± 461",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2809,
            "range": "± 390",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 539,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 579,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1099,
            "range": "± 228",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1860,
            "range": "± 258",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 1060,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1060,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 412,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 404,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 269,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 356,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 823,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 429,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 559343,
            "range": "± 97294",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32866356,
            "range": "± 3290764",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3341,
            "range": "± 556",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14393,
            "range": "± 3191",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4969,
            "range": "± 718",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17037,
            "range": "± 3138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17258,
            "range": "± 2638",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46848,
            "range": "± 7916",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3303,
            "range": "± 439",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 374,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2239112,
            "range": "± 416690",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "42eac410b7d49c5d42524d34d1def07060e1f435",
          "message": "Optimize Expr.",
          "timestamp": "2020-10-31T23:26:21+08:00",
          "tree_id": "a7ee4df7bfb49a780a03d0a54bb4d667cbf89166",
          "url": "https://github.com/schungx/rhai/commit/42eac410b7d49c5d42524d34d1def07060e1f435"
        },
        "date": 1604158384492,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 120611,
            "range": "± 1110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 116,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 354,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2012,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2046,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7386680,
            "range": "± 6006",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 713,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 730,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16975,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14683,
            "range": "± 162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 371,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 660,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 81,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 81,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 80,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2099769,
            "range": "± 3622",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2131475,
            "range": "± 9012",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1999412,
            "range": "± 3533",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2352,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2368,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 507,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 541,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1017,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1691,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 730,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 977,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 387,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 381,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 261,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 330,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 795,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 403,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 509552,
            "range": "± 6901",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27484851,
            "range": "± 102955",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3141,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13481,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4574,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16220,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16654,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44628,
            "range": "± 383",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2982,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 338,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2087680,
            "range": "± 11127",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "a2e2b5e2ef73b188c0d9a9273da9e356b9e65114",
          "message": "Add f32_float feature.",
          "timestamp": "2020-11-01T15:48:48+08:00",
          "tree_id": "555648bec5721a979c77fe9165d552f91aafb926",
          "url": "https://github.com/schungx/rhai/commit/a2e2b5e2ef73b188c0d9a9273da9e356b9e65114"
        },
        "date": 1604217288887,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117212,
            "range": "± 62707",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 111,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 282,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2044,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2093,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7114098,
            "range": "± 62962",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 689,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 719,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16584,
            "range": "± 741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14424,
            "range": "± 613",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 355,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 654,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 77,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 77,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 77,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2014094,
            "range": "± 14783",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2072109,
            "range": "± 32001",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1909760,
            "range": "± 21031",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2248,
            "range": "± 1039",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2290,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 482,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 514,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 955,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1570,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 706,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 944,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 363,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 362,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 238,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 318,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 734,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 389,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 487824,
            "range": "± 5844",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26282698,
            "range": "± 146817",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3047,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13180,
            "range": "± 337",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4429,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15910,
            "range": "± 438",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16065,
            "range": "± 520",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39893,
            "range": "± 748",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2943,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 329,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2005107,
            "range": "± 33132",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "3485f9b00b047c295127d37a2f7e6c8c9d81288c",
          "message": "Fix test.",
          "timestamp": "2020-11-01T16:02:10+08:00",
          "tree_id": "b6a71ec8889e2f115bc43f91503b7dcad808ae0e",
          "url": "https://github.com/schungx/rhai/commit/3485f9b00b047c295127d37a2f7e6c8c9d81288c"
        },
        "date": 1604218106177,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 124375,
            "range": "± 9175",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 114,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 109,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 298,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2056,
            "range": "± 235",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2188,
            "range": "± 270",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7270111,
            "range": "± 729970",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 674,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 717,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17935,
            "range": "± 1659",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14415,
            "range": "± 1790",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 368,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 653,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 75,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 75,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 75,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1981434,
            "range": "± 155158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1920277,
            "range": "± 194617",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1898216,
            "range": "± 137128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2273,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2275,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 523,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 509,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 982,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1605,
            "range": "± 213",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 707,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 998,
            "range": "± 91",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 373,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 259,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 753,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 407,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 494181,
            "range": "± 42818",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29007483,
            "range": "± 2609475",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3056,
            "range": "± 222",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14396,
            "range": "± 1322",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4412,
            "range": "± 431",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16189,
            "range": "± 1499",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16330,
            "range": "± 1374",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 40154,
            "range": "± 5830",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3012,
            "range": "± 391",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 333,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2029916,
            "range": "± 236656",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "c55fc5a9a5d0a4d529ac523f02ace10902ba078a",
          "message": "Optimize Scope.",
          "timestamp": "2020-11-01T22:46:46+08:00",
          "tree_id": "4d7a97f85fde6a728a3b095adc84fa48a19fec07",
          "url": "https://github.com/schungx/rhai/commit/c55fc5a9a5d0a4d529ac523f02ace10902ba078a"
        },
        "date": 1604242483186,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116304,
            "range": "± 20761",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 289,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2024,
            "range": "± 843",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1984,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7370768,
            "range": "± 1101320",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 718,
            "range": "± 427",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 753,
            "range": "± 196",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16142,
            "range": "± 538",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14328,
            "range": "± 883",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 359,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 639,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 81,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 81,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 81,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1994792,
            "range": "± 24836",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1963899,
            "range": "± 66155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1895329,
            "range": "± 48115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2352,
            "range": "± 476",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2413,
            "range": "± 944",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 531,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 559,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1077,
            "range": "± 254",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1659,
            "range": "± 647",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 716,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 961,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 380,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 372,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 238,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 322,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 741,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 391,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 494308,
            "range": "± 67916",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28514899,
            "range": "± 2531494",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3179,
            "range": "± 650",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14951,
            "range": "± 3871",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4579,
            "range": "± 785",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16759,
            "range": "± 3029",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16621,
            "range": "± 2041",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 41618,
            "range": "± 6048",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2914,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 326,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1978371,
            "range": "± 65283",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b07a2aa79c5e58da61aa74a0ec293ae359275e59",
          "message": "Pack Imports.",
          "timestamp": "2020-11-01T23:42:00+08:00",
          "tree_id": "0635bd7826ed508d39b0345476bd93980a514261",
          "url": "https://github.com/schungx/rhai/commit/b07a2aa79c5e58da61aa74a0ec293ae359275e59"
        },
        "date": 1604245769481,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 115457,
            "range": "± 776",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 113,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 282,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1967,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1986,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7103798,
            "range": "± 32702",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 717,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 745,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16436,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14232,
            "range": "± 380",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 368,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 649,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 82,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 82,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 82,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2120609,
            "range": "± 5712",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2089368,
            "range": "± 14976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2020416,
            "range": "± 4630",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2294,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2380,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 527,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 562,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1130,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1713,
            "range": "± 708",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 718,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 951,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 373,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 369,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 245,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 328,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 748,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 398,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 503460,
            "range": "± 50948",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27727963,
            "range": "± 1992280",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3119,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14085,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4525,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16028,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16111,
            "range": "± 445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39959,
            "range": "± 789",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2949,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 334,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2007861,
            "range": "± 37156",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "d7d6f74dfd0f6a81f80af0acb84aaca52884d024",
          "message": "Add constant NO_POS.",
          "timestamp": "2020-11-02T12:50:27+08:00",
          "tree_id": "c6d5f18fe347cf6444931600b8bbe7f7506c307c",
          "url": "https://github.com/schungx/rhai/commit/d7d6f74dfd0f6a81f80af0acb84aaca52884d024"
        },
        "date": 1604292834457,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 89656,
            "range": "± 25890",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 87,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 81,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 228,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1621,
            "range": "± 323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1519,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 5633042,
            "range": "± 772678",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 546,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 593,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 12902,
            "range": "± 2450",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 10902,
            "range": "± 1276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 284,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 487,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 63,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 64,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 61,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1501153,
            "range": "± 345249",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1528413,
            "range": "± 312232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1468523,
            "range": "± 189499",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1694,
            "range": "± 237",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1870,
            "range": "± 674",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 397,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 418,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 862,
            "range": "± 309",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1339,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 545,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 791,
            "range": "± 244",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 297,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 305,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 191,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 245,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 554,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 298,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 384870,
            "range": "± 56597",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 23985427,
            "range": "± 1424679",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2262,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 9774,
            "range": "± 3016",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3394,
            "range": "± 324",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 11891,
            "range": "± 1215",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 12038,
            "range": "± 1279",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34614,
            "range": "± 8373",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2164,
            "range": "± 269",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 244,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1596952,
            "range": "± 192926",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "cc304ba5134c87d9cd3fa46cd156a418e6c6a79a",
          "message": "Fix serde build.",
          "timestamp": "2020-11-02T13:18:37+08:00",
          "tree_id": "5c13c8146ad48e83bc0dfdd736c28dcd92ce08b5",
          "url": "https://github.com/schungx/rhai/commit/cc304ba5134c87d9cd3fa46cd156a418e6c6a79a"
        },
        "date": 1604294489974,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116260,
            "range": "± 5654",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 120,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 115,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1995,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2004,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7241161,
            "range": "± 69216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 730,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 762,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16788,
            "range": "± 443",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14329,
            "range": "± 236",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 368,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 652,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 85,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 85,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 85,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2042658,
            "range": "± 44338",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2038368,
            "range": "± 31587",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1978878,
            "range": "± 40419",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2360,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2441,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 536,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 583,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1116,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1712,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 723,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 957,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 378,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 378,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 249,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 328,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 751,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 397,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 508662,
            "range": "± 16797",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28007749,
            "range": "± 279447",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3029,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13772,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4483,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15855,
            "range": "± 616",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15870,
            "range": "± 1806",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39295,
            "range": "± 368",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2887,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 326,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2018747,
            "range": "± 41388",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "f74d947c6b850e377a0ee55b6f79f3c171beaa6d",
          "message": "Fix constant assignment.",
          "timestamp": "2020-11-03T13:08:19+08:00",
          "tree_id": "b4369cae1867ffb24c3bd9d8baa1f3e669603a08",
          "url": "https://github.com/schungx/rhai/commit/f74d947c6b850e377a0ee55b6f79f3c171beaa6d"
        },
        "date": 1604380492249,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118686,
            "range": "± 8518",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 118,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 109,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 316,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1921,
            "range": "± 413",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2002,
            "range": "± 579",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7518725,
            "range": "± 919218",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 766,
            "range": "± 537",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 797,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15923,
            "range": "± 4032",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13842,
            "range": "± 2010",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 372,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 660,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 79,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 79,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 79,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1972973,
            "range": "± 298271",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1984552,
            "range": "± 191161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1923801,
            "range": "± 164618",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2287,
            "range": "± 569",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2296,
            "range": "± 354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 530,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 582,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1150,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1728,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 723,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 986,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 388,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 402,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 257,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 329,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 795,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 406,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 526205,
            "range": "± 171103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32152149,
            "range": "± 2931546",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3243,
            "range": "± 377",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13208,
            "range": "± 828",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4486,
            "range": "± 776",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15863,
            "range": "± 1030",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16091,
            "range": "± 4515",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43203,
            "range": "± 7803",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2983,
            "range": "± 1465",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 326,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2071079,
            "range": "± 191618",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "2168fd536111263d9dd6000e85a6ad1ca98b25b5",
          "message": "Expr::Stmt takes a statements block.",
          "timestamp": "2020-11-04T11:49:02+08:00",
          "tree_id": "eda35197d0d3f365976a052e0499525b8ab1ddca",
          "url": "https://github.com/schungx/rhai/commit/2168fd536111263d9dd6000e85a6ad1ca98b25b5"
        },
        "date": 1604462603190,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117710,
            "range": "± 33353",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 113,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1901,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2097,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7400228,
            "range": "± 401095",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 808,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 845,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17221,
            "range": "± 1651",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14385,
            "range": "± 2824",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 381,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 664,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 90,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 90,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2162841,
            "range": "± 326883",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2132431,
            "range": "± 286968",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2065870,
            "range": "± 135158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2368,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2393,
            "range": "± 500",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 555,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 587,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1120,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1761,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 750,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 985,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 388,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 384,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 258,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 765,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 411,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 519802,
            "range": "± 52629",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29115597,
            "range": "± 1730307",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3057,
            "range": "± 151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13117,
            "range": "± 773",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4437,
            "range": "± 505",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16007,
            "range": "± 6539",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15847,
            "range": "± 732",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44117,
            "range": "± 3472",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2919,
            "range": "± 155",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 324,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2075429,
            "range": "± 100798",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "7e60e9ff5475605824bb21259ba2d6dc8cc3932b",
          "message": "Docs revision.",
          "timestamp": "2020-11-04T17:10:26+08:00",
          "tree_id": "4641cb3e76ec6facea94e94679e9fbd8a4947bc3",
          "url": "https://github.com/schungx/rhai/commit/7e60e9ff5475605824bb21259ba2d6dc8cc3932b"
        },
        "date": 1604481353931,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 107136,
            "range": "± 12909",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 115,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 105,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 261,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1706,
            "range": "± 279",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1853,
            "range": "± 275",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6368135,
            "range": "± 762368",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 688,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 748,
            "range": "± 119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14442,
            "range": "± 3042",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14269,
            "range": "± 3934",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 373,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 674,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 87,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 81,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1907363,
            "range": "± 577929",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1968072,
            "range": "± 555976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1940397,
            "range": "± 476588",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2198,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2323,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 505,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 527,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1075,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1670,
            "range": "± 251",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 745,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 918,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 356,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 359,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 219,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 293,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 663,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 368,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 447572,
            "range": "± 52407",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25262420,
            "range": "± 3849448",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2921,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12645,
            "range": "± 1286",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4275,
            "range": "± 336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13789,
            "range": "± 2227",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14790,
            "range": "± 1917",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 34889,
            "range": "± 5136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2671,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 312,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1947348,
            "range": "± 534931",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "dd53937ddd1bcf703edbba8f394a5acb4e98015e",
          "message": "f32_float for no-std example.",
          "timestamp": "2020-11-04T17:12:21+08:00",
          "tree_id": "4452e32086f20b4afc988f956e59a709d2dcaf80",
          "url": "https://github.com/schungx/rhai/commit/dd53937ddd1bcf703edbba8f394a5acb4e98015e"
        },
        "date": 1604481597839,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113857,
            "range": "± 16247",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 119,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 111,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 296,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1975,
            "range": "± 190",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1966,
            "range": "± 505",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7338196,
            "range": "± 929005",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 824,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 831,
            "range": "± 172",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15786,
            "range": "± 3287",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13828,
            "range": "± 2659",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 380,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 660,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 81,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 82,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 80,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1996443,
            "range": "± 270449",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1931045,
            "range": "± 240846",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1940292,
            "range": "± 215118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2360,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2421,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 533,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 550,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1171,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1770,
            "range": "± 268",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 726,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1032,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 399,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 385,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 250,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 767,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 418,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 520947,
            "range": "± 60728",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 31849228,
            "range": "± 3876318",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3122,
            "range": "± 598",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12831,
            "range": "± 2375",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4408,
            "range": "± 464",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15274,
            "range": "± 1724",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15535,
            "range": "± 2367",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39985,
            "range": "± 5724",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2848,
            "range": "± 582",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 397,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2054652,
            "range": "± 307046",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "99669b5909bf3fec5b8ff674a5e25915ce44c657",
          "message": "Bump version.",
          "timestamp": "2020-11-04T17:17:21+08:00",
          "tree_id": "eaa1ba77f8e441c525e5e71bb5174a6a0ee66b08",
          "url": "https://github.com/schungx/rhai/commit/99669b5909bf3fec5b8ff674a5e25915ce44c657"
        },
        "date": 1604481834929,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 111497,
            "range": "± 14166",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 116,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 105,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 273,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1833,
            "range": "± 261",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1900,
            "range": "± 311",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6889522,
            "range": "± 958752",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 782,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 797,
            "range": "± 124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15521,
            "range": "± 2242",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14218,
            "range": "± 1400",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 368,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 655,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 85,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 83,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 86,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2064975,
            "range": "± 331580",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1893181,
            "range": "± 251335",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1904965,
            "range": "± 274835",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2247,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2352,
            "range": "± 424",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 529,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 566,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1068,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1659,
            "range": "± 172",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 748,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 924,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 366,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 361,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 241,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 317,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 741,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 389,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 491845,
            "range": "± 75898",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27795565,
            "range": "± 3287515",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2937,
            "range": "± 336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13131,
            "range": "± 2651",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4230,
            "range": "± 674",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15331,
            "range": "± 2404",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15292,
            "range": "± 2302",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38195,
            "range": "± 5282",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2755,
            "range": "± 526",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 316,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1954815,
            "range": "± 262104",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6bc5ba666831886083008f13e05cb310b19eb444",
          "message": "Enable functions to use global imports.",
          "timestamp": "2020-11-06T19:17:07+08:00",
          "tree_id": "0ed20b19b19d83b7ad4fa6dabff82e161ecf4a83",
          "url": "https://github.com/schungx/rhai/commit/6bc5ba666831886083008f13e05cb310b19eb444"
        },
        "date": 1604661813339,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 109139,
            "range": "± 2963",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 121,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 290,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2089,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2130,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7256115,
            "range": "± 16535",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 857,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 796,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16205,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14225,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 375,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 656,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 88,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 88,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 88,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2056369,
            "range": "± 12665",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1987038,
            "range": "± 9177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1946886,
            "range": "± 15907",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2383,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2415,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 547,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 581,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1089,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1750,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 754,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 980,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 386,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 382,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 255,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 767,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 409,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 509238,
            "range": "± 4293",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27927737,
            "range": "± 149812",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3073,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13168,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4393,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15703,
            "range": "± 232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15857,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39547,
            "range": "± 321",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2896,
            "range": "± 103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 327,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2054215,
            "range": "± 57431",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "d5d70367fa0afa7ec1c45871bbd3617b704b91f4",
          "message": "Fix no-std buld.",
          "timestamp": "2020-11-06T19:22:00+08:00",
          "tree_id": "3e9ed23da44a7755e607cf102014cf5234df1ad8",
          "url": "https://github.com/schungx/rhai/commit/d5d70367fa0afa7ec1c45871bbd3617b704b91f4"
        },
        "date": 1604662190448,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 110865,
            "range": "± 2296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2038,
            "range": "± 167",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2091,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7207639,
            "range": "± 151172",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 744,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 778,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15867,
            "range": "± 403",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13924,
            "range": "± 683",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 377,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 655,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 88,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 88,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 88,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2076983,
            "range": "± 37963",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2028117,
            "range": "± 53241",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1968987,
            "range": "± 52764",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2360,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2401,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 542,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 587,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1099,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1727,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 755,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 980,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 385,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 393,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 253,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 765,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 409,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 506531,
            "range": "± 12095",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27781239,
            "range": "± 396147",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3073,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13116,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4414,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15647,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15665,
            "range": "± 663",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43993,
            "range": "± 866",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2961,
            "range": "± 148",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 327,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2041317,
            "range": "± 88732",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b3d318ef7f9ba344ec9061f1e58b8e4418485fc8",
          "message": "Module resolver returns shared module.",
          "timestamp": "2020-11-07T23:33:21+08:00",
          "tree_id": "a371448f08f9500e0bae963cd2ad134a6722936a",
          "url": "https://github.com/schungx/rhai/commit/b3d318ef7f9ba344ec9061f1e58b8e4418485fc8"
        },
        "date": 1604763582092,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 97767,
            "range": "± 26389",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 120,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 94,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 260,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1897,
            "range": "± 421",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1882,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6495406,
            "range": "± 790460",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 714,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 667,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 14237,
            "range": "± 2257",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 11977,
            "range": "± 2812",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 331,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 592,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 76,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 71,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 69,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1771224,
            "range": "± 179143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1740022,
            "range": "± 177975",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1703957,
            "range": "± 184921",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2001,
            "range": "± 538",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2029,
            "range": "± 418",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 512,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 499,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 964,
            "range": "± 201",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 995,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 647,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 862,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 348,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 346,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 222,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 288,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 666,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 350,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 484235,
            "range": "± 91143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26368952,
            "range": "± 2152217",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2673,
            "range": "± 660",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 11216,
            "range": "± 2194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 3733,
            "range": "± 756",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 13858,
            "range": "± 1648",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 13207,
            "range": "± 2771",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 32666,
            "range": "± 5374",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2440,
            "range": "± 458",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 292,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1835801,
            "range": "± 189097",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "1e07e4356e2296feaed50a3e8d62d480515dec42",
          "message": "Re-index imported modules if they are not yet indexed.",
          "timestamp": "2020-11-08T14:29:54+08:00",
          "tree_id": "e9c1986395e9fc9ca59f4677f2f795477691d112",
          "url": "https://github.com/schungx/rhai/commit/1e07e4356e2296feaed50a3e8d62d480515dec42"
        },
        "date": 1604817432275,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 109511,
            "range": "± 4494",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 113,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2094,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2138,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7107360,
            "range": "± 371238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 741,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 776,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16249,
            "range": "± 1870",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13835,
            "range": "± 2606",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 383,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 671,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 85,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 83,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 83,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1965035,
            "range": "± 129842",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1982117,
            "range": "± 155839",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1944569,
            "range": "± 72402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2397,
            "range": "± 445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2478,
            "range": "± 417",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 539,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 581,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1098,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1174,
            "range": "± 261",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 744,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 972,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 380,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 382,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 773,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 406,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 505176,
            "range": "± 26546",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27302134,
            "range": "± 1121274",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3103,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13227,
            "range": "± 419",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4517,
            "range": "± 223",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15565,
            "range": "± 531",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15637,
            "range": "± 2037",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 42185,
            "range": "± 1276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2912,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 328,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2027433,
            "range": "± 83546",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b926eba50172601d84a7d77a440ff32a162139ef",
          "message": "Fix doc test.",
          "timestamp": "2020-11-08T16:49:59+08:00",
          "tree_id": "45ad48960b2cc07324211309d4c6d86c3fd72623",
          "url": "https://github.com/schungx/rhai/commit/b926eba50172601d84a7d77a440ff32a162139ef"
        },
        "date": 1604825840541,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 114653,
            "range": "± 1351",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 126,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 116,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 302,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2176,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2237,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7592267,
            "range": "± 13762",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 907,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 808,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17222,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14951,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 408,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 705,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2118813,
            "range": "± 1887",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2070638,
            "range": "± 4504",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2053885,
            "range": "± 3448",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2450,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2553,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 564,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 594,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1150,
            "range": "± 105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1221,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 772,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1009,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 398,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 393,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 262,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 813,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 423,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 525043,
            "range": "± 2492",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29215555,
            "range": "± 242651",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3301,
            "range": "± 253",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13766,
            "range": "± 678",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4671,
            "range": "± 2139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16312,
            "range": "± 1319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16469,
            "range": "± 1528",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43777,
            "range": "± 230",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2968,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 345,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2134762,
            "range": "± 75505",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "232ba2754893c8a058007ed34784470710dd47f3",
          "message": "Add NativeCallContext::new.",
          "timestamp": "2020-11-08T18:15:23+08:00",
          "tree_id": "ef946984596b17439d2cb7ab24b0b682ed00cd8b",
          "url": "https://github.com/schungx/rhai/commit/232ba2754893c8a058007ed34784470710dd47f3"
        },
        "date": 1604830925265,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 109785,
            "range": "± 3563",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 120,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2098,
            "range": "± 356",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2145,
            "range": "± 317",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7199164,
            "range": "± 278590",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 864,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 776,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16767,
            "range": "± 680",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14199,
            "range": "± 407",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 390,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 672,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 85,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 85,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 85,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2029721,
            "range": "± 13480",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1992320,
            "range": "± 59902",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1938904,
            "range": "± 67550",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2380,
            "range": "± 201",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2436,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 540,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 569,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1091,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1157,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 745,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 973,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 380,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 383,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 333,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 814,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 406,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 505820,
            "range": "± 18927",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27938752,
            "range": "± 1316355",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3137,
            "range": "± 510",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13270,
            "range": "± 2419",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4515,
            "range": "± 460",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15741,
            "range": "± 3916",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15918,
            "range": "± 2703",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 40099,
            "range": "± 4601",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2799,
            "range": "± 1377",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 322,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2029081,
            "range": "± 87096",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "48886eacc82ff6df7acc8f2d729a5cbdd126d89d",
          "message": "Add == and != to arrays and maps.",
          "timestamp": "2020-11-08T23:00:37+08:00",
          "tree_id": "e98f49e2e73d4377087047863e374a62efba5f99",
          "url": "https://github.com/schungx/rhai/commit/48886eacc82ff6df7acc8f2d729a5cbdd126d89d"
        },
        "date": 1604848021864,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118779,
            "range": "± 63665",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 128,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 117,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 298,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2160,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2197,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7656564,
            "range": "± 9524",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 784,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 810,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16824,
            "range": "± 171",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14679,
            "range": "± 103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 395,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 695,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2116871,
            "range": "± 4027",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2055515,
            "range": "± 5452",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2029414,
            "range": "± 6927",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2473,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2537,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 564,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 607,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1121,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1185,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 779,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1006,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 394,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 390,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 261,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 793,
            "range": "± 539",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 418,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 526591,
            "range": "± 76893",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28739223,
            "range": "± 118846",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3176,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13461,
            "range": "± 227",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4558,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16141,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16145,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45152,
            "range": "± 342",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2951,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 342,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2135742,
            "range": "± 6222",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "4b622a8830110173bf7bb53987596740951fd259",
          "message": "Enable export let/export const short-hand.",
          "timestamp": "2020-11-09T12:21:11+08:00",
          "tree_id": "95c7e5f27d0e891350528f7077d243a65824b906",
          "url": "https://github.com/schungx/rhai/commit/4b622a8830110173bf7bb53987596740951fd259"
        },
        "date": 1604896204278,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 125968,
            "range": "± 18051",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 149,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 115,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 322,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2241,
            "range": "± 283",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2304,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7876731,
            "range": "± 815756",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 902,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 849,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16535,
            "range": "± 2688",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14676,
            "range": "± 2614",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 415,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 736,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 90,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2139786,
            "range": "± 261133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2169573,
            "range": "± 256928",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2077053,
            "range": "± 190848",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2497,
            "range": "± 263",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2621,
            "range": "± 262",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 608,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 612,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1260,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1325,
            "range": "± 213",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 829,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1096,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 423,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 412,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 284,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 365,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 838,
            "range": "± 134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 459,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 555705,
            "range": "± 62388",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32607839,
            "range": "± 4388445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3369,
            "range": "± 1038",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13777,
            "range": "± 2824",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4651,
            "range": "± 853",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16490,
            "range": "± 2136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16206,
            "range": "± 2293",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 40311,
            "range": "± 4352",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2883,
            "range": "± 636",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 372,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2274609,
            "range": "± 511936",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "173f8474d69092e6a9565bc2a4715d3d3fa7afed",
          "message": "Fix no_module build.",
          "timestamp": "2020-11-09T12:50:18+08:00",
          "tree_id": "b3ce43b022b5d1c36c1ac2d9e61899a2ce8a2fe1",
          "url": "https://github.com/schungx/rhai/commit/173f8474d69092e6a9565bc2a4715d3d3fa7afed"
        },
        "date": 1604897794719,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117615,
            "range": "± 2770",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 116,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 297,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2170,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2202,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7681704,
            "range": "± 9867",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 797,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 866,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16969,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14755,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 391,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 687,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2134909,
            "range": "± 3595",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2091513,
            "range": "± 3980",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2063822,
            "range": "± 3553",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2469,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2493,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 582,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 607,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1196,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 785,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1013,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 399,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 392,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 263,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 348,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 802,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 422,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 529393,
            "range": "± 1861",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29040631,
            "range": "± 135723",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3195,
            "range": "± 124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13727,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4629,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16170,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16372,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43707,
            "range": "± 290",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2971,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 343,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2135626,
            "range": "± 6580",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "821e64adc41e2b7fe39d8423485aa1a9553836f7",
          "message": "Allow multiple exports.",
          "timestamp": "2020-11-09T14:38:33+08:00",
          "tree_id": "cd24aa8c61b7992b45fa45c177a06c1d2502b9f8",
          "url": "https://github.com/schungx/rhai/commit/821e64adc41e2b7fe39d8423485aa1a9553836f7"
        },
        "date": 1604904308665,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113109,
            "range": "± 1065",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2124,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2139,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7282630,
            "range": "± 15740",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 810,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 812,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16236,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14018,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 380,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 660,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 87,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 86,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 87,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2035136,
            "range": "± 5320",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2022575,
            "range": "± 91820",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1967974,
            "range": "± 4040",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2400,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2437,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 579,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 607,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1143,
            "range": "± 884",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1184,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 751,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 997,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 382,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 382,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 803,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 410,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 508248,
            "range": "± 24690",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28706088,
            "range": "± 1991505",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3095,
            "range": "± 372",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13548,
            "range": "± 2868",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4519,
            "range": "± 203",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16080,
            "range": "± 768",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16204,
            "range": "± 2583",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44474,
            "range": "± 3234",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2841,
            "range": "± 758",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 341,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2117422,
            "range": "± 212592",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "e69444293c68a6659de7d6223e5ad7f1b1ff5b84",
          "message": "Encapsulate imported modules into AST.",
          "timestamp": "2020-11-09T21:52:23+08:00",
          "tree_id": "8ce4c66a63d04d7b133fec6166e937709bf7abeb",
          "url": "https://github.com/schungx/rhai/commit/e69444293c68a6659de7d6223e5ad7f1b1ff5b84"
        },
        "date": 1604930403410,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 111737,
            "range": "± 4311",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 121,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 115,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2083,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2137,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7051903,
            "range": "± 295001",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 760,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 799,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16261,
            "range": "± 1345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 13576,
            "range": "± 1493",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 371,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 655,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 87,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1999587,
            "range": "± 100496",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1991394,
            "range": "± 111781",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1931882,
            "range": "± 123564",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2353,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2485,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 574,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 632,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1102,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1160,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 751,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 944,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 369,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 369,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 250,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 331,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 742,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 389,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 486787,
            "range": "± 26526",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27693944,
            "range": "± 1478069",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2943,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12336,
            "range": "± 692",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4217,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 14921,
            "range": "± 1093",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 14899,
            "range": "± 853",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 38990,
            "range": "± 2291",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2725,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 319,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1984656,
            "range": "± 119840",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "c41f5aefcb646e22e928300ec6362417b7d63653",
          "message": "Change sub-modules to shared.",
          "timestamp": "2020-11-09T22:44:20+08:00",
          "tree_id": "130e60e430ae2c16eb2248e403bc0b6053219ece",
          "url": "https://github.com/schungx/rhai/commit/c41f5aefcb646e22e928300ec6362417b7d63653"
        },
        "date": 1604934052454,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 115090,
            "range": "± 981",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 118,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 299,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2202,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2233,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7566509,
            "range": "± 20726",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 926,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 852,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16893,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14762,
            "range": "± 234",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 390,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 682,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 92,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 92,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 92,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2149526,
            "range": "± 1801",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2087433,
            "range": "± 6893",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2023936,
            "range": "± 73316",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2593,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2583,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 614,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 655,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1174,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1233,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 782,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1028,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 402,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 401,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 262,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 345,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 794,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 418,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 530733,
            "range": "± 3621",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29448130,
            "range": "± 670369",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3210,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13627,
            "range": "± 139",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4575,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16512,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16364,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46035,
            "range": "± 559",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2960,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 344,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2153135,
            "range": "± 1789",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "7b27dcdd625cc24834ccc706909eaaf6d52a1c34",
          "message": "Fix error messages.",
          "timestamp": "2020-11-11T13:25:45+08:00",
          "tree_id": "14dd0f4ec1b935a75597e2769f9a1fba0e7b2f8f",
          "url": "https://github.com/schungx/rhai/commit/7b27dcdd625cc24834ccc706909eaaf6d52a1c34"
        },
        "date": 1605104848839,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 108955,
            "range": "± 2567",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 285,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2103,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2114,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7287324,
            "range": "± 53950",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 784,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 811,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15992,
            "range": "± 783",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14109,
            "range": "± 520",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 383,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 665,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2058049,
            "range": "± 35119",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1992549,
            "range": "± 36028",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1954349,
            "range": "± 34619",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2403,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2442,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 580,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 622,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1127,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1186,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 746,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 972,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 387,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 381,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 332,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 751,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 405,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 511852,
            "range": "± 17079",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28181161,
            "range": "± 292230",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3066,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13145,
            "range": "± 302",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4479,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15687,
            "range": "± 459",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15723,
            "range": "± 665",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43706,
            "range": "± 1424",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2950,
            "range": "± 242",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 334,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2058868,
            "range": "± 40933",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "41c815f3555a013612a6a5c996450efa3038966e",
          "message": "Optimize in-place.",
          "timestamp": "2020-11-12T12:37:42+08:00",
          "tree_id": "a5502f2c64ddc045f2dfbd210b6dd33cd4de0ee8",
          "url": "https://github.com/schungx/rhai/commit/41c815f3555a013612a6a5c996450efa3038966e"
        },
        "date": 1605156471335,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 115777,
            "range": "± 23691",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 113,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 308,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2128,
            "range": "± 364",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2211,
            "range": "± 397",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7494956,
            "range": "± 1255772",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 789,
            "range": "± 180",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 826,
            "range": "± 160",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 15814,
            "range": "± 3038",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 14067,
            "range": "± 3823",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 390,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 672,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 85,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 85,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 84,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1985664,
            "range": "± 327748",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1955688,
            "range": "± 340838",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1958312,
            "range": "± 255455",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2372,
            "range": "± 1076",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2519,
            "range": "± 387",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 574,
            "range": "± 276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 603,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1188,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1240,
            "range": "± 179",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 741,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1031,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 397,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 391,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 295,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 812,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 422,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 532174,
            "range": "± 74920",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 30830634,
            "range": "± 3503014",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3142,
            "range": "± 372",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 12871,
            "range": "± 2299",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4502,
            "range": "± 530",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15537,
            "range": "± 2586",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15282,
            "range": "± 2692",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 39230,
            "range": "± 5195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2809,
            "range": "± 398",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 340,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2064081,
            "range": "± 314173",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "69a0f044f452f1f2a4059649069ca6a35b72659c",
          "message": "Use interned strings to prepare for match statement.",
          "timestamp": "2020-11-12T22:36:13+08:00",
          "tree_id": "a4174bb001367e1c85eae6ee2a217e47b429c1c1",
          "url": "https://github.com/schungx/rhai/commit/69a0f044f452f1f2a4059649069ca6a35b72659c"
        },
        "date": 1605192721448,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 127331,
            "range": "± 2206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 124,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 123,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 290,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2129,
            "range": "± 1336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2160,
            "range": "± 1107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7333133,
            "range": "± 25911",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 806,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 835,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20352,
            "range": "± 442",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16598,
            "range": "± 287",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 382,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 660,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 92,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 92,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 91,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2052541,
            "range": "± 22428",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1963978,
            "range": "± 26703",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1940077,
            "range": "± 16714",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2430,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2465,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 582,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 629,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1141,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1214,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 734,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 963,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 380,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 378,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 250,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 334,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 753,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 400,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 508944,
            "range": "± 10120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28353852,
            "range": "± 180336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3213,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15406,
            "range": "± 341",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4776,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17420,
            "range": "± 132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17601,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45284,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3222,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 339,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2098606,
            "range": "± 8515",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "27619b86db98606ef817a83cd947f326430f653e",
          "message": "Fix builds.",
          "timestamp": "2020-11-12T23:09:27+08:00",
          "tree_id": "91a1a2c4ac695327cc0b0bbfa660c594c04e6637",
          "url": "https://github.com/schungx/rhai/commit/27619b86db98606ef817a83cd947f326430f653e"
        },
        "date": 1605194389057,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 128155,
            "range": "± 12994",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 123,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 112,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 313,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2202,
            "range": "± 212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2260,
            "range": "± 198",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7653342,
            "range": "± 750780",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 823,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 841,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18083,
            "range": "± 1655",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16925,
            "range": "± 2569",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 398,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 706,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 90,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 88,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 86,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2083091,
            "range": "± 215887",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2012293,
            "range": "± 193881",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1938603,
            "range": "± 128703",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2466,
            "range": "± 245",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2490,
            "range": "± 193",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 622,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 650,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1200,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1251,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 775,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 996,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 407,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 397,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 276,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 350,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 812,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 421,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 529178,
            "range": "± 43240",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32034892,
            "range": "± 2091831",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3314,
            "range": "± 311",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15126,
            "range": "± 1598",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4842,
            "range": "± 352",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17483,
            "range": "± 1258",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17592,
            "range": "± 1780",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45681,
            "range": "± 3024",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3231,
            "range": "± 243",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 357,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2140606,
            "range": "± 267224",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "7d1b971b39293a88442e10973dd905eb9f430047",
          "message": "More interned strings.",
          "timestamp": "2020-11-13T10:43:54+08:00",
          "tree_id": "6e810e9fa730872e8b40ad053532dc33602d205d",
          "url": "https://github.com/schungx/rhai/commit/7d1b971b39293a88442e10973dd905eb9f430047"
        },
        "date": 1605235837680,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 111884,
            "range": "± 1692",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 290,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2078,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2125,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7298695,
            "range": "± 11539",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 804,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 832,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20762,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16763,
            "range": "± 224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 378,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 661,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2060721,
            "range": "± 6006",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2034247,
            "range": "± 5707",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1980059,
            "range": "± 5151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2418,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2454,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 584,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 618,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1158,
            "range": "± 235",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1194,
            "range": "± 585",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 751,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 983,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 385,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 379,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 334,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 769,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 409,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 514959,
            "range": "± 133245",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29064375,
            "range": "± 2978507",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3298,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15772,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4739,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17982,
            "range": "± 199",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17949,
            "range": "± 252",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45990,
            "range": "± 312",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3235,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 346,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2065607,
            "range": "± 5769",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "55b4907f19d5abded62db62afb0877899e9bc392",
          "message": "Add switch expression.",
          "timestamp": "2020-11-13T18:32:18+08:00",
          "tree_id": "9dad0b7174af9737e55f1dfd569802b7f898f894",
          "url": "https://github.com/schungx/rhai/commit/55b4907f19d5abded62db62afb0877899e9bc392"
        },
        "date": 1605264600310,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 125755,
            "range": "± 36166",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 137,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 120,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 351,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2259,
            "range": "± 236",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2244,
            "range": "± 482",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7693029,
            "range": "± 724492",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 814,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 845,
            "range": "± 267",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21606,
            "range": "± 3466",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17421,
            "range": "± 4090",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 413,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 706,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 91,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 88,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2186419,
            "range": "± 284674",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2026072,
            "range": "± 437868",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1965099,
            "range": "± 415556",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2516,
            "range": "± 433",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2570,
            "range": "± 393",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 612,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 639,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1260,
            "range": "± 242",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1278,
            "range": "± 308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 810,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1057,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 413,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 415,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 276,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 366,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 796,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 432,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 551324,
            "range": "± 82337",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 33941636,
            "range": "± 4061057",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3519,
            "range": "± 585",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16376,
            "range": "± 3907",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5073,
            "range": "± 946",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18396,
            "range": "± 2579",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 19035,
            "range": "± 4239",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 48378,
            "range": "± 10780",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3356,
            "range": "± 575",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 398,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2159286,
            "range": "± 298879",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "bc07b4fa8447c4583e2e83d50c52812c0889f194",
          "message": "Fix builds.",
          "timestamp": "2020-11-13T18:49:23+08:00",
          "tree_id": "40e12b2b950d5f2da54dd3fceb40b18215a047fe",
          "url": "https://github.com/schungx/rhai/commit/bc07b4fa8447c4583e2e83d50c52812c0889f194"
        },
        "date": 1605265098557,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 104314,
            "range": "± 8769",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 112,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 105,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 271,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1929,
            "range": "± 197",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1889,
            "range": "± 437",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6682187,
            "range": "± 537785",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 727,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 755,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17236,
            "range": "± 1942",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 15485,
            "range": "± 1667",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 346,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 617,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 84,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 82,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 82,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1870178,
            "range": "± 185703",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1848942,
            "range": "± 182899",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1846870,
            "range": "± 179880",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2216,
            "range": "± 222",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2231,
            "range": "± 191",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 542,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 568,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1036,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1181,
            "range": "± 194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 677,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 871,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 347,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 358,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 227,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 307,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 699,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 371,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 479299,
            "range": "± 49141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26782859,
            "range": "± 1616016",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3060,
            "range": "± 308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 14642,
            "range": "± 1590",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4471,
            "range": "± 518",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 16798,
            "range": "± 1813",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 16867,
            "range": "± 1468",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44027,
            "range": "± 3304",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3081,
            "range": "± 278",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 339,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1914148,
            "range": "± 214751",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b0c66eb5e573b92a64040bebc21303907ff55975",
          "message": "Add benchmarks for switch statement.",
          "timestamp": "2020-11-13T19:35:51+08:00",
          "tree_id": "cba052e92b6a2cf541224ea4ce534b01e2843157",
          "url": "https://github.com/schungx/rhai/commit/b0c66eb5e573b92a64040bebc21303907ff55975"
        },
        "date": 1605267945296,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 120122,
            "range": "± 14825",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 120,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 318,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2086,
            "range": "± 314",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2098,
            "range": "± 290",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7151692,
            "range": "± 640908",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 758,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 792,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20729,
            "range": "± 2272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17073,
            "range": "± 3050",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 380,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 676,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 82,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 83,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 83,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1936623,
            "range": "± 303584",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1878183,
            "range": "± 282752",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1806060,
            "range": "± 341560",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2417,
            "range": "± 298",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2397,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 576,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 606,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1134,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1182,
            "range": "± 168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 743,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1048,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 416,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 381,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 259,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 333,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 748,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 428,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 495641,
            "range": "± 55062",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 30540716,
            "range": "± 2967495",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3275,
            "range": "± 479",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15369,
            "range": "± 1877",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4737,
            "range": "± 748",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18062,
            "range": "± 3694",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18494,
            "range": "± 3402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46116,
            "range": "± 5820",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3324,
            "range": "± 565",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 393,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2019202,
            "range": "± 219650",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "56fbe39b7bea3da6ff573a93b33aec20c387aaa1",
          "message": "Use references for switch expressions, if possible.",
          "timestamp": "2020-11-14T16:08:48+08:00",
          "tree_id": "7b9b62b69bd0240f268124ef1fb1f6c2efbc0f53",
          "url": "https://github.com/schungx/rhai/commit/56fbe39b7bea3da6ff573a93b33aec20c387aaa1"
        },
        "date": 1605341951157,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 112640,
            "range": "± 19249",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 126,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 111,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 291,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2007,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2054,
            "range": "± 442",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7198357,
            "range": "± 1049901",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 792,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 803,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20585,
            "range": "± 4205",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17346,
            "range": "± 3345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 385,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 645,
            "range": "± 168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 86,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 89,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 89,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2022642,
            "range": "± 409289",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1992919,
            "range": "± 254501",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2019801,
            "range": "± 379074",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2358,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2336,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 562,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 578,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1071,
            "range": "± 219",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1178,
            "range": "± 188",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 729,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 969,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 374,
            "range": "± 297",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 372,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 257,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 330,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 716,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 388,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 492390,
            "range": "± 34503",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27077661,
            "range": "± 2038113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3139,
            "range": "± 439",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15245,
            "range": "± 1762",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4597,
            "range": "± 630",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17981,
            "range": "± 1300",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17590,
            "range": "± 1316",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45541,
            "range": "± 4521",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3238,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 359,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2094697,
            "range": "± 296446",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "89254a04c4fe37fc03daa84b5d45795a8ca19853",
          "message": "Fix tests.",
          "timestamp": "2020-11-14T17:22:01+08:00",
          "tree_id": "595e105f1bfbf161ab29b76966d8ad8f328ca942",
          "url": "https://github.com/schungx/rhai/commit/89254a04c4fe37fc03daa84b5d45795a8ca19853"
        },
        "date": 1605346329487,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 129400,
            "range": "± 6583",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 130,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 117,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 330,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2303,
            "range": "± 202",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2285,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7869921,
            "range": "± 483298",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 842,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 868,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 19715,
            "range": "± 1601",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17881,
            "range": "± 687",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 422,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 716,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 88,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 88,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 88,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2079460,
            "range": "± 209404",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2130368,
            "range": "± 136912",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2022557,
            "range": "± 73956",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2566,
            "range": "± 497",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2606,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 616,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 643,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1276,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1310,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 800,
            "range": "± 166",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1048,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 431,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 419,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 275,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 363,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 817,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 436,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 557080,
            "range": "± 56752",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32888324,
            "range": "± 2602968",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3422,
            "range": "± 256",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16203,
            "range": "± 1098",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4946,
            "range": "± 301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18420,
            "range": "± 1122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18844,
            "range": "± 924",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 48165,
            "range": "± 3910",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3515,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 417,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2241017,
            "range": "± 202494",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "c104afbdce03f13e3be0c418b9142717140b1354",
          "message": "Fix switch test.",
          "timestamp": "2020-11-14T18:30:26+08:00",
          "tree_id": "250afafec938e3b0208a0c9594f8d464e59cbd4d",
          "url": "https://github.com/schungx/rhai/commit/c104afbdce03f13e3be0c418b9142717140b1354"
        },
        "date": 1605350373125,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 105112,
            "range": "± 20781",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 118,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 109,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 278,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1937,
            "range": "± 425",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2013,
            "range": "± 412",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6595374,
            "range": "± 1540045",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 720,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 790,
            "range": "± 195",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17981,
            "range": "± 4630",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16860,
            "range": "± 3123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 370,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 638,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 80,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 81,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 80,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1872375,
            "range": "± 431835",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1894421,
            "range": "± 427124",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1810988,
            "range": "± 261639",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2259,
            "range": "± 469",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2231,
            "range": "± 569",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 571,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 590,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1099,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1174,
            "range": "± 285",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 733,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 949,
            "range": "± 192",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 352,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 361,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 236,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 311,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 697,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 373,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 479529,
            "range": "± 97697",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26478368,
            "range": "± 3770765",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3084,
            "range": "± 538",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15372,
            "range": "± 2801",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4428,
            "range": "± 1052",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18027,
            "range": "± 4872",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17700,
            "range": "± 3709",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 43543,
            "range": "± 9034",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3254,
            "range": "± 618",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 344,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1914929,
            "range": "± 505465",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "28de155f086f037c988032aea7425e04bc2333ce",
          "message": "Add Expr::DynamicConstant.",
          "timestamp": "2020-11-14T19:04:49+08:00",
          "tree_id": "e25cdae1e12b1afd0ea2c47c61189733b1805e86",
          "url": "https://github.com/schungx/rhai/commit/28de155f086f037c988032aea7425e04bc2333ce"
        },
        "date": 1605352676715,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 96556,
            "range": "± 10584",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 111,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 100,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 256,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1893,
            "range": "± 243",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1902,
            "range": "± 319",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6507655,
            "range": "± 705970",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 692,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 726,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 16353,
            "range": "± 2308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 15843,
            "range": "± 2390",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 330,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 576,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 76,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 79,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 76,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1752041,
            "range": "± 210660",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1771328,
            "range": "± 238466",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1679223,
            "range": "± 257859",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2176,
            "range": "± 288",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2212,
            "range": "± 297",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 503,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 541,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 978,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1036,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 637,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 839,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 337,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 329,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 219,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 292,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 669,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 349,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 422661,
            "range": "± 57461",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 24419694,
            "range": "± 2307423",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2851,
            "range": "± 472",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 13689,
            "range": "± 1762",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4148,
            "range": "± 414",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 15747,
            "range": "± 1924",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 15699,
            "range": "± 2652",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 40452,
            "range": "± 9774",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 2906,
            "range": "± 346",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 322,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1857712,
            "range": "± 297162",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "eb49a4b40ac5a07be20a2b419ebc9d2f9016f3c9",
          "message": "Speed up switch benches.",
          "timestamp": "2020-11-14T19:23:10+08:00",
          "tree_id": "f7a1b36a8bbc8263e8444ca27c2fa85573ceb2b4",
          "url": "https://github.com/schungx/rhai/commit/eb49a4b40ac5a07be20a2b419ebc9d2f9016f3c9"
        },
        "date": 1605353438411,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 112616,
            "range": "± 29457",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 122,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 114,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 296,
            "range": "± 47",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2093,
            "range": "± 680",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2149,
            "range": "± 509",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7371949,
            "range": "± 1387837",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 820,
            "range": "± 1045",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 840,
            "range": "± 196",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21132,
            "range": "± 6733",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17171,
            "range": "± 3296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 371,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 647,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 88,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 86,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 88,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2055602,
            "range": "± 48551",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1980329,
            "range": "± 98844",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1903115,
            "range": "± 127951",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16966,
            "range": "± 854",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7983,
            "range": "± 282",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2436,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2478,
            "range": "± 149",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 587,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 622,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1129,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1187,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 744,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 966,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 381,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 385,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 251,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 333,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 760,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 406,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 501667,
            "range": "± 25000",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27805620,
            "range": "± 785199",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3246,
            "range": "± 327",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15728,
            "range": "± 530",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4839,
            "range": "± 378",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18318,
            "range": "± 984",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17635,
            "range": "± 855",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46003,
            "range": "± 2212",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3262,
            "range": "± 208",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 362,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2093829,
            "range": "± 317290",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "a63f14b59c4cc41e2e250b645950aa451f079ccc",
          "message": "Expr::Switch -> Stmt::Switch.",
          "timestamp": "2020-11-14T23:43:36+08:00",
          "tree_id": "9772a2adcc488b666806f18a9d92812dc190b1b3",
          "url": "https://github.com/schungx/rhai/commit/a63f14b59c4cc41e2e250b645950aa451f079ccc"
        },
        "date": 1605369030262,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 115213,
            "range": "± 1991",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 132,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 119,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 302,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 2165,
            "range": "± 629",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 2228,
            "range": "± 218",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7600689,
            "range": "± 6361",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 836,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 857,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22022,
            "range": "± 233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17667,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 397,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 689,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 96,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 96,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 93,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2147633,
            "range": "± 1873",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2112093,
            "range": "± 4069",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2062242,
            "range": "± 5436",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 18040,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8661,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2524,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2566,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 611,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 638,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1177,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1247,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 773,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1006,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 401,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 395,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 263,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 349,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 793,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 427,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 530993,
            "range": "± 2064",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29497082,
            "range": "± 92331",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3437,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16548,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5135,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18865,
            "range": "± 306",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18893,
            "range": "± 151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 49001,
            "range": "± 289",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3441,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 383,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2158877,
            "range": "± 2387",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "bde8917ed4a2551bdc3cf315771a876751aa6fef",
          "message": "Set capacity of hash maps.",
          "timestamp": "2020-11-15T12:07:35+08:00",
          "tree_id": "e1cca4b3c827fc27df42bf93f192c5fb1a5d828e",
          "url": "https://github.com/schungx/rhai/commit/bde8917ed4a2551bdc3cf315771a876751aa6fef"
        },
        "date": 1605413850789,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118535,
            "range": "± 27603",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 447,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 455,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 622,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1413,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1446,
            "range": "± 222",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7265125,
            "range": "± 1092976",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 604,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 629,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20732,
            "range": "± 3371",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19227,
            "range": "± 6403",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 369,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 652,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 87,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 82,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 81,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1760929,
            "range": "± 821877",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1675978,
            "range": "± 289378",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1671321,
            "range": "± 637993",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 15651,
            "range": "± 3831",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7157,
            "range": "± 1793",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1668,
            "range": "± 538",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1809,
            "range": "± 579",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 503,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 555,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1184,
            "range": "± 912",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1170,
            "range": "± 855",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 674,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 904,
            "range": "± 183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 362,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 367,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 231,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 295,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 676,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 360,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 443896,
            "range": "± 75062",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27456757,
            "range": "± 6810350",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3037,
            "range": "± 707",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15343,
            "range": "± 4101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4480,
            "range": "± 1299",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 19049,
            "range": "± 6557",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18127,
            "range": "± 5006",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 49094,
            "range": "± 18372",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3359,
            "range": "± 1135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 711,
            "range": "± 207",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1675907,
            "range": "± 398323",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "fbe9425794544490bcc7334eb8007844db376602",
          "message": "Add discriminant to Dynamic::hash.",
          "timestamp": "2020-11-15T18:39:23+08:00",
          "tree_id": "096020ef4f7397205a81c09dbcca6b732e12f32f",
          "url": "https://github.com/schungx/rhai/commit/fbe9425794544490bcc7334eb8007844db376602"
        },
        "date": 1605437561466,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 111284,
            "range": "± 12678",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 245,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 244,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 399,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1421,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1482,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7020639,
            "range": "± 453402",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 614,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 655,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21678,
            "range": "± 4232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20051,
            "range": "± 3629",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 416,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 689,
            "range": "± 121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 126,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 123,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 124,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1963515,
            "range": "± 151161",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1935418,
            "range": "± 185076",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1873222,
            "range": "± 172839",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16598,
            "range": "± 1006",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7946,
            "range": "± 595",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1953,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2003,
            "range": "± 133",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 534,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 575,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1138,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1169,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 717,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 930,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 366,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 364,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 240,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 318,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 749,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 389,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 501347,
            "range": "± 166216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27099262,
            "range": "± 3375662",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3516,
            "range": "± 299",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 18086,
            "range": "± 971",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5234,
            "range": "± 477",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 19785,
            "range": "± 2132",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 20844,
            "range": "± 1991",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 50381,
            "range": "± 5191",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3669,
            "range": "± 354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 715,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2014239,
            "range": "± 377482",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b75964e383f3dc18d450cf7f34cbb3a539d56855",
          "message": "Add Dynamic::UNIT.",
          "timestamp": "2020-11-15T23:14:29+08:00",
          "tree_id": "0c8b6adf8d60edd152be28835a75fa2dac76ae20",
          "url": "https://github.com/schungx/rhai/commit/b75964e383f3dc18d450cf7f34cbb3a539d56855"
        },
        "date": 1605453720649,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 109369,
            "range": "± 4795",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 256,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 242,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 395,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1433,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1456,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6982193,
            "range": "± 333940",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 636,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 673,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21993,
            "range": "± 878",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20473,
            "range": "± 1750",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 417,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 679,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 135,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 131,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 133,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1967257,
            "range": "± 69194",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1933286,
            "range": "± 69851",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1872316,
            "range": "± 87657",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16285,
            "range": "± 599",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7973,
            "range": "± 367",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1921,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1980,
            "range": "± 153",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 533,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 575,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1108,
            "range": "± 96",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1160,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 716,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 940,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 391,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 380,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 254,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 353,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 739,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 409,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 482868,
            "range": "± 19600",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 26810206,
            "range": "± 901181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3368,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 17808,
            "range": "± 646",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5356,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 20497,
            "range": "± 741",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 20659,
            "range": "± 677",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 51990,
            "range": "± 3809",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3863,
            "range": "± 238",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 732,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1941042,
            "range": "± 65854",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "ef02150afdf3daf7634003affdf6ac0e78a3960d",
          "message": "Expose methods for Engine::register_module.",
          "timestamp": "2020-11-16T14:07:48+08:00",
          "tree_id": "88185b24537f5753ec5324d29e9095361b4a9db2",
          "url": "https://github.com/schungx/rhai/commit/ef02150afdf3daf7634003affdf6ac0e78a3960d"
        },
        "date": 1605507541275,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116698,
            "range": "± 20781",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 252,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 246,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 397,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1298,
            "range": "± 265",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1318,
            "range": "± 138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6726784,
            "range": "± 866350",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 586,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 607,
            "range": "± 114",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 19743,
            "range": "± 3240",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17591,
            "range": "± 2736",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 387,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 670,
            "range": "± 150",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 123,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 121,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 122,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1713350,
            "range": "± 312551",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1787171,
            "range": "± 343296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1721933,
            "range": "± 295193",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 15656,
            "range": "± 2197",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7457,
            "range": "± 1453",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1794,
            "range": "± 295",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1813,
            "range": "± 305",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 504,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 536,
            "range": "± 60",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1085,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1161,
            "range": "± 236",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 680,
            "range": "± 142",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 916,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 368,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 366,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 248,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 330,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 762,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 387,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 456150,
            "range": "± 52425",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 27799506,
            "range": "± 2666330",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3205,
            "range": "± 566",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16117,
            "range": "± 2393",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4905,
            "range": "± 916",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18681,
            "range": "± 3354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18368,
            "range": "± 2526",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46198,
            "range": "± 6426",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3592,
            "range": "± 684",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 669,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1839594,
            "range": "± 310943",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "adb902326e641b4ca2512ef65a536cdb08a34093",
          "message": "Reduce feature gates on imports.",
          "timestamp": "2020-11-16T16:28:04+08:00",
          "tree_id": "442da9c24866c83501a56387d3c7d129f3b52459",
          "url": "https://github.com/schungx/rhai/commit/adb902326e641b4ca2512ef65a536cdb08a34093"
        },
        "date": 1605515863143,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 116506,
            "range": "± 15601",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 259,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 253,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 397,
            "range": "± 69",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1425,
            "range": "± 288",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1413,
            "range": "± 338",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6643416,
            "range": "± 1065874",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 599,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 635,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21327,
            "range": "± 3599",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19924,
            "range": "± 3396",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 457,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 741,
            "range": "± 216",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 142,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 136,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 141,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1903332,
            "range": "± 212209",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1984059,
            "range": "± 228090",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1827978,
            "range": "± 278672",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 18148,
            "range": "± 3587",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8372,
            "range": "± 1286",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1982,
            "range": "± 337",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1952,
            "range": "± 329",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 564,
            "range": "± 103",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 576,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1189,
            "range": "± 204",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1236,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 707,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 973,
            "range": "± 285",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 389,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 389,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 258,
            "range": "± 57",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 337,
            "range": "± 58",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 727,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 412,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 476007,
            "range": "± 87347",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28726007,
            "range": "± 3488867",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3443,
            "range": "± 409",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 18080,
            "range": "± 3829",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5086,
            "range": "± 815",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 19513,
            "range": "± 3079",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 20335,
            "range": "± 2974",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 49656,
            "range": "± 5932",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3749,
            "range": "± 671",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 726,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1889693,
            "range": "± 367074",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "699220057feef5b6485307ac162bea5ef07b449e",
          "message": "Fix bug.",
          "timestamp": "2020-11-16T23:17:34+08:00",
          "tree_id": "0040620537d02518e0776484ab99e1ae0cad6b05",
          "url": "https://github.com/schungx/rhai/commit/699220057feef5b6485307ac162bea5ef07b449e"
        },
        "date": 1605540303414,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 107746,
            "range": "± 18144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 266,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 247,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 368,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1115,
            "range": "± 330",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1416,
            "range": "± 379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7590660,
            "range": "± 1106611",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 682,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 720,
            "range": "± 117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 24394,
            "range": "± 2487",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20900,
            "range": "± 4016",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 475,
            "range": "± 62",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 794,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 138,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 137,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 139,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1958124,
            "range": "± 515276",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1827009,
            "range": "± 472228",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1684612,
            "range": "± 453834",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16402,
            "range": "± 4264",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7312,
            "range": "± 2055",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1928,
            "range": "± 485",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2028,
            "range": "± 464",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 554,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 614,
            "range": "± 126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1167,
            "range": "± 222",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "7ef3bd6b201a60edcf504b1c02d6148787b93c62",
          "message": "Fix no_function build.",
          "timestamp": "2020-11-16T23:25:19+08:00",
          "tree_id": "7372d2ef4c24a8e8f17b506b18f2268668c7e035",
          "url": "https://github.com/schungx/rhai/commit/7ef3bd6b201a60edcf504b1c02d6148787b93c62"
        },
        "date": 1605540778032,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 112785,
            "range": "± 16755",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 236,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 237,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 374,
            "range": "± 86",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1368,
            "range": "± 645",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1320,
            "range": "± 354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6559457,
            "range": "± 788024",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 561,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 575,
            "range": "± 164",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 19037,
            "range": "± 10121",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17246,
            "range": "± 5484",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 381,
            "range": "± 143",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 641,
            "range": "± 245",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 124,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 119,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 114,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1722634,
            "range": "± 139373",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1765929,
            "range": "± 275607",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1660106,
            "range": "± 332732",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 15751,
            "range": "± 2653",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7053,
            "range": "± 1623",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1694,
            "range": "± 453",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1709,
            "range": "± 432",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 482,
            "range": "± 120",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 547,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1089,
            "range": "± 136",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "999a87f86e98e4128d75931968979a9cf0869580",
          "message": "Fix no_optimize build.",
          "timestamp": "2020-11-16T23:32:44+08:00",
          "tree_id": "8d23f74d4ac20748249b81d54e4a883570ca0311",
          "url": "https://github.com/schungx/rhai/commit/999a87f86e98e4128d75931968979a9cf0869580"
        },
        "date": 1605542515092,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 113869,
            "range": "± 1715",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 277,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 268,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 430,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1467,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1487,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7198989,
            "range": "± 74593",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 643,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 674,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21971,
            "range": "± 825",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19942,
            "range": "± 613",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 437,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 709,
            "range": "± 43",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 140,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 141,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 141,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2055759,
            "range": "± 121691",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1992198,
            "range": "± 8123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1936768,
            "range": "± 17778",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17272,
            "range": "± 671",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8264,
            "range": "± 171",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2032,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2075,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 560,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 590,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1156,
            "range": "± 9",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "038e3c2554619ba77754f77449551e4ef73fcbf2",
          "message": "Add FnNamespace for module functions.",
          "timestamp": "2020-11-17T12:23:53+08:00",
          "tree_id": "55c3144fca2adbdabdedfe5ab5b5f423d1dd4ae5",
          "url": "https://github.com/schungx/rhai/commit/038e3c2554619ba77754f77449551e4ef73fcbf2"
        },
        "date": 1605587352343,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117820,
            "range": "± 9243",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 266,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 261,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 435,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1459,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1487,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7282499,
            "range": "± 209720",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 642,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 673,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21383,
            "range": "± 642",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20111,
            "range": "± 929",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 428,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 715,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 141,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 139,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 140,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2078228,
            "range": "± 62736",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2023983,
            "range": "± 60182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1996922,
            "range": "± 51590",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17450,
            "range": "± 174",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8344,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2025,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2061,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 558,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 584,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1174,
            "range": "± 23",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "df72d324ba7264af8bb866fbe42be1c2e864af83",
          "message": "Fix tests.",
          "timestamp": "2020-11-17T12:40:12+08:00",
          "tree_id": "79024d280024850c6a534244c82ba016b58bda12",
          "url": "https://github.com/schungx/rhai/commit/df72d324ba7264af8bb866fbe42be1c2e864af83"
        },
        "date": 1605588387078,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 117824,
            "range": "± 4200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 269,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 273,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 431,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1450,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1481,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7365145,
            "range": "± 69326",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 644,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 678,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21946,
            "range": "± 498",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20117,
            "range": "± 262",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 436,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 718,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 145,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 145,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 151,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2185258,
            "range": "± 17454",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2138018,
            "range": "± 9331",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2089108,
            "range": "± 22007",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17660,
            "range": "± 186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8506,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2001,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2039,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 552,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 579,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1193,
            "range": "± 14",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "bd094d95b4ab142b31030329de14008415ab3f5b",
          "message": "Fix codegen doc error.",
          "timestamp": "2020-11-17T13:11:31+08:00",
          "tree_id": "c14bc8e6b33c76742a06113a62512b774b2529a3",
          "url": "https://github.com/schungx/rhai/commit/bd094d95b4ab142b31030329de14008415ab3f5b"
        },
        "date": 1605590375863,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 119963,
            "range": "± 19739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 244,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 241,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 392,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1381,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1475,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7153814,
            "range": "± 1325091",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 615,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 633,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21740,
            "range": "± 4559",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19354,
            "range": "± 4205",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 415,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 668,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 128,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 130,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 120,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1833346,
            "range": "± 430126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1820607,
            "range": "± 310778",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1941453,
            "range": "± 372394",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17058,
            "range": "± 4027",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7622,
            "range": "± 1689",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1759,
            "range": "± 410",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1787,
            "range": "± 382",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 506,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 533,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1169,
            "range": "± 196",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "21c641d21f790137f8253f5a04bb18fe8ac73434",
          "message": "Set namespace in Module::set_fn_XXX_mut calls.",
          "timestamp": "2020-11-17T15:11:58+08:00",
          "tree_id": "5573d3a990fbd26e9493a7e8c5a17b60d8dcf7eb",
          "url": "https://github.com/schungx/rhai/commit/21c641d21f790137f8253f5a04bb18fe8ac73434"
        },
        "date": 1605626018619,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 103665,
            "range": "± 31736",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 270,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 247,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 365,
            "range": "± 65",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1192,
            "range": "± 206",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1308,
            "range": "± 333",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6143011,
            "range": "± 949475",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 520,
            "range": "± 159",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 555,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18889,
            "range": "± 3320",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 18322,
            "range": "± 2908",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 367,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 606,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 115,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 118,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 116,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1696477,
            "range": "± 309872",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1787263,
            "range": "± 347162",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1688088,
            "range": "± 291844",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 14404,
            "range": "± 2491",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 6836,
            "range": "± 1025",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1706,
            "range": "± 366",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1721,
            "range": "± 328",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 457,
            "range": "± 82",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 504,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 997,
            "range": "± 183",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "1de85d749dd46fe039e5ae9e07b3c34ecbe97cb5",
          "message": "Optimize AST.",
          "timestamp": "2020-11-19T10:41:08+08:00",
          "tree_id": "22b231a4d73fd5134b314d53ed9d1ee0a808676e",
          "url": "https://github.com/schungx/rhai/commit/1de85d749dd46fe039e5ae9e07b3c34ecbe97cb5"
        },
        "date": 1605754151871,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 126658,
            "range": "± 1101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 280,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 279,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 445,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1531,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1559,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7927811,
            "range": "± 653781",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 667,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 694,
            "range": "± 193",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 23271,
            "range": "± 3018",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 21211,
            "range": "± 1499",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 455,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 760,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 133,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 133,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 132,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2194459,
            "range": "± 209177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2145999,
            "range": "± 207506",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2101209,
            "range": "± 276962",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 18639,
            "range": "± 2921",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8892,
            "range": "± 1263",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2086,
            "range": "± 125",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2113,
            "range": "± 228",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 570,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 601,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1233,
            "range": "± 123",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "5e4ede6f07be7855cff0a288f0d44a7bc6a474e0",
          "message": "global_module -> global_namespace.",
          "timestamp": "2020-11-19T13:56:03+08:00",
          "tree_id": "46af44980b7e4a6b39947b8d1e765bfc6d3659c8",
          "url": "https://github.com/schungx/rhai/commit/5e4ede6f07be7855cff0a288f0d44a7bc6a474e0"
        },
        "date": 1605766067549,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 124103,
            "range": "± 6613",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 270,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 261,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 426,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1399,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1425,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7175877,
            "range": "± 330891",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 620,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 658,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21900,
            "range": "± 2774",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20267,
            "range": "± 1177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 426,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 709,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 127,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 127,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2080386,
            "range": "± 183051",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1986008,
            "range": "± 105171",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1952351,
            "range": "± 97400",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17069,
            "range": "± 987",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8176,
            "range": "± 537",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1944,
            "range": "± 156",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1980,
            "range": "± 56",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 518,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 563,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1186,
            "range": "± 78",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b87fa87a75ae3125c77363a31b6e77c3530a733c",
          "message": "Fix internals.",
          "timestamp": "2020-11-19T14:08:58+08:00",
          "tree_id": "53a6bbe1540570bdab00bf0a30a5077ae27a80e7",
          "url": "https://github.com/schungx/rhai/commit/b87fa87a75ae3125c77363a31b6e77c3530a733c"
        },
        "date": 1605793302317,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 126076,
            "range": "± 24498",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 237,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 207,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 396,
            "range": "± 98",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1083,
            "range": "± 225",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1110,
            "range": "± 553",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6627712,
            "range": "± 1103230",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 542,
            "range": "± 84",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 604,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 19155,
            "range": "± 4715",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17324,
            "range": "± 3030",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 408,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 630,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 109,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 110,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 107,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2017945,
            "range": "± 807012",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1815932,
            "range": "± 473972",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1647159,
            "range": "± 318468",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16903,
            "range": "± 4891",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7846,
            "range": "± 1663",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1767,
            "range": "± 273",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1732,
            "range": "± 422",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 481,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 485,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1132,
            "range": "± 213",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "2afcecc6ba0cb4686c3c7848b97d7a0f99d6b053",
          "message": "Fix benchmark.",
          "timestamp": "2020-11-20T10:35:25+08:00",
          "tree_id": "63e0a0bc9ba631af14a5d807fbdfcf0d6eed3a4f",
          "url": "https://github.com/schungx/rhai/commit/2afcecc6ba0cb4686c3c7848b97d7a0f99d6b053"
        },
        "date": 1605840305980,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 127070,
            "range": "± 20295",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 267,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 270,
            "range": "± 25",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 439,
            "range": "± 72",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1401,
            "range": "± 273",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1439,
            "range": "± 257",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7328242,
            "range": "± 759427",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 638,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 685,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 20714,
            "range": "± 5343",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19339,
            "range": "± 6354",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 427,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 700,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 115,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 117,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 115,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2027073,
            "range": "± 341782",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2047701,
            "range": "± 221922",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1994310,
            "range": "± 418112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 18283,
            "range": "± 3374",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8512,
            "range": "± 1804",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1883,
            "range": "± 229",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1953,
            "range": "± 252",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 518,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 548,
            "range": "± 97",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1241,
            "range": "± 292",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1200,
            "range": "± 134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 764,
            "range": "± 109",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1077,
            "range": "± 345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 446,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 409,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 254,
            "range": "± 52",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 338,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 783,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 402,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 528463,
            "range": "± 102652",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 32259644,
            "range": "± 3389379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3605,
            "range": "± 724",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 17589,
            "range": "± 3530",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5280,
            "range": "± 609",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 21734,
            "range": "± 4024",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 22347,
            "range": "± 3860",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 50058,
            "range": "± 8468",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3829,
            "range": "± 678",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 770,
            "range": "± 219",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2046649,
            "range": "± 252588",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "783803ec469a7987a8a5f7e4b51f37f8faf96986",
          "message": "Update comments with links.",
          "timestamp": "2020-11-20T16:52:28+08:00",
          "tree_id": "95b2f4371e89d47701a12793d6526816f320e119",
          "url": "https://github.com/schungx/rhai/commit/783803ec469a7987a8a5f7e4b51f37f8faf96986"
        },
        "date": 1605877710434,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 123847,
            "range": "± 2148",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 268,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 248,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 419,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1408,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1436,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7151130,
            "range": "± 413183",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 613,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 647,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22287,
            "range": "± 1800",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20035,
            "range": "± 671",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 420,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 728,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 122,
            "range": "± 17",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 122,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2092699,
            "range": "± 68807",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2046488,
            "range": "± 63673",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1954662,
            "range": "± 95964",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17617,
            "range": "± 765",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8429,
            "range": "± 270",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1949,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1973,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 511,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 562,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1163,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1165,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 709,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 974,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 391,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 378,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 248,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 332,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 768,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 397,
            "range": "± 24",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 505391,
            "range": "± 45971",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28516230,
            "range": "± 1124270",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3600,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 18233,
            "range": "± 1334",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5518,
            "range": "± 260",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 21683,
            "range": "± 1485",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 21693,
            "range": "± 1288",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 53127,
            "range": "± 1435",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3890,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 780,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2021875,
            "range": "± 37842",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6069a4cf55105526994777de66af3c69de63bc12",
          "message": "do loop.",
          "timestamp": "2020-11-20T22:23:37+08:00",
          "tree_id": "81d80851cfeb8e3d84033e65a00fe54c19874ef2",
          "url": "https://github.com/schungx/rhai/commit/6069a4cf55105526994777de66af3c69de63bc12"
        },
        "date": 1605882531528,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 118708,
            "range": "± 5575",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 257,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 256,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 416,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1396,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1413,
            "range": "± 73",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6995102,
            "range": "± 327969",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 600,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 631,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21938,
            "range": "± 612",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 19075,
            "range": "± 1925",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 395,
            "range": "± 53",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 658,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 123,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 119,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 120,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2033532,
            "range": "± 186967",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1871273,
            "range": "± 293086",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1882587,
            "range": "± 227513",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16507,
            "range": "± 5332",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7617,
            "range": "± 1117",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1792,
            "range": "± 312",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1900,
            "range": "± 240",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 491,
            "range": "± 93",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 506,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1090,
            "range": "± 233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1085,
            "range": "± 232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 647,
            "range": "± 77",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 849,
            "range": "± 89",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 346,
            "range": "± 42",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 341,
            "range": "± 70",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 224,
            "range": "± 44",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 281,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 681,
            "range": "± 112",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 340,
            "range": "± 55",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 451669,
            "range": "± 89301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25003285,
            "range": "± 3488152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3164,
            "range": "± 510",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16047,
            "range": "± 2445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4579,
            "range": "± 784",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18821,
            "range": "± 2289",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18809,
            "range": "± 2022",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46350,
            "range": "± 4295",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3424,
            "range": "± 401",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 675,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1728400,
            "range": "± 206753",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b1ad0695b4c3ca155f55f1547b5be177795ff4e9",
          "message": "Make shadowing variables in custom syntax work.",
          "timestamp": "2020-11-21T12:28:04+08:00",
          "tree_id": "d6862557929059c03c6d55f13f7a974decf8448e",
          "url": "https://github.com/schungx/rhai/commit/b1ad0695b4c3ca155f55f1547b5be177795ff4e9"
        },
        "date": 1605933313776,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 126411,
            "range": "± 13834",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 271,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 265,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 424,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1421,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1481,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7315824,
            "range": "± 8461",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 631,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 655,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22094,
            "range": "± 272",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20666,
            "range": "± 301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 422,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 709,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2142155,
            "range": "± 2016",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2123485,
            "range": "± 1690",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2062821,
            "range": "± 8467",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17746,
            "range": "± 213",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8607,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1959,
            "range": "± 727",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1988,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 528,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 567,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1163,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1176,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 736,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 983,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 391,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 386,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 255,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 328,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 790,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 393,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 508435,
            "range": "± 2973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 28458563,
            "range": "± 84406",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3609,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 19128,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5491,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 21528,
            "range": "± 173",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 21974,
            "range": "± 214",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 53920,
            "range": "± 400",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3930,
            "range": "± 29",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 769,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2029067,
            "range": "± 25516",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "611f6151d5ab355f1e40bfc58e7d942378198a48",
          "message": "Merge branch 'master' of https://github.com/schungx/rhai",
          "timestamp": "2020-11-21T15:10:44+08:00",
          "tree_id": "5b1bd77f911f901877dd3c606262ac5847045ca3",
          "url": "https://github.com/schungx/rhai/commit/611f6151d5ab355f1e40bfc58e7d942378198a48"
        },
        "date": 1605942950770,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 103727,
            "range": "± 1157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 226,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 221,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 361,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1186,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1203,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6161195,
            "range": "± 7749",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 525,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 541,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18252,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17148,
            "range": "± 296",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 349,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 589,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 106,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 106,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 105,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1758751,
            "range": "± 1792",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1727981,
            "range": "± 3480",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1682331,
            "range": "± 4075",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 14820,
            "range": "± 83",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7064,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1634,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1650,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 441,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 464,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1013,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1002,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 623,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 827,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 325,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 320,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 205,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 275,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 647,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 328,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 429785,
            "range": "± 1061",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25345927,
            "range": "± 131934",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3196,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15979,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4610,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18260,
            "range": "± 168",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18470,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45820,
            "range": "± 406",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3330,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 647,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1714341,
            "range": "± 3623",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "eb4636f219a05bc590f9eed844363f47a252d37f",
          "message": "Fix LexError::ImproperSymbol.",
          "timestamp": "2020-11-21T15:15:14+08:00",
          "tree_id": "37771496e6c5975e979dcefe56899dd21d887feb",
          "url": "https://github.com/schungx/rhai/commit/eb4636f219a05bc590f9eed844363f47a252d37f"
        },
        "date": 1605943237949,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 104302,
            "range": "± 904",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 227,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 218,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 365,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1204,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1210,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6282420,
            "range": "± 14563",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 529,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 554,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18202,
            "range": "± 185",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16838,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 352,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 594,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 109,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 108,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 108,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1759503,
            "range": "± 1608",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1727602,
            "range": "± 4044",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1682032,
            "range": "± 3973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 14722,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7064,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1628,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1653,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 444,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 469,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1011,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1002,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 614,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 827,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 334,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 326,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 215,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 282,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 655,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 336,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 449848,
            "range": "± 9437",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25311388,
            "range": "± 85105",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3170,
            "range": "± 88",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15928,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4628,
            "range": "± 26",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18594,
            "range": "± 239",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18572,
            "range": "± 130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 45662,
            "range": "± 432",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3304,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 646,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1691120,
            "range": "± 4016",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "97368da76244ac0efb94cd3641d14016035669e3",
          "message": "Reserve begin/end.",
          "timestamp": "2020-11-21T15:44:17+08:00",
          "tree_id": "b7e9ce2a40e44539cd279754ae78454fa1998dd5",
          "url": "https://github.com/schungx/rhai/commit/97368da76244ac0efb94cd3641d14016035669e3"
        },
        "date": 1605950452372,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 105136,
            "range": "± 22336",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 229,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 229,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 380,
            "range": "± 94",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1235,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1226,
            "range": "± 210",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6404330,
            "range": "± 916798",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 534,
            "range": "± 61",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 559,
            "range": "± 68",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18518,
            "range": "± 2865",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 17791,
            "range": "± 3045",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 372,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 603,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 106,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 107,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 109,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1843168,
            "range": "± 197608",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1793775,
            "range": "± 223277",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1757352,
            "range": "± 252750",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 15088,
            "range": "± 2539",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7207,
            "range": "± 973",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1735,
            "range": "± 181",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1723,
            "range": "± 200",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 449,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 489,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1017,
            "range": "± 159",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1019,
            "range": "± 113",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 623,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 857,
            "range": "± 141",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 327,
            "range": "± 54",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 317,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 204,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 280,
            "range": "± 59",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 647,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 344,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 455897,
            "range": "± 64358",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 24648884,
            "range": "± 1979427",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3288,
            "range": "± 430",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 16090,
            "range": "± 2423",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4764,
            "range": "± 623",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18768,
            "range": "± 2755",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18414,
            "range": "± 2225",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 46124,
            "range": "± 6672",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3410,
            "range": "± 489",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 663,
            "range": "± 110",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1756905,
            "range": "± 239803",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "254fd16e62bfdb50d4a0ff11aa6f83fa49c5853c",
          "message": "Fix type sizes test.",
          "timestamp": "2020-11-21T17:39:43+08:00",
          "tree_id": "b8639a5a5795f7950d2c7494e55a62225cdb69b1",
          "url": "https://github.com/schungx/rhai/commit/254fd16e62bfdb50d4a0ff11aa6f83fa49c5853c"
        },
        "date": 1605951917298,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 103365,
            "range": "± 974",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 229,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 221,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 364,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1196,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1206,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6147161,
            "range": "± 8095",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 526,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 546,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 18612,
            "range": "± 262",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16993,
            "range": "± 140",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 348,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 589,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 105,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 105,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 105,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1767857,
            "range": "± 7186",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1730757,
            "range": "± 20224",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1685486,
            "range": "± 3072",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 14767,
            "range": "± 104",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7093,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1637,
            "range": "± 15",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1653,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 441,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 468,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1078,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1061,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 619,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 827,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 326,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 320,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 205,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 275,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 664,
            "range": "± 22",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 329,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 433931,
            "range": "± 718",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 25180517,
            "range": "± 89954",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3139,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15686,
            "range": "± 99",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4694,
            "range": "± 14",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 18218,
            "range": "± 134",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 18253,
            "range": "± 115",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 44790,
            "range": "± 246",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3287,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 650,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1697001,
            "range": "± 5576",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "07fe132e1a0dfafa4d8940be2d6f10597f4d6c4f",
          "message": "Add gen_fn_siguatures API.",
          "timestamp": "2020-11-22T17:21:34+08:00",
          "tree_id": "e95c285385cefe29e29b4140d7380a72dcee62a1",
          "url": "https://github.com/schungx/rhai/commit/07fe132e1a0dfafa4d8940be2d6f10597f4d6c4f"
        },
        "date": 1606037482189,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 308798,
            "range": "± 43376",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 229,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 228,
            "range": "± 51",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 408,
            "range": "± 100",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1199,
            "range": "± 273",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1306,
            "range": "± 182",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 6768402,
            "range": "± 885930",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 558,
            "range": "± 102",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 600,
            "range": "± 205",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 21282,
            "range": "± 2451",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 18531,
            "range": "± 1472",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 391,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 622,
            "range": "± 123",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 104,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 102,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 108,
            "range": "± 11",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1978382,
            "range": "± 328799",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1932738,
            "range": "± 279177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1828031,
            "range": "± 203962",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 16658,
            "range": "± 2002",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 7441,
            "range": "± 1422",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1725,
            "range": "± 552",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1745,
            "range": "± 308",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 449,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 522,
            "range": "± 63",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1145,
            "range": "± 198",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1140,
            "range": "± 225",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 663,
            "range": "± 157",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 913,
            "range": "± 293",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 366,
            "range": "± 67",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 352,
            "range": "± 74",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 235,
            "range": "± 45",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 309,
            "range": "± 108",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 742,
            "range": "± 158",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 387,
            "range": "± 85",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 492252,
            "range": "± 86565",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29227425,
            "range": "± 3639090",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3487,
            "range": "± 530",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 17640,
            "range": "± 4483",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5067,
            "range": "± 590",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 20656,
            "range": "± 3252",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 22047,
            "range": "± 6504",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 51056,
            "range": "± 25237",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3819,
            "range": "± 975",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 638,
            "range": "± 111",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1913240,
            "range": "± 363876",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "9edd494000ba6580e7a363404217570a079d1a38",
          "message": "Fix builds.",
          "timestamp": "2020-11-22T17:32:10+08:00",
          "tree_id": "070387984b4041e605198fd28b68ad10a8ba114d",
          "url": "https://github.com/schungx/rhai/commit/9edd494000ba6580e7a363404217570a079d1a38"
        },
        "date": 1606037803030,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 252593,
            "range": "± 46893",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 203,
            "range": "± 79",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 213,
            "range": "± 64",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 406,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1118,
            "range": "± 210",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1184,
            "range": "± 430",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 5804137,
            "range": "± 767392",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 491,
            "range": "± 107",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 490,
            "range": "± 170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 17341,
            "range": "± 2739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 16952,
            "range": "± 4657",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 349,
            "range": "± 116",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 585,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 101,
            "range": "± 23",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 101,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 95,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 1698829,
            "range": "± 346199",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 1698344,
            "range": "± 313955",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 1615710,
            "range": "± 240655",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 14099,
            "range": "± 2902",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 6883,
            "range": "± 1356",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1544,
            "range": "± 259",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1604,
            "range": "± 345",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 436,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 456,
            "range": "± 136",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 947,
            "range": "± 144",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 879,
            "range": "± 232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 637,
            "range": "± 211",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 815,
            "range": "± 299",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 334,
            "range": "± 118",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 302,
            "range": "± 80",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 196,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 276,
            "range": "± 66",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 692,
            "range": "± 151",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 343,
            "range": "± 78",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 419796,
            "range": "± 92530",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 23105660,
            "range": "± 3572039",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 2993,
            "range": "± 416",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 15383,
            "range": "± 3657",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 4371,
            "range": "± 669",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 17382,
            "range": "± 4220",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 17803,
            "range": "± 3643",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 42511,
            "range": "± 8211",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3208,
            "range": "± 432",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 661,
            "range": "± 211",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 1666549,
            "range": "± 286803",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "d3f38941268bc1d36e542328d6e22dadf464cc18",
          "message": "Fix typo.",
          "timestamp": "2020-11-22T17:35:33+08:00",
          "tree_id": "e38df43043d41e9d297635629c3a4c9deb469410",
          "url": "https://github.com/schungx/rhai/commit/d3f38941268bc1d36e542328d6e22dadf464cc18"
        },
        "date": 1606038369395,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 331808,
            "range": "± 2780",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 273,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 275,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 471,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1483,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1511,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7635982,
            "range": "± 7379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 657,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 680,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 23321,
            "range": "± 379",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 22215,
            "range": "± 177",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 434,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 735,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 131,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 131,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 131,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2195103,
            "range": "± 2302",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2158522,
            "range": "± 4097",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2091649,
            "range": "± 15244",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 18466,
            "range": "± 129",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8812,
            "range": "± 75",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 2033,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2071,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 551,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 584,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1248,
            "range": "± 9",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1226,
            "range": "± 12",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 759,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 1014,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 405,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 400,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 260,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 345,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 827,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 421,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 536132,
            "range": "± 2679",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 30523213,
            "range": "± 125977",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3985,
            "range": "± 101",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 19886,
            "range": "± 260",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5801,
            "range": "± 128",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 23412,
            "range": "± 175",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 23907,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 55649,
            "range": "± 266",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 4265,
            "range": "± 92",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 679,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2140105,
            "range": "± 4626",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "6222f14fcb7db40034524da11d6a3f0851f268d5",
          "message": "Fix no-std build.",
          "timestamp": "2020-11-22T17:49:00+08:00",
          "tree_id": "44de2f8ac2c6144e497fa6c0b0a8a6926addff28",
          "url": "https://github.com/schungx/rhai/commit/6222f14fcb7db40034524da11d6a3f0851f268d5"
        },
        "date": 1606038867065,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 304044,
            "range": "± 60576",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 239,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 264,
            "range": "± 36",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 453,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1420,
            "range": "± 87",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1452,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7405227,
            "range": "± 429440",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 632,
            "range": "± 152",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 655,
            "range": "± 48",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22601,
            "range": "± 2454",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 21488,
            "range": "± 4743",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 423,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 711,
            "range": "± 19",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 128,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 127,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 6",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2123862,
            "range": "± 241739",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2098641,
            "range": "± 234087",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2045839,
            "range": "± 193170",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17724,
            "range": "± 416",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8446,
            "range": "± 372",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1959,
            "range": "± 50",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2014,
            "range": "± 95",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 530,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 569,
            "range": "± 28",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1179,
            "range": "± 32",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1174,
            "range": "± 76",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 735,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 980,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 386,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 386,
            "range": "± 283",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 252,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 335,
            "range": "± 27",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 796,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 403,
            "range": "± 13",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 531952,
            "range": "± 26825",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29796409,
            "range": "± 1726698",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3727,
            "range": "± 866",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 19097,
            "range": "± 1331",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5474,
            "range": "± 1896",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 22173,
            "range": "± 1275",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 22242,
            "range": "± 2628",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 54405,
            "range": "± 7445",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3920,
            "range": "± 323",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 683,
            "range": "± 233",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2066765,
            "range": "± 246192",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "b43223a94fcebbf7300ca1ecfd116c39a8fc04d0",
          "message": "Add get_fn_metadata_list.",
          "timestamp": "2020-11-23T19:11:32+08:00",
          "tree_id": "58fde7ea95c0cf20a6ed0e4f159bac20b5410622",
          "url": "https://github.com/schungx/rhai/commit/b43223a94fcebbf7300ca1ecfd116c39a8fc04d0"
        },
        "date": 1606130265451,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 378092,
            "range": "± 5587",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 268,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 263,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 431,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1485,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1513,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7457983,
            "range": "± 93232",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 683,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 710,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22652,
            "range": "± 586",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20388,
            "range": "± 459",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 426,
            "range": "± 33",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 719,
            "range": "± 31",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 129,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2123924,
            "range": "± 57629",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2114434,
            "range": "± 32130",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2045713,
            "range": "± 59220",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17761,
            "range": "± 587",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8424,
            "range": "± 339",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1961,
            "range": "± 81",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 2000,
            "range": "± 106",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 528,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 565,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1176,
            "range": "± 37",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1167,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 729,
            "range": "± 16",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 991,
            "range": "± 40",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 394,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 387,
            "range": "± 18",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 248,
            "range": "± 5",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 330,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 782,
            "range": "± 38",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 396,
            "range": "± 2",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 527503,
            "range": "± 4730",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 29349523,
            "range": "± 305153",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3701,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 19321,
            "range": "± 327",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5536,
            "range": "± 253",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 22155,
            "range": "± 665",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 22416,
            "range": "± 675",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 54417,
            "range": "± 641",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 3996,
            "range": "± 122",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 650,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2014502,
            "range": "± 21008",
            "unit": "ns/iter"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "committer": {
            "email": "Stephen.Chung@intexact.com",
            "name": "Stephen Chung",
            "username": "schungx"
          },
          "distinct": true,
          "id": "2d734687232973126a1200eeeaea81295d684324",
          "message": "Add doc on Engine::gen_fn_signatures.",
          "timestamp": "2020-11-23T20:27:20+08:00",
          "tree_id": "9a12a3a98d3d24a87ee6f8d4c3e74cc8b91522de",
          "url": "https://github.com/schungx/rhai/commit/2d734687232973126a1200eeeaea81295d684324"
        },
        "date": 1606134640165,
        "tool": "cargo",
        "benches": [
          {
            "name": "bench_engine_new",
            "value": 376692,
            "range": "± 26866",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw",
            "value": 268,
            "range": "± 10",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_new_raw_core",
            "value": 262,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_engine_register_fn",
            "value": 429,
            "range": "± 21",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_get",
            "value": 1431,
            "range": "± 7",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_large_set",
            "value": 1448,
            "range": "± 90",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_loop",
            "value": 7284506,
            "range": "± 153941",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_get",
            "value": 628,
            "range": "± 4",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_array_small_set",
            "value": 654,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call",
            "value": 22648,
            "range": "± 713",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_call_expression",
            "value": 20783,
            "range": "± 847",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_literal",
            "value": 421,
            "range": "± 1",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_number_operators",
            "value": 713,
            "range": "± 3",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_full",
            "value": 126,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_optimized_simple",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_expression_single",
            "value": 127,
            "range": "± 0",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_number",
            "value": 2100502,
            "range": "± 61138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_build",
            "value": 2077872,
            "range": "± 63779",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_loop_strings_no_build",
            "value": 2021902,
            "range": "± 128579",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_nested_if",
            "value": 17706,
            "range": "± 1126",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_switch",
            "value": 8405,
            "range": "± 455",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_get",
            "value": 1948,
            "range": "± 135",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_large_set",
            "value": 1990,
            "range": "± 71",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_get",
            "value": 529,
            "range": "± 34",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_map_small_set",
            "value": 564,
            "range": "± 30",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_function_call",
            "value": 1166,
            "range": "± 49",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_module",
            "value": 1158,
            "range": "± 20",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_complex",
            "value": 730,
            "range": "± 35",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_longer",
            "value": 990,
            "range": "± 541",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_multiple",
            "value": 388,
            "range": "± 254",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_scope_single",
            "value": 388,
            "range": "± 39",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_field",
            "value": 247,
            "range": "± 41",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method",
            "value": 330,
            "range": "± 46",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_nested",
            "value": 784,
            "range": "± 138",
            "unit": "ns/iter"
          },
          {
            "name": "bench_type_method_with_params",
            "value": 397,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_1000",
            "value": 519353,
            "range": "± 47301",
            "unit": "ns/iter"
          },
          {
            "name": "bench_iterations_fibonacci",
            "value": 30347532,
            "range": "± 3354003",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_array",
            "value": 3771,
            "range": "± 154",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_full",
            "value": 19138,
            "range": "± 278",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_map",
            "value": 5582,
            "range": "± 137",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_full",
            "value": 22322,
            "range": "± 850",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_optimize_simple",
            "value": 22376,
            "range": "± 894",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_primes",
            "value": 54019,
            "range": "± 1316",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_simple",
            "value": 4036,
            "range": "± 131",
            "unit": "ns/iter"
          },
          {
            "name": "bench_parse_single",
            "value": 652,
            "range": "± 8",
            "unit": "ns/iter"
          },
          {
            "name": "bench_eval_primes",
            "value": 2039240,
            "range": "± 484102",
            "unit": "ns/iter"
          }
        ]
      }
    ]
  }
}