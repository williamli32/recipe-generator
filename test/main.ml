(* TEST PLAN: This test suite was developed primarily using glass box testing
   and OUnit. Each function that is used by the final filter function is tested
   with edge cases and with knowledge of the filter.ml functions and the
   contents of the ingredients list. The functions tested in this test suite
   are: list_from_string_tests; make_list_from_list split_list make_tuple
   apply_filter remove_restrictions names_from_tuple find_common get_ingredients
   get_flavor get_secret_sauce_or_spice. Some of the functions above use helper
   functions in the filter.ml file, and thus those helper functions are tested
   as well through the test suite. These functions are tested against each
   branch of the ingredients list at least once, and every function that does
   not depend on random generation is used in one or more test cases. Functions
   that use random generation were tested with utop and with command line and
   gui interaction, due to the nature of random generation and ounit testing.
   The interfaces for the command line and the gui were tested interactively
   rather than through OUnit.*)

open OUnit2
open Recipe.Filter

(* LIST FROM STRING TESTS *)
let make_list_from_string_test test_name expected ingredient_category category
    name =
  test_name >:: fun _ ->
  assert_equal expected
    (Recipe.Filter.make_list_from_string
       (Yojson.Basic.from_file "data/test_ingredients.json")
       ingredient_category category name)

let list_from_string_tests =
  [
    make_list_from_string_test "Empty list1" [] "proteins" "red_bull" "name";
    make_list_from_string_test "Red Meats"
      [ "steak"; "lamb"; "pork" ]
      "proteins" "red meats" "name";
    make_list_from_string_test "Seafood"
      [ "salmon"; "pufferfish"; "mussels" ]
      "proteins" "seafoods" "name";
    make_list_from_string_test "Poultry" [ "chicken"; "turkey" ] "proteins"
      "poultry" "name";
    make_list_from_string_test "Vegetarian Protein"
      [ "tofu"; "chickpeas"; "black beans"; "eggs" ]
      "proteins" "vegetarian" "name";
    make_list_from_string_test "Cruciferous vegetables"
      [ "broccoli"; "cauliflower"; "brussels sprouts" ]
      "vegetables" "cruciferous" "name";
    make_list_from_string_test "Leafy Greens"
      [ "spinach"; "cabbage"; "romaine lettuce" ]
      "vegetables" "leafy greens" "name";
    make_list_from_string_test "Non-Starchy vegetables"
      [ "onion"; "bell pepper"; "asparagus" ]
      "vegetables" "non-starchy vegetables" "name";
    make_list_from_string_test "Starchy vegetables"
      [ "corn"; "pumpkin"; "butternut squash" ]
      "vegetables" "starchy vegetables" "name";
    make_list_from_string_test "Root vegetables"
      [ "potato"; "sweet potato" ]
      "carbs" "root vegetables" "name";
    make_list_from_string_test "Grains" [ "rice"; "quinoa" ] "carbs" "grains"
      "name";
    make_list_from_string_test "Pantry"
      [ "pasta"; "noodles"; "sourdough bread"; "tortilla" ]
      "carbs" "pantry" "name";
  ]

(* LIST FROM LIST TESTS*)

let make_list_from_list_test test_name expected ingredient_category category
    list_name =
  test_name >:: fun _ ->
  assert_equal expected
    (Recipe.Filter.make_list_from_list
       (Yojson.Basic.from_file "data/test_ingredients.json")
       ingredient_category category list_name)

let seafoods_restrictions =
  [ "seafood"; "break"; "seafood"; "break"; "seafood"; "shellfish"; "break" ]

let seafoods_cuisines =
  [
    "American";
    "Chinese";
    "Argentinian";
    "Mediterranean";
    "Japanese";
    "break";
    "Japanese";
    "break";
    "Japanese";
    "Spanish";
    "French";
    "Argentinian";
    "Mediterranean";
    "break";
  ]

let red_meat_restrictions =
  [ "meat"; "beef"; "break"; "meat"; "break"; "meat"; "pork"; "break" ]

let poultry_meals = [ "lunch"; "dinner"; "break"; "lunch"; "dinner"; "break" ]

let vegetarian_protein_restrictions =
  [ "soy"; "break"; "break"; "break"; "non-vegan"; "break" ]

let cruciferous_cuisines =
  [
    "American";
    "Argentinian";
    "Chinese";
    "break";
    "American";
    "break";
    "American";
    "break";
  ]

let leafy_greens_meals =
  [
    "breakfast";
    "lunch";
    "dinner";
    "break";
    "lunch";
    "dinner";
    "break";
    "lunch";
    "break";
  ]

let non_starchy_veg_restrictions = [ "break"; "break"; "break" ]

let starchy_veg_meals =
  [
    "lunch";
    "dinner";
    "break";
    "lunch";
    "dinner";
    "break";
    "lunch";
    "dinner";
    "break";
  ]

let root_veg_meal =
  [
    "breakfast";
    "lunch";
    "dinner";
    "break";
    "breakfast";
    "lunch";
    "dinner";
    "break";
  ]

let grains_cuisines =
  [
    "American";
    "Chinese";
    "Indian";
    "Argentinian";
    "Japanese";
    "Middle Eastern";
    "break";
    "American";
    "break";
  ]

let pantry_restrictions =
  [ "gluten"; "break"; "gluten"; "break"; "gluten"; "break"; "gluten"; "break" ]

let list_from_list_tests =
  [
    make_list_from_list_test "Empty list2" [] "proteins" "seafoods" "coffee";
    make_list_from_list_test "Seafoods restrictions list" seafoods_restrictions
      "proteins" "seafoods" "restrictions";
    make_list_from_list_test "Seafoods cuisines" seafoods_cuisines "proteins"
      "seafoods" "cuisine";
    make_list_from_list_test "Red meat restrictions" red_meat_restrictions
      "proteins" "red meats" "restrictions";
    make_list_from_list_test "Poultry meals" poultry_meals "proteins" "poultry"
      "meal";
    make_list_from_list_test "Vegetarian protein restrictions"
      vegetarian_protein_restrictions "proteins" "vegetarian" "restrictions";
    make_list_from_list_test "Cruciferous cuisines" cruciferous_cuisines
      "vegetables" "cruciferous" "cuisine";
    make_list_from_list_test "Leafy greens meals" leafy_greens_meals
      "vegetables" "leafy greens" "meal";
    make_list_from_list_test "Non starchy veg restrictions"
      non_starchy_veg_restrictions "vegetables" "non-starchy vegetables"
      "restrictions";
    make_list_from_list_test "Starchy veg meals" starchy_veg_meals "vegetables"
      "starchy vegetables" "meal";
    make_list_from_list_test "Root vegetable meals" root_veg_meal "carbs"
      "root vegetables" "meal";
    make_list_from_list_test "Grains cuisines" grains_cuisines "carbs" "grains"
      "cuisine";
    make_list_from_list_test "Pantry restrictions" pantry_restrictions "carbs"
      "pantry" "restrictions";
  ]

(* SPLIT TESTS *)

let make_split_list_test test_name expected cuisine_list =
  test_name >:: fun _ ->
  assert_equal expected (Recipe.Filter.split_list cuisine_list)

let list1 = [ "a"; "b"; "break"; "c"; "d"; "break"; "e"; "f"; "break" ]
let extra2, list2 = Recipe.Filter.split_list list1

let split_list_tests =
  [
    make_split_list_test "test 1"
      ([ "a"; "b" ], [ "c"; "d"; "break"; "e"; "f"; "break" ])
      list1;
    make_split_list_test "test 2" ([ "c"; "d" ], [ "e"; "f"; "break" ]) list2;
    make_split_list_test "salmon cuisines"
      ( [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ],
        [
          "Japanese";
          "break";
          "Japanese";
          "Spanish";
          "French";
          "Argentinian";
          "Mediterranean";
          "break";
        ] )
      seafoods_cuisines;
    make_split_list_test "pufferfish cuisines"
      ( [ "Japanese" ],
        [
          "Japanese";
          "Spanish";
          "French";
          "Argentinian";
          "Mediterranean";
          "break";
        ] )
      (snd (Recipe.Filter.split_list seafoods_cuisines));
    make_split_list_test "steak restrictions"
      ([ "meat"; "beef" ], [ "meat"; "break"; "meat"; "pork"; "break" ])
      red_meat_restrictions;
    make_split_list_test "chicken meals"
      ([ "lunch"; "dinner" ], [ "lunch"; "dinner"; "break" ])
      poultry_meals;
    make_split_list_test "tofu restrictions"
      ([ "soy" ], [ "break"; "break"; "non-vegan"; "break" ])
      vegetarian_protein_restrictions;
    make_split_list_test "broccoli cuisines"
      ( [ "American"; "Argentinian"; "Chinese" ],
        [ "American"; "break"; "American"; "break" ] )
      cruciferous_cuisines;
    make_split_list_test "spinach meals"
      ( [ "breakfast"; "lunch"; "dinner" ],
        [ "lunch"; "dinner"; "break"; "lunch"; "break" ] )
      leafy_greens_meals;
    make_split_list_test "onion restrictions"
      ([], [ "break"; "break" ])
      non_starchy_veg_restrictions;
    make_split_list_test "corn meals"
      ( [ "lunch"; "dinner" ],
        [ "lunch"; "dinner"; "break"; "lunch"; "dinner"; "break" ] )
      starchy_veg_meals;
    make_split_list_test "potato meal"
      ( [ "breakfast"; "lunch"; "dinner" ],
        [ "breakfast"; "lunch"; "dinner"; "break" ] )
      root_veg_meal;
    make_split_list_test "rice cuisines"
      ( [
          "American";
          "Chinese";
          "Indian";
          "Argentinian";
          "Japanese";
          "Middle Eastern";
        ],
        [ "American"; "break" ] )
      grains_cuisines;
    make_split_list_test "pasta restrictions"
      ([ "gluten" ], [ "gluten"; "break"; "gluten"; "break"; "gluten"; "break" ])
      pantry_restrictions;
  ]

(* MAKE TUPLE TESTS *)

let make_tuple_test test_name expected ingredient_category category list_name =
  test_name >:: fun _ ->
  assert_equal expected
    (Recipe.Filter.make_tuple ingredient_category category "name" list_name)

let tuple_tests =
  [
    make_tuple_test "seafood cuisines"
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
        ("pufferfish", [ "Japanese" ]);
        ( "mussels",
          [ "Japanese"; "Spanish"; "French"; "Argentinian"; "Mediterranean" ] );
      ]
      "proteins" "seafoods" "cuisine";
    make_tuple_test "red meat restrictions"
      [
        ("steak", [ "meat"; "beef" ]);
        ("lamb", [ "meat" ]);
        ("pork", [ "meat"; "pork" ]);
      ]
      "proteins" "red meats" "restrictions";
    make_tuple_test "poultry meals"
      [ ("chicken", [ "lunch"; "dinner" ]); ("turkey", [ "lunch"; "dinner" ]) ]
      "proteins" "poultry" "meal";
    make_tuple_test "vegetarian protein restrictions"
      [
        ("tofu", [ "soy" ]);
        ("chickpeas", []);
        ("black beans", []);
        ("eggs", [ "non-vegan" ]);
        ("chorizo", [ "non-vegan" ]);
      ]
      "proteins" "vegetarian" "restrictions";
    make_tuple_test "cruciferous cuisines"
      [
        ( "broccoli",
          [ "American"; "Argentinian"; "Chinese"; "Japanese"; "Korean" ] );
        ("cauliflower", [ "American"; "Argentinian" ]);
        ("brussels sprouts", [ "American" ]);
      ]
      "vegetables" "cruciferous" "cuisine";
    make_tuple_test "leafy grens meals"
      [
        ("spinach", [ "breakfast"; "lunch"; "dinner" ]);
        ("cabbage", [ "lunch"; "dinner" ]);
        ("romaine lettuce", [ "lunch" ]);
      ]
      "vegetables" "leafy greens" "meal";
    make_tuple_test "non starchy restrictions"
      [ ("onion", []); ("bell pepper", []); ("asparagus", []); ("tomato", []) ]
      "vegetables" "non-starchy vegetables" "restrictions";
    make_tuple_test "starchy veg meals"
      [
        ("corn", [ "lunch"; "dinner" ]);
        ("pumpkin", [ "lunch"; "dinner" ]);
        ("butternut squash", [ "lunch"; "dinner" ]);
        ("carrots", [ "lunch"; "dinner" ]);
      ]
      "vegetables" "starchy vegetables" "meal";
    make_tuple_test "root vegetable meals"
      [
        ("potato", [ "breakfast"; "lunch"; "dinner" ]);
        ("sweet potato", [ "breakfast"; "lunch"; "dinner" ]);
      ]
      "carbs" "root vegetables" "meal";
    make_tuple_test "grain cuisines"
      [
        ( "rice",
          [
            "American";
            "Chinese";
            "Indian";
            "Japanese";
            "Middle Eastern";
            "Mediterranean";
          ] );
        ("quinoa", [ "American"; "Indian" ]);
      ]
      "carbs" "grains" "cuisine";
    make_tuple_test "pantry restrictions"
      [
        ("pasta", [ "gluten" ]);
        ("noodles", [ "gluten" ]);
        ("sourdough bread", [ "gluten" ]);
        ("tortilla", [ "gluten" ]);
      ]
      "carbs" "pantry" "restrictions";
  ]

(* APPLY FILTER TESTS *)
let make_apply_filter_test test_name expected list filter =
  test_name >:: fun _ ->
  assert_equal expected (Recipe.Filter.apply_filter list filter)

let apply_filter_tests =
  [
    make_apply_filter_test "none found: seafood cuisines" []
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine")
      "Indian";
    make_apply_filter_test "all filters in common: cruciferous cuisines"
      [
        ( "broccoli",
          [ "American"; "Argentinian"; "Chinese"; "Japanese"; "Korean" ] );
        ("cauliflower", [ "American"; "Argentinian" ]);
        ("brussels sprouts", [ "American" ]);
      ]
      (Recipe.Filter.make_tuple "vegetables" "cruciferous" "name" "cuisine")
      "American";
    make_apply_filter_test "only two share filter: leafy greens meals"
      [
        ("spinach", [ "breakfast"; "lunch"; "dinner" ]);
        ("cabbage", [ "lunch"; "dinner" ]);
      ]
      (Recipe.Filter.make_tuple "vegetables" "leafy greens" "name" "meal")
      "dinner";
    make_apply_filter_test "only one matches filter: leafy greens meals"
      [ ("spinach", [ "breakfast"; "lunch"; "dinner" ]) ]
      (Recipe.Filter.make_tuple "vegetables" "leafy greens" "name" "meal")
      "breakfast";
    make_apply_filter_test "removing one"
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
        ( "mussels",
          [ "Japanese"; "Spanish"; "French"; "Argentinian"; "Mediterranean" ] );
      ]
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine")
      "Argentinian";
    make_apply_filter_test "one item list"
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
      ]
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
      ]
      "Chinese";
    make_apply_filter_test "empty list3" [] [] "Mediterranean";
  ]

(* REMOVE RESTRICTIONS TESTS *)
let make_remove_restrictions_test test_name expected list filter =
  test_name >:: fun _ ->
  assert_equal expected (Recipe.Filter.remove_restrictions list filter)

let remove_restrictions_tests =
  [
    make_remove_restrictions_test "none to remove"
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
        ("pufferfish", [ "Japanese" ]);
        ( "mussels",
          [ "Japanese"; "Spanish"; "French"; "Argentinian"; "Mediterranean" ] );
      ]
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine")
      "Indian";
    make_remove_restrictions_test "one removed"
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
        ("pufferfish", [ "Japanese" ]);
      ]
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine")
      "Spanish";
    make_remove_restrictions_test "two (but not all) removed"
      [ ("pufferfish", [ "Japanese" ]) ]
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine")
      "Argentinian";
    make_remove_restrictions_test "all removed" []
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "meal")
      "dinner";
    make_remove_restrictions_test "empty list4" [] [] "Indian";
    make_remove_restrictions_test "one item list"
      [ ("pufferfish", [ "Japanese" ]) ]
      [ ("pufferfish", [ "Japanese" ]) ]
      "Spanish";
  ]

(* GET NAMES FROM TUPLE TESTS *)
let make_names_from_tuple_test test_name expected tuple_list =
  test_name >:: fun _ ->
  assert_equal expected (Recipe.Filter.get_names_from_tuple tuple_list)

let names_from_tuple_tests =
  [
    make_names_from_tuple_test "empty list5" [] [];
    make_names_from_tuple_test "one item list" [ "salmon" ]
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
      ];
    make_names_from_tuple_test "multiple item list"
      [ "salmon"; "pufferfish"; "mussels" ]
      [
        ( "salmon",
          [ "American"; "Chinese"; "Argentinian"; "Mediterranean"; "Japanese" ]
        );
        ("pufferfish", [ "Japanese" ]);
        ( "mussels",
          [ "Japanese"; "Spanish"; "French"; "Argentinian"; "Mediterranean" ] );
      ];
    make_names_from_tuple_test "seafoods"
      [ "salmon"; "pufferfish"; "mussels" ]
      (Recipe.Filter.make_tuple "proteins" "seafoods" "name" "cuisine");
    make_names_from_tuple_test "red meats"
      [ "steak"; "lamb"; "pork" ]
      (Recipe.Filter.make_tuple "proteins" "red meats" "name" "cuisine");
    make_names_from_tuple_test "poultry" [ "chicken"; "turkey" ]
      (Recipe.Filter.make_tuple "proteins" "poultry" "name" "cuisine");
    make_names_from_tuple_test "vegetarian proteins"
      [ "tofu"; "chickpeas"; "black beans"; "eggs"; "chorizo" ]
      (Recipe.Filter.make_tuple "proteins" "vegetarian" "name" "cuisine");
    make_names_from_tuple_test "cruciferous vegetables"
      [ "broccoli"; "cauliflower"; "brussels sprouts" ]
      (Recipe.Filter.make_tuple "vegetables" "cruciferous" "name" "cuisine");
    make_names_from_tuple_test "leafy greens"
      [ "spinach"; "cabbage"; "romaine lettuce" ]
      (Recipe.Filter.make_tuple "vegetables" "leafy greens" "name" "cuisine");
    make_names_from_tuple_test "starchy vegetables"
      [ "corn"; "pumpkin"; "butternut squash"; "carrots" ]
      (Recipe.Filter.make_tuple "vegetables" "starchy vegetables" "name"
         "cuisine");
    make_names_from_tuple_test "non-starchy vegetables"
      [ "onion"; "bell pepper"; "asparagus"; "tomato" ]
      (Recipe.Filter.make_tuple "vegetables" "non-starchy vegetables" "name"
         "cuisine");
    make_names_from_tuple_test "root vegetables"
      [ "potato"; "sweet potato" ]
      (Recipe.Filter.make_tuple "carbs" "root vegetables" "name" "cuisine");
    make_names_from_tuple_test "grains" [ "rice"; "quinoa" ]
      (Recipe.Filter.make_tuple "carbs" "grains" "name" "cuisine");
    make_names_from_tuple_test "pantry"
      [ "pasta"; "noodles"; "sourdough bread"; "tortilla" ]
      (Recipe.Filter.make_tuple "carbs" "pantry" "name" "cuisine");
  ]

(* FIND COMMON TESTS *)
let make_find_common_test test_name (expected : string list) cuisine meal
    restrictions ingredient_category category =
  test_name >:: fun _ ->
  assert_equal expected
    (Recipe.Filter.find_common cuisine meal restrictions ingredient_category
       category)

let find_common_tests =
  [
    make_find_common_test "none in common - FAIL" [ "FAIL" ] "German" "dinner"
      [] "proteins" "seafoods";
    make_find_common_test "many in common" [ "salmon"; "mussels" ] "Argentinian"
      "dinner" [] "proteins" "seafoods";
    make_find_common_test "unique" [ "mussels" ] "Spanish" "dinner" []
      "proteins" "seafoods";
    make_find_common_test "all in common"
      [ "salmon"; "pufferfish"; "mussels" ]
      "Japanese" "dinner" [] "proteins" "seafoods";
  ]

(* GET INGREDIENTS TESTS *)
let make_get_ingredients_test test_name expected_length cuisine meal
    restrictions add_secret_ingredient =
  test_name >:: fun _ ->
  assert_equal expected_length
    (List.length
       (Recipe.Filter.get_ingredients cuisine meal restrictions
          add_secret_ingredient))

let get_ingredients_test =
  [
    (* cannot test contents of this method due to random selection of values *)
    make_get_ingredients_test "length of 6 with secret ingredient" 6 "American"
      "lunch" [] true;
    make_get_ingredients_test
      "length of 6 with secret ingredient different cuisine" 6 "Chinese" "lunch"
      [] true;
    make_get_ingredients_test
      "length of 6 with secret ingredient different meal" 6 "American"
      "breakfast" [] true;
    make_get_ingredients_test "length of 5 without secret ingredient" 5
      "American" "lunch" [] false;
    make_get_ingredients_test "has 1 restriction" 6 "American" "lunch"
      [ "meat" ] true;
    make_get_ingredients_test "has restrictions" 6 "American" "lunch"
      [ "meat"; "seafood"; "non-vegan" ]
      true;
  ]

(* GET FLAVOR TESTS *)
let make_get_flavor_test test_name expected cuisine ingredient =
  test_name >:: fun _ ->
  assert_equal expected
    (Recipe.Filter.get_flavor
       (Yojson.Basic.from_file "data/flavors.json")
       cuisine ingredient)

let get_flavor_tests =
  [
    make_get_flavor_test "American sauces"
      [ "BBQ sauce"; "hot sauce"; "mustard"; "mayo"; "ketchup"; "ranch" ]
      "American" "sauces";
    make_get_flavor_test "American spices"
      [ "parsley"; "dried onion"; "thyme"; "dried dill" ]
      "American" "spices";
    make_get_flavor_test "empty" [] "American" "s";
    make_get_flavor_test "Japanese sauces"
      [
        "ponzu sauce";
        "teriyaki sauce";
        "miso sauce";
        "tonkatsu sauce";
        "unagi sauce";
        "okonomiyaki sauce";
        "yakisoba sauce";
        "soy sauce";
      ]
      "Japanese" "sauces";
    make_get_flavor_test "Italian spices"
      [
        "herbes de provence";
        "basil";
        "oregano";
        "thyme";
        "rosemary";
        "sage";
        "bay leaf";
        "parsley";
        "majoram";
        "parmesan cheese";
      ]
      "Italian" "spices";
  ]

(* SECRET SAUCE OR SPICE TESTS *)
let make_secret_sauce_or_spice_test test_name cuisine =
  test_name >:: fun _ ->
  assert_equal true
    (List.length [ Recipe.Filter.secret_sauce_or_spice cuisine ] > 0)

let get_secret_sauce_or_spice_test =
  [
    (* cannot test return value due to random selection of values *)
    make_secret_sauce_or_spice_test "test non-empty ssos" "American";
    make_secret_sauce_or_spice_test "test non-empty ssos" "Argentinian";
  ]

let suite =
  "recipe test suite"
  >::: List.flatten
         [
           list_from_string_tests;
           list_from_list_tests;
           split_list_tests;
           tuple_tests;
           apply_filter_tests;
           remove_restrictions_tests;
           names_from_tuple_tests;
           find_common_tests;
           get_ingredients_test;
           get_flavor_tests;
           get_secret_sauce_or_spice_test;
         ]

let () = run_test_tt_main suite
