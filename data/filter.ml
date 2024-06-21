open Yojson.Basic.Util

(** Makes a string list directly from ingredients.json

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [name]: string representing key to retrieve,
    value must be a string. Example: "name": "steak" allowed *)
let make_list_from_string json ingredient_category category name : string list =
  [ json ]
  |> filter_member ingredient_category
  |> flatten |> filter_member category |> flatten |> filter_member name
  |> filter_string

(** Makes a string list directly from ingredients.json

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [list_name]: string representing key to retrieve,
    value must be a list Example: "time": ["lunch", "dinner"] allowed *)
let make_list_from_list json ingredient_category category list_name :
    string list =
  [ json ]
  |> filter_member ingredient_category
  |> flatten |> filter_member category |> flatten |> filter_member list_name
  |> flatten |> filter_string

(** Given a string list representing cuisines, this method splits the list and
    returns a tuple of string lists:
    ([all elements until the first break in JSON], [all elements after])

    [cuisine_list]: list of all cuisines from the JSON file given by
    make_list_from_list *)
let rec split_list list : string list * string list =
  match list with
  | [] -> ([], [])
  | h :: t ->
      if h = "break" then ([], t)
      else
        let current, remaining = split_list t in
        (h :: current, remaining)

(** Makes a (string * string list) list which represents:
    [(name, list_name); ...] for all ingredients of a specified category

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [name]: string representing key to retrieve,
    value must be a string. Example: "name": "steak" allowed [list_name]: string
    representing key to retrieve, value must be a list Example: "time":
    ["lunch", "dinner"] allowed *)
let make_tuple ingredient_category category name list_name :
    (string * string list) list =
  let json = Yojson.Basic.from_file "data/ingredients.json" in
  let names = make_list_from_string json ingredient_category category name in
  let cuisines =
    make_list_from_list json ingredient_category category list_name
  in
  let rec make_tuple_name_helper name_list cuisine_list =
    match name_list with
    | [] -> []
    | h :: t ->
        let current, remaining_cuisine_list = split_list cuisine_list in
        (h, current) :: make_tuple_name_helper t remaining_cuisine_list
  in
  make_tuple_name_helper names cuisines

(** Checks if the elements of list include a string filter, and if they do not,
    they are removed from the list

    [list]: a list of tuples containing a string and a list. Represents all
    ingredient names and a certain list such as cuisine or time associated with
    a category Example:
    [("steak", ["american", "argentinian"]); ("salmon", ["american";
    "japanese"])]
    [filter]: the string that this method looks for in [list] *)
let rec apply_filter list filter =
  match list with
  | [] -> []
  | (i, l) :: t -> (
      match List.find_opt (fun x -> x = filter) l with
      | None -> apply_filter t filter
      | Some y -> (i, l) :: apply_filter t filter)

(** Checks if the elements of list include a string filter, and if they do, they
    are removed from the list

    [list]: a list of tuples containing a string and a list. Represents all
    ingredient names and the restrictions associated with a category [filter]:
    the string that this method looks for in [list] *)
let rec remove_restrictions list filter =
  match list with
  | [] -> []
  | (i, l) :: t -> (
      match List.find_opt (fun x -> x = filter) l with
      | None -> (i, l) :: remove_restrictions t filter
      | Some y -> remove_restrictions t filter)

(** Gets all the strings from a tuple list (type (string * string list) list)*)
let rec get_names_from_tuple tuple_list =
  match tuple_list with
  | [] -> []
  | h :: t ->
      let name, list_name = h in
      name :: get_names_from_tuple t

(** Finds and returns a list of all elements in common between two lists. Helper
    method for find_common. *)
let rec find_common_helper list1 list2 =
  match (list1, list2) with
  | [], _ | _, [] -> []
  | h :: t, l2 ->
      if List.mem h l2 then h :: find_common_helper t l2
      else find_common_helper t l2

(** Finds all ingredients that have the same cuisine and meal as specified in
    the parameters and returns them as a string list.

    [cuisine]: string representing chosen cuisine. First letter must be
    capitalized [meal]: string representing meal: breakfast, lunch, dinner *)
let find_common cuisine meal (restrictions : string list) ingredient_category
    category : string list =
  let cuisine_tuple =
    make_tuple ingredient_category category "name" "cuisine"
  in
  let meal_tuple = make_tuple ingredient_category category "name" "meal" in
  let restriction_tuple =
    make_tuple ingredient_category category "name" "restrictions"
  in
  let cuisine_list =
    get_names_from_tuple (apply_filter cuisine_tuple cuisine)
  in
  let meal_list = get_names_from_tuple (apply_filter meal_tuple meal) in
  let rec remove list r =
    match r with
    | [] -> list
    | h :: t -> remove (remove_restrictions list h) t
  in
  let restriction_list =
    get_names_from_tuple (remove restriction_tuple restrictions)
  in
  let ingredients_list =
    find_common_helper
      (find_common_helper cuisine_list meal_list)
      restriction_list
  in
  if List.length ingredients_list = 0 then [ "FAIL" ] else ingredients_list

(** Counts the number of "FAILS" in a string array. Helper method to
    get_ingredients_helper.

    [list]: string list of potential ingredients to select, including FAILs *)
let rec count_fails list =
  match list with
  | [] -> 0
  | h :: t -> if h = "FAIL" then 1 + count_fails t else count_fails t

(** Removes all FAILs from a string array. Helper method to
    get_ingredients_helper.

    [list]: string list of potential ingredients to select, including FAILs *)
let rec remove_fails list =
  match list with
  | [] -> []
  | h :: t -> if h = "FAIL" then remove_fails t else h :: remove_fails t

(** A method to call find_common repeatedly with each combination of ingredient
    categories and categories.

    [cuisine]: string representing cuisine, passed in from get_ingredients
    [meal]: string representing meal of day, passed in from get_ingredients
    [arguments]: a string list list representing the ingredient categories and
    categories to process *)
let rec get_ingredients_helper cuisine meal restriction arguments : string list
    =
  match arguments with
  | [] -> []
  | h :: t ->
      let ingredient_category = List.hd h in
      let category = List.nth h 1 in
      let ingredients_list =
        find_common cuisine meal restriction ingredient_category category
        @ get_ingredients_helper cuisine meal restriction t
      in
      if count_fails ingredients_list > 0 then
        if count_fails ingredients_list = List.length ingredients_list then
          [ "FAIL" ]
        else remove_fails ingredients_list
      else ingredients_list

(** List of all protein categories *)
let protein_categories_list =
  [
    [ "proteins"; "seafoods" ];
    [ "proteins"; "red meats" ];
    [ "proteins"; "poultry" ];
    [ "proteins"; "vegetarian" ];
  ]

(** List of all vegetable categories *)
let vegetable_categories_list =
  [
    [ "vegetables"; "cruciferous" ];
    [ "vegetables"; "leafy greens" ];
    [ "vegetables"; "non-starchy vegetables" ];
    [ "vegetables"; "starchy vegetables" ];
  ]

(** List of all carb categories *)
let carb_categories_list =
  [
    [ "carbs"; "root vegetables" ]; [ "carbs"; "grains" ]; [ "carbs"; "pantry" ];
  ]

(** Select a random ingredient

    [ingredient_list]: a string list representing possible ingredients to use *)
let select_random_ingredient (ingredient_list : string list) =
  if List.length ingredient_list = 0 then "FAIL"
  else List.nth ingredient_list (Random.int (List.length ingredient_list))

(** Returns a string list of all secret ingredients from the JSON file *)
let get_all_secret_ingredients json : string list =
  [ json ]
  |> filter_member "secret_ingredients"
  |> flatten |> filter_member "name" |> filter_string

(** List of all flavor categories, used for getting sauces and spices *)
let flavor_categories =
  [
    [ "American"; "sauces" ];
    [ "American"; "spices" ];
    [ "Chinese"; "sauces" ];
    [ "Chinese"; "spices" ];
    [ "Argentinian"; "sauces" ];
    [ "Argentinian"; "spices" ];
    [ "Mediterranean"; "sauces" ];
    [ "Mediterranean"; "spices" ];
    [ "Japanese"; "sauces" ];
    [ "Japanese"; "spices" ];
    [ "Spanish"; "sauces" ];
    [ "Spanish"; "spices" ];
    [ "French"; "sauces" ];
    [ "French"; "spices" ];
    [ "Indian"; "sauces" ];
    [ "Indian"; "spices" ];
    [ "Italian"; "sauces" ];
    [ "Italian"; "spices" ];
    [ "Korean"; "sauces" ];
    [ "Korean"; "spices" ];
    [ "Middle Eastern"; "sauces" ];
    [ "Middle Eastern"; "spices" ];
  ]

(** Makes a string list directly from the given JSON file

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [cuisine]: string
    representing the selected cuisine [ingredient]: string representing the
    selected ingredient, either "spices" or "sauces" *)
let get_flavor json cuisine ingredient : string list =
  [ json ] |> filter_member cuisine |> flatten |> filter_member ingredient
  |> flatten |> filter_string

(** Processes sauces and spices and ignores the selected cuisine. Helper method
    for secret_sauce_or_spice.

    [json]: passes in the JSON to process [ignore_cuisine]: string, the cuisine
    to ignore [arguments]: a string list list which stores all of the categories
    for sauces and spices *)
let rec secret_sauce_or_spice_helper json ignore_cuisine arguments =
  match arguments with
  | [] -> []
  | h :: t ->
      let current_cuisine = List.hd h in
      let current_ingredient = List.nth h 1 in
      if current_cuisine = ignore_cuisine then
        [] @ secret_sauce_or_spice_helper json ignore_cuisine t
      else
        get_flavor json current_cuisine current_ingredient
        @ secret_sauce_or_spice_helper json ignore_cuisine t

(** Processes sauce, spice, and secret ingredient options to generate a random
    secret ingredient

    [selected_cuisine]: string, represents the cuisine from which to ignore
    sauces and spices for secret ingredient*)
let secret_sauce_or_spice selected_cuisine : string =
  let flavors_json = Yojson.Basic.from_file "data/flavors.json" in
  let ingredients_json = Yojson.Basic.from_file "data/ingredients.json" in
  let secret_flavor =
    secret_sauce_or_spice_helper flavors_json selected_cuisine flavor_categories
  in
  List.nth
    (secret_flavor @ get_all_secret_ingredients ingredients_json)
    (Random.int
       (List.length
          (secret_flavor @ get_all_secret_ingredients ingredients_json)))

(* Gets the ingredients, and generates the recipe. Returns a string list
   representing all of the ingredients in the recipe.

   [cuisine]: string representing the selected cuisine [meal]: string
   representing the meal of the day (breakfast, lunch, or dinner)
   [restrictions]: a string list representing all of the selected restrictions
   to include [add_secret_ingredient]: bool representing whether the user wants
   a secret ingredient or not *)
let get_ingredients (cuisine : string) (meal : string)
    (restrictions : string list) (add_secret_ingredient : bool) =
  let all_matching_proteins =
    select_random_ingredient
      (get_ingredients_helper cuisine meal restrictions protein_categories_list)
  in
  let all_matching_vegetables =
    select_random_ingredient
      (get_ingredients_helper cuisine meal restrictions
         vegetable_categories_list)
  in
  let all_matching_carbs =
    select_random_ingredient
      (get_ingredients_helper cuisine meal restrictions carb_categories_list)
  in
  let flavors_json = Yojson.Basic.from_file "data/flavors.json" in
  let sauce_and_spice =
    [
      select_random_ingredient (get_flavor flavors_json cuisine "sauces");
      select_random_ingredient (get_flavor flavors_json cuisine "spices");
    ]
  in
  if add_secret_ingredient then
    let secret_ingredient = secret_sauce_or_spice cuisine in
    [ all_matching_proteins; all_matching_vegetables; all_matching_carbs ]
    @ sauce_and_spice @ [ secret_ingredient ]
  else
    [ all_matching_proteins; all_matching_vegetables; all_matching_carbs ]
    @ sauce_and_spice

(** Pretty prints the ingredients for the GUI given the list of ingredients.
    Called by the GUI.

    [ingredient_lst]: string list representing the list of ingredients *)
let print_ingredients ingredient_lst =
  if List.length ingredient_lst = 5 then
    if List.mem "FAIL" ingredient_lst then "Recipe not generated"
    else
      List.nth ingredient_lst 0 ^ " with a side of " ^ List.nth ingredient_lst 1
      ^ " and " ^ List.nth ingredient_lst 2 ^ " flavored with " ^ "\n"
      ^ List.nth ingredient_lst 3 ^ " and spiced with "
      ^ List.nth ingredient_lst 4 ^ "!"
  else if List.mem "FAIL" ingredient_lst then "Recipe not generated"
  else
    List.nth ingredient_lst 0 ^ " with a side of " ^ List.nth ingredient_lst 1
    ^ " and " ^ List.nth ingredient_lst 2 ^ " flavored with " ^ "\n "
    ^ List.nth ingredient_lst 3 ^ " and spiced with "
    ^ List.nth ingredient_lst 4 ^ " and with a secret ingredient of "
    ^ List.nth ingredient_lst 5 ^ "!"
