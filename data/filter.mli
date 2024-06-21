val make_list_from_string :
  Yojson.Basic.t -> string -> string -> string -> string list
(** Makes a string list directly from ingredients.json

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [name]: string representing key to retrieve,
    value must be a string. Example: "name": "steak" allowed *)

val make_list_from_list :
  Yojson.Basic.t -> string -> string -> string -> string list
(** Makes a string list directly from ingredients.json

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [list_name]: string representing key to retrieve,
    value must be a list Example: "time": ["lunch", "dinner"] allowed *)

val split_list : string list -> string list * string list
(** Given a string list representing cuisines, this method splits the list and
    returns a tuple of string lists:
    ([all elements until the first break in JSON], [all elements after])

    [cuisine_list]: list of all cuisines from the JSON file given by
    make_list_from_list *)

val make_tuple :
  string -> string -> string -> string -> (string * string list) list
(** Makes a (string * string list) list which represents:
    [(name, list_name); ...] for all ingredients of a specified category

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [category]: string
    representing category of ingredient_category. Example: seafood, red_meat,
    poultry, etc. for proteins [name]: string representing key to retrieve,
    value must be a string. Example: "name": "steak" allowed [list_name]: string
    representing key to retrieve, value must be a list Example: "time":
    ["lunch", "dinner"] allowed *)

val apply_filter : ('a * 'b list) list -> 'b -> ('a * 'b list) list
(** Checks if the elements of list include a string filter, and if they do not,
    they are removed from the list

    [list]: a list of tuples containing a string and a list. Represents all
    ingredient names and a certain list such as cuisine or time associated with
    a category Example:
    [("steak", ["american", "argentinian"]); ("salmon", ["american";
    "japanese"])]
    [filter]: the string that this method looks for in [list] *)

val remove_restrictions : ('a * 'b list) list -> 'b -> ('a * 'b list) list
(** Checks if the elements of list include a string filter, and if they do, they
    are removed from the list

    [list]: a list of tuples containing a string and a list. Represents all
    ingredient names and the restrictions associated with a category [filter]:
    the string that this method looks for in [list] *)

val get_names_from_tuple : ('a * 'b) list -> 'a list
(** Gets all the strings from a tuple list (type (string * string list) list)*)

val find_common :
  string -> string -> string list -> string -> string -> string list
(** Finds all ingredients that have the same cuisine and meal as specified in
    the parameters and returns them as a string list.

    [cuisine]: string representing chosen cuisine. First letter must be
    capitalized [meal]: string representing meal: breakfast, lunch, dinner *)

val select_random_ingredient : string list -> string
(** Select a random ingredient

    [ingredient_list]: a string list representing possible ingredients to use *)

val get_all_secret_ingredients : Yojson.Basic.t -> string list
(** Returns a string list of all secret ingredients from the JSON file *)

val get_flavor : Yojson.Basic.t -> string -> string -> string list
(** Makes a string list directly from the given JSON file

    [json]: Yojson.Basic.t, passes in the JSON to process [ingredient_category]:
    string, category of ingredient: protein, veg, etc [cuisine]: string
    representing the selected cuisine [ingredient]: string representing the
    selected ingredient, either "spices" or "sauces" *)

val secret_sauce_or_spice : string -> string
(** Processes sauce, spice, and secret ingredient options to generate a random
    secret ingredient

    [selected_cuisine]: string, represents the cuisine from which to ignore
    sauces and spices for secret ingredient*)

val get_ingredients : string -> string -> string list -> bool -> string list
(** Gets the ingredients, and generates the recipe. Returns a string list
    representing all of the ingredients in the recipe.

    [cuisine]: string representing the selected cuisine [meal]: string
    representing the meal of the day (breakfast, lunch, or dinner)
    [restrictions]: a string list representing all of the selected restrictions
    to include [add_secret_ingredient]: bool representing whether the user wants
    a secret ingredient or not *)

val print_ingredients : string list -> string
(** Pretty prints the ingredients for the GUI given the list of ingredients.
    Called by the GUI.

    [ingredient_lst]: string list representing the list of ingredients *)
