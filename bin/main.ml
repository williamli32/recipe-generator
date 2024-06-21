open Yojson.Basic.Util
open Recipe.Filter

(* yojson code adapted from online documentation sources:

   examples: https://github.com/ocaml-community/yojson/tree/master/examples
   types and understanding:
   https://ocaml-community.github.io/yojson/yojson/Yojson/index.html methods and
   uses: https://mjambon.github.io/mjambon2016/yojson-doc/Yojson.Basic.Util.html
   examples and explanation:
   https://ocaml-community.github.io/yojson/yojson/Yojson/Basic/Util/index.html *)

let access_json json =
  [ json ] |> filter_member "proteins" |> flatten |> filter_member "red_meats"
  |> flatten |> filter_member "name" |> filter_string

(* Displays and processes user input for the different cuisines. Is recursive to
   allow for mistakes such as typos, and the list command.

   Can refactor to make dynamic with respect to a list of cuisines to make
   maintenance easier.*)
let rec display_cuisines (input : string) : string =
  let json = Yojson.Basic.from_file "data/ingredients.json" in
  List.iter print_endline (access_json json);
  match input with
  | "american" ->
      print_endline "Generating an American recipe...";
      "American"
  | "chinese" ->
      print_endline "Generating a Chinese recipe...";
      "Chinese"
  | "argentinian" ->
      print_endline "Generating an Argentinian recipe...";
      "Argentinian"
  | "french" ->
      print_endline "Generating a French recipe...";
      "French"
  | "japanese" ->
      print_endline "Generating a Japanese recipe...";
      "Japanese"
  | "italian" ->
      print_endline "Generating an Italian recipe...";
      "Italian"
  | "mediterranean" ->
      print_endline "Generating a Mediterranean recipe...";
      "Mediterranean"
  | "spanish" ->
      print_endline "Generating a Spanish recipe...";
      "Spanish"
  | "indian" ->
      print_endline "Generating an Indian recipe...";
      "Indian"
  | "korean" ->
      print_endline "Generating a Korean recipe...";
      "Korean"
  | "middle eastern" ->
      print_endline "Generating a Middle Eastern recipe...";
      "Middle Eastern"
  | "list" ->
      print_endline
        "Valid cuisines are:\n\n\
         american\n\
         chinese\n\
         french\n\
         japanese\n\
         italian\n\
         mediterranean\n\
         spanish\n\
         indian\n\
         korean\n\
         middle eastern";
      print_string "> ";
      display_cuisines (read_line ())
  | _ ->
      print_endline
        "Please enter a valid cuisine. Type \"list\" to see a list of options.";
      print_string "> ";
      display_cuisines (read_line ())

(* Displays and processes any user-inputted dietary restrictions, and modifies
   the recipe accordingly.

   Modifying recipe functionality currently not implemented *)
let rec display_restrictions (input : string) (list : string list) : string list
    =
  match input with
  | "n" ->
      print_string "";
      []
  | "vegetarian" -> "meat" :: "seafood" :: list
  | "no beef" -> "beef" :: list
  | "no pork" -> "pork" :: list
  | "gluten free" -> "gluten" :: list
  | "seafood allergy" -> "seafood" :: list
  | "vegan" -> "non-vegan" :: "meat" :: list
  | "list" ->
      print_endline
        "Valid options are:\n\
         vegetarian\n\
         no beef\n\
         no pork\n\
         gluten free\n\
         seafood allergy\n\
         vegan";
      print_string "> ";
      display_restrictions (read_line ()) list
  | _ ->
      print_endline
        "Option is not known. Type \"list\" to see a list of options.";
      print_string "> ";
      display_restrictions (read_line ()) list

(* Displays and processes the user-inputted meal: breakfast, lunch, or dinner.

   Modifying recipe functionality currently not implemented *)
let rec display_meal (input : string) : string =
  match input with
  | "b" | "breakfast" ->
      print_endline "Generating a breakfast recipe...";
      "breakfast"
  | "l" | "lunch" ->
      print_endline "Generating a lunch recipe...";
      "lunch"
  | "d" | "dinner" ->
      print_endline "Generating a dinner recipe...";
      "dinner"
  | _ ->
      print_endline "Please enter either breakfast, lunch, or dinner.";
      print_string "> ";
      display_meal (read_line ())

(* Displays and processes user input about the secret ingredient.

   Modifying recipe functionality currently not implemented *)
let rec display_secret_ingredient (input : string) : bool =
  match input with
  | "y" ->
      print_endline "Throwing in a secret ingredient...";
      true
  | "n" ->
      print_string "";
      false
  | "help" ->
      print_endline
        "A secret ingredient is an additional ingredient that may add a small \
         surprise...";
      print_string "> ";
      display_secret_ingredient (read_line ())
  | _ ->
      print_endline
        "Please type y, n, or help for an explanation of secret ingredients.";
      display_secret_ingredient input

let print_meal_no_secret list =
  match list with
  | [ protein; vegetable; carb; sauce; spice ] ->
      print_endline
        (protein ^ " with a side of " ^ vegetable ^ " and " ^ carb
       ^ " flavored with " ^ sauce ^ " and spiced with " ^ spice ^ "!")
  | _ -> print_endline ""

let print_meal_with_secret list =
  match list with
  | [ protein; vegetable; carb; sauce; spice; secret ] ->
      print_endline
        (protein ^ " with a side of " ^ vegetable ^ " and " ^ carb
       ^ " flavored with " ^ sauce ^ ", spiced with " ^ spice
       ^ " and with a secret ingredient of " ^ secret ^ "!")
  | _ -> print_endline ""

(* Displays the main CLI loop for the program *)
let rec display_main () =
  print_string "> ";
  match read_line () with
  | "y" ->
      print_endline
        "What cuisine would you like? Type \"list\" to see a list of cuisine \
         options";
      print_string "> ";
      let cuisine = display_cuisines (read_line ()) in

      print_endline "Is this for breakfast, lunch, or dinner?";
      print_string "> ";
      let meal = display_meal (read_line ()) in

      print_endline
        "Do you have any dietary restrictions? Type \"n\" for no, \"list\" to \
         see a list.";
      print_string "> ";
      let rec handle_restrictions input =
        let restrictions = display_restrictions (read_line ()) input in
        if restrictions = [] then restrictions
        else (
          print_endline
            "Do you have any other restrictions? Type \"n\" for no, \"y\" for \
             yes";
          print_string "> ";
          if read_line () = "y" then (
            print_endline
              "Please enter additional restrictions. Type \"list\" to see a \
               list";
            print_string "> ";
            handle_restrictions restrictions)
          else restrictions)
      in
      let restrictions = handle_restrictions [] in

      print_endline "Would you like a secret ingredient? (y/n/help)";
      print_string "> ";
      let secret_ingredient = display_secret_ingredient (read_line ()) in
      if secret_ingredient then
        print_meal_with_secret
          (Recipe.Filter.get_ingredients cuisine meal restrictions
             secret_ingredient)
      else
        print_meal_no_secret
          (Recipe.Filter.get_ingredients cuisine meal restrictions
             secret_ingredient)
  | "n" -> print_endline "Goodbye."
  | _ ->
      print_endline "Please enter 'y' or 'n'.";
      display_main ()

(*********** command line interface ***********)
let () =
  print_endline "Welcome.";
  print_endline "Do you want a recipe? (y/n)";
  display_main ()
