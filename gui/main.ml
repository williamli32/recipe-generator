open Bogue
open Str
open Recipe.Filter
module W = Widget
module L = Layout

let main () =
  let label_1 = W.label "Chosen cuisine" in
  let label_1' = W.label ~size:60 "                                       " in
  let lst_1 =
    Select.create
      ~action:(fun index ->
        Label.set (W.get_label label_1')
          (List.nth
             [
               "NA";
               "American";
               "Chinese";
               "Argentinian";
               "Mediterranean";
               "Japanese";
               "Spanish";
               "French";
               "Indian";
               "Italian";
               "Korean";
               "Middle eastern";
             ]
             index))
      [|
        "NA";
        "American";
        "Chinese";
        "Argentinian";
        "Mediterranean";
        "Japanese";
        "Spanish";
        "French";
        "Indian";
        "Italian";
        "Korean";
        "Middle eastern";
      |]
      0
  in
  let confirm_1 = W.button ~border_radius:10 "Confirm" in
  let layout_1 =
    L.tower ~sep:10 ~align:Draw.Center
      [ L.resident label_1; L.resident label_1'; lst_1; L.resident confirm_1 ]
  in

  let label_2 = W.label "What meal?" in
  let label_2' = W.label ~size:60 "                                       " in
  let lst_2 =
    Select.create
      ~action:(fun index ->
        Label.set (W.get_label label_2')
          (List.nth [ "NA"; "breakfast"; "lunch"; "dinner" ] index))
      [| "NA"; "breakfast"; "lunch"; "dinner" |]
      0
  in
  let confirm_2 = W.button ~border_radius:10 "Confirm" in
  let layout_2 =
    L.tower ~sep:10 ~align:Draw.Center
      [ L.resident label_2; L.resident label_2'; lst_2; L.resident confirm_2 ]
  in

  let label_3 = W.label "Any restrictions?" in
  let label_3' = W.text_display ~w:525 ~h:20 "" in
  let confirm_3 = W.button ~border_radius:10 "Confirm" in
  let filter_box_1 = W.check_box () in
  let filter_name_1 = W.label "vegetarian" in
  let filter_box_2 = W.check_box () in
  let filter_name_2 = W.label "no-beef" in
  let filter_box_3 = W.check_box () in
  let filter_name_3 = W.label "no-pork" in
  let filter_box_4 = W.check_box () in
  let filter_name_4 = W.label "gluten-free" in
  let filter_box_5 = W.check_box () in
  let filter_name_5 = W.label "seafood-allergy" in
  let filter_box_6 = W.check_box () in
  let filter_name_6 = W.label "lactose-intolerant" in
  let filter_box_7 = W.check_box () in
  let filter_name_7 = W.label "vegan" in

  let restriction_1 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_1; filter_name_1 ]
  in
  let restriction_2 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_2; filter_name_2 ]
  in
  let restriction_3 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_3; filter_name_3 ]
  in
  let restriction_4 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_4; filter_name_4 ]
  in
  let restriction_5 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_5; filter_name_5 ]
  in
  let restriction_6 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_6; filter_name_6 ]
  in
  let restriction_7 =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ filter_box_7; filter_name_7 ]
  in
  let layout_3 =
    L.tower ~sep:10 ~align:Draw.Center
      [
        L.resident label_3;
        L.resident label_3';
        restriction_1;
        restriction_2;
        restriction_3;
        restriction_4;
        restriction_5;
        restriction_6;
        restriction_7;
        L.resident confirm_3;
      ]
  in

  (* Remove the newline chracter from the input [str]. *)
  let remove_newline str = global_replace (regexp "\n") "" str in

  (* Remove the [substring] from the input [str]. *)
  let remove_substring str substring =
    let regex = Str.regexp_string substring in
    Str.global_replace regex "" str
  in

  (* An action function that check whether the check box of vegeterian
     restriction is checked. *)
  let on_check_1 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_1) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "vegetarian " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "vegetarian ");
      W.update label;
      fps ()
  in
  let connect_3_1 =
    W.connect filter_box_1 label_3' on_check_1 Trigger.buttons_down
  in
  (* An action function that check whether the check box of no-beef restriction
     is checked. *)
  let on_check_2 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_2) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "no-beef " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "no-beef ");
      W.update label;
      fps ()
  in
  let connect_3_2 =
    W.connect filter_box_2 label_3' on_check_2 Trigger.buttons_down
  in
  (* An action function that check whether the check box of no-pork restriction
     is checked. *)
  let on_check_3 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_3) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "no-pork " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "no-pork ");
      W.update label;
      fps ()
  in
  let connect_3_3 =
    W.connect filter_box_3 label_3' on_check_3 Trigger.buttons_down
  in
  (* An action function that check whether the check box of gluten-free
     restriction is checked. *)
  let on_check_4 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_4) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "gluten-free " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "gluten-free ");
      W.update label;
      fps ()
  in
  let connect_3_4 =
    W.connect filter_box_4 label_3' on_check_4 Trigger.buttons_down
  in
  (* An action function that check whether the check box of seafood-allergy
     restriction is checked. *)
  let on_check_5 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_5) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "seafood-allergy " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "seafood-allergy ");
      W.update label;
      fps ()
  in
  let connect_3_5 =
    W.connect filter_box_5 label_3' on_check_5 Trigger.buttons_down
  in

  (* An action function that check whether the check box of vegan restriction is
     checked. *)
  let on_check_7 box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check filter_box_7) in
    let label' = W.get_text_display label in
    if b then (
      let text = W.get_text label ^ "vegan " in
      Text_display.update_verbatim label' (remove_newline text);
      W.update label;
      fps ())
    else
      let text = W.get_text label in
      Text_display.update_verbatim label'
        (remove_substring (remove_newline text) "vegan ");
      W.update label;
      fps ()
  in
  let connect_3_7 =
    W.connect filter_box_7 label_3' on_check_7 Trigger.buttons_down
  in

  let label_final_1 = W.label "Cuisine:" in
  let cuisine_chosen = W.label "                               " in
  let label_final_2 = W.label "Meal:" in
  let meal_chosen = W.label "                               " in
  let label_final_3 = W.label "Restriction(s):" in
  let restriction_chosen = W.text_display ~w:550 ~h:20 "" in
  let label_final_4 = W.label "Special Ingedient:" in
  let specialing_chosen = W.label "false" in
  let box_specialing = W.check_box () in
  let label_specialing = W.label "Add special ingredient" in

  let special_ingredient =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ box_specialing; label_specialing ]
  in

  (* An action function that check whether the check box of special ingredient
     restriction is checked. *)
  let on_check_specialing box label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = Check.state (W.get_check box_specialing) in
    if b then (
      Label.set (W.get_label specialing_chosen) "true";
      W.update label;
      fps ())
    else (
      Label.set (W.get_label specialing_chosen) "false";
      W.update label;
      fps ())
  in
  let connect_specialing =
    W.connect box_specialing label_1' on_check_specialing Trigger.buttons_down
  in

  let rnd_button = W.button ~border_radius:10 "Generate" in
  let result = W.text_display ~w:600 ~h:50 "" in

  (* Replace restrictions displayed into strings recognized by our functions *)
  let change_restriction lst =
    let replace_element acc el =
      match el with
      | "vegetarian" -> [ "meat"; "seafood" ] @ acc
      | "no-beef" -> "beef" :: acc
      | "no-pork" -> "pork" :: acc
      | "gluten-free" -> "gluten" :: acc
      | "seafood-allergy" -> "seafood" :: acc
      | "vegan" -> [ "non-vegan"; "meat" ] @ acc
      | _ -> "" :: acc
    in
    List.fold_left replace_element [] lst |> List.rev
  in

  (* An action function that check whether the button of generate recipe is
     hit. *)
  let on_press button label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = W.get_button button in
    let food_list =
      Recipe.Filter.print_ingredients
        (Recipe.Filter.get_ingredients
           (W.get_text cuisine_chosen)
           (W.get_text meal_chosen)
           (change_restriction
              (Str.split (Str.regexp " ") (W.get_text restriction_chosen)))
           (bool_of_string (W.get_text specialing_chosen)))
    in
    if Button.is_pressed b then (
      Text_display.update_verbatim (W.get_text_display label) food_list;
      W.update label;
      fps ())
  in
  let connect_final =
    W.connect ~priority:W.Join rnd_button result on_press Trigger.buttons_down
  in
  (* An action function that check whether the button of confirm on page
     ingredient is hit. *)
  let on_confirm_1 button label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = W.get_button button in
    if Button.is_pressed b then (
      Label.set (W.get_label label) (W.get_text label_1');
      print_endline (W.get_text label);
      W.update label;
      fps ())
  in
  let connect_1 =
    W.connect ~priority:W.Join confirm_1 cuisine_chosen on_confirm_1
      Trigger.buttons_down
  in
  (* An action function that check whether the button of confirm on page meal is
     hit. *)
  let on_confirm_2 button label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = W.get_button button in
    if Button.is_pressed b then (
      Label.set (W.get_label label) (W.get_text label_2');
      print_endline (W.get_text label);
      W.update label;
      fps ())
  in
  let connect_2 =
    W.connect ~priority:W.Join confirm_2 meal_chosen on_confirm_2
      Trigger.buttons_down
  in

  let remove_last_character s =
    let len = String.length s in
    if len <= 1 then "" else String.sub s 0 (len - 1)
  in
  (* An action function that check whether the button of confirm on page
     restriction is hit. *)
  let on_confirm_3 button label _ev =
    let start, fps = Time.adaptive_fps 60 in
    let () = start () in
    let b = W.get_button button in
    if Button.is_pressed b then (
      Text_display.update_verbatim (W.get_text_display label)
        (remove_last_character (W.get_text label_3'));
      print_endline (W.get_text label);
      W.update label;
      fps ())
  in
  let connect_3 =
    W.connect ~priority:W.Join confirm_3 restriction_chosen on_confirm_3
      Trigger.buttons_down
  in
  let chosen_cuisine =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ label_final_1; cuisine_chosen ]
  in
  let chosen_meal =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ label_final_2; meal_chosen ]
  in
  let chosen_restriction =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ label_final_3; restriction_chosen ]
  in
  let chosen_specialing =
    L.flat_of_w ~align:Draw.Center
      ~background:(L.color_bg Draw.(transp grey))
      [ label_final_4; specialing_chosen ]
  in
  let rl = L.resident ~background:(L.color_bg Draw.(transp white)) result in
  let bl = L.resident rnd_button in
  let layout =
    L.tower ~sep:10 ~align:Draw.Center
      [
        chosen_cuisine;
        chosen_meal;
        chosen_restriction;
        chosen_specialing;
        special_ingredient;
        rl;
        bl;
      ]
  in

  let tab =
    Tabs.create ~adjust:Width ~expand:false
      [
        ("             Cuisine             ", layout_1);
        ("             Meal             ", layout_2);
        ("             Restrictions             ", layout_3);
        ("        Final        ", layout);
      ]
  in
  let display =
    Main.of_layout
      ~connections:
        [
          connect_final;
          connect_3_1;
          connect_3_2;
          connect_3_3;
          connect_3_4;
          connect_3_5;
          connect_3_7;
          connect_1;
          connect_2;
          connect_3;
          connect_specialing;
        ]
      tab
  in
  Random.self_init ();
  Main.run display

let () =
  main ();
  Bogue.quit ()
