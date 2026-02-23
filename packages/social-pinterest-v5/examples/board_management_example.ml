(* Example: Pinterest default-board resolution and posting.

   This package does not currently expose full board CRUD/listing APIs.
   Instead, post_single resolves a default board automatically, and
   get_default_board is available as a helper.
*)

open Social_pinterest_v5

module Pinterest = Make (Your_config)

let account_id = "your-account-id"
let access_token = "your-access-token"

let show_default_board () =
  Pinterest.get_default_board
    ~access_token
    (fun board_id -> Printf.printf "Default board id: %s\n" board_id)
    (fun err -> Printf.eprintf "Failed to resolve default board: %s\n" err)

let create_image_pin () =
  Pinterest.post_single
    ~account_id
    ~text:"Workspace inspiration for small teams"
    ~media_urls:[ "https://example.com/workspace.jpg" ]
    ~alt_texts:[ Some "Minimal desk setup with plants and natural light" ]
    (fun outcome ->
      match outcome with
      | Error_types.Success pin_id -> Printf.printf "Created pin: %s\n" pin_id
      | Error_types.Failure err ->
          Printf.eprintf "Post failed: %s\n" (Error_types.error_to_string err)
      | Error_types.Partial_success { successes; warnings } ->
          Printf.printf "Partial success (%d success entries)\n" (List.length successes);
          List.iter (fun w -> Printf.printf "warning: %s\n" w) warnings)

let () =
  show_default_board ();
  create_image_pin ()
