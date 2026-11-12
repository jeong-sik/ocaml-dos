type region = { x : int; y : int; w : int; h : int }

let bytes_per_pixel = 3

let region_digest m r =
  let fw, fh = Dos_machine.frame_dims m in
  if r.x < 0 || r.y < 0 || r.w <= 0 || r.h <= 0 || r.x + r.w > fw || r.y + r.h > fh
  then None
  else begin
    let rgb = Dos_machine.frame_rgb m in
    let row = r.w * bytes_per_pixel in
    let crop = Buffer.create (row * r.h) in
    for y = r.y to r.y + r.h - 1 do
      Buffer.add_substring crop rgb (((y * fw) + r.x) * bytes_per_pixel) row
    done;
    Some (Digest.to_hex (Digest.string (Buffer.contents crop)))
  end

type step =
  | Wait_for of { what : string; region : region; digest : string; budget : int }
  | Press of { what : string; keys : string list; budget : int }

(* The rhythm the masc DOS lane plays with (see test/zzt_run_test.ml): a
   game loop eats a key and asks again within hundreds of instructions,
   while the screen is still being drawn. Asking and a still screen
   together mean the game finished answering. *)
let chunk_steps = 50_000

type failure = { index : int; what : string; reason : string; machine_steps : int }

let failure_to_string f =
  Printf.sprintf "step %d (%s): %s after %d machine steps" f.index f.what f.reason
    f.machine_steps

type outcome = Done | Out_of_budget | Program_exited

(* Runs chunks until [finished] holds, at most [budget] machine steps.
   [finished] is asked before the first chunk and after each one, with
   whether the guest asked for a key during that chunk. *)
let run_chunks m ~spent ~budget ~finished =
  let rec loop used =
    if Dos_machine.exited m then Program_exited
    else if used >= budget then Out_of_budget
    else begin
      let before = Dos_machine.input_requests m in
      let report =
        Dos_machine.run_until m ~max_steps:(min chunk_steps (budget - used))
          ~stop:(fun _ -> false)
      in
      spent := !spent + report.machine_steps;
      if finished ~asked:(Dos_machine.input_requests m > before) then Done
      else loop (used + report.machine_steps)
    end
  in
  if finished ~asked:false then Done else loop 0

let wait_for m ~spent ~region ~digest ~budget =
  run_chunks m ~spent ~budget ~finished:(fun ~asked:_ ->
      region_digest m region = Some digest)

let ring_empty m ~spent ~budget =
  run_chunks m ~spent ~budget ~finished:(fun ~asked:_ ->
      not (Dos_state.key_pending m))

let press m ~spent ~budget word =
  match ring_empty m ~spent ~budget with
  | (Out_of_budget | Program_exited) as o -> o
  | Done ->
    Dos_machine.push_key m word;
    let prev = ref (Dos_machine.screen_digest m) in
    let answered =
      run_chunks m ~spent ~budget ~finished:(fun ~asked ->
          let now = Dos_machine.screen_digest m in
          let settled = asked && now = !prev in
          prev := now;
          settled)
    in
    (* A key that quits the program is answered by the exit itself, once
       the program took the key. *)
    (match answered with
     | Program_exited when not (Dos_state.key_pending m) -> Done
     | o -> o)

let reason_of = function
  | Done -> None
  | Out_of_budget -> Some "budget ran out"
  | Program_exited -> Some "the program exited"

let run m steps =
  let spent = ref 0 in
  let fail index what reason = Error { index; what; reason; machine_steps = !spent } in
  let rec go index = function
    | [] -> Ok !spent
    | Wait_for { what; region; digest; budget } :: rest ->
      (match reason_of (wait_for m ~spent ~region ~digest ~budget) with
       | None -> go (index + 1) rest
       | Some reason ->
         let seen =
           Option.value (region_digest m region) ~default:"outside the frame"
         in
         fail index what (Printf.sprintf "%s; the region shows %s" reason seen))
    | Press { what; keys; budget } :: rest ->
      let rec keys_go = function
        | [] -> go (index + 1) rest
        | name :: more ->
          (match Dos_machine.key_of_string name with
           | Error e -> fail index what e
           | Ok word ->
             (match reason_of (press m ~spent ~budget word) with
              | None -> keys_go more
              | Some reason -> fail index what (Printf.sprintf "key %S: %s" name reason)))
      in
      keys_go keys
  in
  go 0 steps
