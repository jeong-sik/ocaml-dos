(* See host_tree.mli. *)

let max_depth = 32

let rec walk dir rel depth =
  if depth > max_depth then
    Error (Printf.sprintf "%s: folders nest deeper than %d levels" dir max_depth)
  else
    let step acc n =
      match acc with
      | Error _ -> acc
      | Ok found ->
        let path = Filename.concat dir n in
        let name = if rel = "" then n else rel ^ "/" ^ n in
        if not (Sys.file_exists path) then Ok found
        else if Sys.is_directory path then
          Result.map (fun below -> List.rev_append below found) (walk path name (depth + 1))
        else Ok ((name, path) :: found)
    in
    Sys.readdir dir |> Array.to_list |> List.sort compare
    |> List.fold_left step (Ok [])
    |> Result.map List.rev

let files dir = walk dir "" 0
