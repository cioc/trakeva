(**************************************************************************)
(*  Copyright 2014, Sebastien Mondet <seb@mondet.org>                     *)
(*                                                                        *)
(*  Licensed under the Apache License, Version 2.0 (the "License");       *)
(*  you may not use this file except in compliance with the License.      *)
(*  You may obtain a copy of the License at                               *)
(*                                                                        *)
(*      http://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(*  Unless required by applicable law or agreed to in writing, software   *)
(*  distributed under the License is distributed on an "AS IS" BASIS,     *)
(*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or       *)
(*  implied.  See the License for the specific language governing         *)
(*  permissions and limitations under the License.                        *)
(**************************************************************************)

open Nonstd
open Pvem_lwt_unix
open Pvem_lwt_unix.Deferred_result
module String = StringLabels
let (//) = Filename.concat

let say fmt = ksprintf (printf "%s\n%!") fmt

module Test = struct
  exception Tests_failed

  let max_failures = 
    try Sys.getenv "MAX_FAILURES" |> int_of_string with _ -> 2_000_000

  let failed_tests = ref []
  let fail s =
    failed_tests := s :: !failed_tests;
    if List.length !failed_tests > max_failures then (
      List.iter !failed_tests ~f:(fun t ->
          eprintf "Failed test: %S\n%!" t
        );
      raise Tests_failed 
    ) else ()

  let new_tmp_dir () =
    let db_file = Filename.temp_file  "trakeva_tmp_test" ".d" in
    Sys.command (sprintf "rm -rf %s" db_file) |> ignore;
    Sys.command (sprintf "mkdir -p %s" db_file) |> ignore;
    db_file

  let run_monad name f =
    Lwt_main.run (f ())
    |> function
    | `Ok () -> ()
    | `Error (`Database e) ->
      ksprintf fail "%S ends with error: %s" name
        (Trakeva_interface.Error.to_string e)

  let check names c =
    if c then ()
    else ksprintf fail "test.assert failed: %s" (String.concat ~sep:" → " names)
end

  
module type TEST_DATABASE = sig
  val test_name: string
  module DB: Trakeva_interface.KEY_VALUE_STORE
end

let open_close_test (module Test_db : TEST_DATABASE) uri_string () =
  let open Test_db in
  DB.load uri_string
  >>= fun db ->
  DB.close db

let basic_test (module Test_db : TEST_DATABASE) uri_string () =
  let open Test_db in
  let open Trakeva_interface.Action in
  let local_assert name c =
    Test.check (name :: test_name :: uri_string :: []) c in
  DB.load uri_string
  >>= fun db ->
  let test_get ?(handle=db) ?collection k f =
    DB.get handle ?collection ~key:k
    >>= fun opt ->
    local_assert (sprintf "get %s/%s" (Option.value collection ~default:"") k)
      (f opt);
    return () in
  let is_none = ((=) None) in
  test_get "k" is_none >>= fun () ->
  test_get ~collection:"c" "k" is_none >>= fun () ->
  DB.get_all db ~collection:"c"
  >>= fun list ->
  local_assert "all c" (list = []);
  let test_actions res actions =
    let action = seq actions in
    DB.act db ~action
    >>= function
    | r when r = res -> return ()
    | `Done ->
      ksprintf Test.fail "Action %s should be Not_done" (to_string action);
      return ()
    | `Not_done ->
      ksprintf Test.fail "Action %s should be Done" (to_string action);
      return ()
  in
  test_actions `Done [set ~key:"k" "v"] >>= fun () ->
  test_get  "k" ((=) (Some "v")) >>= fun () ->
  test_actions `Done [
    contains ~key:"k" "v";
    unset "k";
    set ~key:"k1" ~collection:"c" "V";
  ] >>= fun () ->
  test_actions `Not_done [
    contains ~key:"k" "v";
    set ~key:"k1" ~collection:"c" "V";
  ] >>= fun () ->
  test_actions `Done [
    is_not_set "k";
    set ~key:"k2" ~collection:"c" "V2";
    set ~key:"k3" ~collection:"c" "V3";
    set ~key:"k4" ~collection:"c" "V4";
    set ~key:"k5" ~collection:"c" "V5";
  ] >>= fun () ->
  DB.get_all db ~collection:"c"
  >>= fun list ->
  local_assert "full collection 'c'"
    (List.sort ~cmp:String.compare list = ["V"; "V2"; "V3"; "V4"; "V5"]);
  DB.close db

let git_db_test () =
  let module DB = Trakeva_git_commands in
  let open Trakeva_interface.Action in
  let db_file  = Test.new_tmp_dir () in
  DB.load db_file
  >>= fun db ->
  DB.get db ~key:"k"
  >>= fun res ->
  begin match res with
  | None -> return ()
  | Some v -> Test.fail (sprintf "key k got %S" v); return ()
  end
  >>= fun () ->
  begin DB.act db DB.(seq [ is_not_set "k"; set ~key:"k" "V" ])
    >>= function
    | `Done -> return ()
    | `Not_done -> Test.fail "seq 1 not done"; return ()
  end
  >>= fun () ->
  let check_k current_db =
    begin DB.get current_db ~key:"k" >>= function
      | Some v when v = "V" -> return ()
      | None -> Test.fail (sprintf "get k got None"); return ()
      | Some v -> Test.fail (sprintf "get k got %S" v); return ()
    end
  in
  check_k db >>= fun () ->
  DB.close db >>= fun () ->
  DB.load db_file
  >>= fun db2 ->
  check_k db2 >>= fun () ->
  begin DB.act db2 DB.(seq [contains ~key:"k" "V"])
    >>= function
    | `Done -> return ()
    | `Not_done -> Test.fail "seq 2 not done"; return ()
  end
  >>= fun () ->
  (* Transation that fails: *)
  begin DB.act db2 DB.(seq [
      set ~key:"k2" "vvv";
      set ~collection:"c3" ~key:"k3" "vvv";
      set ~key:"k2" "uuu";
      contains ~key:"k" "u"])
    >>= function
    | `Not_done -> return ()
    | `Done -> Test.fail "seq 3 done"; return ()
  end
  >>= fun () ->
  (* Transation that succeeds: *)
  begin DB.act db2 DB.(seq [
      is_not_set "k2";
      set ~key:"k2" "vvv";
      contains ~key:"k2" "vvv";
      set ~key:"k2" "uuu";
      contains ~key:"k" "V";
      contains ~key:"k2" "uuu";
      unset "k2";
      is_not_set "k2";
    ])
    >>= function
    | `Done -> return ()
    | `Not_done -> Test.fail "seq 4 not done"; return ()
  end
  >>= fun () ->
  (* Transations that fail hard: *)
  let test_with_debug_artificial_failure name f =
    DB.Debug.(global_debug := f "k2");
    begin
      Lwt.catch (fun () ->
          DB.act db2 DB.(seq [
              set ~key:"k2" "rrr";
              set ~key:"k2" "uuu";
              unset "k2";
            ])
          >>< function
          | _ -> Test.fail (sprintf "seq %s not exn" name); return ())
        (fun e -> return ())
    end
    >>= fun () ->
    DB.Debug.(global_debug := No);
    (* We should be like end of seq 6 *)
    begin DB.act db2 DB.(seq [
        is_not_set "k2";
        set ~collection:"c3" ~key:"k3" "uuu";
        unset ~collection:"c3" "k3";
        unset ~collection:"c3" "k3";
      ])
      >>= function
      | `Done -> return ()
      | `Not_done -> Test.fail (sprintf "seq %s+1 not done" name); return ()
    end
  in
  test_with_debug_artificial_failure "After_write"
    (fun k -> DB.Debug.After_write k)
  >>= fun () ->
  test_with_debug_artificial_failure "After_git_add"
    (fun k -> DB.Debug.After_git_add k)
  >>= fun () ->
  test_with_debug_artificial_failure "After_git_rm"
    (fun k -> DB.Debug.After_git_rm k)
  >>= fun () ->
  let check_collection collection result =
    let check r =
      let sort = List.sort ~cmp:String.compare in
      sort r = sort result in
    DB.get_all db2 ~collection
    >>= function
    | r when check r -> return ()
    | other ->
      Test.fail (sprintf "Collection test: in %S  \nexpecting [%s]  \ngot [%s]"
                   collection (String.concat ~sep:", " result)
                   (String.concat ~sep:", " other));
      return ()
  in
  check_collection "" [] >>= fun () ->
  check_collection "aslkdj" [] >>= fun () ->
  check_collection "c3" [] >>= fun () ->
  let collection = "c3" in
  DB.act db2 DB.(seq [set ~collection ~key:"k1" "v1";
                      set ~collection ~key:"k2" "v2"])
  >>= fun _ ->
  check_collection collection ["v1"; "v2"] >>= fun () ->
  let collection = "c4" in
  DB.act db2 DB.(seq [set ~collection ~key:"k1" "v1";
                      set ~collection ~key:"k2" "v2"])
  >>= fun _ ->
  check_collection collection ["v1"; "v2"] >>= fun () ->
  (* check_collection "c3" ["sld"] >>= fun () -> *)
  return ()

module Test_git_commands = struct
  let test_name = "Test_git_commands"
  module DB = Trakeva_git_commands
end
module Test_sqlite = struct
  let test_name = "Test_sqlite" 
  module DB = Trakeva_sqlite
end

let () =
  let argl = Sys.argv |> Array.to_list |> List.tl_exn in
  Test.run_monad "git-db" git_db_test;
  Test.run_monad "basic with git"
    (basic_test (module Test_git_commands) (Test.new_tmp_dir ()));
  let sqlite_path = "/tmp/trakeva-sqlite-test" in
  ksprintf Sys.command "rm -fr %s" sqlite_path |> ignore;
  Test.run_monad "basic/sqlite" (basic_test (module Test_sqlite) sqlite_path);
  begin match !Test.failed_tests with
  | [] ->
    say "No tests failed \\o/ (arg-list: [%s])"
      (String.concat ~sep:"." (List.map argl ~f:(sprintf "%S")));
    exit 0
  | some ->
    say "Some tests failed (arg-list: [%s]):"
      (String.concat ~sep:"." (List.map argl ~f:(sprintf "%S")));
    List.iter some ~f:(fun t ->
        say "%s" t;
        say "%s" (String.make 80 '-');
      );
    exit 3
  end
