open Nonstd
open Pvem_lwt_unix
open Pvem_lwt_unix.Deferred_result
open Mysql
open Mysql.Prepared
open Trakeva
module String = StringLabels

let debug = ref false

let dbg fmt = ksprintf (eprintf "Trakeva_mysql: %s\n%!") fmt

(* Deciding to not maintain a global handle, instead open a connection for each transaction.  This avoids the need to lock. On the other hand, we pay a tcp handshake on each call.  Better would be to pool connections and borrow them from the pool for the duration of the transaction.  Not doing this now.   *)
type t = {
  table_name: string;
  conninfo : string;
}

(* TODO : move to common file *)
let in_posix_thread ~on_exn f =
  Lwt_preemptive.detach (fun () ->
  (* (fun f () -> Lwt.return (f ()))  (fun () -> *)
    try `Ok (f ())
    with e -> on_exn e) ()

(* Explicitly calling for InnoDB engine because many thoughts on consistency and performance were made solely with respect to InnoDB, not other storage engines. *)
(* Choose LONGBLOB so that we can store up to 2^32 - 1 bytes, trying to match Postgres's BYTEA which, to my knowledge, stores the size of its binary string in a 32 bit field imposing a bound.*)
let create_table_stmt (table : string) : string =
  sprintf "CREATE TABLE %s \
           (collection LONGBLOB, key LONGBLOB, value LONGBLOB, \
           PRIMARY KEY(collection, key)) \
           ENGINE=InnoDB" table

(* TODO - determine correct substitution string for mysql *)
(* TODO - ask about conditions from collection_sql_condition *)
let set_stmt (table : string) : string =  
  sprintf "INSERT INTO %s (collection, key, value) \
           VALUES ($1, $2, $3) \
           ON DUPLICATE KEY UPDATE value=$3" table
  
let unset_stmt (table : string) (collection : string) : string = 
  sprintf "DELETE FROM %s WHERE collection = %s AND key = $2" table collection 

let get_stmt (table : string) (collection : string) : string = 
  sprintf "SELECT value FROM %s \
           WHERE collection = %s AND key = $2" table collection  

(* postgres default ordering is ascending *)
let get_keys_stmt (table : string) : string = 
  sprintf "SELECT key FROM %s \
           WHERE collection = $1 ORDER BY key ASC" table 

(* default for InnoDB is repeatable read.  repeatable read is nice because you get a consistent snapshot during the transaction for reads.  It's not so nice because it doesn't lock the read rows by default.  So, it doesn't match the behavior of the postgres impl of trakeva.  Serializable transaction level causes each read to implicitily table a row lock i.e. like adding LOCK IN SHARE MODE to each SELECT *)
(* another discussion to have is if this level of consistency is needed or not. For now, trying to get close to postgres existing behavior without locking the entire table. *) 
let set_txn_level_stmt = "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"

let start_txn_stmt = "START TRANSACTION"

let commit_txn_stmt = "COMMIT"

let rollback_txn_stmt = "ROLLBACK"

let default_table = "trakeva_default_table"

let table_name t = t.table_name

let get_db (conninfo : string) : Mysql.db =
  (* TODO - actually parse the URI *)
  { dbhost = None;
    dbname = None;
    dbport = None;
    dbpwd = None;
    dbuser = None;
    dbsocket = None
  } 

let get_conn (conninfo : string) : Mysql.dbd = 
  (* TODO - think about how this call can fail *)
  Mysql.connect (get_db conninfo)

let exn_to_string e = "Not implemented" (* TODO implement *)

(* We'll think in terms of transaction since we are dealing with a sql (my). *)
let start_txn conninfo : Mysql.dbd = 
  let conn = get_conn conninfo in
  (* TODO - handle errors *)
  let (_ : Mysql.result) = exec conn set_txn_level_stmt in
  let (_ : Mysql.result) = exec conn start_txn_stmt in
  conn

let exec (conn : Mysql.dbd) (stmt : string) : Mysql.status =   
  let result = Mysql.exec conn stmt in
  Mysql.status conn

let query (conn : Mysql.dbd) (stmt : string) (params : string array) : (Prepared.stmt_result) = 
  let prepared = Prepared.create conn stmt in
  Prepared.execute prepared params 

let rollback_txn (conn : Mysql.dbd) = 
  (* TODO - handle errors *)
  let status = exec conn rollback_txn_stmt in
  Mysql.disconnect conn

let commit_txn (conn : Mysql.dbd) = 
  (* TODO - handle errors *)
  let status = exec conn commit_txn_stmt in
  Mysql.disconnect conn

(* TODO how is table_name actually changed by user? *)
let load_exn conninfo = 
  let conn = get_conn conninfo in
  let table_name = default_table in 
  let status = exec conn (create_table_stmt table_name) in
  commit_txn conn ; (*TODO Is this the best way to run a sequence of expressions? *)
  match status with
  | Mysql.StatusOK 
  | Mysql.StatusEmpty ->
    {table_name; conninfo} 
  | Mysql.StatusError error_code ->
    ksprintf failwith "Cannot create table %S" table_name (* Q: what happends when the table already exists? TODO - error code translation *)

let load conninfo =
  let on_exn e = `Error (`Database (`Load conninfo, exn_to_string e)) in
  in_posix_thread ~on_exn (fun() -> load_exn conninfo) 

(* avoiding persistent connections for now, so this is a NoOp *)
let close = ()

let get ?collection t ~key =
  let conn = start_txn t.conninfo in
     
(*
let get_all t ~collection =

let iterator t ~collection =

let act t ~(action: Action.t) = *)
