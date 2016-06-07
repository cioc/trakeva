(** Implementation of [Trakeva_interface.KEY_VALUE_STORE] with a mysql backend *)

(** Mysql impl largely follows postgres impl *)  

include Trakeva.KEY_VALUE_STORE

(** {3 Debugging } *)

val debug : bool ref
(** Set [dbug] to [true] to print debug messages on [stderr], this variable
    is also set when {!load} is called while the environment variable
    ["TRAKEVA_SQLITE_DEBUG"] is set with the string ["true"]. *)

val table_name: t -> string
(** The SQL table that is used internally. *)
