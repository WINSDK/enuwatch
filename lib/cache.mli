open! Core

type t

val create : capacity:int -> t
val get : t -> string -> string option
