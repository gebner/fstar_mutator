module Rand
open FStar.Compiler.Effect
val self_init : unit -> ML unit
val full_int : n:nat -> ML (i:nat { i < n })
