let self_init () = Random.self_init ()
let full_int (n: Z.t) : Z.t = Z.of_int (Random.full_int (Z.to_int n))
