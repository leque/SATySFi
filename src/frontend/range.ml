
type t =
  | Dummy of string
  | Normal of {
      filename: string;
      line1 : int;
      col1 : int;
      line2 : int;
      col2 : int
    }
[@@deriving show]


let dummy msg = Dummy(msg)


let is_dummy rng =
  match rng with
  | Dummy(_) -> true
  | _        -> false


let message rng =
  match rng with
  | Dummy(msg)            -> msg
  | Normal _ -> "*NORMAL*"


let to_string rng =
  match rng with
  | Dummy(msg) ->
      Printf.sprintf "dummy(%s)" msg

  | Normal{ filename; line1; col1; line2; col2 } ->
      if line1 = line2 then
        Printf.sprintf "%s:%d.%d-%d" filename line1 (col1 + 1) (col2 + 1)
      else
        Printf.sprintf "%s:%d.%d-%d.%d" filename line1 (col1 + 1) line2 (col2 + 1)

let unite rng1 rng2 =
  match (rng1, rng2) with
  | (Normal{ filename; line1; col1; _ }, Normal{ line2; col2; _ }) ->
      Normal { filename; line1; col1; line2; col2 }
  | (Normal _, _) -> rng1
  | (_, Normal _) -> rng2
  | _ -> Dummy("unite")


let make filename line1 col1 col2 =
  Normal{ filename; line1; col1; line2 = line1; col2 }


let make_large filename line1 col1 line2 col2 =
  Normal{ filename; line1; col1; line2; col2 }
