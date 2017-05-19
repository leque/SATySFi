
module type S =
  sig
    type key
    type 'a t
    val empty : 'a -> 'a t
    val to_string : (key -> string) -> ('a -> string) -> 'a t -> string
    val find_stage : 'a t -> key list -> 'a
    val update : 'a t -> key list -> ('a -> 'a) -> 'a t
    val add_stage : 'a t -> key list -> key -> 'a -> 'a t
    val search_backward : 'a t -> key list -> key list -> ('a -> 'b option) -> 'b option
  end


module Make (Key : Map.OrderedType) =
  struct

    module InternalMap = Map.Make(Key)

    type key = Key.t

    type 'a t = Stage of 'a * ('a t) InternalMap.t


    let empty (vroot : 'a) = Stage(vroot, InternalMap.empty)


    let rec to_string (strk : key -> string) (strf : 'a -> string) (Stage(x, imap) : 'a t) =
      (strf x) ^ ", { " ^ (InternalMap.fold (fun k hshtr s -> (strk k) ^ ": " ^ (to_string strk strf hshtr) ^ " " ^ s) imap "") ^ "}"


    let rec find_stage (Stage(x, imap) : 'a t) (addr : key list) =
      match addr with
      | []        -> x
      | k :: tail ->
          let hshtr = InternalMap.find k imap in find_stage hshtr tail


    let rec update (Stage(x, imap) : 'a t) (addr : key list) (f : 'a -> 'a) =
      match addr with
      | []        -> Stage(f x, imap)
      | k :: tail ->
          let hshtr = InternalMap.find k imap in
            Stage(x, InternalMap.add k (update hshtr tail f) imap)


    let rec add_stage (Stage(x, imap) : 'a t) (addr : key list) (knew : key) (vnew : 'a) =
      match addr with
      | []        -> Stage(x, InternalMap.add knew (Stage(vnew, InternalMap.empty)) imap)
      | k :: tail ->
          let hshtr = InternalMap.find k imap in
          let res = add_stage hshtr tail knew vnew in
          let imapnew = InternalMap.add k res imap in
            Stage(x, imapnew)


    let rec search_backward (Stage(x, imap) as hshtr : 'a t) (addr : key list) (addrlast : key list) (findf : 'a -> 'b option) =
      match addr with
      | []        ->
          begin
            try
              let xsub = find_stage hshtr addrlast in
                findf xsub
            with
            | Not_found -> None
          end

      | k :: tail ->
          let hshtr = InternalMap.find k imap in
          let res = search_backward hshtr tail addrlast findf in
          begin
            match res with
            | None    -> findf x
            | Some(_) -> res
          end

  end
