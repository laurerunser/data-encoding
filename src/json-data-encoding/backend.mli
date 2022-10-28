val construct : 'a Encoding.t -> 'a -> (JSON.t, string) result
val destruct : 'a Encoding.t -> JSON.t -> ('a, string) result
