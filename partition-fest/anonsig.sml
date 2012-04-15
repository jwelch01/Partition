signature ANONYMIZER = 
sig
  type salt

  (* takes in a list of names and a salt
     returns an anonymization function *)
  val anonymize : string list * salt -> string -> string

end