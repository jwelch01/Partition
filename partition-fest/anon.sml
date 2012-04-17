functor AnimalAnonymizer(val instructor : string) :> ANONYMIZER
  where type salt = string =
struct
  type salt = Anonymizer.salt
  fun anonymize (ids, salt) =
    let val ids = List.filter (fn s => s <> instructor) ids
        val numbers = Anonymizer.toNumbers (salt, ids)
        fun add ((id, n), map) =
              Map.bind (explode id, Vector.sub(Animals.animals, n), map)
        val map =
          foldl add (Map.bind (explode instructor, Animals.special, Map.empty)) numbers
    in  fn s => Map.lookup (explode s, map)
    end
end


structure AnonTest = AnimalAnonymizer(val instructor = "nr")
