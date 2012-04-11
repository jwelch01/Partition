structure Sample = struct
  fun sample' 0 g = ([], QCheck.Gen.new())
    | sample' n g =
        let val (xs, rand) = sample' (n-1) g
            val (x, rand)  = g rand
        in  (x :: xs, rand)
        end

  fun sample n g = let val (xs, _) = sample' n g in xs end
end
