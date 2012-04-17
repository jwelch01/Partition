structure Anonymizer :> ANONYMIZER = struct

    type salt = string

    fun sort op < xs =
        let fun insert (x, []) = [x]
              | insert (x, y::ys) = if x < y then x :: y :: ys
                                    else y :: insert (x, ys)
        in  foldr insert [] xs
        end
      
    val md5sum =
      MD5.toHexString o MD5.final o (fn v => MD5.update (MD5.init, v)) o
      Byte.stringToBytes

    fun toNumbers (salt, ids : string list) =
      let val ids = sort op < ids
          val salt = concat (foldr (fn (id, ss) => id :: "\000" :: ss) [salt] ids)
          val hashpairs = map (fn id => (id, md5sum (salt ^ id))) ids
          val hashpairs = sort (fn ((_, h), (_, h')) => h < h') hashpairs
          fun number n ((id, h) :: ps) = (id, n) :: number (n+1) ps
            | number _ []              = []
      in  number 0 hashpairs
      end

end
