signature COMPARABLE_COLLECTION = 
sig
  type elem
  type collection

  exception NotFound

  val empty  : collection
  val add    : elem * collection -> collection
  val member : elem * collection -> bool
  val isEmpty : collection -> bool

  val eq     : elem * elem -> bool

  val fold : (elem * 'a -> 'a) -> 'a -> collection -> 'a

  val getId : elem -> string * string

  val partition : (elem * elem -> bool) -> collection -> collection list
    (* arg is an equivalence relation,
       partitions collection into maximal equivalent subsets, i.e. if

          p = partition eq s

       then for all s' in p : for all x, x' in s' : eq (x, x')
       also for all s, s' in p :
               if s <> s' then forall x in s, x' in s' : not (eq (x, x'))


       Example: partition (fn (n, m) => n mod 10 = n mod 10) { 1, 3, 11, 13, 33 }
                 =
                [ {1, 11}, {3, 13, 33} ]

   *)           

  val /</  : elem * elem -> bool (* strictly lower in rank order *)
  val /==/ : collection * collection -> bool (* equality *)

  val toString : collection -> string

  val representative : collection -> elem option
    (* representative empty == NONE *)
    (* if s is not empty, member (representative s, s) *)

end
