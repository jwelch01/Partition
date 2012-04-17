signature SET = 
sig
  type elem
  type set

  exception NotFound

  val empty  : set
  val add    : elem * set -> set
  val member : elem * set -> bool
  val isEmpty : set -> bool

  val eq     : elem * elem -> bool

  val fold : (elem * 'a -> 'a) -> 'a -> set -> 'a

  val getId : elem -> string * string

  val partition : (elem * elem -> bool) -> set -> set list
    (* arg is an equivalence relation,
       partitions set into maximal equivalent subsets, i.e. if

          p = partition eq s

       then for all s' in p : for all x, x' in s' : eq (x, x')
       also for all s, s' in p :
               if s <> s' then forall x in s, x' in s' : not (eq (x, x'))


       Example: partition (fn (n, m) => n mod 10 = n mod 10) { 1, 3, 11, 13, 33 }
                 =
                [ {1, 11}, {3, 13, 33} ]

   *)           

  val /</  : elem * elem -> bool (* strictly lower in rank order *)
  val /==/ : set * set -> bool (* equality *)

  val toString : set -> string

  val representative : set -> elem option
    (* representative empty == NONE *)
    (* if s is not empty, member (representative s, s) *)

  structure QC : sig
    val reflexive : elem -> bool
    val rprop : elem QCheck.prop
    val lt_prop : elem QCheck.prop
(*    val lte_test : elem QCheck.prop *)
    val elem : elem QCheck.Gen.gen
    val set  : set  QCheck.Gen.gen
  end

end