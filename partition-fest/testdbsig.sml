signature TEST_DB = 
sig
  type db

  val empty  : db
  val bind   : string * string * string * Outcome.outcome * db -> db
  val lookup : string * string * string * db -> Outcome.outcome
  val fold : ((string * string * string * Outcome.outcome * 'b) -> 'b) -> 'b -> db -> 'b
  
  (*calls function once for each student in the DB, regardless of whether they
    have a test result, allowing for DNRs *)
  val foldStudents : ((string * string * (string * Outcome.outcome) list * 'b) -> 'b) -> 'b -> db -> 'b
  
end