(**** you can put all your code here ****)

fun only_capitals xs =
    List.filter (fn x => Char.isUpper (String.sub (x, 0))) xs

fun longest_string1 xs =
    List.foldl (fn (x, y) => if (String.size x) > (String.size y) then x else y) "" xs

fun longest_string2 xs =
    List.foldl (fn (x, y) => if (String.size x) >= (String.size y) then x else y) "" xs

fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				                        then s
				                        else sofar) ""
val longest_string3 = longest_string_helper (fn (x,y) => x > y) 
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

exception NoAnswer

fun first_answer f xs =
    case xs of
    [] => raise NoAnswer
    | x::xs' => case f x of 
                NONE => first_answer f xs'
                | SOME y => y

fun all_answers f xs =  (* f: 'a -> 'b list option *)
    case xs of
    [] => SOME []
    | x::[] => f x 
    | x::tail =>    case (f x) of
                    NONE => NONE
                    | SOME l => case (all_answers f tail) of
                                NONE => NONE
                                | SOME ll => SOME (l @ ll)

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
fun count_wildcards ps = g (fn x => 1) (fn x => 0) ps
fun count_wild_and_variable_lengths p = g (fn x => 1) (fn x => String.size x) p
fun count_some_var (s, p) =
    g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let
        fun all_string p = 
            case p of
            Variable x => [x]
            | TupleP ps => List.foldl (fn (p,i) => (all_string p) @ i) [] ps
            | ConstructorP(_,p) => all_string p
            | _ => [] 
        fun check_no_duplicate xs =
            case xs of
            [] => true
            | x::tail => (not (List.exists (fn y => y = x) tail)) andalso (check_no_duplicate tail)
    in
        (check_no_duplicate o all_string) p
    end
(* apply pattern match to pair is more elegant *)
(*fun match (v, p) =
    case p of
    Wildcard => SOME []
    | Variable s => SOME [(s, v)]
    | UnitP => (case v of Unit => SOME [] | _ => NONE)
    | ConstP i => (case v of Const j => if j = i then SOME [] else NONE | _ => NONE)
    | ConstructorP(s1, pa) => 
        (
            case v of 
            Constructor(s2, v) => 
                if s1 <> s2 then NONE else match(v, pa)
            | _ => NONE
        )
    | TupleP ps => (
            case v of 
            Tuple vs => if List.length vs = (List.length ps) then
                            all_answers (fn x => case x of (pattern, value) => match(value, pattern)
                            ) (ListPair.zip(ps, vs))
                        else NONE
            | _ => NONE)*)
fun match (v, p) =
    case (v, p) of
    (_, Wildcard) => SOME []
    | (_, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const va, ConstP pa) => if va = pa then SOME [] else NONE
    | (Tuple(vs), TupleP(ps)) => if length vs = length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
    | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 <> s2 then NONE
                                                   else match(v,p)
    | _ => NONE
    
fun first_match v ps =
    SOME (first_answer (fn p => match(v,p)) ps)
    handle NoAnswer => NONE
    

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string


