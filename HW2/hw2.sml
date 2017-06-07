(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2
fun reverse(xs) =
    let
        fun rev(ys,accu) =
            case ys of
              [] => accu
            | y::tl => rev(tl,y::accu)
    in
      rev(xs,[])
    end
(* put your solutions for problem 1 here *)
fun all_except_option (y, xs) = 
    case xs of
      [] => NONE
    | x::tl =>  if same_string(x,y) then SOME tl else
                    case all_except_option(y,tl) of
                      NONE => NONE
                    | SOME rs => SOME(x::rs)
fun get_substitutions1 (xss, y) =
    case xss of
      [] => []
    | xs::tll => 
        case all_except_option(y,xs) of
          NONE => get_substitutions1 (tll,y)
        | SOME rs => rs @ get_substitutions1 (tll,y)
fun get_substitutions2 (xss, y) =
    let
        fun get_subs(xss,accumulator) =
            case xss of 
              [] => accumulator
            | xs::tll => 
                case all_except_option(y,xs) of
                  NONE => get_subs(tll,y,accumulator)
                | SOME rs => get_subs(tll,y,accumulator @ rs)
    in
        get_subs(xss,[])
    end
fun similar_names (xss, fullname) =
    let
        val {first=f,middle=m,last=l} = fullname
        val firsts = f::get_substitutions2(xss,f)
        fun combine(firsts, names) =
            case firsts of 
              [] => names
            | fir::tl => combine(tl, {first=fir,middle=m,last=l} :: names)
    in
      reverse (combine(firsts,[]))
    end
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color card =
    case card of
      (Spades,_) => Black
    | (Clubs,_) => Black
    | _ => Red
fun card_value card =
    case card of
      (_,Num v) => v
    | (_,Ace) => 11
    | _ => 10
fun remove_card (cs, c, e) =
    case cs of 
      [] => raise e
    | card::tl => if card = c then tl else card :: remove_card(tl,c,e)
fun all_same_color (cards) =
    case cards of 
      [] => true
    | card::[] => true
    | first::(second::tail) => card_color first = card_color(second) andalso all_same_color(second::tail)
fun sum_cards (cards) =
    let
        fun sum_c(cards, accumulator) =
            case cards of
              [] => accumulator
            | card::tl => sum_c(tl, card_value card + accumulator)
    in
        sum_c(cards,0)
    end
fun score (cards, goal) =
    let
        val sum = sum_cards cards
        val preliminary = if sum > goal then 3 * (sum - goal) else goal - sum
    in
      if all_same_color cards then preliminary div 2 else preliminary
    end
fun officiate (cards, moves, goal) = 
    let
        fun make_moves(cs, ms, hs) =
            if sum_cards hs > goal then hs else 
            case ms of
              [] => hs
            | m::tail_ms => 
                case m of
                  Discard c => make_moves(cs,tail_ms,remove_card(hs,c,IllegalMove))
                | Draw => 
                    case cs of 
                      [] => hs
                    | c::tail_cs => make_moves(tail_cs, tail_ms, c::hs)
    in
        (*make_moves return cards held in hand when game ends so score function can compute score*)
        score(make_moves(cards,moves,[]),goal)
    end
fun num_ace cs =
    case cs of
        [] => 0
    | c::tl => if card_value c = 11 then 1 + num_ace tl else num_ace tl
fun score_challenge (cards, goal) =
    let
        fun sum_to_score (s) =
            if s > goal then 3 * (s - goal) else goal - s
        fun best (sums) =
            case sums of
              s::[] => s
            | s::tl =>  let val n_tl = best tl in
                        if sum_to_score s < (sum_to_score n_tl) then s else n_tl
                        end
        val num_ace = num_ace cards
        val original_sum = sum_cards cards
        val original_score = score (cards, goal)
        fun get_sums (ori_sum, n_ace) =
            case n_ace of
              0 => [ori_sum]
            | n => ori_sum :: get_sums(ori_sum - 10, n - 1)
        val sums = get_sums (original_sum, num_ace)
        val best_sum = best sums
        val best_score = sum_to_score best_sum
    in
        if all_same_color cards then best_score div 2 else best_score
    end
fun officiate_challenge (cards, moves, goal) = 
    let
        fun make_moves(cs, ms, hs) =
            let
                val n_ace = num_ace hs
                val current_sum = sum_cards hs
                val current_smallest_sum = current_sum - (10 * n_ace)
            in
                if current_smallest_sum > goal then hs else 
                case ms of
                  [] => hs
                | m::tail_ms => 
                    case m of
                      Discard c => make_moves(cs,tail_ms,remove_card(hs,c,IllegalMove))
                    | Draw => 
                        case cs of 
                          [] => hs
                        | c::tail_cs => make_moves(tail_cs, tail_ms, c::hs)
            end
    in
        score_challenge(make_moves(cards,moves,[]),goal)
    end

fun possible_moves (cards, moves, helds) = 
    case cards of
      [] => 