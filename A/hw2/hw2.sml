(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, xs) = 
    case xs of 
        [] => NONE
      | x::xs' => 
            if same_string(s, x)
            then SOME xs'
            else case all_except_option(s, xs') of 
                NONE => NONE
              | SOME lst => SOME (x::lst) 

fun get_substitutions1 (xss, s) =
    case xss of
         [] => []
       | xs::xss' => 
            (case all_except_option(s, xs) of
                NONE => get_substitutions1(xss', s)
              | SOME lst => lst @ get_substitutions1 (xss', s))

fun get_substitutions2 (xss, s) =
    let fun aux (xss, s, acc) = 
        case xss of
             [] => acc 
           | xs::xss' => 
                (case all_except_option(s,xs) of
                      NONE => aux(xss', s, acc)
                    | SOME lst => aux(xss', s, acc @ lst))
    in 
        aux(xss, s, [])
    end

fun similar_names (xss, {first = first, middle = middle, last = last}) =
    let fun helper xs =
            case xs of
                [] => []
              | x::xs' => {first = x, middle = middle, last = last} ::
                helper xs'
    in
        {first = first, middle = middle, last = last} :: 
          helper(get_substitutions2(xss, first)) 
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
         (Clubs, _) => Black 
       | (Diamonds, _) => Red 
       | (Hearts, _) => Red 
       | (Spades, _) => Black 

fun card_value card =
    case card of
         (_, Jack) => 10
       | (_, Queen) => 10
       | (_, King) => 10
       | (_, Ace) => 11
       | (_, Num x) => x
   
fun remove_card (cs, c, e) = 
    case cs of
         [] => raise e
       | x::xs' => 
            if x = c
            then xs'
            else x::remove_card(xs', c, e)
         
fun all_same_color cs =
    case cs of 
         [] => true
       | _::[] => true
       | x::y::rest => card_color(x) = card_color(y) 
                       andalso all_same_color(y::rest)

fun sum_cards cs =
    let fun aux (cs, acc) =
            case cs of
                 [] => acc 
               | c::cs' => aux(cs', acc + card_value(c))
    in
        aux(cs, 0)
    end
    
fun score (cs, goal) = 
    let fun prelimscore(cs, goal) = 
            let val sum = sum_cards(cs)
            in
                if sum > goal
                then 3 * (sum - goal)
                else goal - sum 
            end
    in
        if all_same_color(cs)
        then prelimscore(cs, goal) div 2
        else prelimscore(cs, goal)
    end

fun officiate (cs, ms, goal) = 
    let fun aux (cs, ms, goal, hs) =
            case ms of
                 [] => score(hs, goal)
               | m::ms' => if sum_cards hs > goal
                           then score(hs, goal)
                           else (case m of
                                Discard card => 
                                  aux(cs, ms', goal,
                                  remove_card(hs, card, IllegalMove))
                              | Draw =>  
                                  (case cs of
                                    [] => score(hs, goal)
                                  | c::cs' => aux(cs', ms', goal, c::hs)))
    in 
        aux(cs, ms, goal, [])
    end

fun score_challenge (cs, goal) =
    let fun ace_replacer cs =
            case cs of
                [] => []
              | (cardsuit, Ace)::cs' => (cardsuit, Num 1)::cs'
              | card::cs'=> card::ace_replacer cs'
    in
        let val orig_score = score(cs, goal)
            val replaced_score = score(ace_replacer cs, goal)
        in
            if orig_score > replaced_score
            then score_challenge(ace_replacer cs, goal)
            else orig_score
        end
    end

fun officiate_challenge (cs, ms, goal) =
    let fun replace_all_aces ccs =
            case ccs of
                  [] => []
               | (cardsuit, Ace)::ccs' => (cardsuit, Num 1)::replace_all_aces ccs'
               | card::ccs' => card::replace_all_aces ccs'
        fun aux (cs, ms, goal, hs) =
            case ms of
                 [] => score_challenge(hs, goal)
               | m::ms' => if sum_cards(replace_all_aces hs) > goal 
                           then score_challenge(hs, goal)
                           else (case m of
                                Discard card => 
                                    aux(cs, ms', goal,
                                    remove_card(hs, card, IllegalMove))
                              | Draw =>  
                                    (case cs of
                                      [] => score_challenge(hs, goal)
                                    | c::cs' => aux(cs', ms', goal, c::hs)))
    in 
        aux(cs, ms, goal, [])
    end

fun careful_player (cs, goal) = 
    let fun card_finder (xcs, yc) =
            case xcs of 
                 [] => NONE
               | xc::xcs' => if card_value xc = yc
                            then SOME xc
                            else card_finder (xcs', yc)
        fun aux (cs, hs, goal) =
            let val sum = sum_cards hs
            in
                if goal - sum > 10
                then case cs of
                        [] => [Draw]
                      | c::cs' => Draw::aux(cs', c::hs, goal)
                else if goal - sum = 0
                then []
                else case cs of
                        [] => []
                      | c::cs' => let val opt_card_value = card_value c + sum - goal 
                                  in 
                                    case card_finder (hs, opt_card_value) of
                                         NONE => []
                                       | SOME c => [Discard c,Draw]
                                  end
            end

    in
        aux(cs, [], goal)
    end 
