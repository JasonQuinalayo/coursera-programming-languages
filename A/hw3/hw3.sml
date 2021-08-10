(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals sl = 
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) sl

fun longest_string1 sl =
    foldl (fn (x,y) => if String.size x > String.size y then x else y) "" sl 

fun longest_string2 sl =
    foldl (fn (x,y) => if String.size x >= String.size y then x else y) "" sl 

fun longest_string_helper f sl =
    foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" sl

val longest_string3 = 
  longest_string_helper (fn (x,y) => x > y)

val longest_string4 = 
  longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode

fun first_answer f lst = 
    case lst of 
         [] => raise NoAnswer
       | x::xs' => case f x of
                        SOME v => v
                      | NONE => first_answer f xs'

fun all_answers f lst = 
    let fun aux (lst, acc) =
            case lst of
                 [] => SOME acc 
               | x::xs' => case f x of
                              NONE => NONE
                            | SOME x' => aux(xs', acc @ x')
    in aux (lst, [])
    end

fun count_wildcards p = g (fn () => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) String.size p

fun count_some_var (s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat pat = 
    let fun extractStrings p = 
            case p of
                 Variable x => [x]
               | TupleP ps => foldl (fn (x,y) => extractStrings x @ y) [] ps 
               | ConstructorP (_,s) => extractStrings s 
               | _ => [] 
        fun checkRepeats lst =
            case lst of
                 [] => false
               | l::lst' => List.exists (fn w => w = l) lst' orelse checkRepeats lst' 
    in not (checkRepeats (extractStrings pat))
    end

fun match (v,p) = 
    case (v,p) of
         (_,Wildcard) => SOME []
       | (Unit, UnitP) => SOME []
       | (Const x, ConstP y) => if x = y then SOME [] else NONE
       | (vluV, Variable varbV) => SOME [(varbV, vluV)]
       | (Tuple tList, TupleP tpList) => if length tList = length tpList 
                                         then all_answers match (ListPair.zip
                                         (tList, tpList))
                                         else NONE
       | (Constructor (strn, vlu), ConstructorP (strnP, vluP)) => if strn = strnP 
                                                                  then match
                                                                  (vlu, vluP)
                                                                  else NONE
       | _ => NONE

fun first_match v patLst = 
    SOME (first_answer (fn p => match (v, p)) patLst)
    handle NoAnswer => NONE

fun typecheck_patterns (ctrLst, patLst) =
    let exception NoCommonType
        fun convertToTyp pat = 
            case pat of 
                 Wildcard => Anything
               | Variable _ => Anything
               | UnitP => UnitT
               | ConstP _ => IntT
               | TupleP tLst => TupleT (List.map convertToTyp tLst)
               | ConstructorP cP => convertCnstrctor cP ctrLst
        and convertCnstrctor ctr lst =
            case ctr of 
                 (ctrname, ctrvalue) =>
                    (case lst of 
                        [] => raise NoCommonType
                        | (c, dat, vlu)::cs' => 
                                   if ctrname = c 
                                   then (if vlu = convertToTyp ctrvalue 
                                        then Datatype dat 
                                        else raise NoCommonType)
                                   else convertCnstrctor ctr cs')
        fun checkCommonType (typB, typA) =
            case typA of
                 TupleT tpl => (case typB of
                                    TupleT tppl => (if length tpl = length tppl
                                                   then TupleT (List.map
                                                   checkCommonType (ListPair.zip
                                                   (tpl, tppl)))
                                                   else raise NoCommonType)
                                  | Anything => TupleT tpl
                                  | _=> raise NoCommonType )
               | Anything => typB
               | x => case typB of
                           Anything => x
                         | y => (if x = y
                                 then y
                                 else raise NoCommonType)
    in SOME (foldl checkCommonType Anything (List.map convertToTyp patLst))
    end
    handle NoCommonType => NONE

