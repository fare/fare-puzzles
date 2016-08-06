(*
String cleaning
===============

Your spy, Beta Rabbit, has managed to infiltrate a lab of mad scientists who are turning rabbits into zombies. He sends a text transmission to you, but it is intercepted by a pirate, who jumbles the message by repeatedly inserting the same word into the text some number of times. At each step, he might have inserted the word anywhere, including at the beginning or end, or even into a copy of the word he inserted in a previous step. By offering the pirate a dubloon, you get him to tell you what that word was. A few bottles of rum later, he also tells you that the original text was the shortest possible string formed by repeated removals of that word, and that the text was actually the lexicographically earliest string from all the possible shortest candidates. Using this information, can you work out what message your spy originally sent?

For example, if the final chunk of text was "lolol," and the inserted word was "lol," the shortest possible strings are "ol" (remove "lol" from the beginning) and "lo" (remove "lol" from the end). The original text therefore must have been "lo," the lexicographically earliest string.

Write a function called answer(chunk, word) that returns the shortest, lexicographically earliest string that can be formed by removing occurrences of word from chunk. Keep in mind that the occurrences may be nested, and that removing one occurrence might result in another. For example, removing "ab" from "aabb" results in another "ab" that was not originally present. Also keep in mind that your spy's original message might have been an empty string.

chunk and word will only consist of lowercase letters [a-z].
chunk will have no more than 20 characters.
word will have at least one character, and no more than the number of characters in chunk.

Languages
=========

To provide a Python solution, edit solution.py
To provide a Java solution, edit solution.java

Test cases
==========

Inputs:
    (string) chunk = "lololololo"
    (string) word = "lol"
Output:
    (string) "looo"

Inputs:
    (string) chunk = "goodgooogoogfogoood"
    (string) word = "goo"
Output:
    (string) "dogfood"

Use verify [file] to test your solution and see how it does. When you are finished editing your code, use submit [file] to submit your answer. If your solution passes the test cases, it will be removed from your home folder.
 *)

open Printf

let letter_index c = int_of_char c - int_of_char 'a'
;;

let is_better_answer_ a b =
  match compare (String.length a) (String.length b) with
    -1 -> true
  | 0 -> ( String.compare a b = -1 )
  | _ -> false
;;

let is_better_answer a b =
  let answer = is_better_answer_ a b in
     Printf.printf "\"%s\" better than \"%s\" => %s\n" a b (if answer then "true" else "false") ;
  answer
;;

let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []
;;

let print_int x = Printf.printf "%d" x
;;

let print_list p l =
  Printf.printf "[";
  List.iteri (fun i x -> (if i > 0 ; then Printf.printf ";"); p x) l ;
  Printf.printf "]"
;;

let print_array p a =
  Printf.printf "[|";
  Array.iteri (fun i x -> (if i > 0 ; then Printf.printf ";"); p x) a ;
  Printf.printf "|]"
;;

let print_int_list l = print_list print_int l ;;
let print_int_list_array a = print_array print_int_list a ;;

let answer (chunk, word) =
  let l = String.length(chunk) in
  assert (l > 0) ;
  let w = String.length(word) in
  assert (w > 0) ;
  let letter_positions = Array.make 26 [] in
  for i = 1 to w do
    let j = w - i in
    let letter = letter_index(word.[j]) in
    assert (0 <= letter && letter < 26) ;
    letter_positions.(letter) <- j :: letter_positions.(letter)
  done ;
  let parse = Array.make l ("", (Array.make w [])) in
  let rec loop i previous_best previous_reductions =
    if (i = l)
    then
      previous_best
    else
      let positions = letter_positions.(letter_index chunk.[i]) in
      let best = ref (previous_best ^ (String.make 1 chunk.[i])) in
      let reductions = Array.make w [] in
      let reduce_position p =
        let reduced = previous_reductions.(p) in
	begin
	  if p == w-1 then
	    List.iter (fun r ->
	      let candidate = fst parse.(r) in
		begin
		  if is_better_answer candidate !best then
		    begin
		      best := candidate ;
		      Array.iteri
			(fun j l -> reductions.(j) <- List.append (snd parse.(r)).(j) l)
			reductions
		    end
		end) reduced
          else
	    begin
	      (*Printf.printf "reductions.(%d) <- %d\n" (p+1) reduced;*)
	      reductions.(p+1) <- reduced
	    end
	end
      in
      begin
	Printf.printf "%d \"%s\" " i previous_best;
	print_int_list_array previous_reductions;
	Printf.printf " => %c " chunk.[i];
	print_int_list positions;
	Printf.printf "\n";

	parse.(i) <- (previous_best, previous_reductions) ;
	reductions.(0) <- [i+1] ;
	List.iter reduce_position positions ;
	loop (i + 1) !best reductions
      end
  in loop 0 "" (let a = Array.make w [] in a.(0) <- [0] ; a)
;;


let check (chunk, word, expected) =
  let actual = answer (chunk, word) in
  if actual = expected then
    Printf.printf "correct answer(\"%s\",\"%s\"): got \"%s\"\n" chunk word expected
  else
    Printf.printf "wrong answer(\"%s\",\"%s\"): expected \"%s\", got \"%s\"\n"
      chunk word expected actual
;;

let tests () =
  List.map check
    [
     ("olclloo", "lo", "olc");
     ("lloo", "lo", "");
     ("aaaaa", "a", "");
     ("canaan", "a", "cnn");
     ("lolo", "lo", "");
     ("llolol", "lol", "");
     ("goodgooogoogfogoood", "goo", "dogfood");
     ("lololollol", "lolol", "");
     ("lololololo", "lol", "looo");
     ("ololololol", "olo", "llol");
     ("lololololololol", "lolol", "loloo");
     ("lllllloololololllollo", "llo", "lol");
     ("banana", "an", "ba");
(*
*)
 ]
;;

tests ()
