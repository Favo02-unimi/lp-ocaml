#load "str.cma";;

module PlaylingWithStrings =
  struct

    let filter_punctuation s = Str.global_replace (Str.regexp "[,.:;?!\ ]") "" (String.lowercase_ascii s)

    let is_palindrome str =
      let rec is_palindrome str =
        match String.length str with
          | 0 -> true
          | 1 -> true
          | _ -> if (str.[0] == str.[(String.length str)-1])
                  then (is_palindrome (String.sub str 1 ((String.length str)-2)))
                  else false
      in is_palindrome (filter_punctuation str)

    let anagram str dict =
      let sum_string str =
        let rec sum_string str sum i =
          match i with
            | 0 -> sum
            | _ -> sum_string str (sum + (int_of_char (Bytes.get (Bytes.of_string str) (i-1)))) (i-1)
        in sum_string str 0 (String.length str)
      in let check_anagram str word =
        if (((sum_string str) - (sum_string word)) == 0)
          then true
          else false
      in List.fold_left (fun res word -> res || (check_anagram str word)) false dict

    let (-) str filt =
      Str.global_replace (Str.regexp (String.concat "" ["["; filt; "]"])) "" str

  end;;

PlaylingWithStrings.is_palindrome "ciac";;
PlaylingWithStrings.is_palindrome "Do geese see God?";;
PlaylingWithStrings.is_palindrome "Rise to vote, sir.";;
PlaylingWithStrings.("Walter Cazzola" - "abcwxyz");;
PlaylingWithStrings.("Walter Cazzola" - "abcWxyz");;
PlaylingWithStrings.anagram "ciao" ["c"; "ooociao"; "acio"; "i"];;
PlaylingWithStrings.anagram "puffo" ["opff"; "opfffu"; "acio"; "i"];;
