let rec langl (w : char list) : bool = match w with
| [] -> false
| ['0'] -> true
| ['1'] -> true
| '0'::ls -> langl ls
| '1' :: ls -> langl ls
| _ -> false;;

let lang1 _ = failwith ""

let lang2 _ = failwith ""

let lang3 _ = failwith ""

let lang4 _ = failwith ""

let lang5 _ = failwith ""
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
