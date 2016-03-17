(* get legenshtein distance between two strings *)

(* minimum of 3 values *)
let min3 x y z =
  if (x > y) then
    if (y > z) then
      z
    else
      y
  else
    if (x > z) then
      z
    else
      x

(* get Levensthein's distance using recurssion *)
let distance s t =
   let rec dist i j = match (i,j) with
      | (i,0) -> i
      | (0,j) -> j
      | (i,j) ->
         if s.[i-1] = t.[j-1] then dist (i-1) (j-1)
         else let d1, d2, d3 = dist (i-1) j, dist i (j-1), dist (i-1) (j-1) in
         1 + min d1 (min d2 d3)
   in
   dist (String.length s) (String.length t)


(*
let distance s t =
  if s = t then 0
  else
   begin 
    if String.length s = 0 then String.length t
    else
      begin
      if String.length t = 0 then String.length s
      else
        begin
  let v0 = Array.make (String.length t + 1) 0 and
      v1 = Array.make (String.length t + 1) 0 in

  for i = 0 to Array.length v0  - 1 do
    v0.(i) <- i;
  done;

  for i = 0 to String.length s - 1 do
    v1.(0) <- i + 1;
    for j = 0 to String.length t - 1 do
      let cost = ref 0 in
      if s.[i] = t.[j] then
        cost := 1
      else
        cost := 0;
        v1.(j + 1) = min3 (v1.(j) + 1) (v0.(j + 1) + 1) (v0.(j) + !cost);
    done;

    for j = 0 to Array.length v0 - 1 do
      v0.(j) <- v1.(j);
    done;
  done;
  v1.(String.length t)
        end
      end
   end
   
   *)
