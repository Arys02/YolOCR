type 'a g_tree =
  |Sons of 'a * ('a g_tree)list 

let init key = Sons(key,[])

let add_son x = function
  |Sons(k,l) -> Sons(k,l@x)

let rec map_tree f = function
   Sons(k,[]) -> Sons(f k, [])
  |Sons(k,l) -> match l with
      [] -> Sons(k,l)
     |e::l -> map_tree f e

 
