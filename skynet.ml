(*****************************************************************************************)
(*                                                 Classes                               *)
(*****************************************************************************************)

class neuron nb_weights =
object
  val mutable value  = 0.  
  val mutable activated = false
  val mutable weight = Array.make (nb_weights) 0.
  val mutable nbweights = nb_weights



  (* Renvoie la sortie du neurone *)
  method get_output i  = weight.(i) *. value
  method get_weight i = weight.(i) 
  method get_Nbweights = nbweights
  method is_activated = activated  
  method set_value e = value <- e
  method set_weight i w = weight.(i) <- w 
  method set_activation bool_val = activated <- bool_val
  method randomly_fill_neurons = Array.init (nb_weights) (fun _ -> (Random.float 1. -. 0.5))
end


(*******************)


class layer nb_w nb_n = 
object (s)
  val mutable neurons = Array.make (nb_n) ( new neuron nb_w)

  method print = 
    begin 
      let nbw = neurons.(0)#get_Nbweights in
	for i = 0 to Array.length neurons do
	  for j = 0 to nbw do
	    print_float ( neurons.(i)#get_output(j))
	  done;
	  done ;
    end


method sigmoide x = 1./.(exp( (float_of_int (-x))) +. 1.)

method activation sum = match s#sigmoide sum with
  | x when x >= 0.7 -> 1
  | x when x < 0.7-> -1
end
   
(*****************************************************************************************)
(*                                     Méthodes                                          *)
(*****************************************************************************************)


let sum layer int neuron =
begin 
  let res = ref 0 in
  for i = 0 to Array.length layer#neurons do 
    res :=  ( !res + layer#neurons.(i)#get_output(int))
  done;
  !res;
end


let big_array_to_array big_array =
begin
  let i =  (Matrix.width big_array) *( Matrix.height big_array) in
  let  array = ref ( Array.make i 0.) in 
  let  _int = ref 0 in 
  for j = 0 to  Matrix.width big_array do
    for k = 0 to Matrix.height big_array do
      !array.(!_int) <- Matrix.get big_array j k ;
      _int := !_int  + 1;
    done
  done
end


(*let network _ =
  let first_layer = new layer 10 10 in
  let hidden_layer = new layer 10 1 in
  *)
