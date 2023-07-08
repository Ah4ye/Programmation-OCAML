(*Exercice 1*)
(*Question 1*)

module type T234_SIG =
  sig
    type 'a t234_tree
    val t234_empty : unit -> 'a t234_tree
    val t234_rooting : 'a list* 'a t234_tree * 'a t234_tree* 'a t234_tree * 'a t234_tree -> 'a t234_tree
    val t234_subleft :'a t234_tree -> 'a t234_tree
    val t234_submidleft : 'a t234_tree -> 'a t234_tree
    val t234_submidright :'a t234_tree -> 'a t234_tree
    val t234_subright :'a t234_tree -> 'a t234_tree
    val left_val : 'a t234_tree -> 'a
    val mid_val : 'a t234_tree  -> 'a
    val right_val : 'a t234_tree -> 'a
    val t234_root : 'a t234_tree  -> 'a list
    val t234_isempty : 'a t234_tree -> bool
      
    val insere_list :  'a * 'a list  -> 'a list
    val tri_list :  'a list -> 'a list
    val count_list : 'a list -> int
    val append : 'a list* 'a list -> 'a list
    val print_tree : int t234_tree -> unit
      
    val search :  'a * 'a t234_tree -> bool
    val smallest :  'a t234_tree -> 'a
    val remove_smallest : 'a t234_tree -> 'a t234_tree
    
    val rotate : 'a t234_tree -> 'a t234_tree * 'a t234_tree
    val rotate_left : 'a t234_tree -> 'a t234_tree
    val rotate_right : 'a t234_tree -> 'a t234_tree
    val split_NOEUD4 :  'a t234_tree -> 'a t234_tree * 'a * 'a t234_tree
    val insert :  'a * 'a t234_tree ->  'a t234_tree
  end
;;

(*--------------------------------------------------*)


(*--------------------------------------------------*)

module T234_Sum : T234_SIG =
  struct
    type 'a t234_tree = LEAF
                       | NODE2  of 'a * 'a t234_tree * 'a t234_tree
                       | NODE3  of 'a * 'a * 'a t234_tree * 'a t234_tree * 'a t234_tree
                       | NODE4  of 'a * 'a * 'a *'a t234_tree*'a t234_tree*'a t234_tree*'a t234_tree
                          
                                    
     let t234_empty() = LEAF
             
     let t234_isempty( bt : 'a t234_tree ) : bool =
       match bt with
         LEAF -> true 
        |NODE2(_) -> false
        |NODE3(_) -> false
        |NODE4(_) -> false
                  
     let t234_root( bt : 'a t234_tree) : 'a list =
       match bt with
         LEAF -> failwith(" noeud vide/feuille ")
        |NODE2(x,_,_) -> [x]
        |NODE3(y1,y2,_,_,_) -> [y1;y2]
        |NODE4(z1,z2,z3,_,_,_,_) -> [z1;z2;z3]

     let rec insere_list ( e, l2 : 'a * 'a list): 'a list =
       match l2 with
         [] -> [e]
        |x::l2' -> if e < x
                   then e::l2
                   else x::(insere_list(e,l2'))
                 
     let rec tri_list( l1  : 'a list): 'a list =
       match l1 with
         [] -> []
        |x::l1' ->insere_list(x , tri_list(l1'))

     let rec count_list( l : 'a list) : int =
       match l with
         [] -> 0
       | _::b -> 1+count_list(b)

     let rec append( lista,listb : 'a list* 'a list) : 'a list =
       (* Fonction de concatenation *)
       match listb with
       | h :: taillist -> if taillist != []
                          then 
                            begin 
                              lista @ [h];
                            end else
                            append (lista ,taillist);
       | [] -> lista
             
     
     let rec print_tree( tree : int t234_tree) : unit  =
       (* Le print ne marche qu'avec des entiers *)
       match tree with
       | LEAF -> print_string "Leaf"
       | NODE2(value,left, right) ->
          print_string "[";
          print_int value ;
           print_string ", ";
           print_tree left;
          print_string ", ";
          print_tree right;
          print_string "]"
       | NODE3(value1,value2,left, middle, right) ->
          print_string "[";
          print_int value1;
          print_string ", ";
          print_int value2;
          print_string ", ";
          print_tree left;
          print_string ", ";
          print_tree middle;
          print_string ", ";
          print_tree right;
          print_string "]"
       | NODE4(value1, value2, value3, left, middle1, middle2, right) ->
          print_string "[";
          print_int value1;
          print_string ", ";
          print_int value2; 
          print_string ", ";
          print_int value3;
          print_string ", ";
          print_tree left;
          print_string ", ";
          print_tree middle1;
          print_string ", ";
          print_tree middle2;
          print_string ", ";
          print_tree right;
          print_string "]"


             
     let t234_rooting(valeurs,arbre1,arbre2,arbre3,arbre4 : 'a list* 'a t234_tree * 'a t234_tree * 'a t234_tree* 'a t234_tree) : 'a t234_tree =
       let valeurs_tri :'a list  = tri_list(valeurs) in
       match valeurs_tri  with
         [] -> LEAF 
        |[valeur1] -> NODE2(valeur1,arbre1,arbre2)
        |[valeur1;valeur2] -> NODE3(valeur1,valeur2,arbre1,arbre2,arbre3)
        |[valeur1;valeur2;valeur3] -> NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4)
        |_ -> failwith ("Erreur de type ou sur le nombre de valeur donné")
             
     let t234_subleft (tree :   'a t234_tree) : 'a t234_tree =
       match tree with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> arbre1
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> arbre1
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> arbre1

     let t234_submidleft (tree : 'a t234_tree) : 'a t234_tree =
       match tree with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> arbre2
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> arbre2
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> arbre2
                                                                     
     let t234_submidright (tree : 'a t234_tree) : 'a t234_tree =
       match tree with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> t234_empty()
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> arbre3
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> arbre3

     let t234_subright (tree : 'a t234_tree) : 'a t234_tree =
       match tree with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> t234_empty()
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) ->  t234_empty()
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> arbre4

     let left_val( l : 'a t234_tree) : 'a =
       match l with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> valeur1
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> valeur1
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> valeur1
                  
     let mid_val( l : 'a t234_tree) : 'a =
       match l with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> failwith("erreur")
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> valeur2
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> valeur2
                  
     let right_val( l : 'a t234_tree) : 'a =
       match l with
         LEAF -> failwith("erreur")
       | NODE2(valeur1,arbre1,arbre2)-> failwith("erreur")
       | NODE3(valeur1,valeur2,arbre1,arbre2,arbre3) -> failwith("erreur")
       | NODE4(valeur1,valeur2,valeur3,arbre1,arbre2,arbre3,arbre4) -> valeur3
                                                                     
     let rec search (value , tree  : 'a * 'a t234_tree) : bool  =
       match tree with
       | LEAF -> false
       | NODE2 (v,left, right) ->
          if value = v
          then true
          else if value < v
          then search(value,left)
          else search(value,right)
       | NODE3 (v1,v2,left, middle, right) ->
          if value = v1 || value = v2 then true
          else if value < v1 then search(value,left)
          else if value > v2 then search(value,right)
          else search(value,middle)
       | NODE4 (v1,v2,v3 ,left, middle1, middle2, right) ->
          if value = v1 || value = v2 || value = v3 then true
          else if value < v1 then search(value,left)
          else if value > v3 then search(value,right)
          else if value < v2 then search(value,middle1)
          else search(value ,middle2)

     let rec smallest( tree : 'a t234_tree) : 'a =
       match tree with
       (* Prend la plus petite valeur d'un arbre de recherche donc tout a gauche *)
       | LEAF -> failwith (" c'est une feuille ")
       | NODE2(x,LEAF, _) | NODE3(x,_, LEAF, _, _) | NODE4(x, _, _, LEAF, _, _, _) -> x
       | NODE2(_, l, _) | NODE3(_, _, l, _, _) | NODE4(_, _, _, l, _, _, _) -> smallest(l)

     let rec remove_smallest ( tree : 'a t234_tree) : 'a t234_tree =
       match tree with 
       | LEAF-> failwith (" c'est une feuille ")
       | NODE2(_, LEAF, r) -> r
       | NODE3(x,y, LEAF, m, r) -> NODE2(y, m, r)
       | NODE4(x, y, z, LEAF, m, n, r) -> NODE3(y, z, m, n, r)
       | NODE2(x, l, r) -> NODE2(x, remove_smallest(l), r)
       | NODE3(x, y,l, m, r) -> NODE3(x, y, remove_smallest(l), m, r)
       | NODE4(x, y, z, l, m, n, r) -> NODE4(x, y, z, remove_smallest(l), m, n, r)

     let rotate( tree : 'a t234_tree ) :  'a t234_tree * 'a t234_tree =
       match tree with
       | NODE2(_, l, r) -> (l, r)
       | NODE3(_, _, l, m, r) -> (NODE2(smallest(m), l, remove_smallest(m)), r)
       | NODE4(_, _, _, l, m1, m2, r) ->
          (NODE3(smallest(m1), smallest(m2),l ,remove_smallest(m1),remove_smallest(m2)), r)
       | LEAF -> failwith (" c'est une feuille ")


     let rec rotate_left( tree :  'a t234_tree ) :  'a t234_tree =
       match tree with
       | NODE2(x, left, NODE2(y, middle, right)) -> 
          NODE3(y, x, left, middle, right)
       | NODE3(x, y, left, NODE2(z, middle, right), right_right) ->
          NODE4(z, x, y, left, middle, right, right_right)
       | tree -> tree

     let rec rotate_right( tree :  'a t234_tree ) : 'a t234_tree =
       match tree with
       | NODE2(x, NODE2(y, left, middle), right) ->
          NODE3(y, x, left, middle, right)
       | NODE3(x, y, left_left, NODE2(z, left, middle), right) ->
          NODE4(z, x, y, left_left, left, middle, right)
       | tree -> tree


     let rec split_NOEUD4 ( tree : 'a t234_tree ) : 'a t234_tree* 'a * 'a t234_tree =
       match tree with
       | NODE4(y, z, w, left, middle1, middle2, right) ->
          let left = NODE3(y, z, left, middle1, middle2) in
          let right = NODE2(w, right, LEAF) in
          (left, y, right)
       | _ -> failwith "Not a NODE4"

     let rec insert( x , tree :  'a * 'a t234_tree) : 'a t234_tree  =
       match tree with 
       | LEAF -> NODE2(x, LEAF, LEAF)
       | NODE2(y, left, NODE2(z, middle, right)) ->
          rotate_left(NODE2(y, left, insert(x ,(NODE2(z, middle, right)))))
       | NODE3(y, z, left, NODE2(w, middle, right), right_right) ->
          rotate_left (NODE3(y, z, left, insert(x, (NODE2(w, middle, right))), right_right))
       | NODE2(y, NODE2(z, left, middle), right) ->
          rotate_right (NODE2(z, left, insert(x, (NODE2 (y, middle, right)))))
       | NODE3(y, z, left_left, NODE2(w, left, middle), right) ->
          rotate_right (NODE3( y, z, left_left, insert(x, (NODE2 (w, left, middle))), right))
       | NODE2(y, left, right) ->
          if x < y then
            let new_left = insert (x,left) in
            NODE2(y, new_left, right)
          else 
            let new_right = insert(x,right) in
            NODE2(y, left, new_right)
       | NODE3(y, z, left, middle, right)  ->
          if x < y then
            let new_left = insert (x,left) in
            NODE3(y, z, new_left, middle, right)
          else if x < z then
            let new_middle = insert(x,middle) in
            NODE3(y, z, left, new_middle, right)
          else
            let new_right = insert(x,right) in
            NODE3(y, z, left, middle, new_right)
       | NODE4(y, z, w, left, middle1, middle2, right) ->
          let (left, median, right) = split_NOEUD4(tree) in
          if x < median then 
            let new_left = insert(x,left) in
            NODE2(median, new_left, right)
          else 
            let new_right = insert(x,right) in
            NODE2(median, left, new_right)

     
     end
;;
(*--------------------------------------------------*)
open T234_Sum;;


(*Question 3 *)

let t1 = t234_rooting([1], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t2 = t234_rooting([3], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t3 = t234_rooting([6;7], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t4 = t234_rooting([10], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t5 = t234_rooting([13], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t6 = t234_rooting([18], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t7 = t234_rooting([20;21], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t8 = t234_rooting([25;28;31], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t9 = t234_rooting([37], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t10 = t234_rooting([42;50], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let t11 = t234_rooting([2;4], t1, t2, t3,t234_empty());;
let t12 = t234_rooting([12], t4, t5,t234_empty(),t234_empty());;
let t13 = t234_rooting([19;22], t6, t7, t8,t234_empty());;
let t14 = t234_rooting([39], t9, t10,t234_empty(),t234_empty());;
let t15 = t234_rooting([8;15;35], t11, t12, t13, t14);;

print_tree(t15);;

let tr1 = t234_rooting([2], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr2 = t234_rooting([5;7], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr3 = t234_rooting([8;10], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr4 = t234_rooting([13], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr5 = t234_rooting([21;25], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr6 = t234_rooting([30], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr7 = t234_rooting([33;37], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tr8 = t234_rooting([4;8;12],tr1,tr2,tr3,tr4);;
let tr9 = t234_rooting([28;32], tr5,tr6,tr7,t234_empty());;
let tr10 = t234_rooting([20],tr8,tr9,t234_empty(),t234_empty());;

let tree1 = t234_rooting([1;3;6], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree2 = t234_rooting([7;9], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree3 = t234_rooting([11], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree4 = t234_rooting([13], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree5 = t234_rooting([15], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree6 = t234_rooting([18;21], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree7 = t234_rooting([29], t234_empty(),t234_empty(),t234_empty(),t234_empty());;
let tree8 = t234_rooting([5;10],tree1,tree2,tree3,t234_empty());;
let tree9 = t234_rooting([14;16;25],tree4,tree5,tree6,tree7);;
let tree10 = t234_rooting([12], tree8,tree9,t234_empty(),t234_empty());;


(*Question 5*)

print_tree(insert(6,insert(11,insert(20,insert(40,insert(7,insert(12,insert(15,insert(30,insert(3,insert(13,insert(10,insert(35,insert(4,t234_empty()))))))))))))));;


(* Exercice 2*)
(*-----------------------DEBUT_MODULE--------------------------*)
(* Question 2*)

module type RbtSIG =
  sig
    type 'a t_rbtree
       and color
    val rbt_empty : unit -> 'a t_rbtree
    val black : unit -> color
    val red : unit -> color
    val rbt_rooting : color * 'a * 'a t_rbtree * 'a t_rbtree -> 'a t_rbtree
    val rbt_root : 'a t_rbtree -> 'a
    val rbt_color : 'a t_rbtree -> color
    val printcolor :color -> string 
    val rbt_subleft : 'a t_rbtree -> 'a t_rbtree
    val rbt_subright : 'a t_rbtree -> 'a t_rbtree
    val rbt_isempty : 'a t_rbtree -> bool
  end


module RbtSum : RbtSIG =
  struct
    type 'a t_rbtree = RBEMPTY | RT of color * 'a * 'a t_rbtree * 'a t_rbtree
     and color = RED | BLACK
    let rbt_empty() : 'a t_rbtree =
      RBEMPTY
    let black() : color=
      BLACK
    let red() : color=
      RED
    let rbt_rooting(c,n,rbt1,rbt2 : color * 'a * 'a t_rbtree * 'a t_rbtree ): 'a t_rbtree =
      RT(c,n,rbt1,rbt2)
    let rbt_root(rbt : 'a t_rbtree) : 'a =
      match rbt with
        RBEMPTY -> failwith "error arbre vide "
       |RT(_,n,_,_) -> n
    let rbt_color(rbt : 'a t_rbtree) : color =
      match rbt with
        RBEMPTY -> BLACK
      | RT(c,_,_,_) -> c
    let printcolor(c :color): string =
      match c with
        RED->"Rouge"
       |BLACK->"noir"          
    let rbt_subleft( rbt : 'a t_rbtree ) : 'a t_rbtree =
      match rbt with
        RBEMPTY -> failwith "error arbre vide"
       |RT(_,_,g,_) -> g
    let rbt_subright( rbt : 'a t_rbtree) : 'a t_rbtree =
       match rbt with
        RBEMPTY -> failwith "error arbre vide"
       |RT(_,_,_,d) -> d
    let rbt_isempty( rbt : 'a t_rbtree ) : bool =
      match rbt with
        RBEMPTY -> true
       |RT(_,_,_,_) -> false
  end 
;;

(*-----------------------FIN_MODULE-----------------------*)
open RbtSum;;
                    
let rb1=rbt_rooting(black(),3,rbt_empty(),rbt_empty());;
let rb2=rbt_rooting(black(),17,rbt_empty(),rbt_empty());;
let rb3=rbt_rooting(black(),66,rbt_empty(),rbt_empty());;
let rb4=rbt_rooting(black(),96,rbt_empty(),rbt_empty());;
let rb5=rbt_rooting(red(),14,rb1,rb2);;
let rb6=rbt_rooting(red(),69,rb3,rb4);;
let rb7=rbt_rooting(black(),26,rb1,rb3);;


let rbt1=rbt_rooting(black(),222,rbt_empty(),rbt_empty());;
let rbt2=rbt_rooting(black(),555,rbt_empty(),rbt_empty());;
let rbt3=rbt_rooting(black(),700,rbt_empty(),rbt_empty());;
let rbt4=rbt_rooting(black(),800,rbt_empty(),rbt_empty());;
let rbt5=rbt_rooting(black(),999,rbt_empty(),rbt_empty());;
let rbt6=rbt_rooting(black(),444,rbt1,rbt2);;
let rbt7=rbt_rooting(red(),777,rbt3,rbt4);;
let rbt8=rbt_rooting(black(),888,rbt7,rbt5);;
let rbt9=rbt_rooting(red(),444,rbt6,rbt8);;


let rt1=rbt_rooting(black(),1,rbt_empty(),rbt_empty());;
let rt2=rbt_rooting(black(),4,rbt_empty(),rbt_empty());;
let rt3=rbt_rooting(black(),11,rbt_empty(),rbt_empty());;
let rt4=rbt_rooting(red(),36,rbt_empty(),rbt_empty());;
let rt5=rbt_rooting(red(),42,rbt_empty(),rbt_empty());;
let rt6=rbt_rooting(black(),98,rbt_empty(),rbt_empty());;
let rt7=rbt_rooting(black(),38,rt4,rt5);;
let rt8=rbt_rooting(red(),34,rt3,rt7);;
let rt9=rbt_rooting(black(),3,rt1,rt2);;
let rt10=rbt_rooting(black(),45,rt8,rt6);;
let rt11=rbt_rooting(black(),5,rt9,rt10);;


(*Question 3 *)


let rb_balance(c,n,rbt1,rbt2 : color * 'a* 'a t_rbtree * 'a t_rbtree) : 'a t_rbtree =
  if (rbt_isempty(rbt_subleft(rbt1)) &&  rbt_isempty(rbt_subright(rbt1)) &&  rbt_isempty(rbt_subleft(rbt2)) &&  rbt_isempty(rbt_subright(rbt2))) || ((rbt_isempty(rbt1)) || ( rbt_isempty(rbt2)))
  then rbt_rooting(c, n, rbt1, rbt2)

  else if rbt_color(rbt1)=red() && rbt_color(rbt_subleft(rbt1))=red()
  then rbt_rooting(red(), rbt_root(rbt1), rbt_rooting(black(), rbt_root(rbt_subleft(rbt1)), rbt_subleft(rbt_subleft(rbt1)), rbt_subright(rbt_subleft(rbt1))), rbt_rooting(black(),n,rbt_subright(rbt1),rbt2))

  else if  rbt_color(rbt1)=red() && rbt_color(rbt_subright(rbt1))=red()
  then rbt_rooting(red(), rbt_root(rbt_subright(rbt1)), rbt_rooting(black(), rbt_root(rbt1), rbt_subleft(rbt1), rbt_subleft(rbt_subright(rbt1))), rbt_rooting(black(), n, rbt_subright(rbt_subright(rbt1)), rbt2))

  else if  rbt_color(rbt2)=red() && rbt_color(rbt_subleft(rbt2))=red()
       then rbt_rooting(red(), rbt_root(rbt_subleft(rbt2)), rbt_rooting(black(), n, rbt1, rbt_subleft(rbt_subleft(rbt2))), rbt_rooting(black(), rbt_root(rbt2),rbt_subright(rbt_subleft(rbt2)), rbt_subright(rbt2)))

  else rbt_rooting(rbt_color(rbt2),rbt_root(rbt2),rbt_rooting(c, n, rbt1, rbt_subleft(rbt_subright(rbt2))), rbt_rooting(black(), rbt_root(rbt_subright(rbt2)),rbt_subleft(rbt_subright(rbt2)), rbt_subright(rbt_subright(rbt2))))
;;

rbt_root(rb_balance(rbt_color(rb7), rbt_root(rb7), rbt_subleft(rb7), rbt_subright(rb7)));;


(*Question 4 *)


let rec rb_insert(rbt, v : 'a t_rbtree * 'a) : 'a t_rbtree =
    if rbt_isempty(rbt)
    then rbt_rooting(red(), v, rbt_empty(), rbt_empty())
    else if(v < rbt_root(rbt))
    then if rbt_color(rbt_subleft(rbt)) = red() && v < rbt_root(rbt_subleft(rbt)) && not(rbt_isempty(rbt_subleft(rbt_subleft(rbt))))
         then rb_balance(rbt_color(rbt), rbt_root(rbt), rbt_rooting(red(),rbt_root(rbt_subleft(rbt)), rbt_rooting(red(), v, rbt_empty(), rbt_empty()), rbt_subright(rbt_subleft(rbt))), rbt_subright(rbt)) 

         else if  rbt_color(rbt_subleft(rbt)) = red() && (v > rbt_root(rbt_subleft(rbt)) && not(rbt_isempty(rbt_subright(rbt_subleft(rbt)))))
         then  rb_balance(rbt_color(rbt), rbt_root(rbt), rbt_rooting(red(), rbt_root(rbt_subleft(rbt)), rbt_rooting(red(), v, rbt_empty(), rbt_empty()), rbt_subright(rbt_subleft(rbt))), rbt_subright(rbt))

         else rbt_rooting(black(), v, rb_insert(rbt_subleft(rbt), v), rbt_subright(rbt))

    else if v > rbt_root(rbt)
    then if  (rbt_color(rbt_subright(rbt)) = red() && (v < rbt_root(rbt_subright(rbt)) && not(rbt_isempty(rbt_subleft(rbt_subright(rbt))))))
         then rb_balance(rbt_color(rbt),rbt_root(rbt),rbt_subleft(rbt),rbt_rooting(red(),rbt_root(rbt_subright(rbt)),rbt_subleft(rbt_subright(rbt)), rbt_rooting(red(),v,rbt_empty(),rbt_empty())))

         else if (rbt_color(rbt_subright(rbt)) = red() && (v >rbt_root(rbt_subright(rbt)) && not(rbt_isempty(rbt_subright(rbt_subright(rbt))))))
         then rb_balance(rbt_color(rbt), rbt_root(rbt), rbt_subleft(rbt), rbt_rooting(red(), rbt_root(rbt_subright(rbt)),rbt_empty(),rbt_rooting(red(), v, rbt_empty(), rbt_empty())))
         else rbt_rooting(black(), v, rbt_subleft(rbt), rb_insert(rbt_subright(rbt), v))

    else failwith("valeur non correcte pour insertion")
;;


(*Question 5*)

printcolor(rbt_color(rbt_subleft(rbt_subleft(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rb_insert(rbt_empty(),4),35),10),13),3),30),15),12),7),40),20),11),6)))));;



(*Question 6 *)
    
let rec to_rb(t : 'a t234_tree) : 'a t_rbtree =
if t234_isempty(t)
then rbt_empty()
else if count_list(t234_root(t)) = 3
then rbt_rooting(black(),mid_val(t), rbt_rooting(red(), left_val(t), to_rb(t234_subleft(t)), to_rb(t234_submidleft(t))), rbt_rooting(red(), right_val(t), to_rb(t234_submidright(t)), to_rb(t234_subright(t))))

else if count_list(t234_root(t)) = 2
then  rbt_rooting(black(), mid_val(t), rbt_rooting(red(), left_val(t), to_rb(t234_subleft(t)), to_rb(t234_submidleft(t))), to_rb(t234_submidright(t)))

else  rbt_rooting(black(), left_val(t), to_rb(t234_submidleft(t)), to_rb(t234_submidright(t)))
;;

rbt_root(to_rb(t15));;


(*Question 7 *)

let rec to_234(rbt : 'a t_rbtree) : 'a t234_tree =
  if rbt_isempty(rbt)
  then t234_empty()
  else if rbt_isempty(rbt_subleft(rbt)) && rbt_isempty(rbt_subright(rbt))
  then t234_rooting(insere_list(rbt_root(rbt),[]), t234_empty(), t234_empty(), t234_empty(), t234_empty())

  else if rbt_isempty(rbt_subleft(rbt))
  then t234_rooting(insere_list(rbt_root(rbt),insere_list(rbt_root(rbt_subright(rbt)),[])), t234_empty(), t234_empty(), to_234(rbt_subleft(rbt_subright(rbt))), to_234(rbt_subright(rbt_subright(rbt))))

  else if rbt_isempty(rbt_subright(rbt))
  then t234_rooting(insere_list(rbt_root(rbt_subleft(rbt)),insere_list(rbt_root(rbt),[])), to_234(rbt_subleft(rbt_subleft(rbt))), to_234(rbt_subright(rbt_subleft(rbt))), t234_empty(), t234_empty())

  else t234_rooting(insere_list(rbt_root(rbt_subleft(rbt)),insere_list(rbt_root(rbt),insere_list(rbt_root(rbt_subright(rbt)),[]))), to_234(rbt_subleft(rbt_subleft(rbt))), to_234(rbt_subright(rbt_subleft(rbt))),  to_234(rbt_subleft(rbt_subright(rbt))), to_234(rbt_subright(rbt_subright(rbt))))
;;

t234_root(to_234(rb7));;



(* Exercice 3 *)


let rec taille_rb(rbt : 'a t_rbtree) : 'a =
  if rbt_isempty(rbt)
  then 0
  else 1 + taille_rb(rbt_subleft(rbt)) + taille_rb(rbt_subright(rbt))
;;

let rec arbre_min_rbt(rbt : 'a t_rbtree) : 'a t_rbtree=
  if rbt_isempty(rbt)
  then failwith("arbre vide")
  else if rbt_isempty(rbt_subleft(rbt))
  then rbt_rooting(rbt_color(rbt), rbt_root(rbt), rbt_empty(), rbt_subright(rbt))
  else arbre_min_rbt(rbt_subleft(rbt))(*cherche le pluspetit arbre du sous arbre droit*)
;;

let rec arbre_max_rbt(rbt : 'a t_rbtree) : 'a t_rbtree=
  if rbt_isempty(rbt)
  then failwith("arbre vide")
  else if rbt_isempty(rbt_subright(rbt))
  then rbt_rooting(rbt_color(rbt), rbt_root(rbt), rbt_subleft(rbt), rbt_empty())
  else arbre_max_rbt(rbt_subright(rbt))(*cherche le plus grand arbre du sous arbre gauche*)
;;

let tri_rb(rbt : 'a t_rbtree) :'a t_rbtree =
  let (rbt1, rbt2, n) : 'a t_rbtree * 'a t_rbtree * 'a = (rbt_subleft(rbt), rbt_subright(rbt), rbt_root(rbt)) in
  if rbt_isempty(rbt1) && rbt_isempty(rbt2)
  then rbt
  else if not(rbt_isempty(rbt1))
  then if rbt_color(rbt1) = black() && ((rbt_color(rbt_subleft(rbt1)) = red() && not(rbt_isempty(rbt_subleft(rbt1)))) || (rbt_color(rbt_subright(rbt1)) = red() && not(rbt_isempty(rbt_subright(rbt1)))))
       then rbt_rooting(rbt_color(rbt), rbt_root(rbt1), rbt_rooting(black(), rbt_root(rbt_subleft(rbt1)), rbt_subleft(rbt_subleft(rbt1)), rbt_subright(rbt_subleft(rbt1))), rbt_rooting(black(), n, rbt_subright(rbt1), rbt2))

       else if  rbt_color(rbt1) = red() && rbt_color(rbt) = black()
       then if rbt_isempty(rbt_subright(rbt1)) && not(rbt_isempty(rbt_subleft(rbt1)))
           then rbt_rooting(black(), rbt_root(rbt1), rbt_rooting(black(), rbt_root(rbt_subleft(rbt1)), rbt_subleft(rbt_subleft(rbt1)), rbt_subright(rbt_subleft(rbt1))), rbt_rooting(red(), n, rbt_subright(rbt1), rbt2))

           else rbt_rooting(black(), rbt_root(rbt1), rbt_rooting(black(), rbt_root(rbt_subright(rbt1)), rbt_subright(rbt_subleft(rbt1)), rbt_subleft(rbt_subleft(rbt1))),rbt_rooting(red(),n,rbt_subright(rbt1),rbt2))

       else if not(rbt_isempty(rbt2))
       then if  rbt_color(rbt2) = black() && ((rbt_color(rbt_subleft(rbt2)) = red() && not(rbt_isempty(rbt_subleft(rbt2)))) || (rbt_color(rbt_subright(rbt2)) = red() && not(rbt_isempty(rbt_subright(rbt2)))))

            then rbt_rooting(rbt_color(rbt), rbt_root(rbt2), rbt_rooting(black(), n, rbt1, rbt_subleft(rbt_subright(rbt2))), rbt_rooting(black(), rbt_root(rbt_subright(rbt2)), rbt_subleft(rbt_subright(rbt2)), rbt_subright(rbt_subright(rbt2))))

            else rbt_rooting(black(),rbt_root(rbt2),rbt_rooting(red(),n,rbt1,rbt_subleft(rbt_subright(rbt2))),rbt_rooting(black(),rbt_root(rbt_subright(rbt2)), rbt_subleft(rbt_subright(rbt2)), rbt_subright(rbt_subright(rbt2))))

       else  rbt_rooting(black(), rbt_root(rbt2), rbt_rooting(red(), n, rbt1, rbt_subleft(rbt_subright(rbt2))), rbt_rooting(black(), rbt_root(rbt_subleft(rbt2)), rbt_subleft(rbt_subright(rbt2)), rbt_subright(rbt_subright(rbt2))))
  else rbt
;;

    
let rec suppression_rb(rbt, v : 'a t_rbtree * 'a) : 'a t_rbtree =
  if rbt_isempty(rbt)
  then failwith("erreur valeur non présente dans l'arbre")
  else if v = rbt_root(rbt)
  then if rbt_isempty(rbt_subleft(rbt)) && rbt_isempty(rbt_subright(rbt))
       then rbt_empty()
       else if rbt_isempty(rbt_subleft(rbt)) 
       then tri_rb(rbt_rooting(rbt_color(rbt_subright(rbt)), rbt_root(rbt_subright(rbt)), rbt_subleft(rbt_subright(rbt)), rbt_subright(rbt_subright(rbt))))

       else if  rbt_isempty(rbt_subright(rbt)) 
       then tri_rb(rbt_rooting(rbt_color(rbt_subleft(rbt)), rbt_root(rbt_subleft(rbt)), rbt_subleft(rbt_subleft(rbt)), rbt_subright(rbt_subleft(rbt))))

       else if taille_rb(rbt_subleft(rbt)) >= taille_rb(rbt_subright(rbt))
            then let rb1: 'a t_rbtree = arbre_max_rbt(rbt_subleft(rbt)) in
                    rbt_rooting(rbt_color(rb1), rbt_root(rb1), suppression_rb(arbre_max_rbt(rbt_subleft(rbt)), rbt_root(rb1)), rbt_subright(rbt))
            else let rb2: 'a t_rbtree = arbre_min_rbt(rbt_subright(rbt)) in
                    rbt_rooting(rbt_color(rb2), rbt_root(rb2), rbt_subleft(rbt), suppression_rb(arbre_min_rbt(rbt_subright(rbt)), rbt_root(rb2)))

  else if v>rbt_root(rbt)
  then rbt_rooting(rbt_color(rbt), rbt_root(rbt),rbt_subleft(rbt), suppression_rb(rbt_subright(rbt), v))
  else rbt_rooting(rbt_color(rbt), rbt_root(rbt), suppression_rb(rbt_subleft(rbt), v), rbt_subright(rbt))
;;

rbt_root(suppression_rb(rt11,5));;
