(* 
** Nathan Quirk
** ML Project - Binary Trees
** CSC 345
** Submitted 12.11.18
*)


(* DATATYPES *)
datatype 'a BT = empty | bTree of 'a * 'a BT * 'a BT ;

datatype X = A|B|C|D|E|F|G|H;


(* TRAVERSALS *)
fun preOrder  (empty) = nil
  | preOrder  (bTree(node, left, right)) = [node] @ preOrder(left) @ preOrder(right);

fun inOrder   (empty) = nil
  | inOrder   (bTree(node, left, right)) = inOrder(left) @ [node] @ inOrder(right);

fun postOrder (empty) = nil
  | postOrder (bTree(node, left, right)) = postOrder(left) @ postOrder(right) @ [node];


(* PRINT FUNCTIONS *)
fun printInt n = print(Int.toString n);

fun printReal n = print(Real.toString n);

fun printX A = print "A"
  | printX B = print "B"
  | printX C = print "C"
  | printX D = print "D"
  | printX E = print "E"
  | printX F = print "F"
  | printX G = print "G"
  | printX H = print "H" ;


(* DISPLAY FUNCTIONS *)
fun tab (0)   = print "  "
  | tab (num) = ((print "  "); tab(num - 1));

fun dash (num) = (tab num; print "-\n");

fun displayNode (node, num, prnt) = (tab num; prnt node; print "\n");

fun displayTreeIndent (empty, num,prnt) = dash(num + 1)
				  
  | displayTreeIndent (bTree(node,empty,empty), num, prnt) = (displayNode(node, num + 1, prnt))
								   
  | displayTreeIndent (bTree(node,left,right),  num, prnt) = (displayNode(node, num + 1, prnt);
							 displayTreeIndent(left, num + 1,prnt);
							 displayTreeIndent(right,num + 1,prnt));

fun displayTree (empty, _) = print "empty tree\n"
  | displayTree ((bTree(node, left,  right)), prnt) = (displayNode(node, 0, prnt);
						displayTreeIndent(left,  0, prnt);
						displayTreeIndent(right, 0, prnt));


(* TEST DATA *)
val t1 = bTree(1,
           bTree(2,bTree(3,empty, empty), bTree(4,empty, empty)),
           bTree(5,bTree(6,empty, empty),bTree(7,empty, empty)));

val t2 = bTree(A,
           bTree(B,bTree(D,empty, empty), bTree(E,empty, empty)),
           bTree(C,bTree(F,empty, empty),bTree(G,empty, empty)));

val t3 = bTree(1.22, 
           bTree(2.33,empty, empty), bTree(3.44,empty,empty));

val t4 = bTree("A",
           bTree("B",
                     bTree("C",bTree("E",empty,empty), empty),
                     bTree("D",
                           bTree("F",empty,empty),
                           bTree("G",bTree("H",empty,empty), empty))),
           bTree("I",
                     bTree("J",empty, bTree("K",
                        bTree("L",empty, empty),
                        bTree("M",empty, empty))),
                     empty));


