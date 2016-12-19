/* Homework Assignment 5
   Programming Languages
   CS471, Fall 2016
   Binghamton University */

/* <h1>Instructions</h1> */

/* This section deals with general submission instructions. First, grab this assignment's <a href="hw5.asn">template source file</a>. BEFORE MOVING ON RENAME hw5.asn to hw5.pl. You will be able to code in and run the file in the prolog interpreter directly. I recommend reading this assignment directly from the source file.

We will be using swipl for our prolog environment: To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw5].

cd PATH_TO_FILE
swipl
[hw5].

From then on you may execute queries (goals) in the prompt. As usual, you should provide your answers in the designated spot. Once you have added some code to the file, rerun [hw5]. in the swipl prompt to reload. 

In addition, there are unit tests for each problem. These are there to help you better understand what the question asks for, as well as check your code. They are included in our "database" as queries and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the initial % for each test for the associated problem and reload the assignment file ([hw5].). Each SUCCEED line should silently load and succeed, and each FAIL line should throw a WARNING. If a SUCCEED line throws a WARNING, or a FAIL line fails to, then you solution is not correct. If you pass the tests there is a good chance that your code is correct, but not guaranteed; the tests are meant as guided feedback and are not a check for 100% correctness. */

/* <h1>Submission</h1> */

/* For this assignment -- and the remaining Prolog assignments -- you must submit only the source (template) file. There is no need to tar anything as all coding should be done directly in hw5.pl. */

/* <h1>Homework 5</h1> */

/* Due: Next Wednesday, 11:59 PM */

/* Purpose: To get comfortable with unfication and pattern matching. */

/* <h3>Problem 0A (NOT GRADED):</h3>
Each line is an individual Prolog query; it's a good idea type them in your prompt (not the file itself) to get a feel for the way Prolog works. You should think about whether or not each query will succeed, and if so how the variables will be initialized (unified). You may find these helpful in solving some of the problems in this assignment.

?- S = 19, V = 3, C is S div V.   
?- S = 19, V = 3, C is S // V. 
?- S = 19, V = 3, C is S / V. 
?- A = zzz, atom(A).
?- N = 23, number(N).
?- A = 12, atom(A).
*/


/* <h3>Problem 1:</h3> 
In class we discussed that we can encode a binary search tree in Prolog using complex terms: i.e, the following BST

        5
       / \
      3   7
     / \
    1   4

can be encoded as node(5,node(3,node(1,nil,nil),node(4,nil,nil)),node(7,nil,nil)).

Write a predicate insert(X,Y,Z) that succeeds if Z is the tree Y with X inserted (insert X into Y). You may assume you have a binary search tree. */

/* <h3>Problem 1 Answer:</h3> */

insert(X,nil,node(X,nil,nil)).
insert(X,node(X,B,C),node(X,B,C)).
insert(X,node(A,B,C),Z):-
	X < A,
	insert(X,B,LTR),
	Z = node(A,LTR,C).

insert(X,node(A,B,C),Z):-
	X > A,
	insert(X,C,RTR),
	Z = node(A,B,RTR).

	

/* <h3>Problem 1 Test:</h3> */
%:- insert(5,node(5,nil,nil),X), X = node(5,nil,nil).                                                                            %SUCCEED
%:- insert(3,node(5,nil,nil),X), X = node(5,node(3,nil,nil),nil).                                                                %SUCCEED
%:- insert(7,node(5,nil,nil),X), X = node(5,nil,node(7,nil,nil)).                                                                %SUCCEED
%:- insert(1,node(5,node(3,nil,nil),node(7,nil,nil)),X), X = node(5,node(3,node(1,nil,nil),nil),node(7,nil,nil)).                 %SUCCEED
%:- insert(1,node(5,node(3,node(2,nil,nil),nil),node(7,nil,nil)),X), X = node(5,node(3,node(2,node(1,nil,nil),nil),nil),node(7,nil,nil)). %SUCCEED

%:- insert(3,node(5,node(3,node(2,nil,nil),nil),node(7,nil,nil)),X), X = node(5,node(3,node(2,node(3,nil,nil),nil)),node(7,nil,nil)). %FAIL


/* <h3>Problem 2:</h3> 
Using the same encoding for a binary search tree, write a predicate to_list(X,Y) that succeeds if Y is an in-order list of the elements of all the nodes of tree X (Y should show an inorder traversel of X). If you are rusty and do not remember what an inorder traversal is, give https://en.wikipedia.org/wiki/Tree_traversal#In-order a quick glance.

For example...
to_list(node(5,node(3,node(1,nil,nil),node(4,nil,nil)),node(7,nil,nil)),X) will succeed with X = [1,3,4,5,7]. */

/* <h3>Problem 2 Answer:</h3>  */

to_list(nil,[]).
to_list(node(X,Lt,Rt), Z):-
	to_list(Lt,Ltr),
	to_list(Rt,Rtr),
	append(Ltr, [X|Rtr], Z).

/* <h3>Problem 2 Tests:</h3>  */
%:- to_list(node(3,nil,nil),L), L = [3]. %SUCCEED
%:- to_list(node(5,node(3,nil,nil),nil),L), L = [3,5].  %SUCCEED
%:- to_list(node(5,node(3,node(1,nil,nil),node(4,nil,nil)),node(7,nil,nil)),L), L = [1,3,4,5,7]. %SUCCEED

%:- to_list(node(3,nil,nil),L), L = [5]. %FAIL

/* <h3>Problem 3:</h3> 
Write a predicate right_rotate(X,Y) that succeeds if Y is the tree X rotated right at its root. Read https://en.wikipedia.org/wiki/Tree_rotation to refresh tree rotation in full. This problem may seem hard at first, but once you "see" the answer it really demonstrates the elegance of unfication/pattern matching. You do not need to handle error cases.

For example, the following shows a right rotation at the root.

        5                        3
       / \                      / \
      3   7         -->        2   5
     / \                          / \
    2   4                        4   7


        5                        3
       / \                      / \
      3   7         -->        	   5
     / \                          / \
    	4                        4   7



	
*/

/* <h3>Problem 3 Answer:</h3> */

right_rotate( node(A, node(B,C,D), E), node(B,C,node(A,D,E))). 

/* <h3>Problem 3 Test:</h3> */

%:- right_rotate(node(5, node(3,node(2,nil,nil),node(4,nil,nil)), node(7,nil,nil)),X), X = node(3, node(2, nil, nil), node(5, node(4, nil, nil), node(7, nil, nil))). %SUCCEED
%:- right_rotate(node(5,node(3,nil,node(4,nil,nil)),node(7,nil,nil)),X), X = node(3, nil, node(5, node(4, nil, nil), node(7, nil, nil))). %SUCCEED
%:- right_rotate(node(3,node(2,node(1,nil,nil),nil),nil),X), right_rotate(X,Y), Y = node(1,nil,node(2,nil,node(3,nil,nil))). %SUCCEED

%:- right_rotate(node(5,nil,node(7,nil,nil)),_). %FAIL


/* <h3>Problem 4:</h3> 
Write a predicate drop_level(X,N,Y) that succeeds if Y is the tree X with all nodes "below" level N removed. We will consider the root node level 0.

For example, "dropping" at level 2 will result in the following.

        5                        5
       / \                      / \
      3   7         -->        3   7
     / \   \                   
    2   4   9                 

or

drop_level(node(5,node(3,node(2,nil,nil),node(4,nil,nil)),node(7,nil,node(9,nil,nil))),2,X) will succeed with X = node(5,node(3,nil,nil),node(7,nil,nil)).  */

/* <h3>Problem 4 Answer:</h3>  */

drop_level(_,0,nil).
drop_level(node(A,_,_),1,node(A,nil,nil)).
drop_level(node(A,B,C),N,node(A,BR,CR)):-
	drop_level(B, N1, BR), drop_level(C, N1, CR), N1 is N - 1.

/* <h3>Problem 4 Tests:</h3>  */
%:- drop_level(node(5,nil,nil),1,X), !, X = node(5,nil,nil).
%:- drop_level(node(5,nil,nil),0,X), !, X = nil.
%:- drop_level(node(5,node(3,node(2,nil,nil),node(4,nil,nil)),node(7,nil,node(9,nil,nil))),2,X), !, X = node(5,node(3,nil,nil),node(7,nil,nil)).

%:- drop_level(node(5,node(3,node(2,nil,nil),node(4,nil,nil)),node(7,nil,node(9,nil,nil))),1,X), !, X = node(5,node(3,nil,nil),node(7,nil,nil)).


/* <h3>Problem 5:</h3> 
We will encode a mini-AST in Prolog using complex data structures. A "node" will consist of either a nb(Functor,LeftExpr,RightExpr), nu(Functor,Expr) or nn(Number). 

nb(Functor,LeftExpr,RightExpr) -- "node binary", Functor is guaranteed to be a binary arithmatic predicate that can be evaluated with `is`. LeftExpr and RightExpr are recursively defined "nodes".

nu(Functor,Expr) -- "node unary", Functor is guaranteed to be a unary arithmatic predicate that can be evaluated with `is`. Expr is a recursively defined "node".

nn(Number) -- "node number", Number is guaranteed to be just that.

Hence, the following AST
      +
     / \
    *   random
   / \       \ 
  2  3        5
would be encoded as nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))). 

Write a predicate run(X,Y) that succeeds if Y is the result obtained from "running" (evaluating) X. You will need to use the =.. predicate. It may be helped to view some of the binary and unary arithmetic predicates -- http://www.swi-prolog.org/man/arith.html. If you write your predicate correctly, you do not need to concern yourself with the actual arithmetic functor supplied in the nodes. */

/* <h3>Problem 5 Answer:</h3> */

run(nn(X),Y):-
	Y is X.
run(nb(F,nn(Le),nn(Re)),Y):-
	Y is op(Le, Re, F).
run(nu(F,nn(Ex)), Y):-
	Y is op(Ex, F).



	

/* <h3>Problem 5 Tests:</h3> */
:- run(nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))),_).
:- run(nb(+,nb(*,nn(2),nn(3)),nn(3)),E), E=9.
:- run(nb(+,nb(*,nn(2),nn(3)),nb(-,nn(6),nn(3))),E), E=9.
:- run(nn(2),E), E=2.
:- run(nu(abs,nn(-2)),E), E=2.

:- run(nb(+,nb(*,nn(2),nn(3)),nb(-,nn(6),nn(3))),E), E=8.


/* <h3>Problem 6:</h3> 
Using the AST described in problem 5, write a predicate binaryAP/2.  binaryAP(AST, BPlst) succeeds if all the binary arithmetic predicates that occur in AST are collected into BPlst.  Use an inorder traversal of AST.  */

/* <h3>Problem 6 Answer:</h3> */

%binaryAP(AST, BPlst)=..BPlst.

/* <h3>Problem 6 Tests:</h3> */
%:- T = nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))), binaryAP(T,L), L = [*, +].  %SUCCEED
%:- T = nb(+, nb(*, nn(2), nn(3)), nb(-,nn(3), nn(5))),  binaryAP(T,L), L = [*, +, -]. %SUCCEED
%:- T = nb(+, nb(*, nn(2),  nb(-,nn(3), nb(//, nn(2), nn(5)))),nn(9)) ,  binaryAP(T,L), L = [*, -, //, +]. %SUCCEED

%:- T = nb(+,nb(*,nn(2),nn(3)),nu(random,nn(5))), binaryAP(T,L), L = [+,*].      %FAIL
%


/* <h3>Problem 7:</h3> 

Write a predicate noNestedLst/2.  noNestedLst(+NestedLists, -C) succeeds if all atoms and numbers in the NestedLists concatenated into a list C.  The list C should NOT contain any list/nested lists. The atoms/numbers should appear in the same order in both lists.

This can be done in only 3 or 4 clauses.... think What NOT how.  */ 

/* <h3>Problem 7 Answer:</h3> */

noNestedLst([],[]) :-!.
noNestedLst([H|T], R):-
	!,
	noNestedLst(H,H1),
	noNestedLst(T,T1),
	append(H1,T1,R).
noNestedLst(H, [H]).



/* <h3>Problem 7 Tests:</h3> */
%:- noNestedLst([[1],[a,b,c],[],[100,91]],C),  C = [1, a, b, c, 100, 91] .               %SUCCEED
%:- noNestedLst([[1],[a,[b,[c]],[]], [[[100],91],x] ],C), C = [1, a, b, c, 100, 91, x] . %SUCCEED

%:- noNestedLst([[[]],[a]],[[],a]).                                                      %FAIL


/* <h3>Problem 8:</h3> 

Write a predicate change/2 that given the change amount, computes the way in which exact change can be given. Use the following USA's coin facts at your solution. */
      
coin(dollar, 100).
coin(half, 50).
coin(quarter, 25).
coin(dime,10).
coin(nickel,5).
coin(penny,1).

/* The predicate change(S,CL) succeeds if given a positive integer S, CL is a list of tuples that contains the name of the coin and the number of coins needed to return the correct change.

The Coin Changing problem is an interesting problem usually studied in Algorthms.  
http://condor.depaul.edu/~rjohnson/algorithm/coins.pdf is a nice discussion.
Think about (no need to turn in)
   1) How could we generalize this problem to handle coins from other currencys?
   2) What are the different techinques to find the change with the fewest number of coins ?
   3) What happens if the order of the "coin" facts change?  */

/* <h3>Problem 8 Answer:</h3> */


change(0,[]).	

change(X, [(Y, Xnum)|L]) :- 
	coin(Y, VAL),
	X >= VAL, 
	Xnum is floor(X / VAL), 
	REM is X mod VAL, 
	change(REM, L).
	



/* <h3>Problem 8 Tests:</h3> */
%:- change(168,C), C = [ (dollar, 1), (half, 1), (dime, 1), (nickel, 1), (penny, 3)] .  %SUCCEED
%:- change(75,C),  C = [ (half, 1), (quarter, 1)] .                                     %SUCCEED

%:- change(75,C), C = [(half, 2)].                                                      %FAIL                             


/* <h3>Problem 9:</h3> 

In class we discussed difference lists and how to append two of them in "constant" time. 

Write a predicate, append3DL(A,B,C,D) that succeeds if D is the difference lists A, B, and C appended.
*/

/* <h3>Problem 9 Answer:</h3> */

%appendDL(R-M,M-N,R-N).
append3DL(R-B,B-C,C-M,R-M).	

/* <h3>Problem 9 Tests:</h3> */
%:- append3DL([1,2|A]-A,[3,4|B]-B,[5,6|[]]-[],L), L = [1,2,3,4,5,6]-[]. % SUCCEED
%:- append3DL([a,b|A]-A,[b,1,2|B]-B,[3|C]-C,L), L = [a, b, b, 1, 2, 3|C]-C. % SUCCEED


%:- append3DL([1,2|A]-A,[3,4|B]-B,[5,6|[]]-[],L), L = [1,2,3,4,5]-[].   % FAIL
