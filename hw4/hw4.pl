/* Homework Assignment 4 - Prolog 2
   Programming Languages
   CS471, FALL 2016
   Binghamton University */

/* <h1>Instructions</h1> */

/* This section deals with general submission instructions. First, grab this assignment's <a href="hw4.asn">template source file</a>. BEFORE MOVING ON RENAME hw4.asn to hw4.pl. You will be able to code in and run the file in the Prolog interpreter directly. I recommend reading this assignment directly from the source file.

We will be using swipl for our Prolog environment: To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw4].

cd PATH_TO_FILE
swipl
[hw4].

From then on you may execute queries (goals) in the prompt. As usual, you should provide your answers in the designated spot. Once you have added some code to the file, rerun [hw4]. in the swipl prompt to reload. 

In addition, there are unit tests for each problem. These are there to help you better understand what the question asks for, as well as check your code. They are included in our "database" as queries and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the initial % for each test for the associated problem and reload the assignment file ([hw4].). Each SUCCEED line should silently load and succeed, and each FAIL line should throw a WARNING. If a SUCCEED line throws a WARNING, or a FAIL line fails to, then you solution is not correct. If you pass the tests there is a good chance that your code is correct, but not guaranteed; the tests are meant as guided feedback and are not a check for 100% correctness. */

/* <h1>Submission</h1> */

/* For this assignment -- and the remaining Prolog assignments -- you must submit only the source (template) file. There is no need to tar anything as all coding should be done directly in hw4.pl. */

/* <h1>Homework 4</h1> */

/* Due: Wednesday, 2/24 at 11:59 PM */

/* Purpose: To get comfortable with backtracking, become familar with reflective mechanism of Prolog, and Prolog as a symbolic programming language.
*/

my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]) :- my_append(Xs,Ys,Zs).

my_prefix(_,[]).
my_prefix([X|Xs], [X|Ys]) :- my_prefix(Xs,Ys).



/* <h3>Problem 0A (NOT GRADED):</h3> 
Using the preceding predicates, draw the execution trees for the following queries (goals). You should also enter the queries in swipl to test.

my_append([a,b],[c],Z).
my_append(Xs,[c],[a,b,c]).
my_append(Xs,Ys,[a,b,c]). 
my_prefix([a,b,c],Z).  

After drawing the execution trees, enable tracking on my_append and my_prefix by running (two separate queries)

trace(my_append). 
trace(my_prefix).

in swipl. Now, execute the above queries and try and match what you drew to the way the actual query is executed in swipl. */


/* <h3>Problem 0B (NOT GRADED):</h3>
Each line is an individual Prolog query; it's a good idea type them in your prompt (not the file itself) to get a feel for the way Prolog works. You should think about whether or not each query will succeed, and if so how the variables will be initialized (unified). It will help in doing some of the problems.

?- number(A), A = 5.6.
?- A = 5.6, number(A).
?- integer(4).
?- functor(foo(a,b,c),F,N).
?- functor(T,foo,3).
?- arg(3, foo(a,b,c),A).
?- T =.. [foo,x, y, z].
?- E =.. ['+',2,3], R is E.
?- foo(who, what) =.. T.
?- foo(who, what) =.. [A, B,C].
?- clause(ack(M,0,B),C).
?- clause(H,(B is 2*0)).

*/

my_member(X,[X|_]).
my_member(X,[_|Ys]) :- my_member(X,Ys).


/* <h3>Problem 1:</h3> 
We discussed the predicate findall/3 in class: findall(Object,Goal,List) will attempt to solve Goal and save all solutions that Object is unified too. For example, the query findall(X,my_member(X,[1,2,1]),Y) will solve my_member(X,[1,2,1]) for ALL solutions and store each unified X in a list Y.

Execute the following queries in swipl to gain a better understanding of backtracking. Respond to each resolution with a semicolon to tell Prolog to continue searching for solutions.

my_member(X,[1,2,1]).
my_member(1,[1,2,1]).
findall(X,my_member(X,[1,2,1]),Y).
my_append(Xs,Ys,[1,2,3,4]).
findall(Ys,my_append(Xs,Ys,[1,2,3,4]),Z).

Now, using the predicate my_prefix above and findall, write a predicate all_prefix(X,Y) which succeeds if Y is a list containing all the prefixes of X. */

/* <h3>Problem 1 Answer:</h3> */

all_prefix([],[]).
all_prefix(X1,Y1):-
	findall(K, my_prefix(X1, K), Y1).
	
	
	

/* <h3>Problem 1 Test:</h3> */
%:- all_prefix([1,2,3],[[],[1],[1,2],[1,2,3]]).  % SUCCEED
%:- all_prefix([a],[[],[a]]).                    % SUCCEED

%:- all_prefix([a,b],[[a]]).                     % FAIL


/* <h3>Problem 2</h3> 
We will use a predicate edge(X,Y) to encode a graph -- just like we did in class. edge(X,Y) is true if there is a directed edge from X to Y. The following is a mini graph encoded in Prolog. */
edge(a,b).
edge(a,e).
edge(a,c).
edge(b,a).
edge(b,c).
edge(b,d).
edge(c,e).
edge(f,e).

/* Using your knowledge of backtracking and the findall predicate, write predicates outgoing/2 and incoming/2.

outgoing(X,Y) should succeed if Y is a list of all immediate vertices reached from X's outgoing edges. incoming(X,Y) should succeed if Y is a list of all vertices that have outgoing edges to X.  */

/* <h3>Problem 2 Answer</h3> */

outgoing(_,[]).
outgoing(X2,R2):-
	findall(V,edge(X2,V),R2).

incoming([],_). 
incoming(X3,R3):-
	findall(V2,edge(V2,X3),R3).
	

/* <h3>Problem 2 Test</h3> */
%:- outgoing(a,X), X = [b,e,c].  %SUCCEED
%:- outgoing(e,X), X = [].       %SUCCEED
%:- outgoing(a,X), X = [b,e,c].  %SUCCEED
%:- incoming(a,X), X = [b].      %SUCCEED
%:- incoming(f,X), X = [].       %SUCCEED

%:- outgoing(e,X), X = [a].      %FAIL
%:- incoming(e,X), X = [].       %FAIL


/* <h3>Problem 3</h3>
In class we discussed the 'is' predicate for evaluating expressions.  Write a predicate results/2.  

result(Elst,RLst) succeed if Rlst is unifies with the values computed from the list of expressions, Elst.  */

/* <h3>Problem 3 Answer:</h3> */

result([],[]).
result([X|Xs],[Y|Ys]) :- result(Xs,Ys), Y is X.

/* <h3>Problem 3 Test</h3> */
%:- result([],[]).                                      %SUCCEED
%:- result([+(3,7), mod(104,7),-(5)],[10, 6, -5]).      %SUCCEED
%:- result([+(3,7), +(15, -(3,11))],X), X = [10, 7].    %SUCCEED 

%:- result([+(3,7), mod(104,7)],[10,13]).               %FAIL


/* <h3>Problem 4</h3> 
Write a predicate computeS/4. computeS(Op, Arg1, Arg2, Result) succeeds if Result is the value of 
Arg1 Op Arg2. Use the insight you gained in Problem 0B. Op must a builtin Prolog operator. */

/* <h3>Problem 4 Answer:</h3> */

computeS(+, Arg1, Arg2, Result):-
	Result is Arg1 + Arg2.
computeS(-, Arg1, Arg2, Result):-
	Result is Arg1 - Arg2.
computeS(/, Arg1, Arg2, Result):-
	Result is Arg1 / Arg2.
computeS(div, Arg1, Arg2, Result):-
	Result is Arg1 div Arg2.
computeS(*, Arg1, Arg2, Result):-
	Result is Arg1 * Arg2.
computeS(mod, Arg1, Arg2, Result):-
	Result is Arg1 mod Arg2.
computeS(rem, Arg1, Arg2, Result):-
	Result is Arg1 rem Arg2.
computeS(**, Arg1, Arg2, Result):-
	Result is Arg1 ** Arg2.
%computeS(sin, Arg1, Arg2, Result):-
	%Result is sin Arg1 sin Arg2.


/* <h3>Problem 4 Test:</h3> */
%:- computeS(-, 19, 7, 12).                            %SUCCEED  
%:- computeS(div, 19, 7, 2).                           %SUCCEED 
%:- computeS(/, 19, 7, R), R = 2.7142857142857144.     %SUCCEED 

%:- computeS(/, 19, 7, 2).                             %FAIL
%:- computeS(sin, 90, 1, R).                           % Gives error


/* <h3>Problem 5 </h3> 
A good example of symbolic computation is symbolic differentiation.  Below are the rules for symbolic differentiation where U, V are mathematical expressions, C is a number constant, N is an integer constant and x is a variable:

        dx/dx = 1
        d(C)/dx = 0.
        d(Cx)/dx = C          
        d(-U)/dx = -(dU/dx)
        d(U+V)/dx = dU/dx + dV/dx
        d(U-V)/dx = dU/dx - dV/dx
        d(U*V)/dx = U*(dV/dx) + V*(dU/dx)
        d(U^N)/dx = N*U^(N-1)*(dU/dx)

Translate these rules into Prolog. (Keep the order of the rules the same for testing  */

/* <h3>Problem 5 Answer:</h3> */

d(X,X,1).
d(C,X,0):- (C \= X).
d(C*X,X,C).

d(-U,X, -A):-
	d(U,X,A).	
d(U+V,X, A + B):-
	d(U,X,A),d(V,X,B).
d(U-V,X, A - B):-
	d(U,X,A),d(V,X,B).
d(U*V,X, A*V + B*U):-
	d(U,X,A),d(V,X,B).
d(U^N,X, N*U^(F)*A):-
	F is N - 1,
	d(U,X,A).

%doesn't fully work



/* <h3>Problem 5 Test:</h3> */
%:- d(x,x,R), R = 1 .                                                                   %SUCCEED 
%:- d(7*x,x,R), R = 7 .                                                                 %SUCCEED 
%:- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^2*1+ (x*1+x*1))+ (x^3+x*x)*0) .  %SUCCEED  
%:- d(-(1.24*x -x^3),x,Result), Result = - (1.24-3*x^2*1) .                             %SUCCEED
%:- d(-(1.24*x -2*x^3),x,Result), Result = - (1.24- (2* (3*x^2*1)+x^3*0)) .             %SUCCEED

% Pay careful attention to why this fails.
%:- d(x +2*(x^3 + x*x),x,Result), Result = 1+ (2* (3*x^(3-1)*1+ (x*1+x*1))+ (x^3+x*x)*0) .  %FAIL  


/* <h3>Problem 6 </h3>
In class we will discuss how Prolog can be used for solving puzzles.  We did a well know puzzle: "Send more money".  Each of the letters  D,E,M,N,O,R,S and Y represents a different digit. Moreover, when each letter is mapped to its corresponding digit the equation SEND + MORE = MONEY holds.

Our solution was very naive using Prolog ability to search for the solution. The search is very slow because 8 letters to be solved, it simply explore the 10*9*...*3 mappings of letters to digits.  

You can speed up the search by implementing the following insights:
    Clearly, SEND < 9999 and MORE < 9999. Thus MONEY < 19998 and hence M = 1. 
    Now we have SEND + 1ORE = 1ONEY. 
    Again SEND < 9999 and now 1ORE < 1999 so 1ONEY < 11998. Since M is already bound to 1, O must be bound to 0. 
    A little more thought shows that S must be bound to 8 or 9, and that N = E + 1. 

Using these insights to reduce the number of solutions that must be explored, write a Prolog predicate sendMoreMoney([D,E,M,N,O,R,S,Y]) that solves this puzzle by binding the correct digits to each of the variables in the list. */ 

/* <h3>Problem 6 Answer:</h3> */

select(X, [X|R], R).
select(X, [Y|Xs], [Y|Ys]):- select(X, Xs, Ys).

assign_digit([], _List).
assign_digit([D|Ds], List):-
        select(D, List, NewList),
        assign_digit(Ds, NewList).

sendMoreMoney([D,E,M,N,O,R,S,Y]):-
	X = [S,E,N,D,M,O,R,Y],
        Digits = [0,1,2,3,4,5,6,7,8,9],
        assign_digit(X, Digits),
        M = 1,
	O = 0,
        N is E + 1,
        SEND is 1000*S + 100*E + 10*N + D,
	MORE is 1000*M + 100*O + 10*R + E,
	MONEY is 10000*M + 1000*O + 100*N + 10*E + Y,
	MONEY is SEND + MORE.

/* <h3>Problem 6 Test:</h3> */

:- M = 1, sendMoreMoney( [D,E,M,N,O,R,S,Y]), M = 1, D = 7, E = 5, N = 6, O = 0, R = 8, S = 9, Y = 2.  % SUCCEEDS


/* <h3>Problem 7:</h3> 

In the assignment 3 problem 9 we defined a binary tree as follows.
"Binary trees are trees where all internal nodes have exactly two children. The smallest binary trees consist of only one leaf node. We will represent leaf nodes as leaf(Label). For instance, leaf(3) and leaf(7) are leaf nodes, and therefore small binary trees. Given two binary trees B1 and B2 we can combine them into one binary tree using the predicate tree: tree(B1,B2). So, from the leaves leaf(1) and leaf(2) we can build the binary tree tree(leaf(1), leaf(2)). And from the binary trees tree(leaf(1), leaf(2)) and leaf(4) we can build the binary tree tree(tree(leaf(1), leaf(2)), leaf(4))."

Using this definition of a binary tree, write a predicate equivT/2. equivT(Tree,ETree), success if Tree and ETree have the same structure and all the corresponding leaves have equivalent values. */

/* <h3>Problem 7 Answer:</h3> */

equivT(leaf(X), leaf(X)).
equivT(tree(L1,R1), tree(L2,R2)):-
	equivT(L1,L2), equivT(R1,R2).

/* <h3>Problem 7 Test:</h3> */
%:- equivT(leaf(2),leaf(2)).                                                             %SUCCEED 
%:- equivT(tree(tree(leaf(1), leaf(2)),leaf(3)), tree(tree(leaf(1), leaf(2)),leaf(3))).  %SUCCEED 

%:- equivT(tree(tree(leaf(1), leaf(2)),leaf(3)), tree(leaf(1), leaf(2))).               %FAIL
%:- equivT(tree(tree(leaf(1), leaf(2)),leaf(3)), tree(tree(leaf(1), leaf(2)),leaf(10))). %FAIL


/* <h3>Problem 8</h3> 
In class we discussed green and red cuts (!). A green cut is a cut that DOES NOT change correctness (the answer returned) but simply improves efficiency by preventing unnecessary backtracking. Red cuts change correctness -- if a predicate is correct and contains a cut that, when removed, is no longer correct, it is a red cut.

Insert cuts into the following 3 predicates. The first two my_member1/2 and my_max/3 are already correct, but using a cut (green) will improve their efficiency. The last, my_max1/3 is wrong, but inserting a cut (red) will make it correct.  */

/* <h3>Problem 8 Answer</h3> */
my_member1(X,[X|_]).
my_member1(X,[_|Ys]) :- my_member1(X,Ys),!.

my_max(X,Y,Y) :- X =< Y,!.
my_max(X,Y,X) :- X > Y.

my_max1(X,Y,Z) :- X =< Y,!, Y = Z.
my_max1(X,_,X).

/* <h3>Problem 8 Test</h3> */
% You're own your own for this one :) */

%:-my_member1(3, [3,5,2]). %SUCCEED
%:-my_member(1, [2,3]). %FAIL
%:-my_max(1,2,R), R = 2. %SUCCEED
%:-my_max1(1,2,R), R = 2. %SUCCEED

/* <h3>Problem 9</h3> 
Place a cut in the definition of my_prefix1 below so only following goal will only find the first non-empty prefix of the list. You must place a cut, do not modify my_prefix1 in another other way.

For example, the following is the correct behavior for the modified my_prefix1.
:- my_prefix([1,2,3],X).
   X = [] ;
   X = [1] ;
   fail. */

/* <h3>Problem 9 Answer </h3> */

my_prefix1(_,[]).
my_prefix1([X|Xs], [X|Ys]) :- my_prefix1(Xs,Ys),!.

/* <h3>Problem 9 Test </h3> */
% You're own your own for this one :) */

%:- my_prefix1([1,2,3],X), X = [1,2]. %FAIL
%:- my_prefix1([1,2,3],X), X = [1]. %SUCCEED

/* <h3>Problem 10 :</h3>
A) What is the mathematical definition of:
     a) relation
     b) function
B) Is every function a relation?  If false give a counter example.
C) Is every relation a function? If false give a counter example. */ 

/* <h3>Problem 10 Answer:</h3> */
/*
A)	a relation is a connection between elements in different sets
	a function is a relation between a set of inputs and a set of outputs
	where each input has a unique output
B)	every function is a relation because it maps from an input to an
	output value
C)	every relation is not necessarily a function. For example, y^2 = x
	is a relation but not a function because any given y will have 2 x
	values, positive and negative
*/
/* <h3>Problem 11 :</h3>
Define homoiconic. Is Prolog homoiconic? Briefly what does it mean to say a language is fully reflective? Is Prolog fully reflective? (See page 584 and Chapter 12) */

/* <h3>Problem 11 Answer:</h3> */
/*
a homoiconic language is a language where the program structure and
data look the same. Prolog is homoiconic.
A language is fully reflective if it allows a program to reason about
all aspects of its current structure and state. Prolog is not fully
reflective but it is reflective because it allows a program to reason
about its own structure.*/

/* <h3>Problem 0C (NOT GRADED):</h3> 
The following are examples from class; they will not be graded as part of this assignment, but you will be expected to understand them before the test. You should be able to solve them yourself and I highly recommend answering them.

0B.1. Write a predicate pack(X,Y) that succeeds if Y is the list X with all successive occurences of the same element element packed together...
pack([a,a,a,a,b,b,a,c,c,c],Y). will succeed with Y = [a,b,a,c]

0B.2. Write a predicate encode(X,Y) that succeeds if Y is the list X with all successive occurences of the same element elements packed together in [Occurence,Element] form..
encode([a,a,a,a,b,b,a,c,c,c],Y). will succeed with Y = [[4,a],[2,b],[1,a],[3,c]].

0B.3. Write a predicate decode(X,Y) that succeeds if Y is the list of encoded [Occurence,Element] decoded from X...
decode([[4,a],[2,b],[1,a],[3,c]],Y). will succeed with Y = [a,a,a,a,b,b,a,c,c,c]. */


