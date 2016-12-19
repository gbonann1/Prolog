/* Homework Assignment 3
   Programming Languages
   CS471, Fall 2016
   Binghamton University */

/* <h1>Instructions</h1> */

/* This section deals with general submission instructions. First, grab this assignment's <a href="hw3.asn">template source file</a>. BEFORE MOVING ON RENAME hw3.asn to hw3.pl. You will be able to code in and run the file in the prolog interpreter directly. I recommend reading this assignment directly from the source file.

We will be using swipl for our prolog environment: To load/reload this file, cd to its directory and run swipl. Then, in the prompt, type [hw3].

cd PATH_TO_FILE
swipl
[hw3].

From then on you may execute queries (goals) in the prompt. As usual, you should provide your answers in the designated spot. Once you have added some code to the file, rerun [hw3]. in the swipl prompt to reload. 

In addition, there are unit tests for each problem. These are there to help you better understand what the question asks for, as well as check your code. They are included in our "database" as queries and are initially commented out -- % is a Prolog line comment.

%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

After you have finished a problem and are ready to test, remove the initial % for each test for the associated problem and reload the assignment file ([hw3].). Each SUCCEED line should silently load and succeed, and each FAIL line should throw a WARNING. If a SUCCEED line throws a WARNING, or a FAIL line fails to, then you solution is not correct. If you pass the tests there is a good chance that your code is correct, but not guaranteed; the tests are meant as guided feedback and are not a check for 100% correctness. */

/* <h1>Submission</h1> */

/* For this assignment -- and the remaing Prolog assignments -- you must submit only the source (template) file. There is no need to tar anything as all coding should be done directly in hw3.pl. */

/* <h1>Homework 3</h1> */

/* Due: Next Wednesday, 11:59 PM */

/* Purpose: To get comfortable with Logic programming, and get a good grasp on list manipulation in Prolog. */

/* <h3>Problem 0A (NOT GRADED):</h3> 
The following is a mini-database you should use to answer the question.  */
happy(yolanda). 
happy(mia). 
young(john). 

listens_to_music(X) :- happy(X). 
listens_to_music(X) :- young(X).  

/* 1. Name the clauses, predicates, rules, and facts.
2. Name the constants, variables, and complex structures. */

/* <h3>Problem 0B (NOT GRADED):</h3>
Each line is an individual Prolog query; it's a good idea type them in your prompt (not the file itself) to get a feel for the way Prolog works. You should think about whether or not each query will succeed, and if so how the variables will be initialized (unified). You can expect these sort of questions on the test.

X = 1+2.
X is 1+2.
1+2=1+2.
1+2=2+1.
1+2=3.
1+2 is 3.
3 is 1+2.
3 =< 6.
6 =< 3.
X = Y, Y = Z, Z = 3.
X = Y, Y = Z, Z = 3, X = 4.
T = (X,Y), X = a, Y = b.
X = [1,2,3], [H|T] = X.
X = [1,2,3], [H1,H2|T] = X.
X = [1], [H|T] = X.
X = [1], Y = [2|X].


help(member).
X = 3, member(X, [Y]).
*/

/* <h3>Problem 1:</h3> 
The following are two basic predicates for list manipulation: my_first/2 and my_last/2. We may refer to a predicate by writings it as name/arity; hence, my_first/2 means a predicate named my_first with two arguments. 

my_first(X,Y) succeeds if X is the first element of list Y. my_last(X,Y) succeeds if X is the last element of list Y. */

my_first(X,[X|_]).

my_last(X,[X]).
my_last(X,[_|Ys]) :- my_last(X,Ys).

/* Note my use of the _ (wildcard). It is used to say to Prolog "I don't care, match anything" and should be used to avoid singleton warnings (a variable that is not unified). We will discuss this during lab.

Try the following queries before moving on. Note that they are included in the comment section for a reason: They would be interpreted as facts in the database otherwise. 

my_first(X,[3,a,dd]).
my_last(X,[3,a,dd]).
my_first(3,[3,a,dd]).
my_first(a,[3,a,dd]).
my_last(dd,[3,a,dd]).
my_last(a,[3,a,dd]).

Now, write a predicate my_member(X,Y) that succeeds if X is a member of the list Y. 

NOTE: my_first/2 and my_last/2 are provided as examples for manipulating lists. You should not use them in definition of my_member/2. */

/* <h3>Problem 1 Answer:</h3> */

my_member(X, [X|_]).
my_member(X, [_|T]):- my_member(X,T).
/* <h3>Problem 1 Test:</h3> */

%:- my_member(3,[1,2,3]).     % SUCCEED
%:- my_member(3,[3]).         % SUCCEED
%:- my_member(4,[1,2,3]).     % FAIL

/* <h3>Problem 2:</h3> 
Write a predicate member_times(X,Y,Z) that succeeds if X is a member of list Y, Z times (Y contains exactly Z copies of X). */

/* <h3>Problem 2 Answer:</h3> */

member_times(_, [], 0).
member_times(X1, [X1|T1], Z):-
	member_times(X1, T1, Y) , Z is Y + 1.
member_times(X1, [_|T1], Z):-
	member_times(X1,T1,Z).
	
/* <h3>Problem 2 Test:</h3> */

%:- member_times(3,[1,2,3],1).   % SUCCEED
%:- member_times(3,[3,3,2,3],3). % SUCCEED
%:- member_times(4,[3,3,2,3],0). % SUCCEED
%:- member_times(4,[1,2,3],3).   % FAIL

/* <h3>Problem 3:</h3> 
Write a predicate is_decreasing(X) that succeeds if X is a list of decreasing numbers -- Each number is either the same or lower than the preceding number. 

NOTE: You may match two elements at a time against a list: [X,Y|Xs] = List. It's preferable to do it in the rule head however...
some_rule([X,Y|Xs]) :- ...  */

/* <h3>Problem 3 Answer:</h3> */

is_decreasing([]).
is_decreasing([_]).
is_decreasing([X2,Y2|T2]):-
	X2 >= Y2, is_decreasing([Y2|T2]).

/* <h3>Problem 3 Test:</h3> */
%:- is_decreasing([]).            % SUCCEED
%:- is_decreasing([10]).          % SUCCEED
%:- is_decreasing([10,9]).        % SUCCEED
%:- is_decreasing([10,9,7]).      % SUCCEED
%:- is_decreasing([10,9,7,7,2]).  % SUCCEED
%:- is_decreasing([1,1,1,1,1]).   % SUCCEED

%:- is_decreasing([10,9,7,9]).    % FAIL
%:- is_decreasing([2,3,1]).       % FAIL
%:- is_decreasing([1,2,3]).       % FAIL
%:- is_decreasing([7,19]).        % FAIL

/* <h3>Problem 4:</h3>
Write a predicate element_at(X,Y,N) that succeeds if X is the Nth element of list Y. Y is 0-indexed.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* <h3>Problem 4 Answer:</h3> */

element_at(X3, [X3], 0).
element_at(X3, [_|T3], N):-
	element_at(X3, T3, N1), N is N1 + 1.

/* <h3>Problem 4 Test:</h3> */
%:- element_at(3,[1,2,3],2).   % SUCCEED
%:- element_at(1,[1,2,3],0).   % SUCCEED

%:- element_at(1,[1,2,3],1).     % FAIL

/* <h3>Problem 5:</h3>
Write a predicate insert_at(X,Y,N,Z) that succeeds if Z is the list Y with X inserted at index N -- Insert X at index N in Y.

NOTE: Don't worry about the error cases: i.e, N greater than the length of Y.  */

/* <h3>Problem 5 Answer:</h3> */

insert_at(Y4, X4, 1, [X4|Y4]).
insert_at(Y4, X4, 0, [Y4|X4]).
insert_at(Y4, [H4|T4], N4, [H4|R4]):-
	N5 is N4 - 1,
	insert_at(Y4, T4, N5, R4).

/* <h3>Problem 5 Test:</h3> */
%:- insert_at(3,[1,2,3],2,[1,2,3,3]).  % SUCCEED
%:- insert_at(1,[1,2,3],0,[1,1,2,3]).  % SUCCEED
%:- insert_at(a,[1,2,3],1,[1,a,2,3]).  % SUCCEED

%:- insert_at(1,[1,2,3],0,[1,2,3]).    % FAIL

/* <h3>Problem 6:</h3>
Write a predicate zip(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together. 

For example...
zip([1,2,3],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip([1],[a],Zs) should give Zs = [(1,a)]

NOTE: You may assume X and Y have the same length. */

/* <h3>Problem 6 Answer:</h3> */

zip([],[],[]).
zip([X6|Xs], [Y6|Ys], [(X6,Y6)|Zs]):-
	zip(Xs,Ys,Zs).

/* <h3>Problem 6 Test:</h3> */
%:- zip([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip([],[],[]).                      % SUCCEED
%:- zip([1],[2],[(1,2)]).               % SUCCEED

%:- zip([1],[2],[(2,3)]).               % FAIL
%:- zip([1],[2,3],[(1,2)]).             % FAIL

/* <h3>Problem 7:</h3>

Write a predicate zip2(Xs,Ys,Zs) that succeeds if Zs is a list where each element is a tuple, (X,Y), with Xs and Ys paired together. However, the length of Zs will be equal to the length of Xs or Ys which ever is less.

For example...
zip2([1,2,3,4],[a,b,c],Zs) should give Zs = [(1,a),(2,b),(3,c)]
zip2([1],[a,b],Zs) should give Zs = [(1,a)] */

/* <h3>Problem 7 Answer:</h3> */


zip2([],_,[]).
zip2(_,[],[]).
zip2([X6|Xs], [Y6|Ys], [(X6,Y6)|Zs]):-
	zip2(Xs,Ys,Zs).

/* <h3>Problem 7 Test:</h3> */
%:- zip2([1,2,3],[a,b,c],[(1,a),(2,b),(3,c)]). % SUCCEED
%:- zip2([],[a,b,c],[]).                  % SUCCEED
%:- zip2([1,3],[],[]).                    % SUCCEED
%:- zip2([1,3],[2],[(1,2)]).              % SUCCEED

%:- zip2([1],[2],[(2,3)]).                 % FAIL
%:- zip2([1],[a,b],[(1,a),(1,b)]).         % FAIL

/* <h3>Problem 8:</h3>
Write a predicate merge(A,B,M) that succeed if the list M has all the items from lists A and B in decreasing order.  Assume that A and B are sorted in decreasing order.  Items do not need to be unique.

For example:
merge([10,3,2], [11,5,2], M) should give M =[11,10,5,3,2,2].

 */

/* <h3>Problem 8 Answer:</h3> */

merge([],[],[]).
merge([],B,B).
merge(A,[],A).
merge([H8|T8], [H9|T9], [_|R8]):-
	H8 > H9, merge(T8, [H9|T9], R8).
merge([H8|T8], [_|T9], [_|R8]):-
	merge([H8|T8], T9, R8).


/* <h3>Problem 8 Test:</h3> */
%:- merge([10,3,2],[11,5,2],[11,10,5,3,2,2]) .       % SUCCEED
%:- merge([0],[],[0]).                               % SUCCEED
%:- merge([],[3],[3]).                               % SUCCEED

%:- merge([4,3],[3],[3]).                            % FAIL

/* <h3>Problem 9:</h3>
(From Learn Prolog NOW!) Binary trees are trees where all internal nodes have exactly two children. The smallest binary trees consist of only one leaf node. We will represent leaf nodes as leaf(Label). For instance, leaf(3) and leaf(7) are leaf nodes, and therefore small binary trees. Given two binary trees B1 and B2 we can combine them into one binary tree using the predicate tree: tree(B1,B2). So, from the leaves leaf(1) and leaf(2) we can build the binary tree tree(leaf(1), leaf(2)). And from the binary trees tree(leaf(1), leaf(2)) and leaf(4) we can build the binary tree tree(tree(leaf(1), leaf(2)), leaf(4)).

Now define a predicate isBinaryTree(+BT) which succeeds if BT is a binary tree. The "+" indicates that it is assumed BT is instantiate in the query.
For example:

If BT = tree( leaf(1), tree( leaf(2),leaf(4)) ), then isBinaryTree(BT) succeeds.

*/

/* <h3>Problem 9 Answer:</h3> */

isBinaryTree(leaf(_)).
isBinaryTree(tree(Left,Right)):-
	isBinaryTree(Left),
	isBinaryTree(Right).

	

/* <h3>Problem 9 Test:</h3> */
%:- isBinaryTree(leaf(1)).                                           %SUCCEED
%:- isBinaryTree(tree(leaf(a),leaf(b))).                             %SUCCEED 
%:- BT = tree( leaf(b), tree( leaf(x),leaf(y)) ), isBinaryTree(BT).  %SUCCEED
%:- BT = tree(tree(leaf(1), leaf(2)), tree(leaf(10), tree(leaf(4), leaf(11)))), isBinaryTree(BT).  %SUCCEED

%:- isBinaryTree( tree(leaf(1)) ).                                   % FAIL
%:- isBinaryTree( tree() ).                                          % FAIL

/* <h3>Problem 10 </h3>
 
Write a predicate has_subseq(X,Y) that succeeds if Y is a list that is a subsequence of a list X. 

For example...
has_subseq([a,b,c,d],[b,d]) should succeed, but has_subseq([a,b,c,d],[b,e]) should fail. */

/* <h3>Problem 10 Answer:</h3> */

has_subseq([],[]).
has_subseq([_|T10],Y10):-
	has_subseq(T10,Y10).
has_subseq([H10|T10], [H10|Y10]):-
	has_subseq(T10,Y10).

/* <h3>Problem 10 Test:</h3> */
:- has_subseq([a,g,b,d],[g,b]).     % SUCCEED
:- has_subseq([1,2,3,4],[2,4]).     % SUCCEED
:- has_subseq([1,2,3,4],[2,3]).     % SUCCEED
:- has_subseq([1,2,3,4],[]).        % SUCCEED

:- has_subseq([1,2,3,4],[2,5]).     % FAIL
:- has_subseq([1,2,3,4],[4,3]).     % FAIL
