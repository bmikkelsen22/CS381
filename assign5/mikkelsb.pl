% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).

% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_). 
isFather(X) :- male(X), parent(X,_).

% 3. Define a predicate `grandparent/2`.
grandparent(A,C) :- parent(A,B), parent(B,C).

% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(A,B) :- child(A,P), child(B,P), A \= B.

% 5. Define two predicates `brother/2` and `sister/2`.
brother(B1,B2) :- sibling(B1,B2), male(B1).
sister(S1,S2) :- sibling(S1,S2), female(S1).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(S1,S2) :- married(S1,X), sibling(X,S2); sibling(S1,Y), married(Y,S2).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.

aunt(A,B) :- child(B,P), siblingInLaw(P, A), female(A).
uncle(A,B) :- child(B,P), siblingInLaw(P, A), male(A).


% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- child(X,P1), child(Y,P2), sibling(P1,P2).

% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).

% Extra credit: Define the predicate `related/2`.



%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.
num(N) :- number(N).
str(S) :- string(S).
bool(B) :- B = t | B = f.

cmd(N, S1, S2) :- num(N), append([N], S1, S2).
cmd(S ,S1 , S2) :- str(S), append([S], S1, S2).
cmd(add, [X,Y,Tail], S2) :- num(X), num(Y), addN(X,Y,R), append([R],Tail, S2).
cmd(lte, [X,Y,Tail], S2) :- num(X), num(Y), (X =< Y -> append([t],Tail, S2) ; append([f],Tail, S2)).

append([f],Tail, S2)).
remove([Head|Tail],Tail, Head).
addN(X,Y,R) :- R is +(X,Y).

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

prog(P, S1, S2) :- cmd(P, S1, S2).
prog([P,Tail], S1, S2) :- cmd(P, S1, R),  prog(Tail, R, S2).
