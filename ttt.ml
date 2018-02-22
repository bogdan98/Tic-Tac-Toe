datatype 'a tree = Lf of int (*0 for draw, 1 for naughts win, 2 for crosses win*)
| Br of 'a * ('a tree list);(*0 for empty, 1 for naught, 2 for cross*)
exception NoElement;
fun count ([]) (el) = 0
| count (x::xs)(el) = if (el=x) then 1+ count(xs)(el)
(*counting the number of instances of some element in a list*) 
else count(xs)(el);
fun insertnext (x::xs) (y::ys) = if (count(y::ys)(1) = count(y::ys)(2)) then 
if (x=0) then 1::xs 
(*This means that if number of 1 and 2 are same, its turn for the first
person to put a naught*)
else x::insertnext(xs)(y::ys)
else if (x=0) then 2::xs else x::insertnext(xs)(y::ys);
(*This means if number of 1 and 2 are not equal in list y::ys, its turn 
for the second person to put a cross. In both cases, zero is replaced by 1 or 2*)
fun insNext(x::xs) = insertnext(x::xs)(x::xs);
(*This function simply uses the previous function*)
fun nthel(x::xs, 1) = x
| nthel(x::xs, n) = nthel(xs, n-1);
(*Returns nth element of a list*)
fun position ([])(el) = raise NoElement
| position(x::xs)(el) = if (x=el) then 1 else 1+position(xs)(el);
(*determines the position of the first instance of a certain element in the list*)
fun take(x::xs, 0) = []
| take(x::xs, n) = x::take(xs, n-1);
fun drop([],_) = []
| drop(x::xs, n) = if n>0 then drop(xs, n-1) else x::xs;
(*Standard take and drop functions for a list*)
fun map (f) [] = []
| map (f) (x::xs) = (f x) :: map(f)(xs);
(*Standard map function for a list*)
fun canbefilled(x::xs) = (x=0) orelse canbefilled(xs)
| canbefilled([]) = false;
(*function that determines whether any more elements can be added 
to the game board*)
fun nextstates1([])(_) = []
| nextstates1(x::xs)(y::ys) = if not (canbefilled(x::xs)) then 
[] else (insertnext(x::xs)(y::ys)) :: map(fn(ml)=>(take(x::xs, 
(position(x::xs)(0)))) @ ml)(nextstates1(drop(x::xs, position(x::xs)(0)))(y::ys));
fun nextStates(x::xs) = nextstates1(x::xs)(x::xs);
(*function that gives a list of next states starting from a certain state.
Returns empty list if a state is full, otherwise returns a list of lists,
which are produced in the following way: the first zero is found, replaced by 1 or 2,
then the first part of the list up to zero is discarded(by drop function), but
saved by take function and then appended to every next state calculated on smaller lists
in which other zeros are replaced by 1 or 2*)
fun naughtWins(xs) = ((nthel(xs, 1) = 1) andalso (nthel(xs, 2)=1) andalso
(nthel(xs, 3) = 1)) orelse ((nthel(xs, 4) = 1) andalso (nthel(xs, 5)=1) andalso
(nthel(xs, 6) = 1)) orelse ((nthel(xs, 7) = 1) andalso (nthel(xs, 8)=1) andalso
(nthel(xs, 9) = 1)) orelse ((nthel(xs, 1) = 1) andalso (nthel(xs, 5)=1) andalso
(nthel(xs, 9) = 1)) orelse ((nthel(xs, 3) = 1) andalso (nthel(xs, 5)=1) andalso
(nthel(xs, 7) = 1)) orelse ((nthel(xs, 1) = 1) andalso (nthel(xs, 4)=1) andalso
(nthel(xs, 7) = 1)) orelse ((nthel(xs, 2) = 1) andalso (nthel(xs, 5)=1) andalso
(nthel(xs, 8) = 1)) orelse ((nthel(xs, 3) = 1) andalso (nthel(xs, 6)=1) andalso
(nthel(xs, 9) = 1)) ; 
fun crossWins(xs) = ((nthel(xs, 1) = 2) andalso (nthel(xs, 2)=2) andalso
(nthel(xs, 3) = 2)) orelse ((nthel(xs, 4) = 2) andalso (nthel(xs, 5)=2) andalso
(nthel(xs, 6) = 2)) orelse ((nthel(xs, 7) = 2) andalso (nthel(xs, 8)=2) andalso
(nthel(xs, 9) = 2)) orelse ((nthel(xs, 1) = 2) andalso (nthel(xs, 5)=2) andalso
(nthel(xs, 9) = 2)) orelse ((nthel(xs, 3) = 2) andalso (nthel(xs, 5)=2) andalso
(nthel(xs, 7) = 2)) orelse ((nthel(xs, 1) = 2) andalso (nthel(xs, 4)=2) andalso
(nthel(xs, 7) = 2)) orelse ((nthel(xs, 2) = 2) andalso (nthel(xs, 5)=2) andalso
(nthel(xs, 8) = 2)) orelse ((nthel(xs, 3) = 2) andalso (nthel(xs, 6)=2) andalso
(nthel(xs, 9) = 2)) ; 
(*just conditions for the naughts or crosses to win *)
fun makenode(xs) = if (crossWins(xs)) then Br(xs, [Lf(2)]) else 
if (naughtWins(xs)) then Br(xs, [Lf(1)]) else if not(canbefilled(xs)) then 
Br(xs, [Lf(0)]) else Br(xs, map(fn(x)=>makenode(x))(nextStates(xs)));
(*the function makes node out of a list that represents a state: if the game is won or drawn,
just a node with the list and a leaf with corresponding number is created. If the state is not 
the end of the game, a Br is created with the list and a list of next states that are in turn 
converted into nodes by map function*)
fun mktree() = makenode([0,0,0,0,0,0,0,0,0]);
(*creates a tree that begins with an empty board*)
fun Sum([]) = 0
| Sum(x::xs) = x + Sum(xs);
(*summing all the elements of the list*)
fun Owins(Lf(0)) = 0
| Owins(Lf(2)) = 0
| Owins(Lf(1)) = 1
| Owins(Br(xs, xss)) = Sum (map (fn(y)=> Owins(y))(xss));
(*counts 1 if a game is won by O, in case of a big tree, it takes all the trees from it
, i.e. from the list of trees, recursively applied Owins to them and sums them to give 
the total number of games won by O. Result is 131184 for the whole tree of games,
wikipedia gives exactly the same result*)