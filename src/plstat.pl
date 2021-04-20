:- module(plstat,[
    mean/2,
    variance/2,
    sum_of_squares/2,
    std_dev/2,
    range/2,
    midrange/2,
    mean_absolute_deviation/2,
    occurrences/3,
    seq/4,
    factorial/2,
    choose/3
    ]).


%% descriptive statistics

% list all the available predicates
list:- true. 

/**
* mean(-List,+Mean:float) is det
*
* Mean is the Mean of the list 
*/

mean([],0):- !.
mean(L,Mean):-
	length(L,N),
	sum_list(L,S),
	Mean is S/N.
% test: mean([1,2,3],2).


/**
 * sum_of_squares(+List:number,-SumOfSquares:number)
 * compute \sum_n (x - \mu)^2
 * test: sum_of_squares([1,2,3],2)
 * */
diff_square(Mu,B,D):-
    AB is B - Mu,
    pow(AB,2,D).
sum_of_squares([],0).
sum_of_squares(L,SumSquared):-
    mean(L,Mean),
    maplist(diff_square(Mean),L,Res),
    sum_list(Res,SumSquared).

/**
 * variance(+List:number,-Variance:number)
 * Variance is the sample variance of the List
 * 1/(N - 1) * \sum_n (x_i - \mu)^2
 * test: variance([1,2,4,6,7,8,9],9.2380952)
 * */
% variance(List,Var)
variance([],0).
variance(L,Var):-
    length(L,N),
    sum_of_squares(L,SS),
    N1 is N -1,
    Var is (1/N1) * SS.

/* std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation (square root of the variance)
 * test: std_dev([1,2,4,6,7,8,9],3.039424)
 * */
std_dev(L,StdDev):-
    variance(L,V),
    StdDev is sqrt(V).

/**
 * range(+List:numbers,-Range:number)
 * Range is the difference between the biggest and the smallest
 * element of the list
 * test: range([1,2,4,6,7,8,9],8).
 * */
range([],0).
range(L,Range):-
    min_list(L,Min),
    max_list(L,Max),
    Range is Max - Min.
    
/**
 * midrange(+List:numbers,-Midrange:number)
 * Midrange is (Max - Min) / 2
 * test: midrange([1,2,4,6,7,8,9],4).
 * */
midrange(L,Midrange):-
    range(L,Range),
    Midrange is Range / 2.

/**
 * mean_absolute_deviation(+List:numbers,-MAS:number)
 * MAD is the sum of the absolute value of the differences between data values and the mean, divided by the sample size.
 * MAD = 1/N * \sum_i |x - \mu|
 * test: mean_absolute_deviation([1,2,4,6,7,8,9],2.5306122).
 * */
diff_abs(A,B,D):-
    AB is A - B,
    D is abs(AB).
mean_absolute_deviation([],0).
mean_absolute_deviation(L,MAD):-
    mean(L,Mean),
    maplist(diff_abs(Mean),L,LD),
    sum_list(LD,S),
    length(L,N),
    MAD is (1/N) * S.

/**
 * frequency(+List:numbers,-FreqList:list)
 * FreqList is a pair [Element,Frequency] for all the elements of the List
 * 
 * */

/**
 * occurrences(+Number:number,+List:numbers,-Occ:list)
 * Occ is the occurrences of Number in List
 * test: occurrences([1,2,4,6,7,8,9,1],1,2).
 * */
occurrences(_,[],0).
occurrences(E,L,0):- ground(E), \+member(E,L).
occurrences(E,L,N):-
    findall(I,nth0(I,E,L),LI),
    length(LI,N).

% median: TODO
% mode: TODO
% quartiles: TODO
% Interquartile Range: The range from Q1 to Q3 is the interquartile range (IQR).
% Root Mean Square is the square root of the sum of the squared data values divided by n.
% frequency:

%%%%%%%%%%%%%%%%%%%%%%%%

/**
seq(A:number,B:number,Step:number,Seq:List): creates a list with a sequence ranging from A to B with step Step
if A < B, the list is in decreasing order
*/
seq(A,A,1,[A]):- !.
seq(A,A,V,[]):- V \= 1.
seq(_,_,0,[]).
seq(A,B,Step,[A|T]):-
	A < B,
	A1 is A + Step,
	seq(A1,B,Step,T).
seq(A,B,Step,[A|T]):-
	A > B,
	A1 is A - Step,
	seq(A1,B,Step,T).

% test: 
% seq(1,3,1,[1,2,3])
% seq(3,1,1,[3,2,1])
% seq(1,3,2,[1,3])

/**
 * factorial(+N:int,-Factorial:int)
* Factorial is N! = N*(N-1)*...*2*1
 * */
factorial(0,1).
factorial(1,1).
factorial(N,F):-
    factorial_aux(N,1,F).
factorial_aux(1,F,F).
factorial(N,V,F):-
    V1 is V * N,
    N1 is N-1,
    factorial(N1,V1,F).
% test
% factorial(10,3628800).

/**
 * choose(+N:int,+K:int,-C:int)
 * binomial coefficient: fact(N) / (fact(N-K) * fact(K))
 * */
choose(N,K,1):- N =< K.
choose(N,K,C):-
    N > 0,
    K > 0,
    N > K,
    factorial(N,NF),
    factorial(K,KF),
    NMinusK is N - K,
    factorial(NMinusK),
    C is NF / (NMinusK * KF).

% covariance(List1,List2,Cov)
% correlation(List1,List2,Corr)
% choose(N,K,V) % binomial coeff
% weighted mean(V,P,W)

% random variables 
% expected_value_var(Val,Occ,Exp)
% variance_var(Val,Occ,Exp)
