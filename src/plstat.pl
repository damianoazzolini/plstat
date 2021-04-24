:- module(plstat,[
    mean/2,
    median/2,
    mode/2,
    rms/2,
    sum_of_squares/2,
    variance/2,
    pop_variance/2,
    std_dev/2,
    pop_std_dev/2,
    range/2,
    midrange/2,
    mean_absolute_deviation/2,
    sum/2, % wrapper for sum_list
    prod/2,
    min_val/2, % wrapper for min_list
    max_val/2, % wrapper for max_list
    occurrences/2,
    occurrences/3,
    seq/4,
    factorial/2,
    choose/3
    ]).

% list all the available predicates
list:- true. 

/**
 * mean(-List:number,+Mean:float)
 * Mean is the Mean of the list
 * test: mean([1,2,3],2).
*/
mean([],0):- !.
mean(L,Mean):-
	length(L,N),
	sum_list(L,S),
	Mean is S/N.

/**
 * median(-List:number,+Median:number)
 * Median is the Median of the list
 * test: median([1,2,3],2).
*/
median([],0):- !.
median(L,Median):-
	length(L,N),
    (1 is N mod 2 ->
        N1 is N + 1,
        M is N1/2,
        nth1(M,L,Median) ;
    	NH is N/2, 
        N2 is (N + 2)/2,
        nth1(NH,L,XN2),
        nth1(N2,L,XN22),
        Median is (XN2 + XN22) / 2        
    ).

/**
 * mode(-List:number,+Mode:list)
 * Mode is the Mode of the list
 * test: mode([1,2,3,1],[1]).
*/
comp(<,[_,A1],[_,A2]) :- A1 > A2. % to have in descending order
comp(>, _, _).
mode([],0):- !.
mode(L,Mode):-
	occurrences(L,Occ),
    predsort(comp,Occ,X),
    X = [[_,O]|_],
    findall(V,member([V,O],Occ),Mode).

/**
 * percentile(-List:number,-K:number,+Percentile:number)
 * Percentile is the k-th percentile of the list
 * test: percentile([1,2,3,4,6,5,9],40,3.2).
 * TODO
 * */
% percentile([],_,0):- !.
% percentile(_,N,_):-
%     (N < 0 ; N > 100),
%     writeln('k must be between 0 and 100'),
%     false.
% percentile(L,K,Percentile):-
%     msort(L,LS),
%     KP is K / 100,
%     length(L,N),
%     I is KP * N,
%     RI is round(I),
%     nth1(RI, LS, Percentile).

/**
 * iqr(+List:number,IQR:number)
 * IQR is the inter quartile range of List
 * 3 - 1 quartile
 * TODO
 * test: iqr([1,2,3,4,6,5,9],I) 
 * */
% iqr([],_):- false.
% iqr(L,IQR):-
%     percentile(75,III),
%     percentile(25,I),
%     IQR is III - I.

/**
 * rms(+List:number,RMS:number)
 * RMS is the root mean square of List
 * Square root of the sum of the squared data values divided by the number of values
 * test: rms([1,5,8,3],4.97493).
 * */
square(X,XS):-
    pow(X,2,XS).
rms([],_):- writeln('The list for rms cannot be empty'),false.
rms(L,RMS):-
    length(L,N),
    maplist(square,L,LS),
    sum_list(LS, SS),
    RMS is sqrt(SS / N).

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
 * (1/(N - 1)) * \sum_n (x_i - \mu)^2
 * test: variance([1,2,4,6,7,8,9],9.2380952)
 * */
variance([],0).
variance(L,Var):-
    length(L,N),
    sum_of_squares(L,SS),
    N1 is N -1,
    Var is (1/N1) * SS.

/**
 * pop_variance(+List:number,-Variance:number)
 * Variance is the population variance of List
 * (1/N) * \sum_n (x_i - \mu)^2
 * */
pop_variance([],0).
pop_variance(L,Var):-
    length(L,N),
    sum_of_squares(L,SS),
    Var is (1/N) * SS.

/* std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation (square root of the variance)
 * test: std_dev([1,2,4,6,7,8,9],3.039424)
 * */
std_dev(L,StdDev):-
    variance(L,V),
    StdDev is sqrt(V).

/* pop_std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation (square root of the variance)
 * test: pop_std_dev([1,2,4,6,7,8,9],3.039424)
 * */
pop_std_dev(L,StdDev):-
    pop_variance(L,V),
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

% covariance(List1,List2,Cov)
% correlation(List1,List2,Corr)
% weighted mean(V,P,W)

%%%%%%%%
% other utils
%%%%%%%%

/**
 * occurrences(+List:number,-Occ:list)
 * Occ is a list [Value,Occurrences] for each element in List
 * test: occurrences([1,2,4,6,7,8,9,1],[[1,2],[2,1],[4,1],[6,1],[7,1],[8,1],[9,1]]).
 * TODO: maybe there is a faster way?
 * */
occurrences(L,LO):-
    sort(L,LS),
    count_occ(LS,L,LO).
count_occ([],_,[]).
count_occ([H|T],L,[[H,Occ]|T1]):-
    occurrences(L,H,Occ),
    count_occ(T,L,T1).

/**
 * occurrences(+Number:number,+List:numbers,-Occ:list)
 * Occ is the occurrences of Number in List
 * test: occurrences([1,2,4,6,7,8,9,1],1,2).
 * */
occurrences([],_,0).
occurrences(L,E,0):- ground(E), \+member(E,L).
occurrences(L,E,N):-
    findall(I,nth0(I,L,E),LI),
    length(LI,N).

/**
 * min_val(+List:numbers,-Min:number)
 * Min is the minimum of the list, wrapper for min_list/2
 * test: min_val([1,2,4,6,7,8,9,1],1).
 * */
min_val(L,M):-
    min_list(L,M).

/**
 * max_val(+List:numbers,-Max:number)
 * Max is the minimum of the list, wrapper for max_list/2
 * test: max_val([1,2,4,6,7,8,9,1],9).
 * */
max_val(L,M):-
    max_list(L,M).

/**
 * sum(+List:numbers,-Sum:number)
 * Sum is the sum of the elements in List, wrapper for sum_list/2
 * test: sum([1,24,2,3,-1],29). 
 * */
sum(L,S):-
    sum_list(L,S).

/**
 * prod(+List:numbers,-Prod:number)
 * Prod is the product of the elements in List
 * test: prod([1,24,2,3,-1],-144). 
 * */
prod([],0).
prod(L,P):-
    prod(L,1,P).
prod([],N,N).
prod([0|_],_,0):- !.
prod([H|T],P0,P1):-
    PT is P0 * H,
    prod(T,PT,P1).


%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * seq(A:number,B:number,Step:number,Seq:List). 
 * List is a list with a sequence ranging from A to B with step Step
 * test: seq(1,10,1,[1,2,3,4,5,6,7,8,9,10])
*/
seq(A,A,1,[A]):- !.
seq(A,A,V,[]):- V \= 1.
seq(_,_,0,[]).
seq(A,B,_,[]):- A > B.
seq(A,B,Step,[A|T]):-
	A < B,
	A1 is A + Step,
	seq(A1,B,Step,T).

/**
 * factorial(+N:int,-Factorial:int)
 * Factorial is N! = N*(N-1)*...*2*1
 * */
factorial(0,1).
factorial(1,1):- !.
factorial(N,F):-
    factorial_aux(N,1,F).
factorial_aux(1,F,F):- !.
factorial_aux(N,V,F):-
    V1 is V * N,
    N1 is N-1,
    factorial_aux(N1,V1,F).

/**
 * choose(+N:int,+K:int,-C:int)
 * C is the binomial coefficient N,K
 * fact(N) / (fact(N-K) * fact(K))
 * */
choose(N,K,1):- N =< K.
choose(N,K,C):-
    N > 0,
    K > 0,
    N > K,
    factorial(N,NF),
    factorial(K,KF),
    NMinusK is N - K,
    factorial(NMinusK,NMKF),
    C is NF / (NMKF * KF).

% random variables 
% expected_value_var(Val,Occ,Exp)
% variance_var(Val,Occ,Exp)
