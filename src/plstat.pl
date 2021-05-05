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
    covariance/3,
    correlation/3,
    weighted_mean/3,
    harmonic_mean/2,
    trimmed_mean/4,
    trimmed_variance/4,
    skew/2,
    kurtosis/2,
    moment/3,
    sum/2, % wrapper for sum_list
    prod/2,
    min_val/2, % wrapper for min_list
    max_val/2, % wrapper for max_list
    rank/2,
    rank/3,
    nth_row/3,
    nth_column/3,
    swap_rows_columns/2,
    split_n_parts/3,
    occurrences/2,
    occurrences/3,
    normalize_prob/2,
    delete_nth/3,
    sample/3,
    sample/4,
    sample/5,
    seq/4,
    factorial/2,
    choose/3
    ]).

% list all the available predicates
list:- true. 

% multidimensional wrapper
multidim2(Predicate,List,Res):-
    ( List = [A|_], is_list(A) -> 
        maplist(Predicate,List,Res);
        Call =.. [Predicate,List,Res],
        Call
    ).

/**
 * mean(-List:number,+Mean:float)
 * Mean is the mean of the list List
 * List can also be a list of lists
 * example: mean([1,2,3],2).
 * example: mean([[1,3,4],[7,67]],[2.6666666666666665, 37]).
*/
mean([],0):- !.
mean([E],E):- number(E), !.
mean(L,Mean):-
    multidim2(mean_,L,Mean).
mean_(L,Mean):-
	length(L,N),
	sum_list(L,S),
	Mean is S/N.

/**
 * median(-List:number,+Median:number)
 * Median is the median of the list List
 * List can also be a list of lists
 * example: median([1,2,3],2).
 * example: median([[1,5,64],[27,67]],[5, 47]).
*/
median([],0):- !.
median([E],E):- number(E), !.
median(L,Median):-
    multidim2(median_,L,Median).
median_(L,Median):-
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
 * Mode is the mode of the list List
 * List can also be a list of lists
 * example: mode([1,2,3,1],[1]).
 * example: mode([[1,5,64],[27,67]],[[1,5,64], [27,67]])
*/
comp(<,[_,A1],[_,A2]) :- A1 > A2. % to have in descending order
comp(>, _, _).
mode([],0):- !.
mode([E],E):- number(E), !.
mode(L,Mode):-
    multidim2(mode_,L,Mode).
mode_(L,Mode):-
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
 * RMS is the root mean square of the list List
 * List can also be a list of lists
 * Square root of the sum of the squared data values divided by the number of values
 * example: rms([1,5,8,3],4.97493).
 * */
square(X,XS):-
    pow(X,2,XS).
rms([],_):- writeln('The list for rms cannot be empty'), false.
rms(L,RMS):-
    multidim2(rms_,L,RMS).
rms_(L,RMS):-
    length(L,N),
    maplist(square,L,LS),
    sum_list(LS, SS),
    RMS is sqrt(SS / N).

/**
 * sum_of_squares(+List:number,-SumOfSquares:number)
 * SumOfSquares is the sum of squares of the list List
 * List can also be a list of lists
 * compute \sum_n (x - \mu)^2
 * example: sum_of_squares([1,2,3],2)
 * */
diff_square(Mu,B,D):-
    AB is B - Mu,
    pow(AB,2,D).
sum_of_squares([],0).
sum_of_squares([_],0).
sum_of_squares(List,SumOfSquares):-
    multidim2(sum_of_squares_,List,SumOfSquares).
sum_of_squares_(L,SumSquared):-
    mean(L,Mean),
    maplist(diff_square(Mean),L,Res),
    sum_list(Res,SumSquared).

/**
 * variance(+List:number,-Variance:number)
 * Variance is the sample variance of the list List
 * List can also be a list of lists
 * (1/(N - 1)) * \sum_n (x_i - \mu)^2
 * example: variance([1,2,4,6,7,8,9],9.2380952)
 * */
variance([],0):- !.
variance([E],0):- number(E), !.
variance(L,Var):-
    multidim2(variance_,L,Var).
variance_(L,Var):-
    length(L,N),
    sum_of_squares(L,SS),
    N1 is N - 1,
    Var is (1/N1) * SS.

/**
 * pop_variance(+List:number,-Variance:number)
 * Variance is the population variance of the list List
 * List can also be a list of lists
 * (1/N) * \sum_n (x_i - \mu)^2
 * pop_variance([1,4,6,72,1],765.3600).
 * */
pop_variance([],0):- !.
pop_variance([E],0):- number(E), !.
pop_variance(L,Var):-
    multidim2(pop_variance_,L,Var).
pop_variance_(L,Var):-
    length(L,N),
    sum_of_squares(L,SS),
    Var is (1/N) * SS.

/* std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation (square root of the sample variance)
 * List can also be a list of lists
 * example: std_dev([1,2,4,6,7,8,9],3.039424)
 * */
std_dev(L,StdDev):-
    multidim2(std_dev_,L,StdDev).
std_dev_(L,StdDev):-
    variance(L,V),
    StdDev is sqrt(V).

/* pop_std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation (square root of the population variance)
 * List can also be a list of lists
 * example: pop_std_dev([1,2,4,6,7,8,9],3.039424)
 * */
pop_std_dev(L,StdDev):-
    multidim2(pop_std_dev_,L,StdDev).
pop_std_dev_(L,StdDev):-
    pop_variance(L,V),
    StdDev is sqrt(V).

/**
 * range(+List:numbers,-Range:number)
 * Range is the difference between the biggest and the smallest
 * element of the list List
 * List can also be a list of lists
 * example: range([1,2,4,6,7,8,9],8).
 * */
range([],0):- !.
range([E],E):- number(E), !.
range(L,Range):-
    multidim2(range_,L,Range).
range_(L,Range):-
    min_list(L,Min),
    max_list(L,Max),
    Range is Max - Min.
    
/**
 * midrange(+List:numbers,-Midrange:number)
 * Midrange is (Max - Min) / 2
 * List can also be a list of lists
 * example: midrange([1,2,4,6,7,8,9],4).
 * */
midrange(L,Range):-
    multidim2(midrange_,L,Range).
midrange_(L,Midrange):-
    range(L,Range),
    Midrange is Range / 2.

/**
 * mean_absolute_deviation(+List:numbers,-MAS:number)
 * MAD is the sum of the absolute value of the differences between data values and the mean, divided by the sample size.
 * MAD = 1/N * \sum_i |x - \mu|
 * List can also be a list of lists
 * example: mean_absolute_deviation([1,2,4,6,7,8,9],2.5306122).
 * */
diff_abs(A,B,D):-
    AB is A - B,
    D is abs(AB).
mean_absolute_deviation([],0):- !.
mean_absolute_deviation(L,MAD):-
    multidim2(mean_absolute_deviation_,L,MAD).
mean_absolute_deviation_(L,MAD):-
    mean(L,Mean),
    maplist(diff_abs(Mean),L,LD),
    sum_list(LD,S),
    length(L,N),
    MAD is (1/N) * S.

/**
 * covariance(+List1:numbers,-List2:numbers,-Covariance:number)
 * Covariance is the covariance of List1 and List2
 * example: covariance([5,12,18,23,45],[2,8,18,20,28],146.1).
 * */
sub(A,B,C):-
    C is B - A.
mul(A,B,C):-
    C is A * B.
covariance(L1,L2,_):-
    length(L1,N),
    length(L2,M),
    M \= N,
    writeln('Lists must be of the same length'),
    write('Found '), write(N), write(' '), writeln(M),
    false.
covariance(L1,L2,Cov):-
    length(L1,N),
    mean(L1,M1),
    mean(L2,M2),
    maplist(sub(M1),L1,LS1),
    maplist(sub(M2),L2,LS2),
    maplist(mul,LS1,LS2,LP),
    sum_list(LP,S),
    Cov is S / (N - 1).

/**
 * correlation(+List1:numbers,-List2:numbers,-Correlation:number)
 * Correlation is the correlation of List1 and List2
 * covariance(List1,List2) / (std_dev(List1) * std_dev(List2))
 * example: correlation([5,12,18,23,45],[2,8,18,20,28],146.1).
 * */
correlation(L1,L2,_):-
    length(L1,N),
    length(L2,M),
    M \= N,
    writeln('Lists must be of the same length'),
    write('Found '), write(N), write(' '), writeln(M),
    false.
correlation(L1,L2,Corr):-
    covariance(L1,L2,Cov),
    std_dev(L1,S1),
    std_dev(L2,S2),
    Corr is Cov / (S1 * S2).

/**
 * weighted_mean(+List:numbers,+Weights:numbers,-WM:number)
 * WM is the weighted mean: \sum x_i*w_i / \sum w_i
 * example: weighted_mean([3,8,10,17,24,27],[2,8,10,13,18,20],19.1972).
 * */
weighted_mean(L1,L2,_):-
    length(L1,N),
    length(L2,M),
    M \= N,
    writeln('Lists must be of the same length'),
    write('Found '), write(N), write(' '), writeln(M),
    false.
weighted_mean(List,Weights,WM):-
    sum_list(Weights, SW),
    maplist(mul,List,Weights,LP),
    sum_list(LP,LS),
    WM is LS / SW.

/**
 * harmonic_mean(+List:numbers,-HM:number)
 * HM is the harmonic mean:  n / (1/x1 + 1/x2 + ... + 1/xn)
 * List can also be a list of lists
 * example: harmonic_mean([1,2,3,4,5,6,7],2.69972).
 * */
rec(0,_):- writeln('Cannot divide by 0'), false.
rec(X,X1):- X1 is 1/X.
harmonic_mean([],_):- false.
harmonic_mean(L,HM):-
    multidim2(harmonic_mean_,L,HM).
harmonic_mean_(L,HM):-
    length(L,N),
    maplist(rec,L,LR),
    sum_list(LR,SLR),
    HM is N / SLR.

/**
 * trimmed_mean(+List:numbers,+Lower:number,+Upper:number,-TM:number)
 * TM is the trimmed mean: the mean computed by considering only numbers 
 * in the range [Lower,Upper]
 * example: trimmed_mean([1,2,3,4,5,6,7],3,5,4).
 * */
trimmed_mean([],_,_,0).
trimmed_mean(L,Lower,Upper,TM):-
    include(between(Lower,Upper),L,LO),
    mean(LO,TM).

/**
 * trimmed_variance(+List:numbers,+Lower:number,+Upper:number,-TV:number)
 * TV is the trimmed variance: the variance computed by considering only numbers 
 * in the range [Lower,Upper]
 * example: trimmed_variance([1,2,3,4,5,6,7],3,5,1.0).
 * */
trimmed_variance([],_,_,0).
trimmed_variance(L,Lower,Upper,TV):-
    include(between(Lower,Upper),L,LO),
    variance(LO,TV).

/**
 * moment(+List:numbers,+M:integer,-Moment:number)
 * Moment is the M-th moment about the mean for List
 * 1/n \sum (x_i - x_mean) ^ M
 * example: moment([1,2,3,4,5],2,2)
 * */
diff_and_power(Exp,Mean,Number,R):-
    D is Number - Mean,
    pow(D,Exp,R).
moment([],_,0).
moment(L,M,Moment):-
    length(L,N),
    mean(L,Mean),
    maplist(diff_and_power(M,Mean),L,Res),
    sum(Res,S),
    Moment is 1/N * S.

/**
 * skew(+List:numbers,-Skew:number)
 * Skew is the sample skewness of List
 * Skew = m_3 / (m_2)^(3/2)
 * List can also be a list of lists
 * example: skew([2,8,0,4,1,9,9,0],0.26505541)
 * TODO: consider also the version with bias false
 * */
skew([],0):- !.
skew(L,Skew):-
    multidim2(skew_,L,Skew).
skew_(List,Skew):-
    moment(List,2,M2),
    moment(List,3,M3),
    pow(M2,3/2,Den),
    Skew is M3 / Den.

/**
 * kurtosis(+List:numbers,-Kurtosis:number)
 * Kurtosis is the fourth central moment divided by 
 * the square of the variance.
 * List can also be a list of lists
 * example: kurtosis([3,5,7,2,7],1.3731508875)
 * */
kurtosis([],0).
kurtosis(L,Kurtosis):-
    multidim2(kurtosis_,L,Kurtosis).
kurtosis_(List,Kurtosis):-
    moment(List,4,L4),
    moment(List,2,L2),
    pow(L2,2,L22),
    Kurtosis is L4 / L22.

/**
 * rank(+List:numbers,-RankList:number)
 * rank(+List:numbers,+Method:atom,-RankList:number)
 * RankList is the rank of the list List according to method Method
 * If Method is not supplied, by default it performs 
 * average/fractional ranking
 * Method must be one of the following:
 * - average or fractional: items that compare equal receive the same rank,
 *   which is the mean of ordinal ranking values (see below)
 * - min or competition: items that compare equal receive the same rank
 *   (there will be a gap in the ranking list)
 * - max or modified_competition: as min, but the gap is left before, rather than after 
 * - dense: as min, but no gaps are left
 * - ordinal: all the elements receive a different rank. If the same element
 *   appears more than one time, all the occurrences will have a different
 *   (increasing) rank
 * example: rank([0,2,3,2],[1.0,2.5,4.0,2.5]).
 * example: rank([0,2,3,2],average,[1.0,2.5,4.0,2.5]).
 * example: rank([0,2,3,2],min,[1,2,4,2]).
 * example: rank([0,2,3,2],max,[1,3,4,3]).
 * example: rank([0,2,3,2],dense,[1,2,3,2]).
 * example: rank([0,2,3,2],ordinal,[1,2,4,3]).
 * rank([[0,2,3,2],[1,4,5]],max,[[1,4,5,4],[2,6,7]]).
 * */
rank(L,Rank):-
    rank(L,average,Rank).
rank(LL,Mode,Rank):-
    flatten(LL,L),
    msort(L,LS),
    (   Mode = dense ->    
    	compute_dense(LS,LS,1,LR);
    	(Mode = min ; Mode = competition) ->
    	compute_min(LS,LS,1,LR);
    	(Mode = max ; Mode = modified_competition) ->  
    	compute_max(LS,LS,1,LR);
    	(Mode = average ; Mode = fractional) -> 
     	compute_average(LS,1,LR) ;
    	Mode = ordinal ->  
    	compute_ordinal(LS,1,LR)
    ),
    extract_map(LL,LR,Rank,Mode).

extract_map([],_LR,[],_):- !.
extract_map([H|T],LR,[HM|TM],Mode):-
    extract_map_(H,LR,HM,Mode), !,
	extract_map(T,LR,TM,Mode).    
extract_map([H|T],LR,[V|TV],M):-
    extract_map_([H|T],LR,[V|TV],M).

extract_map_([],_,[],_).
extract_map_([H|T],LR,[V|TV],M):-
    memberchk([H,V],LR),
    ( M = ordinal -> 
    	delete(LR,[H,V],LR1);
    	LR1 = LR
    ),
    extract_map_(T,LR1,TV,M).

compute_ordinal([],_,[]).
compute_ordinal([H|T],I,[[H,I]|T3]):-
    I1 is I+1,
    compute_ordinal(T,I1,T3).

% dense
compute_dense([],_,_,[]).
compute_dense([H|_],L,IN,[[H,IN]|TR]):-
    findall(I,nth0(I,L,H),LI),
    length(LI,N),
    length(LAux,N),
    append(LAux,LN,L),
    I1 is IN+1,
    compute_dense(LN,LN,I1,TR).

% min
compute_min([],_,_,[]).
compute_min([H|_],L,IN,[[H,IN]|TR]):-
    findall(I,nth0(I,L,H),LI),
    length(LI,N),
    length(LAux,N),
    append(LAux,LN,L),
    I1 is IN+N,
    compute_min(LN,LN,I1,TR).

% max
compute_max([],_,_,[]).
compute_max([H|_],L,IN,[[H,IT]|TR]):-
    findall(I,nth0(I,L,H),LI),
    length(LI,N),
    length(LAux,N),
    append(LAux,LN,L),
    I1 is IN+N,
    IT is I1 - 1,
    compute_max(LN,LN,I1,TR).


look_in_average([],[]).
look_in_average([[V,Rank]|T],[[V,Rank1]|T0]):-
    findall(R,member([V,R],T),LR),
    length(LR,NR),
    ( NR is 0 ->  
    	Rank1 = Rank,
        T1 = T;
    	sum_list(LR,S),
        Rank1 is (S+Rank) / (NR+1),
        delete(T,[V,_],T1)
    ),
    look_in_average(T1,T0).

% average
compute_average(LS,I,LAvg):-
    compute_ordinal(LS,I,LAvg1),
    look_in_average(LAvg1,LAvg).

%%%%%%%
% random variables
%%%%%%%


%%%%%%%%
% other utils
%%%%%%%%

/**
 * nth_row(+ListOfList:numbers,+Nth:integer,-NthRow:List)
 * NthRow is the nth row of ListOfList, counting from 1
 * example: nth_row([[1,2],[3,4]],2,[3,4]).
 * */
nth_row(L,Nth,NthRow):-
    nth1(Nth,L,NthRow).

/**
 * nth_column(+ListOfList:numbers,+Nth:integer,-NthColumn:List)
 * NthColumn is the nth column of ListOfList, counting from 1
 * example: nth_column([[1,2],[3,4]],2,[2,4]).
 * */
nth_column(L,Nth,NthColumn):-
    findall(E,(member(R,L),nth1(Nth,R,E)),NthColumn).

/**
 * swap_rows_columns(+LRows:numbers,+ListOfLists:LColumns)
 * LColumns is LRows transposed (rows and columns swapped)
 * example: swap_rows_columns([[1,2,4],[3,6,7]],[[1,3],[2,6],[4,7]]).
 * */
swap_rows_columns([H|T],MT):-
    transpose(H,[H|T],MT).
transpose([],_,[]).
transpose([_|T],M,[HMT|TMT]):-
    transposeIn(M,HMT,M1),
    transpose(T,M1,TMT).
transposeIn([],[],[]).
transposeIn([[V1|V2]|T],[V1|TV],[V2|TM]):-
    transposeIn(T,TV,TM).

/**
 * split_n_parts(+List:numbers,+Parts:number,-PartsList:numbers)
 * PartsList is a list of list obtained by splitting list List
 * in Parts parts
 * example: split_n_parts([1,2,4,3,7,6],2,[[1,2],[4,3],[7,6]]).
 * */
split_n_parts(L,1,L):- !.
split_n_parts(_,N,_):- N =< 0, !, false.
split_n_parts(L,N,_):- 
    length(L,LN), 
    A is LN mod N, 
    A \= 0,
    writeln("List cannot be divided in the desired parts"), !,
    false.
split_n_parts([],_,[]):- !.
split_n_parts(List,Parts,[H|T]):-
    length(H,Parts),
    append(H,LT,List),
    split_n_parts(LT,Parts,T).

/**
 * occurrences(+List:number,-Occ:list)
 * Occ is a list [Value,Occurrences] for each element in List
 * example: occurrences([1,2,4,6,7,8,9,1],[[1,2],[2,1],[4,1],[6,1],[7,1],[8,1],[9,1]]).
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
 * example: occurrences([1,2,4,6,7,8,9,1],1,2).
 * */
occurrences([],_,0).
occurrences(L,E,0):- ground(E), \+member(E,L).
occurrences(L,E,N):-
    findall(I,nth0(I,L,E),LI),
    length(LI,N).

/**
 * min_val(+List:numbers,-Min:number)
 * Min is the minimum of the list, wrapper for min_list/2
 * example: min_val([1,2,4,6,7,8,9,1],1).
 * */
min_val(L,M):-
    min_list(L,M).

/**
 * max_val(+List:numbers,-Max:number)
 * Max is the minimum of the list, wrapper for max_list/2
 * example: max_val([1,2,4,6,7,8,9,1],9).
 * */
max_val(L,M):-
    max_list(L,M).

/**
 * sum(+List:numbers,-Sum:number)
 * Sum is the sum of the elements in List, wrapper for sum_list/2
 * example: sum([1,24,2,3,-1],29). 
 * */
sum(L,S):-
    sum_list(L,S).

/**
 * prod(+List:numbers,-Prod:number)
 * Prod is the product of the elements in List
 * example: prod([1,24,2,3,-1],-144). 
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
 * normalize_prob(+List:numbers,-NormalizedList:number)
 * NormalizedList is the list List normalized.
 * List must contain only elements between 0 and 1 (included)
 * Formula: i / list_sum foreach i in List
 * example: normalize_prob([0.07,0.14,0.07],[0.25,0.5,0.25])
 * */
div(Sum,El,Div):-
    Sum \= 0,
    Div is El / Sum.

between_float(L,U,N):-
    N >= L,
    N =< U.

normalize_prob([],[]):- !.
normalize_prob(L,LNorm):-
    maplist(between_float(0,1),L),
    sum_list(L,SL),
    maplist(div(SL),L,LNorm).

/**
 * https://en.wikipedia.org/wiki/Feature_scaling#Rescaling_(min-max_normalization)
 * TODO: all
 * */

/**
 * delete_nth(+List:numbers,+Index:numbers,-LDeleted:number)
 * LDeleted is List with the element at pos Index removed, counting from 1
 * If Index is greater than the length of the list, fails
 * example: delete_nth([1,2,7,4],3,[1,2,4])
 * TODO: multidimensional data: delete_nth([[1,2,7,4],[1,2,7,4]],3,[[1,2,4],[1,2,4]])
 * */
delete_nth([_|T],1,T):- !.
delete_nth([H|T],Nth,[H|T1]):-
    Nth > 1,
	N is Nth-1,
    delete_nth(T,N,T1).

/**
 * sample(+List:elements,+Size:number,-Result:list)
 * sample(+List:elements,+Size:number,+Replace:bool,-Result:list)
 * sample(+List:elements,+Size:number,+Replace:bool,+Probabilities:list,-Result:list)
 * Takes a sample of size Size from list List.
 * Replace can be true or false, if not provided is false
 * Probabilities is a list of probabilities.
 * If Replace is false and a list of probabilities is specified, the list is
 * normalized, after the removal of the element 
 * example:
 * TODO: test and examples
 * */
sample_list(_,0,[],_):- !.
sample_list(L,N,[H|T],Replace):-
    length(L,Len),
    random_between(1,Len,R),
    nth1(R,L,H),
    ( Replace = true ->
        L1 = L ;
        delete_nth(L,R,L1)
    ),
    N1 is N - 1,
    sample_list(L1,N1,T,Replace).

get_random_el([H|_],P,I,I,H):-
    P =< 0, !.
get_random_el([_|T],P,I,I0,[CP|TP],El):-
    P > 0,
    P1 is P - CP,
    I1 is I + 1,
    get_random_el(T,P1,I1,I0,TP,El).


sample_list_prob(_,0,_,_,[]).
sample_list_prob(L,Size,Replace,[HP|TP],[Out|TOut]):-
    random(R),
    RP is R - HP,
    get_random_el(L,RP,1,Index,[HP|TP],Out),
    ( Replace = true ->
        L1 = L,
        PL = [HP|TP] ;
        delete_nth(L,Index,L1),
        delete_nth([Out|TOut],Index,PTemp),
        normalize_prob(PTemp,PL)
    ),
    S1 is Size - 1,
    sample_list_prob(L1,S1,Replace,PL,TOut).


sample(List,Size,_):-
    length(List,N),
    Size > N,
    writeln("Sample size must be smaller or equal to the possible elements. Set Replace to true if you want to sample with replacement."), !,
    false.
sample(List,Size,Result):-
    sample_list(List,Size,false,Result).
sample(List,Size,Replace,Result):-
    ( ( Replace = true ; Replace = false ) ->
        sample_list(List,Size,Result,true) ;
        writeln("Set Replace to true or false"),
        false
    ).
sample(List,Size,Replace,Probabilities,Result):-
    ( ( Replace \= true , Replace \= false ) ->
        writeln("Set Replace to true or false"),
        false ;
        \+ sum_list(Probabilities,1) ->
            writeln("Probabilities must sum to 1"),
            false ;
            sample_list_prob(List,Size,Replace,Probabilities,Result)
    ).

/**
 * seq(A:number,B:number,Seq:List).
 * seq(A:number,B:number,Step:number,Seq:List).
 * List is a list with a sequence ranging from A to B with step Step
 * If step is not provided, 0 is assumed
 * test: seq(1,10,1,[1,2,3,4,5,6,7,8,9,10])
*/
seq(A,B,L):-
    seq(A,B,1,L).
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

