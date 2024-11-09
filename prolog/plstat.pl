:- module(plstat,[
    % help/0, % defined in utils.pl
    % help/1, % defined in utils.pl
    % help/2, % defined in utils.pl
    bug/0,
    list/0,
    suggestion/0,
    mean/2,
    median/2,
    mode/2,
    percentile/3,
    quartile/3,
    iqr/2,
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
    pearson_correlation/3,
    spearman_correlation/3,
    weighted_mean/3,
    geometric_mean/2,
    harmonic_mean/2,
    trimmed_mean/4,
    trimmed_variance/4,
    skew/2,
    kurtosis/2,
    moment/3,
    sum/2, % wrapper for sum_list
    prod/2,
    rescale/2,
    rescale/4,
    mean_normalize/2,
    standardize/2,
    entropy/2,
    entropy/3,
    min_val/2, % wrapper for min_list
    max_val/2, % wrapper for max_list
    min_max_val/3,
    rank/2,
    rank/3,
    nth_row/3,
    nth_column/3,
    swap_rows_columns/2,
    split_n_parts/3,
    occurrences/2,
    occurrences/3,
    normalize_prob/2,
    delete_nth1/3,
    sample/3,
    sample/4,
    sample/5,
    empirical_distribution/3,
    seq/4,
    factorial/2,
    choose/3,
    search_position_sorted/3,
    search_position_sorted/4
    ]).

:- [utils].

% multidimensional wrapper
multidim2(Predicate,List,Res):-
    ( List = [A|_], is_list(A) ->
        maplist(Predicate,List,Res);
        Call =.. [Predicate,List,Res],
        Call
    ).

ground_and_numbers_and_nonempty(Predicate, L):-
    ground_list(Predicate,L),
    numbers(Predicate,L),
    nonempty(Predicate,L).
nonempty(Predicate, L):-
    length(L,N),
    ( N > 0 -> true ; write(Predicate), writeln(": list cannot be empty"), false).
ground_list(Predicate, L):-
    ( ground(L) -> true ; write(Predicate), writeln(": input list must be ground"), false).
numbers(Predicate, L):- 
    ( maplist(number,L) -> true ; write(Predicate), writeln(": all the elements must be numbers"), false).
same_length_lists(Predicate, L1, L2):-
    length(L1,N),
    length(L2,M),
    ( M = N -> true ;
        write(Predicate), writeln(': input lists must have the same length'),
        write('Found '), write(M), write(' expected '), writeln(N),
        false
    ).

all_positive(Predicate,W):-
    ( maplist(=<(0),W) -> true ; write(Predicate), writeln(": all the weigths must be positive (>= 0)"), false).
all_strictly_positive(Predicate,W):-
    ( maplist(<(0),W) -> true ; write(Predicate), writeln(": all the weigths must be positive (> 0)"), false).
nonzerosum(Predicate,W,Sum):-
    sum(W,Sum),
    (Sum > 0 -> true ; write(Predicate), writeln(": weigths must not sum to 0"), false).
check_bounds(Predicate,L,U):-
    (number(L) -> true; write(Predicate), writeln(": lower bound must be a number."), false),
    (number(U) -> true; write(Predicate), writeln(": upper bound must be a number."), false),
    (L < U ->
        true ;
        write(Predicate),
        writeln(": the lower bound must be smaller than the upper bound"),
        false
    ).

/**
 * mean(+List:number,-Mean:float)
 * Mean is the mean of the list List.
 * List can also be multidimensional (list of lists).
 * Example: mean([1,2,3],M).
 * Expected: M = 2.
 * Example: mean([[1,3,4],[7,67]],L).
 * Expected: [2.666,37].
 * */
mean(L,Mean):-
    multidim2(mean_,L,Mean).
mean_(L,Mean):-
    ground_and_numbers_and_nonempty(mean/2,L),
	length(L,N),
	sum(L,S),
    Mean is S/N.
/**
 * median(+List:number,-Median:number)
 * Median is the median of the list List.
 * List can also be multidimensional (list of lists).
 * Example: median([1,2,3],M).
 * Expected: M = 2.
 * Example: median([[1,5,64],[27,67]],M).
 * Expected: M = [5, 47].
 * */
median(L,Median):-
    multidim2(median_,L,Median).
median_(Lin,Median):-
    ground_and_numbers_and_nonempty(median/2,Lin),
    msort(Lin,L),
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
 * mode(+List:number,-Mode:list)
 * Mode is the mode of the list List
 * List can also be multidimensional (list of lists)
 * Example: mode([1,2,3,1],M).
 * Expected: M = 1.
 * Example: mode([[1,5,64],[27,67]],M).
 * Expected: M = [[1,5,64], [27,67]].
 * TODO: check this
 * */
comp(<,[_,A1],[_,A2]) :- A1 > A2. % to have in descending order
comp(>, _, _).
mode(L,Mode):-
    multidim2(mode_,L,Mode).
mode_(L,ModeC):-
	occurrences(L,Occ),
    predsort(comp,Occ,X), !,
    X = [[_,O]|_],
    findall(V,member([V,O],Occ),Mode),
    length(Mode,Mod),
    (Mod = 1 -> Mode = [ModeC] ; ModeC = Mode).

/**
 * percentile(+List:number,+K:number,-Percentile:number)
 * Percentile is the k-th percentile of the list.
 * Both List and K can be multidimensional (lists of lists).
 * Fails with a message if list is empty of if one of the lists
 * of the list of lists is empty.
 * Algorithm: arrange the data in ascending order,
 * compute r = (p/100) * (n-1) + 1 where p is the percentile
 * If r is integer, then the r-th element is the desired percentile
 * Otherwise, it is x_ceil(r) + (r - ceil(r)) * (x_(ceil(r)+ 1) - x_ceil(r)) 
 * The last formula is valid in both cases
 * Example: percentile([1,2,3,4,6,5,9],40,P).
 * Expected: P = 3.4.
 * Example: percentile([1,2,3,4,6,5],40,P).
 * Expected: P = 3.0.
 * Example: percentile([1,2,3,4,6,5],[10,40],P)
 * Expected: P = [1.5,3.0].
 * Example: percentile([[1,2,3,4,6,5],[15,25]],[10,40],P).
 * Expected: P = [[1.5,3.0],[16.0,19.0]].
 * */
percentile(L,K,Percentile):-
    ( L = [A|_], is_list(A) -> 
        maplist(percentile_list(K),L,Percentile);
        percentile_list(K,L,Percentile)
    ).
percentile_list(K,L,P):-
    ( is_list(K) ->
        maplist(percentile_single(L),K,P);
        percentile_single(L,K,P)
    ).
percentile_single(L,K,Percentile):-
    ground_and_numbers_and_nonempty(percentile/3,L),
    (number(K) -> true ; writeln("percentile/3: the percentile must be a number"), false),
    ( (K >= 0, K =< 100) -> true ; writeln("percentile/3: k must be between 0 and 100"), false),
    ( L = [V] -> % only one value
        Percentile = V ;
        msort(L,LS),
        length(L,N),
        R is (K / 100)*(N - 1) + 1,
        RI is floor(R),
        RII is RI + 1,
        nth1(RI,LS,V1),
        nth1(RII,LS,V2),
        RF is R - RI,
        Percentile is V1 + RF * (V2 - V1)
    ).

/**
 * quartile(+List:number,+Q:number,-Quartile:number)
 * Quartile is the Q quartile of the list List.
 * Both List and Q can be multidimensional (lists of lists).
 * Wrapper for percentile.
 * Example: quartile([15,25],2,Q).
 * Expected: Q = 20.
 * */
quartile(L,Q,Quartile):-
    (number(Q) -> true ; writeln("quartile/3: the quartile must be a number"), false),
    ( member(Q,[1,2,3]) ->
        !,
        QP is Q * 25, 
        percentile(L,QP,Quartile) ;
        writeln("quartile/3: the quartile must be a number among 1, 2, or 3"),
        false
    ).

/**
 * iqr(+List:number,-IQR:number)
 * IQR is the inter quartile range of list List
 * computed as the difference between 3rd - 1st quartile.
 * List can also be multidimensional (list of lists)
 * Example: iqr([1,2,3,4,6,5],I).
 * Expected: I = 2.5.
 * */
iqr(L,IQR):-
    quartile(L,3,III),
    quartile(L,1,I),
    IQR is III - I.

/**
 * rms(+List:number,-RMS:number)
 * RMS is the root mean square of the list List.
 * List can also be multidimensional (list of lists).
 * Square root of the sum of the squared data values divided by the number of values.
 * Example: rms([1,5,8,3],S).
 * Expected: S = 4.97493.
 * */
square(X,XS):-
    pow(X,2,XS).
rms(L,RMS):-
    multidim2(rms_,L,RMS).
rms_(L,RMS):-
    ground_and_numbers_and_nonempty(rms/2,L),
    length(L,N),
    maplist(square,L,LS),
    sum(LS, SS),
    RMS is sqrt(SS / N).

/**
 * sum_of_squares(+List:number,-SumOfSquares:number)
 * SumOfSquares is the sum of squares of the list List.
 * List can also be multidimensional (list of lists).
 * Formula: \sum_n (x - \mu)^2
 * Example: sum_of_squares([1,2,3],S)
 * Expected: S = 2.
 * */
diff_square(Mu,B,D):-
    AB is B - Mu,
    pow(AB,2,D).
sum_of_squares(List,SumOfSquares):-
    multidim2(sum_of_squares_,List,SumOfSquares).
sum_of_squares_(L,SumSquared):-
    ground_and_numbers_and_nonempty(rms/2,L),
    mean(L,Mean),
    maplist(diff_square(Mean),L,Res),
    sum(Res,SumSquared).

/**
 * variance(+List:number,-Variance:number)
 * Variance is the sample variance of the list List.
 * List can also be multidimensional (list of lists).
 * Formula: (1/(N - 1)) * \sum_n (x_i - \mu)^2
 * Example: variance([1,2,4,6,7,8,9],V).
 * Expected: V = 9.2380952.
 * */
variance(L,Var):-
    multidim2(variance_,L,Var).
variance_(L,Var):-
    ground_and_numbers_and_nonempty(rms/2,L),
    length(L,N),
    ( N < 2 -> writeln("variance/3: the quartile must be a number"), false ; true),
    sum_of_squares(L,SS),
    N1 is N - 1,
    Var is (1/N1) * SS.

/**
 * pop_variance(+List:number,-Variance:number)
 * Variance is the population variance of the list List.
 * List can also be multidimensional (list of lists).
 * Formula: (1/N) * \sum_n (x_i - \mu)^2
 * Example: pop_variance([1,4,6,72,1],V).
 * Expected: V = 765.3600.
 * */
pop_variance(L,Var):-
    multidim2(pop_variance_,L,Var).
pop_variance_(L,Var):-
    ground_and_numbers_and_nonempty(rms/2,L),
    length(L,N),
    ( N < 2 -> writeln("variance/3: the quartile must be a number"), false ; true),
    length(L,N),
    sum_of_squares(L,SS),
    Var is (1/N) * SS.

/* std_dev(+List:numbers,-StdDev:number)
 * StdDev is the standard deviation of the list List (square root of the sample variance).
 * List can also be multidimensional (list of lists).
 * Example: std_dev([1,2,4,6,7,8,9],S).
 * Expected: S = 3.039424.
 * */
std_dev(L,StdDev):-
    multidim2(std_dev_,L,StdDev).
std_dev_(L,StdDev):-
    variance(L,V),
    StdDev is sqrt(V).

/* pop_std_dev(+List:numbers,-StdDev:number)
 * StdDev is the population standard deviation of the list List (square root of the population variance)
 * List can also be multidimensional (list of lists)
 * Example: pop_std_dev([1,2,4,6,7,8,9],S).
 * Expected: S = 3.039424.
 * */
pop_std_dev(L,StdDev):-
    multidim2(pop_std_dev_,L,StdDev).
pop_std_dev_(L,StdDev):-
    pop_variance(L,V),
    StdDev is sqrt(V).

/**
 * range(+List:numbers,-Range:number)
 * Range is the difference between the biggest and the smallest
 * element of the list List.
 * List can also be multidimensional (list of lists).
 * Example: range([1,2,4,6,7,8,9],R).
 * Expected: R = 8.
 * */
range(L,Range):-
    multidim2(range_,L,Range).
range_(L,Range):-
    ground_and_numbers_and_nonempty(range/2,L),
    min_val(L,Min),
    max_val(L,Max),
    Range is Max - Min.
    
/**
 * midrange(+List:numbers,-Midrange:number)
 * Midrange is (Max - Min) / 2 of the list List (half of the range).
 * List can also be multidimensional (list of lists).
 * Example: midrange([1,2,4,6,7,8,9],M).
 * Expected: M = 4.
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
 * List can also be multidimensional (list of lists)
 * Example: mean_absolute_deviation([1,2,4,6,7,8,9],M).
 * Expected: M = 2.5306122.
 * */
diff_abs(A,B,D):-
    AB is A - B,
    D is abs(AB).
mean_absolute_deviation(L,MAD):-
    multidim2(mean_absolute_deviation_,L,MAD).
mean_absolute_deviation_(L,MAD):-
    mean(L,Mean),
    maplist(diff_abs(Mean),L,LD),
    sum(LD,S),
    length(L,N),
    MAD is (1/N) * S.

/**
 * covariance(+List1:numbers,+List2:numbers,-Covariance:number)
 * Covariance is the covariance of the lists List1 and List2
 * Example: covariance([5,12,18,23,45],[2,8,18,20,28],C).
 * Expected: C = 146.1
 * */
sub(A,B,C):-
    C is B - A.
mul(A,B,C):-
    C is A * B.
covariance(L1,L2,Cov):-
    ground_and_numbers_and_nonempty(covariance/3,L1),
    ground_and_numbers_and_nonempty(covariance/3,L2),
    same_length_lists(covariance/3,L1,L2),
    length(L1,N),
    mean(L1,M1),
    mean(L2,M2),
    maplist(sub(M1),L1,LS1),
    maplist(sub(M2),L2,LS2),
    maplist(mul,LS1,LS2,LP),
    sum(LP,S),
    Cov is S / (N - 1).

/**
 * correlation(+List1:numbers,-List2:numbers,-Correlation:number)
 * Correlation is the Pearson correlation of List1 and List2
 * Formula: covariance(List1,List2) / (std_dev(List1) * std_dev(List2))
 * Example: correlation([5,12,18,23,45],[2,8,18,20,28],C).
 * Expected: C = 0.9366.
 * Example: spearman_correlation([5,12,18,23,45],[2,8,18,20,28],C).
 * Expected: C = 0.999.
 * */
correlation(L1,L2,Corr):-
    covariance(L1,L2,Cov),
    std_dev(L1,S1),
    std_dev(L2,S2),
    Corr is Cov / (S1 * S2).
pearson_correlation(L1,L2,C):-
    correlation(L1,L2,C).
spearman_correlation(L1,L2,C):-
    rank(L1,fractional,LF1),
    rank(L2,fractional,LF2),
    correlation(LF1,LF2,C).


/**
 * geometric_mean(+List:numbers,+Weights:numbers,-GM:number)
 * GM is the geometric mean of the list List: (\prod x_i)^(1/n)
 * where n is the number of elements in the list.
 * All the elements must be strictly positive (> 0).
 * `List` can also be multidimensional (list of lists).
 * Example: geometric_mean([54, 24, 36], GM).
 * Expected: GM = 36.
 * geometric_mean([[54, 24, 36],[1,2]], GM)
 * Expected: GM = [36,1.4142135623730951].
 * */
geometric_mean(L,Mean):-
    multidim2(geometric_mean_,L,Mean).
geometric_mean_(List, GM):-
    ground_and_numbers_and_nonempty(geometric_mean/3,List),
    all_strictly_positive(geometric_mean/3,List),
    length(List,N),
    prod(List,P),
    GM is P**(1/N).

/**
 * weighted_mean(+List:numbers,+Weights:numbers,-WM:number)
 * WM is the weighted mean of the list List: \sum x_i*w_i / \sum w_i
 * Weights must sum to > 0 and all weights must be positive (> 0).
 * Example: weighted_mean([3,8,10,17,24,27],[2,8,10,13,18,20],WM).
 * Expected: WM = 19.1972.
 * */
weighted_mean(List,Weights,WM):-
    ground_and_numbers_and_nonempty(weighted_mean/3,List),
    ground_and_numbers_and_nonempty(weighted_mean/3,Weights),
    same_length_lists(weighted_mean/3,List,Weights),
    all_positive(weighted_mean/3,Weights),
    nonzerosum(weighted_mean/3,Weights,SW),
    maplist(mul,List,Weights,LP),
    sum(LP,LS),
    WM is LS / SW.

/**
 * harmonic_mean(+List:numbers,-HM:number)
 * HM is the harmonic mean of list List 
 * Formula: n / (1/x1 + 1/x2 + ... + 1/xn).
 * List can also be multidimensional (list of lists).
 * All the elements must be positive (>= 0).
 * Example: harmonic_mean([1,2,3,4,5,6,7],HM).
 * Expected: HM = 2.69972
 * */
rec(X,X1):- X1 is 1/X.
harmonic_mean(L,HM):-
    multidim2(harmonic_mean_,L,HM).
harmonic_mean_(L,HM):-
    ground_and_numbers_and_nonempty(harmonic_mean/2,L),
    all_strictly_positive(harmonic_mean/2,L),
    length(L,N),
    maplist(rec,L,LR),
    sum(LR,SLR),
    HM is N / SLR.

/**
 * trimmed_mean(+List:numbers,+Lower:number,+Upper:number,-TM:number)
 * TM is the trimmed mean of the list List, i.e., 
 * the mean computed by considering only numbers 
 * in the range [Lower,Upper].
 * Example: trimmed_mean([1,2,3,4,5,6,7],3,5,T).
 * Expected: T = 4
 * */
trimmed_mean(L,Lower,Upper,TM):-
    check_bounds(trimmed_mean/3,Lower,Upper),
    include(between(Lower,Upper),L,LO),
    mean(LO,TM).

/**
 * trimmed_variance(+List:numbers,+Lower:number,+Upper:number,-TV:number)
 * TV is the trimmed variance of the list List, i.e, 
 * the variance computed by considering only numbers 
 * in the range [Lower,Upper]
 * Example: trimmed_variance([1,2,3,4,5,6,7],3,5,V).
 * Expected: V = 1.0
 * */
trimmed_variance([],_,_,0).
trimmed_variance(_,L,U,_):-
    L > U, !,
    writeln("The lower bound must be actually smaller than the upper bound"),
    false.
trimmed_variance(L,Lower,Upper,TV):-
    include(between(Lower,Upper),L,LO),
    variance(LO,TV).

/**
 * moment(+List:numbers,+M:integer,-Moment:number)
 * Moment is the M-th moment about the mean for the list List
 * Formula: 1/n \sum (x_i - x_mean) ^ M
 * Example: moment([1,2,3,4,5],2,MO).
 * Expected: MO = 2
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
 * Skew is the sample skewness of list List
 * Formula: m_3 / (m_2)^(3/2)
 * List can also be multidimensional (list of lists)
 * Example: skew([2,8,0,4,1,9,9,0],S).
 * Expected: S = 0.26505541
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
 * List can also be multidimensional (list of lists).
 * Example: kurtosis([3,5,7,2,7],K).
 * Expected: K = 1.3731508875.
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
 * If Method is not provided, by default it performs 
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
 * Example: rank([0,2,3,2],R).
 * Expected: R = [1.0,2.5,4.0,2.5].
 * Example: rank([0,2,3,2],average,R).
 * Expected: R = [1.0,2.5,4.0,2.5].
 * Example: rank([0,2,3,2],min,R).
 * Expected: R = [1,2,4,2].
 * Example: rank([0,2,3,2],max,R).
 * Expected: R = [1,3,4,3].
 * Example: rank([0,2,3,2],dense,R).
 * Expected: R = [1,2,3,2].
 * Example: rank([0,2,3,2],ordinal,R).
 * Expected: R = [1,2,4,3].
 * Example: rank([[0,2,3,2],[1,4,5]],max,[[1,4,5,4],[2,6,7]]).
 * Expected: R = [[1,4,5,4],[2,6,7]].
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
    	sum(LR,S),
        Rank1 is (S+Rank) / (NR+1),
        delete(T,[V,_],T1)
    ),
    look_in_average(T1,T0).

% average
compute_average(LS,I,LAvg):-
    compute_ordinal(LS,I,LAvg1),
    look_in_average(LAvg1,LAvg).

%%%%%%%%
% other utils
%%%%%%%%

/**
 * nth_row(+ListOfList:numbers,+Nth:integer,-NthRow:List)
 * NthRow is the nth row of ListOfList, counting from 1
 * Example: nth_row([[1,2],[3,4]],2,N).
 * Expected: N = [3,4].
 * */
nth_row(L,Nth,NthRow):-
    nth1(Nth,L,NthRow).

/**
 * nth_column(+ListOfList:numbers,+Nth:integer,-NthColumn:List)
 * NthColumn is the nth column of ListOfList, counting from 1
 * Example: nth_column([[1,2],[3,4]],2,N).
 * Expected: N = [2,4]
 * */
nth_column(L,Nth,NthColumn):-
    findall(E,(member(R,L),nth1(Nth,R,E)),NthColumn).

/**
 * swap_rows_columns(+LRows:numbers,+ListOfLists:LColumns)
 * LColumns is LRows transposed (rows and columns swapped)
 * Example: swap_rows_columns([[1,2,4],[3,6,7]],R).
 * Expected: R = [[1,3],[2,6],[4,7]].
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
 * PartsList is a list of lists obtained by splitting list List
 * in Parts parts.
 * Example: split_n_parts([1,2,4,3,7,6],2,S).
 * Expected: S = [[1,2],[4,3],[7,6]].
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
 * Occ is a list [Value,Occurrences] for each element in list List
 * example: occurrences([1,2,4,6,7,8,9,1],O).
 * Expected: O = [[1,2],[2,1],[4,1],[6,1],[7,1],[8,1],[9,1]].
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
 * Occ is the occurrences of Number in list List
 * Example: occurrences([1,2,4,6,7,8,9,1],1,O).
 * Expected: O = 2.
 * */
occurrences([],_,0).
occurrences(L,E,0):- ground(E), \+member(E,L).
occurrences(L,E,N):-
    findall(I,nth0(I,L,E),LI),
    length(LI,N).

/**
 * min_val(+List:numbers,-Min:number)
 * Min is the smallest value of the list List.
 * List can also be multidimensional (list of lists).
 * Example: min_val([1,2,4,6,7,8,9,1],M).
 * Expected: M = 1.
 * */
min_val(L,M):-
    multidim2(min_list,L,M).

/**
 * max_val(+List:numbers,-Max:number)
 * Max is the biggest value of the list List.
 * List can also be multidimensional (list of lists).
 * Example: max_val([1,2,4,6,7,8,9,1],M).
 * Expected: M = 9.
 * */
max_val(L,M):-
    multidim2(max_list,L,M).

/**
 * min_max_val(+List:numbers,-Min:number,-Max:number)
 * Min and Max are the minimum and the maximum of the list List respectively
 * example: min_max_val([1,2,4,6,7,8,9,1],Min,Max).
 * expected: Min = 1, Max = 9.
 * */
min_max_val(L,Min,Max):-
    min_val(L,Min),
    max_val(L,Max).

/**
 * sum(+List:numbers,-Sum:number)
 * Sum is the sum of the elements in List.
 * List can also be multidimensional (list of lists).
 * Example: sum([1,24,2,3,-1],S).
 * Expected: S = 29. 
 * */
sum(L,S):-
    multidim2(sum_list,L,S).

/**
 * prod(+List:numbers,-Prod:number)
 * Prod is the product of the elements in list List.
 * List can also be multidimensional (list of lists).
 * Example: prod([1,24,2,3,-1],P).
 * Expected: P = -144.
 * */
prod([],0).
prod(L,P):-
    ( L = [A|_], is_list(A) -> 
        maplist(prod(1),L,P);
        prod(1,L,P)
    ).
prod(N,[],N):- !.
prod(_,[0|_],0):- !.
prod(P0,[H|T],P1):-
    PT is P0 * H,
    prod(PT,T,P1).


%%%%%%%%%%%%%%%%%%%%%%%%

/**
 * normalize_prob(+List:numbers,-NormalizedList:number)
 * NormalizedList is the list List normalized.
 * List must contain only elements between 0 and 1 (included).
 * List can also be multidimensional (list of lists).
 * Formula: i / list_sum foreach i in List.
 * Example: normalize_prob([0.07,0.14,0.07],L).
 * Expected: L = [0.25,0.5,0.25].
 * */
div(Sum,El,Div):-
    Sum \= 0,
    Div is El / Sum.

between_float(L,U,N):-
    N >= L,
    N =< U.

normalize_prob([],[]):- !.
normalize_prob(L,LNorm):-
    multidim2(normalize_prob_,L,LNorm).
normalize_prob_(L,LNorm):-
    maplist(between_float(0,1),L),
    sum(L,SL),
    maplist(div(SL),L,LNorm).

/**
 * rescale(+List:numbers,-Rescaled:numbers)
 * rescale(+List:numbers,+Lower:number,+Upper:number,-Rescaled:number)
 * Rescaled is list List rescaled in the range [Lower,Upper]
 * Also known as min-max normalization
 * List can also be multidimensional (list of lists)
 * If Lower and Upper are not provided, they are set by default to 0 and 1
 * Every x is rescaled as Lower + ((x - min_list)*(Upper - Lower)) / (max_list - min_list)
 * Example: rescale([0.07,0.14,0.07],L).
 * Expected: L = [0.0,1.0,0.0]
 * Example: rescale([0.07,0.14,0.07],2,3,L).
 * Expected: L = [2.0,3.0,2.0]
 * */
rescale([],[]).
rescale(L,LResc):-
    rescale(L,0,1,LResc).
rescale(L,Lower,Upper,LResc):-
    ( Lower >= Upper ->
        writeln("Upper bound should be greater than lower bound");
        true
    ),
    ( L = [A|_], is_list(A) -> 
        maplist(rescaling_(Lower,Upper),L,LResc);
        rescaling_(Lower,Upper,L,LResc)
    ).
rescaling_(Lower,Upper,L,LR):-
    min_val(L,Min),
    max_val(L,Max),
    (Min = Max ->
        writeln("Min = Max, cannot divide by 0"),
        false;
        maplist(rescaling(Min,Max,Lower,Upper),L,LR)
    ).

rescaling(Min,Max,Lower,Upper,V,VResc):-
    VResc is Lower + ((V - Min)*(Upper - Lower))/(Max - Min).

/**
 * mean_normalize(+List:numbers,-Normalized:numbers)
 * Normalized is list List mean normalized
 * List can also be multidimensional (list of lists)
 * Formula: (x - mean_list) / (Upper - Lower) foreach x in List
 * Example: mean_normalize([1,2,4],L).
 * Expected: L = [-0.444, -0.111, 0.555]. 
 * */
mean_normalize([],[]).
mean_normalize(List,Normalized):-
    multidim2(mean_normalize_,List,Normalized).
mean_normalize_(List,Normalized):-
    min_val(List,Min),
    max_val(List,Max),
    mean(List,Mean),
    (Min = Max ->
        writeln("Min = Max, cannot divide by 0"),
        false;
        maplist(mean_normalizing(Min,Max,Mean),List,Normalized)
    ).
mean_normalizing(Min,Max,Mean,X,Normalized):-
    Normalized is (X - Mean) / (Max - Min).

/**
 * standardize(+List:numbers,-Standardized:numbers)
 * Standardized is list List standardized
 * Formula: (x - mean_list) / std_dev_list foreach x in List
 * List can also be multidimensional (list of lists)
 * Population std_dev is considered (divided by n)
 * Example: standardize([1,2,4],L).
 * Expected: L = [-1.0690449,-0.2672612,1.336306]. 
 * */
standardize(List,Standardized):-
    multidim2(standardize_,List,Standardized).
standardize_(List,Standardized):-
    mean(List,Mean),
    pop_std_dev(List,SD),
    (SD = 0 -> 
        writeln("Cannot standardize, standard deviation in 0"),
        false;
        maplist(standardizing(Mean,SD),List,Standardized)
    ).
standardizing(Mean,SD,X,Standardized):-
    Standardized is (X - Mean) / SD.

/**
 * entropy(+List:numbers,-Entropy:number)
 * entropy(+List:numbers,+Probabilities:number,-Entropy:number)
 * Entropy is the entropy of the list List
 * Formula: is probabilities are not provided, then 
 * E = -sum(pk * log(pk)
 * else
 * E = sum(pk * log(pk / qk)
 * Logarithm in base e (natural logarithm) is computed
 * Example: entropy([9/10,1/10],E).
 * Expected: E = 0.325082.
 * Example: entropy([1/2,1/2],[9/10,1/10],E).
 * Expected: E = 0.5108256.
 * */
prod_and_log(P,PR):-
    PR is P * log(P).
prod_and_log_div(P,Q,PR):-
    D is P/Q,
    PR is P * log(D).
entropy(L,E):-
    maplist(prod_and_log,L,PR),
    sum(PR,S),
    E is -S.
entropy(L,LP,E):-
    sum(LP,S),
    ( S \= 1.0 ->
        writeln("Probabilities must sum to 1"), 
        false;
        true
    ),
    writeln(LPP),
    ( maplist(<(0),LPP),maplist(>(1),LPP) -> 
        true;
        writeln("Probabilities must be between 0 and 1"),
        false
    ),
    maplist(prod_and_log_div,L,LP,P),
    sum(P,E).

/**
 * delete_nth1(+List:numbers,+Index:numbers,-LDeleted:number)
 * LDeleted is List with the element at pos Index removed, counting from 1
 * If Index is greater than the length of the list, fails
 * Example: delete_nth1([1,2,7,4],3,L).
 * Expected: L = [1,2,4].
 * */
delete_nth1(L,N,LDeleted):-
    L = [A|_],
    (is_list(A) ->
        maplist(delete_nth1_(N),L,LDeleted) ;
        delete_nth1_(N,L,LDeleted)
    ).
delete_nth1_(1,[_|T],T):- !.
delete_nth1_(Nth,[H|T],[H|T1]):-
    Nth > 1,
	N is Nth-1,
    delete_nth1_(N,T,T1).

/**
 * sample(+List:elements,+Size:number,-Result:list)
 * sample(+List:elements,+Size:number,+Replace:bool,-Result:list)
 * sample(+List:elements,+Size:number,+Replace:bool,+Probabilities:list,-Result:list)
 * Takes a sample of size Size from list List.
 * Replace can be true or false, if not provided is false
 * Probabilities is a list of probabilities.
 * If Replace is false and a list of probabilities is specified, the list is
 * normalized, after the removal of the element 
 * Example: sample([1,2,3,4,5],5,L).
 * Example: sample([1,2,3,4,5],5,true,L).
 * Example: sample([1,2,3,4,5],5,false,L).
 * Example: sample([1,2,3,4,5],5,true,[0.1,0.1,0.1,0.1,0.6],L).
 * Example: sample([1,2,3,4,5],5,false,[0.1,0.1,0.1,0.1,0.6],L).
 * */
sample_list(_,0,_,[]):- !.
sample_list(L,N,Replace,[H|T]):-
    length(L,Len),
    random_between(1,Len,R),
    nth1(R,L,H),
    ( Replace = true ->
        L1 = L ;
        delete_nth1(L,R,L1)
    ),
    N1 is N - 1,
    sample_list(L1,N1,Replace,T).

get_random_el([H],_,_,_,_,H):- !.
get_random_el([H|_],P,I,I,_,H):-
    P =< 0, !.
get_random_el([_|T],P,I,I0,[CP|TP],El):-
    P > 0,
    P1 is P - CP,
    I1 is I + 1,
    get_random_el(T,P1,I1,I0,TP,El).

sample_list_prob(_,0,_,_,[]):- !.
sample_list_prob(L,Size,Replace,[HP|TP],[Out|TOut]):-
    Size > 0,
    random(R),
    RP is R - HP,
    get_random_el(L,RP,1,Index,[HP|TP],Out),
    ( Replace = true ->
        L1 = L,
        PL = [HP|TP] ;
        delete_nth1(L,Index,L1),
        delete_nth1([HP|TP],Index,PTemp),
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
        sample_list(List,Size,Replace,Result) ;
        writeln("Set Replace to true or false"),
        false
    ).
sample(List,Size,Replace,Probabilities,Result):-
    ( ( Replace \= true , Replace \= false ) ->
        writeln("Set Replace to true or false"),
        false ;
        \+ sum(Probabilities,1.0) ->
            writeln("Probabilities must sum to 1"),
            false ;
            length(List,NL), length(Probabilities,NP),
            NP \= NL ->
            writeln("The list with the probabilities must be of the same length of the list with the elements to sample"),
            false ;
            sample_list_prob(List,Size,Replace,Probabilities,Result)
    ).

/**
 * empirical_distribution(+List:numbers,+X:number,-Result:list)
 * Result is the empirical distribution of list List at point X
 * List and X can also be multidimensional (lists of lists)
 * Example: empirical_distribution([0,1,2,2,4,6,6,7],0,E).
 * Expected: E = 0.125.
 * Example: empirical_distribution([0,1,2,2,4,6,6,7],2,E).
 * Expected: E = 0.5.
 * Example: empirical_distribution([0,1,2,2,4,6,6,7],7,E).
 * Expected: E = 1.
 * Example: empirical_distribution([[0,1,2,2,4,6,6,7],[1,2,4]],[6,7,8],E).
 * Expected: E = [[0.875,1,1],[1,1,1]].
 * */
empirical_distribution(L,X,R):-
    ( L = [A|_], is_list(A) -> 
        maplist(empirical_distribution_list(X),L,R);
        empirical_distribution_list(X,L,R)
    ).
empirical_distribution_list(X,L,R):-
    ( is_list(X) ->
        maplist(empirical_distribution_single(L),X,R);
        empirical_distribution_single(L,X,R)
    ).
empirical_distribution_single(List,X,R):-
    length(List,N),
    ( X < 0 ->
        R = 0 ;
      X >= N ->
        R = 1 ;
        msort(List,LS),
        nth0(X,LS,Num),
        findall(I,nth0(I,LS,Num),LI),
        max_list(LI,Max),
        D is Max - X + 1,
        X1 is X + D,
        R is X1 / N
    ).

/**
 * seq(A:number,B:number,Seq:List).
 * seq(A:number,B:number,Step:number,Seq:List).
 * List is a list with a sequence ranging from A to B with step Step
 * If step is not provided, 1 is assumed
 * Example: seq(1,10,1,S).
 * Expected: S = [1,2,3,4,5,6,7,8,9,10].
 * */
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
 * Example: factorial(10,F).
 * Expected: F = 3628800.
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
 * Example: choose(10,3,C).
 * Expected: C = 120.
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

/**
 * random_list(+NElements:number,+Lower:number,+Upper:number,-List:numbers).
 * List is a list of NElements random numbers between Lower and Upper
 * example: random_list(3,2,4,R).
 * */
% random_list(N,Lower,Upper,L):-
%     ( N =< 0 ->
%         writeln("The number of elements must be greater than 0"),
%         false;
%         sample_var(uniform,Lower,Upper,N,L)
%     ).

% random variables
% expected_value_var(Val,Occ,Exp)
% variance_var(Val,Occ,Exp)

% TODO: set verbose/0 with assert to test also failures without printing

/**
 * search_position_sorted(+List:numbers,+Element:number,-Pos:integer)
 * search_position_sorted(+List:numbers,+Element:number,+Direction:term,-Pos:integer)
 * Pos is the position that the element Element would have when inserted
 * in list List to preserve its order
 * 0 means the first location. If the element should be inserted in the
 * last position, Pos = N where N is the length of List
 * Counting from 1
 * List and Element can also be multidimensional (lists of lists)
 * Direction can be left (default) or right 
 * Example: search_position_sorted([1,2,3,4,5],3,P).
 * Expected result: P = 2.
 * Example: search_position_sorted([1,2,3,4,5],3,right,P).
 * Expected result: P = 3.
 * % https://numpy.org/doc/stable/reference/generated/numpy.searchsorted.html
 * */
search_position_sorted(List,Element,Pos):-
    search_position_sorted(List,Element,left,Pos).
search_position_sorted(List,Element,Direction,Pos):-
    L = [right,left],
    (\+member(Direction,L) -> writeln("Position must be left or right"), false ; true),
    ( List = [A|_], is_list(A) -> 
        maplist(search_position_sorted_list(Element,Direction),List,Pos);
        search_position_sorted_list(Element,Direction,List,Pos)
    ).
search_position_sorted_list(X,Direction,L,R):-
    ( is_list(X) ->
        maplist(search_position_sorted_single(L,Direction),X,R);
        search_position_sorted_single(L,Direction,X,R)
    ).
search_position_sorted_single(L,Direction,X,R):-
    msort(L,Sorted),
    ( Direction = right -> 
        search_pos(L,X,1,0,R) ; 
        length(Sorted,N),
        search_pos(L,X,-1,N,R)
    ).
search_pos([],_,_,P,P).
search_pos([H|T],El,Inc,CP,P):-
    ( El < H -> 
        CP = P ;
        CP1 is CP + Inc,
        search_pos(T,El,Inc,CP1,P)
    ).


% /**
%  * sample_distribution()
%  * Sample from the specified distribution
%  * */
% sample_distribution(uniform,S):-
%     sample_distribution(uniform,1,0,1,S).
% sample_distribution(uniform,Lower,Upper,S):-
%     sample_distribution(uniform,1,Lower,Upper,S).
% sample_distribution(uniform,NSamples,Lower,Upper,S):-
%     ( NSamples < 0 ->
%         writeln("Number of samples must be greater than 0"),
%         false ;
%         length(S,NSamples),
%         maplist(sample_uniform(Lower,Upper),S)
%     ).
% sample_uniform(Lower,Upper,S):-
%     random(R),
%     S is (Upper - Lower + 1) * R + Lower.    