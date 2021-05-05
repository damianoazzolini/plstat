:- module(test_plstat, [test/0,test_plstat_rev/0]).
:- use_module(library(plunit)).

:- set_random(seed(100)).
:- ['../src/plstat'].

test_list([
    mean,
    median,
    mode,
    rms,
    sum_of_squares,
    variance,
    pop_variance,
    std_dev,
    pop_std_dev,
    range,
    midrange,
    mean_absolute_deviation,
    covariance,
    correlation,
    weighted_mean,
    harmonic_mean,
    trimmed_mean,
    trimmed_variance,
    moment,
    skew,
    kurtosis,
    rank,
    nth_row,
    nth_column,
    occurrences_2,
    occurrences_3,
    swap_rows_columns,
    split_n_parts,
    min_val,
    max_val,
    sum,
    prod,
    seq,
    choose,
    factorial
  ]).

test:-
    test_list(L),
    length(L,N),
    write('Testing '), write(N), writeln(' predicates'),
	run_tests(L).

test_plstat_rev:-
    test_list(L),
	reverse(L,Rev),
    run_tests(Rev).

close_to(V,V1):-
    VU is V1 + 0.0005, 
    VL is V1 - 0.0005,
    V >= VL,
    V =< VU.

% statistics
:- begin_tests(mean, []).
test(mean_1):- mean([1,2,3],2).
test(mean_2):- mean([[1,3,4],[7,67]],[2.6666666666666665, 37]).
:- end_tests(mean).

:- begin_tests(median, []).
test(median_1):- median([1,2,3],2).
test(median_2):- median([1,2,3,4],2.5).
test(median_3):- median([[1,5,64],[27,67]],[5,47]).
:- end_tests(median).

:- begin_tests(mode, []).
test(mode_1):- mode([1,2,3,1],[1]).
test(mode_2):- mode([1,2,3,4],[1,2,3,4]).
test(mode_3):- mode([[1,5,64],[27,67]],[[1,5,64],[27,67]]).
:- end_tests(mode).

:- begin_tests(rms, []).
test(rms_1):- rms([1,5,8,3],V),close_to(V,4.97493).
test(rms_2):- rms([[1,5,64],[27,67]],M), L = [37.067505985701274, 51.07837115648854], maplist(close_to,M,L).
:- end_tests(rms).

:- begin_tests(sum_of_squares, []).
test(sum_of_squares_1):- sum_of_squares([1,2,3],2).
test(sum_of_squares_2):- sum_of_squares([1,2,3,46],1454).
test(sum_of_squares_3):- sum_of_squares([[1,5,66],[27,67]],[2654,800]).
:- end_tests(sum_of_squares).

:- begin_tests(variance, []).
test(variance_1):- variance([1,2,4,6,7,8,9],V),close_to(V,9.2380952).
test(variance_2):- variance([1,4,6,72,1],956.7).
:- end_tests(variance).

:- begin_tests(pop_variance, []).
test(pop_variance_1):- pop_variance([1,2,4,6,7,8,9],V),close_to(V,7.9183673).
test(pop_variance_2):- pop_variance([1,4,6,72,1],V),close_to(V,765.3600).
:- end_tests(pop_variance).

:- begin_tests(std_dev, []).
test(std_dev_1):- std_dev([1,2,4,6,7,8,9],S),close_to(S,3.039424).
:- end_tests(std_dev).

:- begin_tests(pop_std_dev, []).
test(std_dev_1):- pop_std_dev([1,2,4,6,7,8,9],S),close_to(S,2.8139594).
:- end_tests(pop_std_dev).

:- begin_tests(range, []).
test(range_1):- range([1,2,4,6,7,8,9],8).
:- end_tests(range).

:- begin_tests(midrange, []).
test(midrange_1):- midrange([1,2,4,6,7,8,9],4).
:- end_tests(midrange).

:- begin_tests(mean_absolute_deviation, []).
test(mean_absolute_deviation_1):- mean_absolute_deviation([1,2,4,6,7,8,9],M),close_to(M,2.5306122).
:- end_tests(mean_absolute_deviation).

:- begin_tests(covariance, []).
test(covariance_1):- covariance([5,12,18,23,45],[2,8,18,20,28],146.1).
:- end_tests(covariance).

:- begin_tests(correlation, []).
test(correlation_1):- correlation([5,12,18,23,45],[2,8,18,20,28],C),close_to(C,0.9366).
:- end_tests(correlation).

:- begin_tests(weighted_mean, []).
test(weighted_mean_1):- weighted_mean([3,8,10,17,24,27],[2,8,10,13,18,20],V),close_to(V,19.1972).
:- end_tests(weighted_mean).

:- begin_tests(harmonic_mean, []).
test(harmonic_mean_1):- harmonic_mean([1,2,3,4,5,6,7],V),close_to(V,2.69972).
test(harmonic_mean_2):- harmonic_mean([1,4],V),close_to(V,1.6).
:- end_tests(harmonic_mean).

:- begin_tests(trimmed_mean, []).
test(trimmed_mean_1):- trimmed_mean([1,2,3,4,5,6,7],3,5,4).
:- end_tests(trimmed_mean).

:- begin_tests(trimmed_variance, []).
test(trimmed_variance_1):- trimmed_variance([1,2,3,4,5,6,7],3,5,1.0).
:- end_tests(trimmed_variance).

:- begin_tests(moment, []).
test(moment_1):- moment([1,2,3,4,5],1,0.0).
test(moment_2):- moment([1,2,3,4,5],2,2.0).
:- end_tests(moment).

:- begin_tests(skew, []).
test(skew_1):- skew([2,8,0,4,1,9,9,0],V),close_to(V,0.26505541).
test(skew_2):- skew([1,2,3,4,5],0.0).
test(skew_3):- skew([2,4,7,3,2],V),close_to(V,0.9704949).
:- end_tests(skew).

:- begin_tests(kurtosis, []).
test(kurtosis_1):- kurtosis([26,12,16,56,112,24],K),close_to(K,3.05052136).
test(kurtosis_2):- kurtosis([3,5,7,2,7],K),close_to(K,1.37315088757).
:- end_tests(kurtosis).

:- begin_tests(rank, []).
test(rank_1):- rank([0,2,3,2],[1,2.5,4,2.5]).
test(rank_2):- rank([0,2,3,2],average,[1,2.5,4,2.5]).
test(rank_3):- rank([0,2,3,2],min,[1,2,4,2]).
test(rank_4):- rank([0,2,3,2],max,[1,3,4,3]).
test(rank_5):- rank([0,2,3,2],dense,[1,2,3,2]).
test(rank_6):- rank([0,2,3,2],ordinal,[1,2,4,3]).
test(rank_7):- rank([[0,2,3,2],[1,4,5]],max,[[1, 4, 5, 4], [2, 6, 7]]).
:- end_tests(rank).

% other utils

:- begin_tests(nth_row, []).
test(nth_row_1):- nth_row([[1,2],[3,4]],2,[3,4]).
:- end_tests(nth_row).

:- begin_tests(nth_column, []).
test(nth_column_1):- nth_column([[1,2],[3,4]],2,[2,4]).
:- end_tests(nth_column).

:- begin_tests(swap_rows_columns, []).
test(swap_rows_columns_1):- swap_rows_columns([[1,2,4],[3,6,7]],[[1,3],[2,6],[4,7]]).
:- end_tests(swap_rows_columns).

:- begin_tests(split_n_parts, []).
test(split_n_parts_1):- split_n_parts([1,2,4,3,7,6],2,[[1,2],[4,3],[7,6]]).
:- end_tests(split_n_parts).

:- begin_tests(occurrences_3, []).
test(occurrences_1):- occurrences([1,2,4,6,7,8,9,1],1,2).
:- end_tests(occurrences_3).

:- begin_tests(occurrences_2, []).
test(occurrences_1):- occurrences([1,2,4,6,7,8,9,1],[[1,2],[2,1],[4,1],[6,1],[7,1],[8,1],[9,1]]).
:- end_tests(occurrences_2).

:- begin_tests(min_val, []).
test(min_1):- min_val([1,2,4,6,7,8,9,1],1).
:- end_tests(min_val).

:- begin_tests(max_val, []).
test(max_1):- max_val([1,2,4,6,7,8,9,1],9).
:- end_tests(max_val).

:- begin_tests(sum, []).
test(sum_1):- sum([1,24,2,3,-1],29).
:- end_tests(sum).

:- begin_tests(prod, []).
test(prod_1):- prod([1,24,2,3,-1],-144).
test(prod_2):- prod([1,24,0,3,-1],0).
:- end_tests(prod).

:- begin_tests(seq, []).
test(seq_1):- seq(1,10,1,[1,2,3,4,5,6,7,8,9,10]).
:- end_tests(seq).

:- begin_tests(choose, []).
test(choose_1):- choose(10,3,120).
:- end_tests(choose).

:- begin_tests(factorial, []).
test(factorial_1):- factorial(10,3628800).
:- end_tests(factorial).