:- module(test_plstat, [test_plstat/0]).
:- use_module(library(plunit)).

:- set_random(seed(100)).
:- ['../prolog/plstat'].

test_list([
    mean,
    median,
    mode,
    percentile,
    quartile,
    iqr,
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
    geometric_mean,
    harmonic_mean,
    trimmed_mean,
    trimmed_variance,
    moment,
    skew,
    kurtosis,
    rescale,
    mean_normalize,
    standardize,
    entropy,
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
    delete_nth1,
    normalize_prob,
    sample,
    empirical_distribution,
    seq,
    choose,
    factorial,
    search_position_sorted
  ]).

test_plstat:-
    test_list(L),
    length(L,N),
    write('Testing '), write(N), writeln(' predicates'),
	run_tests(L).

close_to(V,V1):-
    VU is V1 + 0.0005, 
    VL is V1 - 0.0005,
    V >= VL,
    V =< VU.

% statistics
:- begin_tests(mean, []).
test(mean_0, [fail]):- mean([], _).
test(mean_0_multidim, [fail]):- mean([[],[]], _).
test(mean_1_multidim, [fail]):- mean([[1],[]], _).
test(mean_1):- mean([1], M), M =:= 1.
test(mean_2):- mean([1,2,3], M), M =:= 2.
test(mean_3_multidim):- mean([[1,3,4],[7,67]], M), ground(M), M = [A,37], close_to(A,2.666666).
:- end_tests(mean).

:- begin_tests(median, []).
test(median_0, [fail]):- median([], _).
test(median_0_multidim, [fail]):- median([[],[]], R), ground(R), R = [0,0].
test(median_1_1):- median([1], R), R =:= 1.
test(median_1):- median([1,2,3], R), R =:= 2.
test(median_2):- median([1,2,3,4], R), R =:= 2.5.
test(median_2_unsorted):- median([1,2,4,3], R), R =:= 2.5.
test(median_3):- median([[1,5,64],[27,67]], R), ground(R), R = [5,47].
test(median_4):- median([1,1,2,6,6,9],R), R =:= 4.
:- end_tests(median).

:- begin_tests(mode, []).
test(mode_0, [fail]):- mode([], _).
test(mode_0_multidim, [fail]):- mode([[],[]], _).
test(mode_1):- mode([1,2,3,1],M), M =:= 1.
test(mode_1_det):- mode([1,5,1],M), M =:= 1.
test(mode_2):- mode([1,2,3,4], L), ground(L), L = [1,2,3,4].
test(mode_3):- mode([[1,5,64],[27,67]], L), ground(L), L = [[1,5,64],[27,67]].
test(mode_4):- mode([[1,1,64],[27,67]], L), ground(L), L = [1,[27,67]].
:- end_tests(mode).

:- begin_tests(percentile, []).
test(percentile_00, [fail]):- percentile([],40,_).
test(percentile_01, [fail]):- percentile([],_,_).
test(percentile_02, [fail]):- percentile([_],1,_).
test(percentile_03, [fail]):- percentile([1],101,_).
test(percentile_04, [fail]):- percentile([1],_,_).
test(percentile_05, [fail]):- percentile([a],_,_).
test(percentile_06):- percentile([1],100,P), close_to(P,1).
test(percentile_07):- percentile([1],10,P), close_to(P,1).
test(percentile_08):- percentile([1],0,P), close_to(P,1).
test(percentile_1):- percentile([1,2,3,4,6,5,9],40,P), close_to(P,3.4).
test(percentile_2):- percentile([1,2,3,4,6,5],40,P), close_to(P,3).
test(percentile_3):- percentile([1,2,3,4,6,5],[10,40],P), L = [1.5,3.0], maplist(close_to,L,P).
test(percentile_4):- percentile([[1,2,3,4,6,5],[15,25]],[10,40],[PA,PB]), L1 = [1.5,3.0], L2 = [16.0, 19.0], maplist(close_to,L1,PA), maplist(close_to,L2,PB).
:- end_tests(percentile).

:- begin_tests(quartile, []).
% light testing since it is a wrapper for percentile/3
test(quartile_00, [fail]):- quartile([1,2],40,_).
test(quartile_01, [fail]):- quartile([1,2],-1,_).
test(quartile_02, [fail]):- quartile([1,2],_,_).
:- end_tests(quartile).

:- begin_tests(iqr, []).
% light testing since it is a wrapper for percentile/3
test(iqr_00, [fail]):- iqr([],_).
:- end_tests(iqr).

:- begin_tests(rms, []).
test(rms_00, [fail]):- rms([],_).
test(rms_01, [fail]):- rms([a],_).
test(rms_1):- rms([1,5,8,3],V),close_to(V,4.97493).
test(rms_2):- rms([[1,5,64],[27,67]],M), L = [37.067505985701274, 51.07837115648854], maplist(close_to,M,L).
:- end_tests(rms).

:- begin_tests(sum_of_squares, []).
test(sum_of_squares_00, [fail]):- sum_of_squares([],_).
test(sum_of_squares_01, [fail]):- sum_of_squares([a],_).
test(sum_of_squares_1):- sum_of_squares([1,2,3], R), R =:= 2.
test(sum_of_squares_2):- sum_of_squares([1,2,3,46], R), R =:= 1454.
test(sum_of_squares_3):- sum_of_squares([[1,5,66],[27,67]], R), ground(R), R = [2654,800].
:- end_tests(sum_of_squares).

:- begin_tests(variance, []).
test(variance_00, [fail]):- variance([1,2,4,_,7,8,9],_).
test(variance_01, [fail]):- variance([1],_).
test(variance_02, [fail]):- variance([],_).
test(variance_1):- variance([1,2,4,6,7,8,9],V),close_to(V,9.2380952).
test(variance_2):- variance([1,4,6,72,1],V), V =:= 956.7.
:- end_tests(variance).

:- begin_tests(pop_variance, []).
test(pop_variance_00, [fail]):- pop_variance([1,2,4,_,7,8,9],_).
test(pop_variance_01, [fail]):- pop_variance([1],_).
test(pop_variance_02, [fail]):- pop_variance([],_).
test(pop_variance_1):- pop_variance([1,2,4,6,7,8,9],V),close_to(V,7.9183673).
test(pop_variance_2):- pop_variance([1,4,6,72,1],V),close_to(V,765.3600).
:- end_tests(pop_variance).

:- begin_tests(std_dev, []).
% light testing since it is a wrapper for variance/3
test(std_dev_1):- std_dev([1,2,4,6,7,8,9],S),close_to(S,3.039424).
:- end_tests(std_dev).

:- begin_tests(pop_std_dev, []).
% light testing since it is a wrapper for pop_variance/3
test(std_dev_1):- pop_std_dev([1,2,4,6,7,8,9],S),close_to(S,2.8139594).
:- end_tests(pop_std_dev).

:- begin_tests(range, []).
test(range_00, [fail]):- range([], _).
test(range_01, [fail]):- range([_],_).
test(range_1):- range([1,2,4,6,7,8,9], R), R =:= 8.
test(range_2):- range([1],R), R =:= 0.
:- end_tests(range).

:- begin_tests(midrange, []).
% light testing since it is a wrapper for range/2
test(midrange_1):- midrange([1,2,4,6,7,8,9], R), R =:= 4.
:- end_tests(midrange).

:- begin_tests(mean_absolute_deviation, []).
% light testing since it calls other predicates
test(mean_absolute_deviation_00, [fail]):- mean_absolute_deviation([1,_],_).
test(mean_absolute_deviation_01, [fail]):- mean_absolute_deviation([_],_).
test(mean_absolute_deviation_1):- mean_absolute_deviation([1,2,4,6,7,8,9],M),close_to(M,2.5306122).
:- end_tests(mean_absolute_deviation).

:- begin_tests(covariance, []).
test(covariance_00,[fail]):- covariance([5,12,23,45],[2,8,18,20,28], _).
test(covariance_01,[fail]):- covariance([5,12,23,45],[2,8,18,_], _).
test(covariance_02,[fail]):- covariance([],[2,8,18,_], _).
test(covariance_1):- covariance([5,12,18,23,45],[2,8,18,20,28], R), R =:= 146.1.
:- end_tests(covariance).

:- begin_tests(correlation, []).
% light testing since it is a wrapper for covariance/3
test(correlation_00,[fail]):- correlation([5,12,23,45],[2,8,18,20,28], _).
test(correlation_1):- correlation([5,12,18,23,45],[2,8,18,20,28],C),close_to(C,0.9366).
test(spearman_correlation_1):- spearman_correlation([5,12,18,23,45],[2,8,18,20,28],C), close_to(C,0.999999). 
test(spearman_correlation_2):- spearman_correlation([5,12,18,23,45],[29,8,18,20,40],C), close_to(C,0.399999). 
:- end_tests(correlation).

:- begin_tests(weighted_mean, []).
test(weighted_mean_00, [fail]):- weighted_mean([27],[2,8,10,13,18,20],_).
test(weighted_mean_01, [fail]):- weighted_mean([2,8,10,13,18,20],[27],_).
test(weighted_mean_02, [fail]):- weighted_mean([2,8,10,13,18,20],[_],_).
test(weighted_mean_03, [fail]):- weighted_mean([2,8,10,13,_,20],[20],_).
test(weighted_mean_04, [fail]):- weighted_mean([3,8,10,17,_,27],[2,8,10,13,18,20],_).
test(weighted_mean_05, [fail]):- weighted_mean([3,8,10,17,12,27],[-2,8,10,13,18,20],_).
test(weighted_mean_06, [fail]):- weighted_mean([3,8],[0,0],_).
test(weighted_mean_1):- weighted_mean([3,8,10,17,24,27],[2,8,10,13,18,20],V),close_to(V,19.1972).
:- end_tests(weighted_mean).

:- begin_tests(geometric_mean, []).
test(geometric_mean_00, [fail]):- geometric_mean([-4,4],_).
test(geometric_mean_01, [fail]):- geometric_mean([4,4,-1],_).
test(geometric_mean_01, [fail]):- geometric_mean([0,0,0],_).
test(geometric_mean_02, [fail]):- geometric_mean([],_).
test(geometric_mean_1):- geometric_mean([54, 24, 36], GM),close_to(GM,36).
test(geometric_mean_1):- geometric_mean([[54, 24, 36],[1,2]], GM), E = [36,1.4142135623730951], maplist(close_to,GM,E).
:- end_tests(geometric_mean).

:- begin_tests(harmonic_mean, []).
test(harmonic_mean_00, [fail]):- harmonic_mean([],_).
test(harmonic_mean_01, [fail]):- harmonic_mean([2,-1],_).
test(harmonic_mean_1):- harmonic_mean([1,2,3,4,5,6,7],V),close_to(V,2.69972).
test(harmonic_mean_2):- harmonic_mean([1,4],V),close_to(V,1.6).
test(harmonic_mean_3):- harmonic_mean([[1,4],[1,4]],V), E = [1.6,1.6], maplist(close_to,V,E).
:- end_tests(harmonic_mean).

:- begin_tests(trimmed_mean, []).
test(trimmed_mean_00,[fail]):- trimmed_mean([],_,_,_).
test(trimmed_mean_01,[fail]):- trimmed_mean([1,2,3,4,5,6,7],3,1,_).
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
test(rank_7):- rank([[0,2,3,2],[1,4,5]],max,[[1,4,5,4],[2,6,7]]).
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
test(min_2):- min_val([[1,2,4,6,7,8,9,1],[1,2,4,6,7,8,9,1]],[1,1]).
:- end_tests(min_val).

:- begin_tests(max_val, []).
test(max_1):- max_val([1,2,4,6,7,8,9,1],9).
test(max_2):- max_val([[1,2,4,6,7,8,9,1],[1,2,4,6,7,8,9,1]],[9,9]).
:- end_tests(max_val).

:- begin_tests(sum, []).
test(sum_1):- sum([1,24,2,3,-1],29).
test(sum_2):- sum([[1,24,2,3,-1],[1,24,2,3,-1]],[29,29]).
:- end_tests(sum).

:- begin_tests(prod, []).
test(prod_1):- prod([1,24,2,3,-1],-144).
test(prod_2):- prod([1,24,0,3,-1],0).
test(prod_3):- prod([[1,24,0,3,-1],[8,8]],[0,64]).
:- end_tests(prod).

:- begin_tests(rescale, []).
test(rescale_1):- rescale([0.07,0.14,0.07],[0.0,1.0,0.0]).
test(rescale_2):- rescale([0.07,0.14,0.07],2,3,[2.0,3.0,2.0]).
:- end_tests(rescale).

:- begin_tests(mean_normalize, []).
test(mean_normalize_1):- mean_normalize([1,2,4],L), LRes = [-0.444, -0.1111, 0.5555], maplist(close_to,L,LRes).
:- end_tests(mean_normalize).

:- begin_tests(standardize, []).
test(standardize_1):- standardize([[1,2,4],[1,2,4]],[L,L]), LRes = [-1.06904,-0.2672612,1.336306],  maplist(close_to,L,LRes).
:- end_tests(standardize).

:- begin_tests(entropy, []).
test(entropy_1):- entropy([9/10,1/10],E), close_to(E,0.325082).
test(entropy_2):- entropy([1/2,1/2],[9/10,1/10],E), close_to(E,0.5108256).
:- end_tests(entropy).

:- begin_tests(delete_nth1, []).
test(delete_nth1_1):- delete_nth1([1,2,7,4],3,[1,2,4]).
test(delete_nth1_2, fail):- delete_nth1([1,2,7,4],-3,_).
test(delete_nth1_3):- delete_nth1([1,2,4,7,8,9],4,[1,2,4,8,9]).
test(delete_nth1_4, fail):- delete_nth1([1,2,4,7,8,9],7,_).
test(delete_nth1_5):- delete_nth1([[1,2,7,4],[1,2,7]],3,[[1,2,4],[1,2]]).
test(delete_nth1_6, fail):- delete_nth1([[1,2,7,4],[2,7]],3,_).
:- end_tests(delete_nth1).

:- begin_tests(normalize_prob, []).
test(normalize_prob_1):- normalize_prob([0.07,0.14,0.07],L), LC = [0.25,0.5,0.25], maplist(close_to,L,LC).
test(normalize_prob_2, fail):- normalize_prob([-0,1.14,0.07],_).
test(normalize_prob_3, fail):- normalize_prob([0.3,-0.14,0.07],_).
:- end_tests(normalize_prob).

:- begin_tests(sample, []).
test(sample_1):- LIn = [1,2,3,4,5], sample(LIn,5,L), sort(L,LIn).
test(sample_2):- LIn = [1,2,3,4,5], sample(LIn,5,false,L), sort(L,LIn).
:- end_tests(sample).

:- begin_tests(empirical_distribution, []).
test(empirical_distribution_1):- empirical_distribution([0,1,2,2,4,6,6,7],0,E), close_to(E,0.125).
test(empirical_distribution_2):- empirical_distribution([0,1,2,2,4,6,6,7],2,E), close_to(E,0.5).
test(empirical_distribution_3):- empirical_distribution([0,1,2,2,4,6,6,7],7,1).
test(empirical_distribution_4):- empirical_distribution([[0,1,2,2,4,6,6,7],[1,2,4]],[6,7,8],[E1,E2]), L1 = [0.875,1,1], L2 = [1,1,1], maplist(close_to,L1,E1), maplist(close_to,L2,E2).
:- end_tests(empirical_distribution).

:- begin_tests(seq, []).
test(seq_1):- seq(1,10,1,[1,2,3,4,5,6,7,8,9,10]).
:- end_tests(seq).

:- begin_tests(choose, []).
test(choose_1):- choose(10,3,120).
:- end_tests(choose).

:- begin_tests(factorial, []).
test(factorial_1):- factorial(10,3628800).
:- end_tests(factorial).

:- begin_tests(search_position_sorted, []).
test(search_position_sorted_1):- search_position_sorted([1,2,3,4,5],3,2).
test(search_position_sorted_2):- search_position_sorted([1,2,3,4,5],3,left,2).
test(search_position_sorted_3):- search_position_sorted([1,2,3,4,5],3,right,3).
test(search_position_sorted_4):- search_position_sorted([[1,2,3,4,5],[1,2,3,4,5]],3,right,[3,3]).
test(search_position_sorted_5):- search_position_sorted([[1,2,3,4,5],[1,2,3,4,5]],[3,3],right,[[3,3],[3,3]]).
:- end_tests(search_position_sorted).