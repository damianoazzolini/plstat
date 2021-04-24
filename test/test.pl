:- module(test_plstat, [test_plstat/0,test_plstat_rev/0]).
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
    occurrences_2,
    occurrences_3,
    min_val,
    max_val,
    sum,
    prod,
    seq,
    choose,
    factorial
  ]).

test_plstat:-
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
:- end_tests(mean).

:- begin_tests(median, []).
test(median_1):- median([1,2,3],2).
test(median_2):- median([1,2,3,4],2.5).
:- end_tests(median).

:- begin_tests(mode, []).
test(mode_1):- mode([1,2,3,1],[1]).
test(mode_2):- mode([1,2,3,4],[1,2,3,4]).
:- end_tests(mode).

:- begin_tests(rms, []).
test(rms_1):- rms([1,5,8,3],V),close_to(V,4.97493).
:- end_tests(rms).

:- begin_tests(sum_of_squares, []).
test(sum_of_squares_1):- sum_of_squares([1,2,3],2).
test(sum_of_squares_2):- sum_of_squares([1,2,3,46],1454).
:- end_tests(sum_of_squares).

:- begin_tests(variance, []).
test(variance_1):- variance([1,2,4,6,7,8,9],V),close_to(V,9.2380952).
:- end_tests(variance).

:- begin_tests(pop_variance, []).
test(variance_1):- pop_variance([1,2,4,6,7,8,9],V),close_to(V,7.9183673).
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

% other utils

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