topic(help/0,utils).
topic(help/1,utils).
topic(help/2,utils).
topic(bug/0,utils).
topic(list/0,utils).
topic(suggestion/0,utils).
topic(mean/2,statistics).
topic(median/2,statistics).
topic(mode/2,statistics).
topic(percentile/3,statistics).
topic(quartile/3,statistics).
topic(iqr/3,statistics).
topic(rms/2,statistics).
topic(sum_of_squares/2,statistics).
topic(variance/2,statistics).
topic(pop_variance/2,statistics).
topic(std_dev/2,statistics).
topic(pop_std_dev/2,statistics).
topic(range/2,statistics).
topic(midrange/2,statistics).
topic(mean_absolute_deviation/2,statistics).
topic(covariance/3,statistics).
topic(correlation/3,statistics).
topic(spearman_correlation/3,statistics).
topic(pearson_correlation/3,statistics).
topic(weighted_mean/3,statistics).
topic(harmonic_mean/2,statistics).
topic(trimmed_mean/4,statistics).
topic(trimmed_variance/4,statistics).
topic(skew/2,statistics).
topic(kurtosis/2,statistics).
topic(moment/3,statistics).
topic(sum/2,utils).
topic(prod/2,utils).
topic(rescale/2/4,utils).
topic(mean_normalize/2,utils).
topic(standardize/2,utils).
topic(entropy/2/3,statistics).

topic(min_val/2,utils).
topic(max_val/2,utils).
topic(min_max_val/3,utils).
topic(rank/3,statistics).
topic(nth_row/3,utils).
topic(nth_column/3,utils).
topic(swap_rows_columns/2,utils).
topic(split_n_parts/3,utils).
topic(occurrences/2/3,utils).
topic(normalize_prob/2,utils).
topic(delete_nth1/3,utils).
topic(sample/3/4/5,statistics).
topic(empirical_distribution/3,statistics).

topic(seq/4,utils).
topic(factorial/2,utils).
topic(choose/3,utils).
topic(search_position_sorted/3/4,utils).

/**
 * bug/0: print information to report bugs.
 * */
bug:- writeln("Report bugs on Github: https://github.com/damianoazzolini/plstat").

/**
 * suggestion/0: print information to report suggestions
 * */
suggestion:- writeln("Report suggestions on Github: https://github.com/damianoazzolini/plstat").

/**
 * list/0: lists all available predicates
 * */
list:- writeln("Still to implement"),true.

% help:- true.
% help(utils):-
%     findall(P,topic(P,utils),LU),
%     writeln(LU).
% help(Predicate)

