# List of available predicates
`bug/0`, `list/0`, `suggestion/0`, `mean/2`, `median/2`, `mode/2`, `percentile/3`, `quartile/3`, `iqr/2`, `rms/2`, `sum_of_squares/2`, `variance/2`, `pop_variance/2`, `std_dev/2`, `pop_std_dev/2`, `range/2`, `midrange/2`, `mean_absolute_deviation/2`, `covariance/3`, `correlation/3`, `pearson_correlation/3`, `spearman_correlation/3`, `weighted_mean/3`, `harmonic_mean/2`, `trimmed_mean/4`, `trimmed_variance/4`, `skew/2`, `kurtosis/2`, `moment/3`, `sum/2`, `prod/2`, `rescale/2`, `rescale/4`, `mean_normalize/2`, `standardize/2`, `entropy/2`, `entropy/3`, `min_val/2`, `max_val/2`, `min_max_val/3`, `rank/2`, `rank/3`, `nth_row/3`, `nth_column/3`, `swap_rows_columns/2`, `split_n_parts/3`, `occurrences/2`, `occurrences/3`, `normalize_prob/2`, `delete_nth1/3`, `sample/3`, `sample/4`, `sample/5`, `empirical_distribution/3`, `seq/4`, `factorial/2`, `choose/3`, `search_position_sorted/3`, `search_position_sorted/4`.

# Predicates details

### Mean
`mean(+List:number,-Mean:float)`

`Mean` is the mean of the list `List`.
`List` can also be multidimensional (list of lists).
Fails with a message if list is empty of if one of the lists
of the list of lists is empty.

```
mean([1,2,3],M).
% Expected: M = 2.

mean([[1,3,4],[7,67]],L).
% Expected: L = [2.666,37].
```

### Weighted mean
`weighted_mean(+List:numbers,+Weights:numbers,-WM:number)`

`WM` is the weighted mean of the list `List`.
Formula: \sum x_iw_i / \sum w_i.
Weights must be positive and shoud sum to a value > 0.

```
weighted_mean([3,8,10,17,24,27],[2,8,10,13,18,20],WM).
% Expected: WM = 19.1972.
```

### Harmonic mean
`harmonic_mean(+List:numbers,-HM:number)`

`HM` is the harmonic mean of list `List`.
Formula: n / (1/x1 + 1/x2 + ... + 1/xn).
`List` can also be multidimensional (list of lists).
```
harmonic_mean([1,2,3,4,5,6,7],HM).
Expected: HM = 2.69972
```

### Trimmed mean
`trimmed_mean(+List:numbers,+Lower:number,+Upper:number,-TM:number)`

`TM` is the trimmed mean of the list `List`, i.e., the mean computed by considering only numbers in the range [Lower,Upper].
```
trimmed_mean([1,2,3,4,5,6,7],3,5,T).
% Expected: T = 4
```

### Median
`median(+List:number,-Median:number)`

`Median` is the median of the list `List`.
`List` can also be multidimensional (list of lists).
Fails with a message if list is empty of if one of the lists
of the list of lists is empty.

```
median([1,2,3],M).
% Expected: M = 2.

median([[1,5,64],[27,67]],M).
% Expected: M = [5, 47].
```

### Mode
`mode(+List:number,-Mode:list)`

`Mode` is the mode of the list `List`.
`List` can also be multidimensional (list of lists).
Fails with a message if list is empty of if one of the lists
of the list of lists is empty.
If the list contains multiple modes with the same frequency,
it computes all.

```
mode([1,2,3,1],M).
% Expected: M = 1.

mode([[1,5,64],[27,67]],M).
% Expected: M = [[1,5,64], [27,67]].
```

### Percentile  
`percentile(+List:number,+K:number,-Percentile:number)`

`Percentile` is the `K`-th percentile of the list `List`.
Both `List` and `K` can be multidimensional (lists of lists).
Fails with a message if list is empty of if one of the lists
of the list of lists is empty.

Algorithm: arrange the data in ascending order, compute `r = (p/100)*  (n-1) + 1` where `p` is the percentile.
If `r` is integer, then the r-th element is the desired percentile.
Otherwise, it is `x_ceil(r) + (r - ceil(r))  (x_(ceil(r)+ 1) - x_ceil(r))`.
The last formula is valid in both cases.
```
percentile([1,2,3,4,6,5,9],40,P).
% Expected: P = 3.4.

percentile([1,2,3,4,6,5],40,P).
% Expected: P = 3.0.

percentile([1,2,3,4,6,5],[10,40],P)
% Expected: P = [1.5,3.0].

percentile([[1,2,3,4,6,5],[15,25]],[10,40],P).
% Expected: P = [[1.5,3.0],[16.0,19.0]].
```

### Quartile
`quartile(+List:number,+Q:number,-Quartile:number)`
  
`Quartile` is the `Q`-th quartile of the list `List`.
Both `List` and `Q` can be multidimensional (lists of lists).
Wraps for `percentile/3`.
```
quartile([15,25],2,Q).
% Expected: Q = 20.
```

### Inter quartile range  
`iqr(+List:number,-IQR:number)`

`IQR` is the inter quartile range of list `List`, computed as the difference between 3rd - 1st quartile.
`List` can also be multidimensional (list of lists).
```
iqr([1,2,3,4,6,5],I).
% Expected: I = 2.5.
```

### RMS
`rms(+List:number,-RMS:number)`

`RMS` is the root mean square of the list `List`.
`List` can also be multidimensional (list of lists).
Square root of the sum of the squared data values divided by the number of values.
```
rms([1,5,8,3],S).
% Expected: S = 4.97493.
```

### Sum of squares
`sum_of_squares(+List:number,-SumOfSquares:number)`

`SumOfSquares` is the sum of squares of the list `List`.
`List` can also be multidimensional (list of lists).
Formula: \sum_n (x - \mu)^2.
```
sum_of_squares([1,2,3],S)
% Expected: S = 2.
```

### Variance
`variance(+List:number,-Variance:number)`

`Variance` is the sample variance of the list `List`.
`List` can also be multidimensional (list of lists).
Formula: (1/(N - 1))  \sum_n (x_i - \mu)^2.
```
variance([1,2,4,6,7,8,9],V).
% Expected: V = 9.2380952.
```

### Population variance
`pop_variance(+List:number,-Variance:number)`

`Variance` is the population variance of the list `List`.
`List` can also be multidimensional (list of lists).
Formula: (1/N)  \sum_n (x_i - \mu)^2.
```
pop_variance([1,4,6,72,1],V).
% Expected: V = 765.3600.
```

### Standard devation
`std_dev(+List:numbers,-StdDev:number)`

`StdDev` is the standard deviation of the list `List` (square root of the sample variance).
`List` can also be multidimensional (list of lists).
```
std_dev([1,2,4,6,7,8,9],S).
Expected: S = 3.039424.
```

### Population standard deviation
`pop_std_dev(+List:numbers,-StdDev:number)`

`StdDev` is the population standard deviation of the list `List` (square root of the population variance).
`List` can also be multidimensional (list of lists).
```
pop_std_dev([1,2,4,6,7,8,9],S).
% Expected: S = 3.039424.
```

### Range
`range(+List:numbers,-Range:number)`

`Range` is the difference between the biggest and the smallest element of the list `List`.
`List` can also be multidimensional (list of lists).
```
range([1,2,4,6,7,8,9],R).
% Expected: R = 8.
```

### Midrange
`midrange(+List:numbers,-Midrange:number)`

`Midrange` is (max - min) / 2 of the list `List` (half of the range).
`List` can also be multidimensional (list of lists).
```
midrange([1,2,4,6,7,8,9],M).
% Expected: M = 4.
```

### Mean absolute deviation
`mean_absolute_deviation(+List:numbers,-MAS:number)`

`MAD` is the sum of the absolute value of the differences between data values and the mean, divided by the sample size.
Formula: MAD = 1/N  \sum_i |x - \mu|.
`List` can also be multidimensional (list of lists).
```
mean_absolute_deviation([1,2,4,6,7,8,9],M).
% Expected: M = 2.5306122.
```

### Covariance
`covariance(+List1:numbers,+List2:numbers,-Covariance:number)`

`Covariance` is the covariance of the lists `List1` and `List2`.
```
covariance([5,12,18,23,45],[2,8,18,20,28],C).
% Expected: C = 146.1
```

### Correlation
`correlation(+List1:numbers,-List2:numbers,-Correlation:number)`
`pearson_correlation(+List1:numbers,-List2:numbers,-Correlation:number)`
`spearman_correlation(+List1:numbers,-List2:numbers,-Correlation:number)`

`Correlation` is the Pearson correlation of `List1` and `List2`.
Formula: covariance(List1,List2) / (std_dev(List1)  std_dev(List2)).
Other correlations are self explanatory.
```
correlation([5,12,18,23,45],[2,8,18,20,28],C).
% Expected: C = 0.9366.

spearman_correlation([5,12,18,23,45],[2,8,18,20,28],C).
% Expected: C = 0.999999
```

### Trimmed variance
`trimmed_variance(+List:numbers,+Lower:number,+Upper:number,-TV:number)`

`TV` is the trimmed variance of the list `List`, i.e, the variance computed by considering only numbers in the range [Lower,Upper].
```
trimmed_variance([1,2,3,4,5,6,7],3,5,V).
% Expected: V = 1.0
```

### Moment
`moment(+List:numbers,+M:integer,-Moment:number)`

`Moment` is the M-th moment about the mean for the list `List`.
Formula: 1/n \sum (x_i - x_mean) ^ M.
```
moment([1,2,3,4,5],2,MO).
% Expected: MO = 2
```

### Skew
`skew(+List:numbers,-Skew:number)`

`Skew` is the sample skewness of list `List`. Formula: m_3 / (m_2)^(3/2).
`List` can also be multidimensional (list of lists).
```
skew([2,8,0,4,1,9,9,0],S).
% Expected: S = 0.26505541
```

### Kurtosis
`kurtosis(+List:numbers,-Kurtosis:number)`

`Kurtosis` is the fourth central moment divided by the square of the variance.
`List` can also be multidimensional (list of lists).
```
kurtosis([3,5,7,2,7],K).
% Expected: K = 1.3731508875.
```

### Rank
`rank(+List:numbers,-RankList:number)`

`rank(+List:numbers,+Method:atom,-RankList:number)`

`RankList` is the rank of the list `List` according to method `Method`.
If `Method` is not provided, by default it performs average/fractional ranking.
`Method` must be one of the following:
- `average` or `fractional`: items that compare equal receive the same rank, which is the mean of ordinal ranking values (see below)
- `min` or `competition`: items that compare equal receive the same rank (there will be a gap in the ranking list)
- `max` or `modified_competition`: as `min`, but the gap is left before, rather than after
- `dense`: as `min`, but no gaps are left
- `ordinal`: all the elements receive a different rank. If the same element appears more than one time, all the occurrences will have a different (increasing) rank
```
rank([0,2,3,2],R).
% Expected: R = [1.0,2.5,4.0,2.5].

rank([0,2,3,2],average,R).
% Expected: R = [1.0,2.5,4.0,2.5].

example: rank([0,2,3,2],min,R).
% Expected: R = [1,2,4,2].

example: rank([0,2,3,2],max,R).
% Expected: R = [1,3,4,3].

example: rank([0,2,3,2],dense,R).
% Expected: R = [1,2,3,2].

example: rank([0,2,3,2],ordinal,R).
% Expected: R = [1,2,4,3].

rank([[0,2,3,2],[1,4,5]],max,R).
% Expected: R = [[1,4,5,4],[2,6,7]].
```

### Nth-row
`nth_row(+ListOfList:numbers,+Nth:integer,-NthRow:List)`

`NthRow` is the nth row of `ListOfList`, counting from 1.
```
nth_row([[1,2],[3,4]],2,N).
% Expected: N = [3,4].
```

### Nth-column
`nth_column(+ListOfList:numbers,+Nth:integer,-NthColumn:List)`

`NthColumn` is the nth column of `ListOfList`, counting from 1.
```
nth_column([[1,2],[3,4]],2,N).
% Expected: N = [2,4].
```

### Swap rows columns
`swap_rows_columns(+LRows:numbers,+ListOfLists:LColumns)`

`LColumns` is `LRows` transposed (rows and columns swapped).
```
swap_rows_columns([[1,2,4],[3,6,7]],R).
Expected: R = [[1,3],[2,6],[4,7]].
```

### Split n parts
`split_n_parts(+List:numbers,+Parts:number,-PartsList:numbers)`

`PartsList` is a list of lists obtained by splitting list `List` in `Parts` parts.
```
split_n_parts([1,2,4,3,7,6],2,[[1,2],[4,3],[7,6]]).
% Expected: S = [[1,2],[4,3],[7,6]].
```

### Occurrences
`occurrences(+List:number,-Occ:list)`

`occurrences(+Number:number,+List:numbers,-Occ:list)`

`Occ` is the occurrences of `Number` in list `List`.
If `Number` is not provided, `Occ` is a list [Value,Occurrences] for each element in list `List`.
```
occurrences([1,2,4,6,7,8,9,1],O).
% Expected: O = [[1,2],[2,1],[4,1],[6,1],[7,1],[8,1],[9,1]].
  
occurrences([1,2,4,6,7,8,9,1],1,O).
% Expected: O = 2.
```

### Min value
`min_val(+List:numbers,-Min:number)`
  
`Min` is the smallest value of the list `List`.
`List` can also be multidimensional (list of lists).
```
min_val([1,2,4,6,7,8,9,1],M).
% Expected: M = 1.
```

### Max value
`max_val(+List:numbers,-Max:number)`

`Max` is the biggest value of the list `List`.
`List` can also be multidimensional (list of lists).
```
max_val([1,2,4,6,7,8,9,1],M).
%  Expected: M = 9.
```

### Min max value
`min_max_val(+List:numbers,-Min:number,-Max:number)`

`Min` and `Max` are the minimum and the maximum value of the list `List` respectively. 
```
min_max_val([1,2,4,6,7,8,9,1],Min,Max).
% Expected: Min = 1, Max = 9.
```

### Sum
`sum(+List:numbers,-Sum:number)`

`Sum` is the sum of the elements in the list `List`.
`List` can also be multidimensional (list of lists).
```  
sum([1,24,2,3,-1],S).
% Expected: S = 29. 
```

### Prod
`prod(+List:numbers,-Prod:number)`

`Prod` is the product of the elements in list `List`.
`List` can also be multidimensional (list of lists).
```
prod([1,24,2,3,-1],P).
% Expected: P = -144.
```

### Normalize probability
`normalize_prob(+List:numbers,-NormalizedList:number)`

`NormalizedList` is the list `List` normalized.
`List` must contain only elements between 0 and 1 (included).
`List` can also be multidimensional (list of lists).
Formula: i / list_sum foreach i in `List`.
```
normalize_prob([0.07,0.14,0.07],L).
% Expected: L = [0.25,0.5,0.25].
```

### Rescale
`rescale(+List:numbers,-Rescaled:numbers)`

`rescale(+List:numbers,+Lower:number,+Upper:number,-Rescaled:number)`

`Rescaled` is list List rescaled in the range [Lower,Upper].
Also known as min-max normalization.
`List` can also be multidimensional (list of lists).
If `Lower` and `Upper` are not provided, they are set by default to 0 and 1.
Every x is rescaled as: Lower + ((x - min_list)(Upper - Lower)) / (max_list - min_list)
```
rescale([0.07,0.14,0.07],L).
% Expected: L = [0.0,1.0,0.0]

rescale([0.07,0.14,0.07],2,3,L).
% Expected: L = [2.0,3.0,2.0]
```

### Mean normalize
`mean_normalize(+List:numbers,-Normalized:numbers)`

`Normalized` is list `List` mean normalized.
`List` can also be multidimensional (list of lists).
Formula: (x - mean_list) / (Upper - Lower) foreach x in `List`.
```
mean_normalize([1,2,4],L).
% Expected: L = [-0.444, -0.111, 0.555]. 
```

### Standardize
`standardize(+List:numbers,-Standardized:numbers)`

`Standardized` is list `List` standardized.
Formula: (x - mean_list) / std_dev_list foreach x in `List`.
`List` can also be multidimensional (list of lists).
Population std_dev is considered (divided by n).
```
standardize([1,2,4],L).
% Expected: L = [-1.0690449,-0.2672612,1.336306]. 
```

### Entropy
`entropy(+List:numbers,-Entropy:number)`

`entropy(+List:numbers,+Probabilities:number,-Entropy:number)`

`Entropy` is the entropy of the list `List`.
Formula: if probabilities are not provided, then 
E = -sum(pk  log(pk))
else
E = sum(pk  log(pk / qk))
Logarithm in base e (natural logarithm) is computed.
```
entropy([9/10,1/10],E).
% Expected: E = 0.325082.

entropy([1/2,1/2],[9/10,1/10],E).
% Expected: E = 0.5108256.
```

### Delete nth
`delete_nth1(+List:numbers,+Index:numbers,-LDeleted:number)`

`LDeleted` is List with the element at pos `Index` removed, counting from 1.
If Index is greater than the length of the list, fails.
```
delete_nth1([1,2,7,4],3,L).
% Expected: L = [1,2,4].
```

### Sample
`sample(+List:elements,+Size:number,-Result:list)`

`sample(+List:elements,+Size:number,+Replace:bool,-Result:list)`

`sample(+List:elements,+Size:number,+Replace:bool,+Probabilities:list,-Result:list)`
Takes a sample of size `Size` from list `List`.
Replace can be true or false, if not provided is false.
Probabilities is a list of probabilities.
If `Replace` is false and a list of probabilities is specified, the list is normalized, after the removal of the element. 
```
sample([1,2,3,4,5],5,L).
sample([1,2,3,4,5],5,true,L).
sample([1,2,3,4,5],5,false,L).
sample([1,2,3,4,5],5,true,[0.1,0.1,0.1,0.1,0.6],L).
sample([1,2,3,4,5],5,false,[0.1,0.1,0.1,0.1,0.6],L).
```

### Empirical distribution
`empirical_distribution(+List:numbers,+X:number,-Result:list)`

`Result` is the empirical distribution of list `List` at point `X`.
`List` and `X` can also be multidimensional (lists of lists).
```
empirical_distribution([0,1,2,2,4,6,6,7],0,E).
% Expected: E = 0.125.

empirical_distribution([0,1,2,2,4,6,6,7],2,E).
% Expected: E = 0.5.

empirical_distribution([0,1,2,2,4,6,6,7],7,E).
% Expected: E = 1.

empirical_distribution([[0,1,2,2,4,6,6,7],[1,2,4]],[6,7,8],E).
% Expected: E = [[0.875,1,1],[1,1,1]].
```

### Seq
`seq(A:number,B:number,Seq:List).`

`seq(A:number,B:number,Step:number,Seq:List).`
 
`List` is a list with a sequence ranging from `A` to `B` with step `Step`.
If `Step` is not provided, 1 is assumed.
```
seq(1,10,1,S).
% Expected: S = [1,2,3,4,5,6,7,8,9,10].
```

### Factorial
`factorial(+N:int,-Factorial:int)`

`Factorial` is N! = N*(N-1)*...2*1
```
factorial(10,F).
% Expected: F = 3628800.
```

### Choose
`choose(+N:int,+K:int,-C:int)`
  
`C` is the binomial coefficient `N,K`
Formula: fact(N) / (fact(N-K)  fact(K))
```
choose(10,3,C).
% Expected: C = 120.
```

### Search position sorted
`search_position_sorted(+List:numbers,+Element:number,-Pos:integer)`

`search_position_sorted(+List:numbers,+Element:number,+Direction:term,-Pos:integer)`

`Pos` is the position that the element `Element` would have when inserted in list `List` to preserve its order.
0 means the first location.
If the element should be inserted in the last position, `Pos = N` where `N` is the length of `List`.
Counting from 1.
`List` and Element can also be multidimensional (lists of lists).
`Direction` can be left (default) or right.
``` 
search_position_sorted([1,2,3,4,5],3,P).
% Expected result: P = 2.

search_position_sorted([1,2,3,4,5],3,right,P).
% Expected result: P = 3.
```