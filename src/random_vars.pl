supported_distributions([uniform,normal,gaussian]).

sample_var(uniform,Lower,Upper,Sampled):- 
    sample_var(uniform,Lower,Upper,1,Sampled).
sample_var(uniform,Lower,Upper,Samples,Sampled):-
    true.

sample_var(normal,Sampled):-
    sample_var(gaussian,0,1,1,Sampled).
sample_var(normal,Samples,Sampled):-
    sample_var(gaussian,0,1,Samples,Sampled).
sample_var(gaussian,M,V,Sampled):-
    sample_var(gaussian,M,V,1,Sampled).
sample_var(gaussian,M,V,Samples,Sampled):-
    true.
