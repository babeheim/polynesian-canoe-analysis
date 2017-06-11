data{
    int I;
    int T;
    int Y[T,I];
    real nrh[I];
}
parameters{
    real alpha[T];
    real kappa3;
}
model{
    real p[T,I];
    kappa3 ~ normal(0, .01);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 1 : 11 ){
            p[ t, i ] = alpha[t] + kappa3 * nrh[ i ];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t, i ] ~ binomial_logit(1, p[ t, i ]);
        }
    }
}