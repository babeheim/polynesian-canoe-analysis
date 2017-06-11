data{
    int I;
    int T;
    int Y[T,I];
    real rh[I];
}
parameters{
    real alpha[T];
    real kappa1;
}
model{
    real p[T,I];
    kappa1 ~ normal(0, .01);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 1 : 11 ){
            p[ t, i ] = alpha[t] + kappa1 * rh[ i ];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t, i ] ~ binomial_logit(1, p[ t, i ]);
        }
    }
}