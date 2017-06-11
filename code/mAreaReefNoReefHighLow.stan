data{
    int I;
    int T;
    int Y[T,I];
    real a[I];
    real rh[I];
    real rl[I];
    real nrh[I];
    real nrl[I];
}
parameters{
    real alpha[T];
    real beta;
    real kappa1;
    real kappa2;
    real kappa3;
    real kappa4;
}
model{
    real p[T,I];
    beta ~ normal(0, 0.01);
    kappa1 ~ normal(0, .01);
    kappa2 ~ normal(0, .01);
    kappa3 ~ normal(0, .01);
    kappa4 ~ normal(0, .01);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 1 : 11 ){
            p[ t, i ] = alpha[t] 
            + beta * a[i]
            + kappa1 * rh[i] 
            + kappa2 * rl[i]
            + kappa3 * nrh[i]
            + kappa4 * nrl[i];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t, i ] ~ binomial_logit(1, p[ t, i ]);
        }
    }
}