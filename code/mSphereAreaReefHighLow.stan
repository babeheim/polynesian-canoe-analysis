data{
    int I;
    int T;
    int Y[T,I];
    real<lower=0> sphere[T,I];
    real a[I];
    real rh[I];
    real rl[I];
}
parameters{
    real alpha[T];
    real lambda;
    real beta;
    real kappa1;
    real kappa2;
}
model{
    real p[T,I];
    lambda ~ normal(0, .01);
    beta ~ normal(0, 0.01);
    kappa1 ~ normal(0, .01);
    kappa2 ~ normal(0, .01);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 1 : 11 ){
            p[ t, i ] = alpha[t]
            + lambda * sphere[ t , i ]
            + beta * a[i]
            + kappa1 * rh[i]
            + kappa2 * rl[i];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t, i ] ~ binomial_logit(1, p[ t, i ]);
        }
    }
}