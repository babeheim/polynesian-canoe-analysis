data{
    int I;
    int T;
    int Y[T,I];
    real<lower=0> sphere[T,I];
    real<lower=0> inherit[T,I];
    real a[I];
    real rh[I];
    real rl[I];
}
parameters{
    real alpha[T];
    real psi;
    real<lower=0,upper=1> psi_tau;
    real lambda;
    real beta;
    real kappa1;
    real kappa2;
}
model{
    real p[T,I];
    psi ~ normal(0, .01);
    lambda ~ normal(0, .01);
    beta ~ normal(0, 0.01);
    kappa1 ~ normal(0, .01);
    kappa2 ~ normal(0, .01);
    psi_tau ~ beta(1.5, 1.5);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 9 : 11 ){
            p[ t, i ] = alpha[t] 
            + psi * psi_tau
            + lambda * sphere[ t , i ]
            + beta * a[i]
            + kappa1 * rh[i]
            + kappa2 * rl[i];
        } 
        for( i in 1 : 8 ){
            p[ t, i ] = alpha[t]
            + psi * inherit[ t, i ]
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