data{
    int I;
    int T;
    int Y[T,I];
    real<lower=0> inherit[T,I];
    real a[I];
    real nrh[I];
    real nrl[I];
}
parameters{
    real alpha[T];
    real beta;
    real kappa3;
    real kappa4;
}
model{
    real p[T,I];
    psi ~ normal(0, .01);
    beta ~ normal(0, 0.01);
    kappa3 ~ normal(0, .01);
    kappa4 ~ normal(0, .01);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
        psi_tau[t] ~ beta(1.5, 1.5);
    }
    for( t in 1 : T ){
        for( i in 9 : 11 ){
            p[ t, i ] = alpha[t] 
            + psi * psi_tau[t]
            + beta * a[i]
            + kappa3 * nrh[i]
            + kappa4 * nrl[i];
        } 
        for( i in 1 : 8 ){
            p[ t, i ] = alpha[t]
            + psi * inherit[ t, i ]
            + beta * a[i]
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