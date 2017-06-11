data{
    int I;
    int T;
    int Y[T,I];
    real<lower=0> inherit[T,I];
}
parameters{
    real alpha[T];
    real psi;
    real<lower=0,upper=1> psi_tau;
}
model{
    real p[T,I];
    psi ~ normal(0, .01);
    psi_tau ~ beta(1.5, 1.5);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ){
        for( i in 9 : 11 ){
            p[ t, i ] = alpha[t] + psi * psi_tau;
        } 
        for( i in 1 : 8 ){
            p[ t, i ] = alpha[t] + psi * inherit[ t, i ];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t, i ] ~ binomial_logit(1, p[ t, i ]);
        }
    }
}