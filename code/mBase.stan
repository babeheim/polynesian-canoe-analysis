data{
    int I;
    int T;
    int Y[T,I];
}
parameters{
    real alpha[T];
}
model{
    real p[T,I];
    alpha[T] ~ normal(0 , .1);
    for( t in 1 : T ) {
        alpha[t] ~ normal(0, .1);
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            p[ t , i ] = alpha[t];
        }
    }
    for( t in 1 : T ) {
        for( i in 1 : I ) {
            Y[ t , i ] ~ binomial_logit( 1 , p[ t , i ] );
        }
    }
}
