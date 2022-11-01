data {
    int<lower = 0> T;          
    real<lower = 0> y[T];      
}
parameters {
    real<lower=0> r;
    real<lower=0> K;
    real<lower=0> y0;
    real<lower=0> sigma;
}
model {
    K ~ normal(1,0.5);
    y[1] ~ normal(y0,sigma);
    for(i in 2:T){
      y[i] ~ normal(y[i-1] + r*y[i-1]*(1-(y[i-1]/K)), sigma);
    }
}
generated quantities{
  real ypred[T];
  ypred[1] = y0;
  for(i in 2:T){
    ypred[i] = ypred[i-1] + r*ypred[i-1]*(1-(ypred[i-1]/K));
  }
}




