data {
  int<lower = 0> T;          
  real<lower = 0> y[T];      
  real<lower=0> sigma_x;
}
parameters {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0> sigma;
  real<lower=0> x[T];
}
model {
  K ~ normal(1,0.5);
  x[1] ~ normal(y[1],sigma_x);
  for(i in 2:T){
    x[i] ~ normal(x[i-1] + r*x[i-1]*(1-(x[i-1]/K)), sigma_x);
  }
  y ~ normal(x,sigma);  
}
generated quantities{
  real ypred[T];
  ypred = normal_rng(x,sigma);
}



