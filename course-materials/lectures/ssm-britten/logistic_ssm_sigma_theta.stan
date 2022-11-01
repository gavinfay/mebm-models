data {
  int<lower = 0> T;          
  real<lower = 0> y[T];      
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real<lower=0> sigma_r;
  real<lower=0> alpha;
}
parameters {
  real<lower=0> r[T];
  real<lower=0> K;
  real<lower=0> x[T];
}
model {
  K ~ normal(1,0.5);
  r[1] ~ normal(0.5,0.5);
  for(i in 2:T){
    r[i] ~ normal(alpha*r[i-1],sigma_r);
  }
  
  x[1] ~ normal(y[1],sigma_x);
  for(i in 2:T){
    x[i] ~ normal(x[i-1] + r[i]*x[i-1]*(1-(x[i-1]/K)), sigma_x);
  }
  y ~ normal(x,sigma_y);  
}
generated quantities{
  real ypred[T];
  ypred = normal_rng(x,sigma_y);
}



