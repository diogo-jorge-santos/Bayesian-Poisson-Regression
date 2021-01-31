//modelo de regress�o de poisson com priors n�o informativas
data {
  //declara��o das variaveis
  int<lower=0> N;
  int<lower=0> p;
  int <lower=0> y[N];
  real x[N,p];
  
  
  
}


parameters {
  //declara��o dos parametros
  real beta[p+1];
}


transformed parameters{
  //constru��o da fun��o suporte para a distribui��o
  real lp[N];
  real <lower=0> mu[N];
  
  for(i in 1:N){
    lp[i]=beta[1];
    for(j in 2:p+1){
      lp[i]=lp[i]+beta[j]*x[i,j-1];
    }
    
    
    mu[i]=exp(lp[i]);
  }
  
}


model {
   
  //declara��o da distribui��es das variaveis
  //ao n�o especificar distribui��es � priori , o stan assume "flat priors" que neste caso s�o equivalente � priori de jeffreys
  y ~ poisson(mu);
}


generated quantities {
    int<lower=0> y_pred[N];
    real log_lik[N];
    for (i in 1:N) {
      //valor gerados para a distribui��o � posteriori de cada amostra
        y_pred[i] = poisson_rng(mu[i]);
      //estiam��o da log-likelihood do modelo para cada observa��o
        log_lik[i] = poisson_lpmf(y[i] | mu[i]);
    }
}



