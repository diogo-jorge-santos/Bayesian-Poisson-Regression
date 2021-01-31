//modelo de regressão de poisson com priors não informativas
data {
  //declaração das variaveis
  int<lower=0> N;
  int<lower=0> p;
  int <lower=0> y[N];
  real x[N,p];
  
  
  
}


parameters {
  //declaração dos parametros
  real beta[p+1];
}


transformed parameters{
  //construção da função suporte para a distribuição
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
   
  //declaração da distribuições das variaveis
  //ao não especificar distribuições à priori , o stan assume "flat priors" que neste caso são equivalente à priori de jeffreys
  y ~ poisson(mu);
}


generated quantities {
    int<lower=0> y_pred[N];
    real log_lik[N];
    for (i in 1:N) {
      //valor gerados para a distribuição à posteriori de cada amostra
        y_pred[i] = poisson_rng(mu[i]);
      //estiamção da log-likelihood do modelo para cada observação
        log_lik[i] = poisson_lpmf(y[i] | mu[i]);
    }
}



