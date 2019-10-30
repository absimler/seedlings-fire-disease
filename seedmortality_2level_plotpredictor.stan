data {
  //Observation numbers
  int N; // number of individuals
  int P; //number of plots
  int J; //number of plot predictors
  int S; // number of species
  int Nsims; // number of simulation points for plotting
  
  //Cluster IDs
  int <lower=1, upper=P> PlotID[N]; // map individuals to plot
  int <lower=1, upper=S> SpeciesID[N]; // map individuals to species
  
  //outcome
  int Dead[N];
  
  //predictors
  matrix[P,J] u;
  vector[N] seedsize; // individual predictors
  
  //simulation data
  matrix[Nsims, J] plotsim;
  vector[Nsims] sizesim;
}
parameters{
  real mu_alpha; // global intercept
  vector[P] alpha_plot_tilde;
  vector[S] alpha_species_tilde;
  
  real<lower=0> sigma_species;
  real<lower=0> sigma_plot;
  
  real bsize; //individual parameters
  vector[J] beta; //plot parameters
}
transformed parameters{
  vector[P] alpha_plot =  u * beta + sigma_plot*alpha_plot_tilde;
  vector[S] alpha_species =  sigma_species * alpha_species_tilde;
  vector[N] p = mu_alpha + seedsize * bsize + alpha_plot[PlotID] + alpha_species[SpeciesID];
}
model{
  mu_alpha ~ normal(0, 5);
  alpha_plot_tilde ~ normal(0, 1); // varying intercepts for plot, species
  alpha_species_tilde ~ normal(0, 1); // transformed/non-centered parameterization
  
  sigma_species ~ cauchy(0, 10);
  sigma_plot ~ cauchy(0, 10);
  
  bsize ~ normal(0, 1);
  beta ~ normal(0, 1);
  
  Dead ~ bernoulli_logit(p);
}
generated quantities{
vector[N] log_lik; 
vector[Nsims] p_sims;
vector[Nsims] y_sims;
for ( n in 1:N ) {
log_lik[n] = bernoulli_logit_lpmf(Dead[n] | (mu_alpha + alpha_plot[PlotID[n]] + alpha_species[SpeciesID[n]] + bsize*seedsize[n]));
}
  for (q in 1:Nsims){
    p_sims[q] = inv_logit(mu_alpha + plotsim[q]*beta + bsize*sizesim[q]);
    y_sims[q] = bernoulli_rng(p_sims[q]);
  }
}
