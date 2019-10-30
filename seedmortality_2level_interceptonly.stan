data {
  //Observation numbers
  int N; // number of individuals
  int P; //number of plots
  int S; // number of species
  
  //Cluster IDs
  int <lower=1, upper=P> PlotID[N]; // map individuals to plot
  int <lower=1, upper=S> SpeciesID[N]; // map individuals to species
  
  //outcome
  int Dead[N];
}
parameters{
  real mu_alpha; // global intercept
  vector[P] alpha_plot_tilde; // varying intercepts for plot, forest, species
  vector[S] alpha_species_tilde;
  
  real<lower=0> sigma_species;
  real<lower=0> sigma_plot;
}
transformed parameters{
  vector[P] alpha_plot =  sigma_plot* alpha_plot_tilde;
  vector[S] alpha_species =  sigma_species * alpha_species_tilde;
  vector[N] p = mu_alpha + alpha_plot[PlotID] + alpha_species[SpeciesID];
}
model{
  mu_alpha ~ normal(0, 5);
  alpha_plot_tilde ~ normal(0, 1);
  alpha_species_tilde ~ normal(0, 1);
  
  sigma_species ~ normal(0, 1);
  sigma_plot ~ normal(0, 1);
  
  Dead ~ bernoulli_logit(p);
}
generated quantities{
vector[N] log_lik; 
for ( n in 1:N ) {
log_lik[n] = bernoulli_logit_lpmf(Dead[n] | (mu_alpha + alpha_plot[PlotID[n]] + alpha_species[SpeciesID[n]]));
}
}
