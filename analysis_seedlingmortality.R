## Compound disturbance and facultative seedling regen:
### Do pre-fire disease dynamics alter seedling mortality rates?

library(rstan)
library(rstanarm)
library(shinystan)
library(loo)
library(corrplot)
library(bayesplot)
library(cowplot)

##I. Load data: ######
Plot = read.csv("datasets/plotdata_seedlings.csv", header=T, na.strings=c("", " "))

Seedlings = read.csv("datasets/seedlings_for_ch2.csv", header=T, na.strings=c("", " "))

Seedlings = subset(Seedlings, Species=="LIDE" | Species=="SESE" |  Species=="UMCA" | Species=="ARME" | Species=="QUERCUS")

# Format variables for analysis
m=match(Seedlings$Plot, Plot$BigSurPlot) 
Seedlings$ForestType = Plot$ForestAllianceType[m]
m=match(Seedlings$Plot, Plot$BigSurPlot) 
Seedlings$Burned = Plot$Burned_2008[m]
Seedlings <- subset(Seedlings, Burned==TRUE & Ht1314 <50)
variables=subset(Plot, BigSurPlot %in% unique(Seedlings$Plot)) 

variables <- variables[order(variables$BigSurPlot),]

variables[,2:26] <- scale(variables[,2:26])
variables[,31:50] <- scale(variables[,31:50])

Seedlings[,7] <- scale(Seedlings[,7])
variables <- transform(variables,plotindex=as.numeric(factor(variables$BigSurPlot)))
variables <- transform(variables,forestindex=as.numeric(factor(variables$ForestAllianceType)))
Seedlings <- transform(Seedlings,plotindex=as.numeric(factor(Seedlings$Plot)))
Seedlings <- transform(Seedlings,Speciesindex=as.numeric(factor(Seedlings$Species)))
Seedlings <- transform(Seedlings,forestindex=as.numeric(factor(Seedlings$ForestType)))
variables$disindex <- as.numeric(as.factor(variables$DiseaseStage))

variables[is.na(variables)] <- 0



### PART II. Mortality models: #####
### 2a) Null model (random intercepts for species and plot ONLY): #########

data <- list(Dead = Seedlings$dead, 
             PlotID = Seedlings$plotindex,
             N = nrow(Seedlings), 
             P = length(unique(variables$plotindex)), 
             SpeciesID=Seedlings$Speciesindex, 
             S=length(unique(Seedlings$Speciesindex)))

mortalitymodnull <- stan( file = "seedmortality_2level_interceptonly.stan", data=data,
                          chains=2 , iter=2000 , warmup=500, thin=1,
                          control=list(adapt_delta = 0.99, stepsize = 0.01))

loglik <- extract_log_lik(mortalitymodnull, parameter_name = "log_lik")
mort.loo.null <- loo(loglik, k_threshold=0.7)


#### 2b). Plot-level structured model: ######

## Full model:
variables$int <- (variables$PreFireSODmortality)*(variables$FireMortality)
vars <- as.matrix(variables[c("PreFireSODmortality", 
                              "hostCWD", 
                              "SamplingPrecip", 
                              "AverageSlope", 
                              "FireMortality", 
                              "int",
                              "Shrub.BA.LIVE.13", 
                              "forestindex")])

SODquant <- c(quantile(variables$PreFireSODmortality, probs=c(0.10)), 
              mean(variables$PreFireSODmortality), 
              quantile(variables$PreFireSODmortality, probs=c(0.90)))
SODsim <- rep(SODquant, each=1000)
precipquant <- quantile(variables$SamplingPrecip, probs=c(0.1, 0.5, 0.9))
precipsim <- rep(precipquant, each=1000)
precipquant <- quantile(variables$SamplingPrecip, probs=c(0.1, 0.5, 0.9))
precipsim <- rep(precipquant, each=1000)
slopequant <- quantile(variables$AverageSlope, probs=c(0.1, 0.5, 0.9))
slopesim <- rep(slopequant, each=1000)

OFF <- rep(0, 3000)
varsim <- data.frame(
  SODsim = SODsim,
  hostcwdsim = OFF,
  precipsim = OFF,
  slopesim = OFF,
  FireMortality = OFF,
  Interactionsim =rep(mean(variables$int),3000),
  Shrubsim= OFF,
  forestsim = rep(1.5, 3000))
varsim <- as.matrix(varsim)

sizesim = rep(seq(from = min(Seedlings$Ht1314), 
                  to = max(Seedlings$Ht1314), length.out = 1000),3) 

data <- list(Dead = Seedlings$dead, 
             PlotID = Seedlings$plotindex, 
             seedsize = as.vector(Seedlings$Ht1314), 
             u = vars, N = nrow(Seedlings), 
             P = length(unique(variables$plotindex)), 
             J= ncol(vars), 
             SpeciesID=Seedlings$Speciesindex, 
             S=length(unique(Seedlings$Speciesindex)),
             Nsims= nrow(varsim),
             sizesim = sizesim,
             plotsim = varsim)

mortalitymod.full = stan(file="seedmortality_2level_plotpredictor.stan" , data=data,
                         chains=2 , iter=2000 , warmup=500, thin=1,
                         control=list(adapt_delta = 0.99, stepsize = 0.01, 
                                      max_treedepth=15))

plot(mortalitymod.full, ci_level = 0.5, outer_level=0.9, 
     pars=c("mu_alpha", "beta","alpha_species"))

loglik <- extract_log_lik(mortalitymod.full, parameter_name = "log_lik")
mort.loo.full <- loo(loglik, k_threshold=0.7)

compare(mort.loo.full, mort.loo.null)


##Plot parameter estimates for full model:
parnames <- c("PreFire SOD mortality", 
              "host CWD", 
              "mean precip", 
              "Average slope", 
              "Fire Mortality", 
              "SOD*fire interaction",
              "Shrub BA", "Forest Type")
names(mortalitymod.full)[51:58] <- parnames
intnames <- c("ARME", "LIDE", "QUERCUS", "SESE", "UMCA")
names(mortalitymod.full)[100:104] <- intnames
plot(mortalitymod.full, ci_level = 0.5, outer_level=0.9, 
     pars=c( "bsize", "beta", "mu_alpha", "alpha_species"))
