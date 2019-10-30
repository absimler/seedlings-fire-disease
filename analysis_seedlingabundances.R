## Compound disturbance and facultative seedling regen:
#### Do pre-fire disease dynamics alter post-fire seedling abundances at the plot level? ######

library(rstan)
library(rstanarm)
library(shinystan)
library(loo)
library(bayesplot)
library(dplyr)
library(tidybayes)


## I. Load data ######
variables = read.csv("datasets/plotdata_seedlings.csv", header=T, na.strings=c("", " "))
#dataframe of plot-level candidate predictors and plot-level seedling abundance data by species

variables$Elevation <- as.numeric(variables$Elevation)
variablesRW <- subset(variables, ForestAllianceType=="Redwood")
#Subset to just redwood forest plots for redwood seedling analysis


# Scale and center numerical variables
variables[,2:26] <- scale(variables[,2:26])
variables[,31:50] <- scale(variables[,31:50])
variablesRW[,2:26] <- scale(variablesRW[,2:26])
variablesRW[,31:50] <- scale(variablesRW[,31:50])
variablesRW$PostFireQUAGresprouting <- NULL
variablesRW$PostFireQUCHresprouting <- NULL

##Variables are named as they appear in MS. See Table 1 for explanations of how they were collected.


#### II. Negative binomial models for post-fire seedling abundances by species: #########

### a) Tanoak (LIDE) seedling abundances ######
lide.null.2013 <- stan_glm(lideseed2013 ~ 1 + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

lide.1 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + PostFireLIDEresprouting + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr +  AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13 + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.2 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + PostFireLIDEresprouting + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFtmax5yr +  AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13 + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.3 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + PostFireLIDEresprouting + FireMortality + PreFireSODmortality +  PreFireSODmortality*FireMortality + PFPrecip5yr +  AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13 + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.4 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr +  AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13 + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

lide.5 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.6 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + Shrub.BA.LIVE.13 + AverageSlope + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.7 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + AverageSlope + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.8 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500,adapt_delta=0.99)

lide.9 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + DiseaseStage + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

lide.10 <- stan_glm(lideseed2013 ~ LIDEsurvivingBA + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta=0.99)

# LOOIC scores
loo.lidenull <-loo(lide.null.2013)
loo.lide1 <- loo(lide.1, k_threshold=0.7)
loo.lide2 <- loo(lide.2, k_threshold=0.7)
loo.lide3 <- loo(lide.3, k_threshold=0.7)
loo.lide4 <- loo(lide.4, k_threshold=0.7)
loo.lide5 <- loo(lide.5, k_threshold=0.7)
loo.lide6 <- loo(lide.6, k_threshold=0.7)
loo.lide7 <- loo(lide.7, k_threshold=0.7)
loo.lide8 <- loo(lide.8, k_threshold=0.7)
loo.lide9 <- loo(lide.9, k_threshold=0.7)
loo.lide10 <- loo(lide.10, k_threshold=0.7)

#Model comparison
compare(loo.lidenull, loo.lide1, loo.lide2, loo.lide3, loo.lide4, loo.lide5, loo.lide6, loo.lide7, loo.lide8, loo.lide9, loo.lide10)
elpd.lideabund <- compare(loo.lide6, loo.lidenull)
plot(lide.6,  prob = 0.5, prob_outer=0.90)

####

### b) Coast redwood (SESE) seedling abundances ######
sese.null.2013 <- stan_glm(seseseed2013 ~ 1, data = variablesRW,  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.1 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + PostFireSESEresprouting + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13, data = (variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.2 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + PostFireSESEresprouting + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.3 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + PostFireSESEresprouting + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality + PFtmax5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.4 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + FireMortality + PreFireSODmortality + PreFireSODmortality*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.5 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + FireMortality + PreFireSODmortality + PreFireSODmortality*FireMortality + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.6 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + FireMortality + PreFireSODmortality  + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.7 <- stan_glm(seseseed2013 ~SESEsurvivingBA +  PreFireSODmortality + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.8 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + PreFireSODmortality + Shrub.BA.LIVE.13 + AverageSlope, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.9 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + PreFireSODmortality + AverageSlope, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.10 <- stan_glm(seseseed2013 ~ SESEsurvivingBA + AverageSlope, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

sese.11 <- stan_glm(seseseed2013 ~ SESEsurvivingBA, data = subset(variablesRW),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

loo.sesenull <-loo(sese.null.2013,  k_threshold=0.7)
loo.sese1 <- loo(sese.1, k_threshold=0.7)
loo.sese2 <- loo(sese.2, k_threshold=0.7)
loo.sese3 <- loo(sese.3, k_threshold=0.7)
loo.sese4 <- loo(sese.4, k_threshold=0.7)
loo.sese5 <- loo(sese.5, k_threshold=0.7)
loo.sese6 <- loo(sese.6, k_threshold=0.7)
loo.sese7 <- loo(sese.7, k_threshold=0.7)
loo.sese8 <- loo(sese.8, k_threshold=0.7)
loo.sese9 <- loo(sese.9, k_threshold=0.7)
loo.sese10 <- loo(sese.10, k_threshold=0.7)
loo.sese11 <- loo(sese.11, k_threshold=0.7)

compare(loo.sesenull, loo.sese1, loo.sese2, loo.sese3, loo.sese4, loo.sese5, loo.sese6, loo.sese7, loo.sese8, loo.sese9, loo.sese10, loo.sese11)
compare(loo.sese6, loo.sesenull)

plot(sese.6,  prob = 0.5, prob_outer=0.90) + ggplot2::ggtitle("Posterior medians \n with 90% intervals \n for SESE seeding abundance")
elpd.seseabund <- compare(loo.sese6, loo.sesenull)


###### c) Bay laurel (UMCA) seedling abundances: ########

umca.null <- stan_glm(umcaseed2013 ~ 1, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.1 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.2 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality + PFPrecip5yr +  AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.2 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + PreFireSODmortality + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.3 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + PreFireSODmortality + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.4 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + PreFireSODmortality + PFPrecip5yr + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.5 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + FireMortality + PFPrecip5yr + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.6 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PostFireUMCAresprouting + PFPrecip5yr + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.7 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + PFPrecip5yr + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.8 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA + Shrub.BA.LIVE.13 , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

umca.9 <- stan_glm(umcaseed2013 ~ UMCAsurvivingBA  , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

loo.umcanull <-loo(umca.null, k_threshold=0.7)
loo.umca1 <- loo(umca.1, k_threshold=0.7)
loo.umca2 <- loo(umca.2, k_threshold=0.7)
loo.umca3 <- loo(umca.3, k_threshold=0.7)
loo.umca4 <- loo(umca.4, k_threshold=0.7)
loo.umca5 <- loo(umca.5, k_threshold=0.7)
loo.umca6 <- loo(umca.6, k_threshold=0.7)
loo.umca7 <- loo(umca.7, k_threshold=0.7)
loo.umca8 <- loo(umca.8, k_threshold=0.7)
loo.umca9 <- loo(umca.9, k_threshold=0.7)

compare(loo.umcanull,loo.umca1, loo.umca2, loo.umca3,
        loo.umca4, loo.umca5, loo.umca6, loo.umca7, loo.umca8, loo.umca9)
compare(loo.umca8, loo.umcanull)

plot(umca.8,  prob = 0.5, prob_outer=0.90) + ggplot2::ggtitle("Posterior medians \n with 90% intervals \n for SESE seeding abundance")

elpd.umcaabund <- compare(loo.umca8, loo.umcanull)


### d) Pacific madrone (ARME) seedling abundances ######
arme.null <- stan_glm((armeseed2013) ~ 1, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.1 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.2 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.3 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality  + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.4 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting + FireMortality + PreFireSODmortality + hostCWD + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.5 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting + FireMortality + hostCWD + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)


arme.6 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA + PostFireARMEresprouting +  hostCWD + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.7 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA +  hostCWD + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.8 <- stan_glm((armeseed2013) ~ ARMEsurvivingBA +  hostCWD + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.9 <- stan_glm((armeseed2013) ~   hostCWD + AverageSlope + DistanceFromFireEdge + ARMEsurvivingBA  , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.10 <- stan_glm((armeseed2013) ~ hostCWD + AverageSlope + DistanceFromFireEdge  , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.11 <- stan_glm((armeseed2013) ~ hostCWD + AverageSlope, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

arme.12 <- stan_glm((armeseed2013) ~ AverageSlope, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

loo.armenull <-loo(arme.null, k_threshold=0.7)
loo.arme1 <- loo(arme.1, k_threshold=0.7)
loo.arme2 <- loo(arme.2, k_threshold=0.7)
loo.arme3 <- loo(arme.3, k_threshold=0.7)
loo.arme4 <- loo(arme.4, k_threshold=0.7)
loo.arme5 <- loo(arme.5, k_threshold=0.7)
loo.arme6 <- loo(arme.6, k_threshold=0.7)
loo.arme7 <- loo(arme.7, k_threshold=0.7)
loo.arme8 <- loo(arme.8, k_threshold=0.7)
loo.arme9 <- loo(arme.9, k_threshold=0.7)
loo.arme10 <- loo(arme.10, k_threshold=0.7)
loo.arme11 <- loo(arme.11, k_threshold=0.7)
loo.arme12 <- loo(arme.12, k_threshold=0.7)

compare(loo.armenull, loo.arme1, loo.arme2, loo.arme3, loo.arme4, loo.arme5, loo.arme6, loo.arme7, loo.arme8, loo.arme9, loo.arme10, loo.arme11, loo.arme12)
compare(loo.arme10, loo.armenull)

elpd.armeabund <- compare(loo.arme10, loo.armenull)

plot(arme.10,  prob = 0.5, prob_outer=0.90) + ggplot2::ggtitle("Posterior medians \n with 90% intervals \n for ARME seeding abundance")


#### e) Quercus seedling abundances: #####

quer.null.2013 <- stan_glm((quagseed2013+qupaseed2013) ~ 1, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

quer.1 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

quer.2 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + PreFireSODmortality + hostCWD + PreFireSODmortality*FireMortality + PFPrecip5yr + AverageSlope + DistanceFromFireEdge + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

quer.3 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope + Shrub.BA.LIVE.13  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

quer.4 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr + AverageSlope  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)


quer.5 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality + PFPrecip5yr  + as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

quer.6 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality +  as.factor(ForestAllianceType), data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

quer.7 <- stan_glm((quagseed2013+qupaseed2013) ~ QUERsurvivingBA + FireMortality + DiseaseStage + DiseaseStage*FireMortality , data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500, adapt_delta = 0.99)

quer.8 <- stan_glm((quagseed2013+qupaseed2013) ~ FireMortality + DiseaseStage + DiseaseStage*FireMortality, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

quer.9 <- stan_glm((quagseed2013+qupaseed2013) ~ FireMortality  + DiseaseStage, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)

quer.10 <- stan_glm((quagseed2013+qupaseed2013) ~ DiseaseStage, data = subset(variables, Burned_2008==TRUE),  family = neg_binomial_2, prior = normal(0,1), prior_intercept = normal(0, 10),  chains = 3, cores = 2000, seed = 500)


loo.quernull <-loo(quer.null.2013, k_threshold=0.7)
loo.quer1 <- loo(quer.1, k_threshold=0.7)
loo.quer2 <- loo(quer.2, k_threshold=0.7)
loo.quer3 <- loo(quer.3, k_threshold=0.7)
loo.quer4 <- loo(quer.4, k_threshold=0.7)
loo.quer5 <- loo(quer.5, k_threshold=0.7)
loo.quer6 <- loo(quer.6, k_threshold=0.7)
loo.quer7 <- loo(quer.7, k_threshold=0.7)
loo.quer8 <- loo(quer.8, k_threshold=0.7)
loo.quer9 <- loo(quer.9, k_threshold=0.7)
loo.quer10 <- loo(quer.10, k_threshold=0.7)

compare(loo.quernull, loo.quer1, loo.quer2, loo.quer3, loo.quer4, loo.quer5, loo.quer6, loo.quer7, loo.quer8, loo.quer9, loo.quer10)
compare(loo.quer7, loo.quernull)

plot(quer.7,  prob = 0.5, prob_outer=0.90) + ggplot2::ggtitle("Posterior medians \n with 90% intervals \n for QUAG seeding abundance")

elpd.querabund <- compare(loo.quer7, loo.quernull)