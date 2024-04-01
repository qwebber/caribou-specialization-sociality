

libs <- c('data.table', 'ggplot2', 'gridExtra',
          'MCMCglmm', 'reshape2', 'dplyr','tidyr')
lapply(libs, require, character.only = TRUE)

mod1 <- readRDS("output/models/1-ModAllData.Rds")
summary(mod1)

##### PHENOTYPIC COVARIANCE FROM MODELS #####

##################  CORRELATIONS ##################
## CORRELATIONS between Intercept centrality and Intercept PSi:
mcmc_soc_PSi_ints <- mod1$VCV[,"traitPSi:traitstrength_soc.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"]))

## CORRELATIONS between Intercept Soc and Intercept survival:
mcmc_soc_fit_ints <- mod1$VCV[,"traitSurvival.1:traitstrength_soc.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

## CORRELATIONS between Intercept PSi and Intercept survival:
mcmc_fit_PSi_ints <- mod1$VCV[,"traitSurvival.1:traitPSi.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

## CORRELATIONS between Slope strength and Intercept strength:
mcmc_soc_int_slope <- mod1$VCV[,"traitstrength_soc:scalePopDen:traitstrength_soc.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitstrength_soc:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"]))

## CORRELATIONS between Slope PSi and Intercept PSi:
mcmc_PSi_int_slope <- mod1$VCV[,"traitPSi:scalePopDen:traitPSi.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitPSi:scalePopDen:traitPSi:scalePopDen.ANIMAL_ID"]))

## CORRELATIONS between Slope strength and Slope PSi:
mcmc_soc_PSi_slope <- mod1$VCV[,"traitPSi:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"]/
  (sqrt(mod1$VCV[,"traitstrength_soc:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"])*
     sqrt(mod1$VCV[,"traitPSi:scalePopDen:traitPSi:scalePopDen.ANIMAL_ID"]))


mcmc_cor_all <- data_frame(Traits = c("Centrality, Habitat specialization", 
                                      "Intercept Centrality, Slope Centrality",
                                      "Centrality, Fitness",
                                      "PSi, Fitness",
                                      "Intercept PSi, Slope PSi",
                                      "Slope Centrality, Slope PSi"), 
                           Estimate = c(median(mcmc_soc_PSi_ints), median(mcmc_soc_int_slope),
                                        median(mcmc_soc_fit_ints), median(mcmc_fit_PSi_ints), 
                                        median(mcmc_PSi_int_slope),median(mcmc_soc_PSi_slope)),
                           Lower = c(HPDinterval(mcmc_soc_PSi_ints)[,"lower"], HPDinterval(mcmc_soc_int_slope)[,"lower"],
                                     HPDinterval(mcmc_soc_fit_ints)[,"lower"],HPDinterval(mcmc_fit_PSi_ints)[,"lower"],
                                     HPDinterval(mcmc_PSi_int_slope)[,"lower"], HPDinterval(mcmc_soc_PSi_slope)[,"lower"]), 
                           Upper = c(HPDinterval(mcmc_soc_PSi_ints)[,"upper"], HPDinterval(mcmc_soc_int_slope)[,"upper"],
                                     HPDinterval(mcmc_soc_fit_ints)[,"upper"],HPDinterval(mcmc_fit_PSi_ints)[,"upper"],
                                     HPDinterval(mcmc_PSi_int_slope)[,"upper"], HPDinterval(mcmc_soc_PSi_slope)[,"upper"]))

mcmc_cor_all
#### REPEATABILITY ####
rep_str_winter <- mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"]/(
  mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"] +
    mod1$VCV[,"traitstrength_soc:seasonwinter.units"])

rep_str_calving <- mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"]/(
  mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"] +
    mod1$VCV[,"traitstrength_soc:seasoncalving.units"])

rep_PSi_winter <-  mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"]/(
  mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"] +
    mod1$VCV[,"traitPSi:seasonwinter.units"])

rep_PSi_calving <-  mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"]/(
  mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"] +
    mod1$VCV[,"traitPSi:seasoncalving.units"])

reps <- data_frame(Traits = c("Centrality",
                              "Centrality",
                              "Habitat specialization", 
                              "Habitat specialization"),
                   Density = c("Winter", "Calving", "Winter", "Calving"),
                   Estimate = c(median(rep_str_winter), 
                                median(rep_str_calving),
                                median(rep_PSi_winter), 
                                median(rep_PSi_calving)),
                   Lower = c(HPDinterval(rep_str_winter)[,"lower"],
                             HPDinterval(rep_str_calving)[,"lower"],
                             HPDinterval(rep_PSi_winter)[,"lower"],
                             HPDinterval(rep_PSi_calving)[,"lower"]),
                   Upper = c(HPDinterval(rep_str_winter)[,"upper"],
                             HPDinterval(rep_str_calving)[,"upper"],
                             HPDinterval(rep_PSi_winter)[,"upper"], 
                             HPDinterval(rep_PSi_calving)[,"upper"]),
                   Vres = c(
                     median(mod1$VCV[,"traitstrength_soc:seasonwinter.units"]),
                     median(mod1$VCV[,"traitstrength_soc:seasoncalving.units"]),
                     median(mod1$VCV[,"traitPSi:seasonwinter.units"]),       
                     median(mod1$VCV[,"traitPSi:seasoncalving.units"])))

reps


mod2 <- readRDS("output/models/2-ModLowDensityPSi.RDS")
mod3 <- readRDS("output/models/3-ModHighDensityPSi.RDS")
mod4 <- readRDS("output/models/4-ModLowDensitySocial.RDS")
mod5 <- readRDS("output/models/5-ModHighDensitySocial.RDS")

summary(mod5)

##### PHENOTYPIC COVARIANCE FROM MODELS #####

## CORRELATIONS between Intercept PSi and Intercept survival - Middle Ridge
mcmc_fit_PSi_ints_low <- mod2$VCV[,"traitSurvival.1:traitPSi.ANIMAL_ID"]/
  (sqrt(mod2$VCV[,"traitPSi:traitPSi.ANIMAL_ID"])*
     sqrt(mod2$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

## CORRELATIONS between Intercept PSi and Intercept survival- Lapoile
mcmc_fit_PSi_ints_high <- mod3$VCV[,"traitSurvival.1:traitPSi.ANIMAL_ID"]/
  (sqrt(mod3$VCV[,"traitPSi:traitPSi.ANIMAL_ID"])*
     sqrt(mod3$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

## CORRELATIONS between Intercept PSi and Intercept survival- Lapoile
mcmc_fit_soc_ints_low <- mod4$VCV[,"traitSurvival.1:traitstrength_soc.ANIMAL_ID"]/
  (sqrt(mod4$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])*
     sqrt(mod4$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

## CORRELATIONS between Intercept strength_soc and Intercept survival:
mcmc_fit_soc_ints_high <- mod5$VCV[,"traitSurvival.1:traitstrength_soc.ANIMAL_ID"]/
  (sqrt(mod5$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])*
     sqrt(mod5$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]))

mcmc_cor <- data_frame(Traits = c("PSi, Fitness",
                                  "PSi, Fitness",
                                  "Centrality, Fitness",
                                  "Centrality, Fitness"),
                       Density = c("Low", "High", "Low", "High"), 
                       Estimate = c(median(mcmc_fit_PSi_ints_low), 
                                    median(mcmc_fit_PSi_ints_high), 
                                    median(mcmc_fit_soc_ints_low), 
                                    median(mcmc_fit_soc_ints_high)), 
                       Lower = c(HPDinterval(mcmc_fit_PSi_ints_low)[,"lower"],
                                 HPDinterval(mcmc_fit_PSi_ints_high)[,"lower"],
                                 HPDinterval(mcmc_fit_soc_ints_low)[,"lower"], 
                                 HPDinterval(mcmc_fit_soc_ints_high)[,"lower"]),
                       Upper = c(HPDinterval(mcmc_fit_PSi_ints_low)[,"upper"], 
                                 HPDinterval(mcmc_fit_PSi_ints_high)[,"upper"],
                                 HPDinterval(mcmc_fit_soc_ints_low)[,"upper"], 
                                 HPDinterval(mcmc_fit_soc_ints_high)[,"upper"]))

mcmc_cor
