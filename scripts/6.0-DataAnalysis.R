

libs <- c('data.table', 'ggplot2', 'gridExtra',
          'MCMCglmm', 'reshape2', 'dplyr','tidyr')
lapply(libs, require, character.only = TRUE)

nets <- readRDS('output/5-combined-data.RDS')
nets$Year <- as.factor(nets$Year)

## year by season
num <- nets[, .N, by = c("Year", "season", "HERD")]

## summary stats
mean(nets[, .N, by = "ANIMAL_ID"]$N)
sd(nets[, .N, by = "ANIMAL_ID"]$N)
range(nets[, .N, by = "ANIMAL_ID"]$N)

## seasonal network size
szn_net <- nets[, unique(netSize), by = c("Year", "season")]

## winter
mean(szn_net[season == "winter"]$V1)
sd(szn_net[season == "winter"]$V1)
range(szn_net[season == "winter"]$V1)

## calving
mean(szn_net[season == "calving"]$V1)
sd(szn_net[season == "calving"]$V1)
range(szn_net[season == "calving"]$V1)

## social strength 
mean(nets[season == "winter"]$strength_soc)
sd(nets[season == "winter"]$strength_soc)
mean(nets[season == "calving"]$strength_soc)
sd(nets[season == "calving"]$strength_soc)

## specialization 
mean(nets[season == "winter"]$PSi)
mean(nets[season == "calving"]$PSi)
sd(nets[season == "winter"]$PSi)
sd(nets[season == "calving"]$PSi)

## fitness 
nets[season == "calving"][, .N, by = "Survival"]
## all
241/(241+152)

mn <- nets[, median(PSi), by = "densityCat"]

ggplot() +
  geom_density(data = nets[densityCat == "Low"], aes(PSi), 
               fill = "red", alpha = 0.5) +
  geom_density(data = nets[densityCat == "High"], aes(PSi),
               fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = 0.7719997, color = "red") +
  geom_vline(xintercept = 0.7001294, color = "blue")
  
#############################################################
######################## BRN MODELS #########################
#############################################################

nitt=420000
burnin=20000
thin=100

#### Repeatability analysis #######
#prior2 <- list(R=list(V=diag(18), nu = 4),
#               G=list(G1=list(V=diag(6), nu = 4, 
#                              alpha.V=diag(var(nets$strength_soc, na.rm = TRUE),6,6))))

prior2.1 <- list(R = list(V = diag(6), nu = 6),
                 G = list(G1 = list(V = diag(6), nu = 6, 
                                    alpha.V = 1000*diag(6))))

### Model 1: season + Ind x Env 
mod1 <- MCMCglmm(cbind(scale(strength_soc), scale(PSi), Survival) ~ trait-1 +
                   trait:HERD +
                   trait:scalePopDen + 
                   trait:season +
                   trait:Year,
                 random =~ us(trait + scalePopDen:trait):ANIMAL_ID, 
                 rcov =~ idh(trait:season):units, ## this is the line for varying residuals
                 family = c("gaussian","gaussian", "categorical"),
                 prior = prior2.1,
                 nitt = nitt,
                 burnin = burnin,
                 thin = thin,
                 verbose = TRUE,
                 data = nets,
                 pr=T,saveX = TRUE,saveZ = TRUE)


#######################################################################
######################## BRN MODELS BY DENSITY #########################
#######################################################################

prior2.2 <- list(R = list(V = diag(2), nu = 4),
                 G = list(G1 = list(V = diag(4), nu = 4, 
                                    alpha.V = 1000*diag(4))))

### Model 2: Low density herds (PSi)
mod2 <- MCMCglmm(cbind(scale(PSi), Survival) ~ trait-1 +
                   trait:strength_soc + 
                   trait:HERD +
                   trait:scalePopDen + 
                   trait:season +
                   trait:Year,
                 random =~ us(trait + scalePopDen:trait):ANIMAL_ID, 
                 rcov =~ idh(trait):units,
                 family = c("gaussian", "categorical"),
                 prior = prior2.2,
                 nitt = nitt,
                 burnin = burnin,
                 thin = thin,
                 verbose = TRUE,
                 data = nets[densityCat == "Low"],
                 pr=T,saveX = TRUE,saveZ = TRUE)

### Model 3: High density herds (PSi)
mod3 <- MCMCglmm(cbind(scale(PSi), Survival) ~ trait-1 +
                   trait:strength_soc + 
                   trait:HERD +
                   trait:scalePopDen + 
                   trait:season +
                   trait:Year,
                 random =~ us(trait + scalePopDen:trait):ANIMAL_ID, 
                 rcov =~ idh(trait):units,
                 family = c("gaussian", "categorical"),
                 prior = prior2.2,
                 nitt = nitt,
                 burnin = burnin,
                 thin = thin,
                 verbose = TRUE,
                 data = nets[densityCat == "High"],
                 pr=T,saveX = TRUE,saveZ = TRUE)

### Model 4: Low density herds (centrality)
mod4 <- MCMCglmm(cbind(scale(strength_soc), Survival) ~ trait-1 +
                   trait:PSi + 
                   trait:HERD +
                   trait:scalePopDen + 
                   trait:season +
                   trait:Year,
                 random =~ us(trait + scalePopDen:trait):ANIMAL_ID, 
                 rcov =~ idh(trait):units,
                 family = c("gaussian", "categorical"),
                 prior = prior2.2,
                 nitt = nitt,
                 burnin = burnin,
                 thin = thin,
                 verbose = TRUE,
                 data = nets[densityCat == "Low"],
                 pr=T,saveX = TRUE,saveZ = TRUE)

### Model 5: High density herds (centrality)
mod5 <- MCMCglmm(cbind(scale(strength_soc), Survival) ~ trait-1 +
                   trait:PSi + 
                   trait:HERD +
                   trait:scalePopDen + 
                   trait:season +
                   trait:Year,
                 random =~ us(trait + scalePopDen:trait):ANIMAL_ID, 
                 rcov =~ idh(trait):units,
                 family = c("gaussian", "categorical"),
                 prior = prior2.2,
                 nitt = nitt,
                 burnin = burnin,
                 thin = thin,
                 verbose = TRUE,
                 data = nets[densityCat == "High"],
                 pr=T,saveX = TRUE,saveZ = TRUE)

saveRDS(mod1, "output/models/1-ModAllData.RDS")
saveRDS(mod2, "output/models/2-ModLowDensityPSi.RDS")
saveRDS(mod3, "output/models/3-ModHighDensityPSi.RDS")
saveRDS(mod4, "output/models/4-ModLowDensitySocial.RDS")
saveRDS(mod5, "output/models/5-ModHighDensitySocial.RDS")







