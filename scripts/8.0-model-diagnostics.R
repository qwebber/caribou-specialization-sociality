


libs <- c('data.table', 'ggplot2', 'gridExtra',
          'MCMCglmm', 'reshape2', 'dplyr','tidyr', 
          'tidybayes', 'emmeans')
lapply(libs, require, character.only = TRUE)

mod1 <- readRDS("output/models/1-ModAllData.RDS")
mod2 <- readRDS("output/models/2-ModLowDensityPSi.RDS")
mod3 <- readRDS("output/models/3-ModHighDensityPSi.RDS")
mod4 <- readRDS("output/models/4-ModLowDensitySocial.RDS")
mod5 <- readRDS("output/models/5-ModHighDensitySocial.RDS")


nets <- readRDS('output/5-combined-data.RDS')
nets$Year <- as.factor(nets$Year)


## calculate e means
df <- mod5 %>%
  emmeans(~ season + densityCat, data = nets) %>%
  gather_emmeans_draws() %>%
  median_qi()



## Plot the posterior distributions of the 2 response variables from the bivariate model
ggplot(df, aes(.value, sex, color = grid, shape = mast)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_errorbarh(aes(xmin = .lower, 
                     xmax = .upper, 
                     color = grid, 
                     height = 0),
                 position = position_dodge(width = 0.4)) +
  ylab("") +
  xlab("Posterior distribution") +
  theme_bw() +
  facet_wrap(~trait)

# Visualize posterior distributions

plot(mod1$VCV[,"traitstrength_soc:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitstrength_soc.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitPSi.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:traitSurvival.1.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:traitSurvival.1.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:traitSurvival.1.ANIMAL_ID"]) ## SKEWED DISTRIBUTION
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitSurvival.1.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitSurvival.1.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitSurvival.1.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitstrength_soc:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitPSi:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitstrength_soc:scalePopDen:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitPSi:scalePopDen:traitSurvival.1:scalePopDen.ANIMAL_ID"])
plot(mod1$VCV[,"traitSurvival.1:scalePopDen:traitSurvival.1:scalePopDen.ANIMAL_ID"])





## Heidel test
mod1_heidel <- heidel.diag(mod1$Sol, eps = 0.1, pvalue = 0.05)
mod1_heidel2 <- data.table(mod1_heidel[1:804,])

hist(mod1_heidel2$halfwidth)

## autocorrelation -- we basically want these numbesr to be <0.1
autocorr.diag(mod1$VCV)
autocorr.diag(mod2$VCV)
autocorr.diag(mod3$VCV)
autocorr.diag(mod4$VCV)
autocorr.diag(mod5$VCV)



