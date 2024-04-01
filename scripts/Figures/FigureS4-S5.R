

library(data.table)
library(ggplot2)
library(lme4)

dynNets <- readRDS('output/6-rdmNets-100.RDS')
dynNets$IDSeasonYr <- as.factor(paste(dynNets$ID, dynNets$season, dynNets$Year, sep = "_"))

nets <- readRDS('output/5-combined-data.RDS')
nets$IDSeasonYr <- as.factor(paste(nets$ANIMAL_ID, nets$season, nets$Year, sep = "_"))

setDT(dynNets)[, mean(strength), by = "iteration"]
mean(nets$strength_soc)


dynNets2 <- merge(dynNets[,c("season", "Year", "gDen") := NULL], nets, by = "IDSeasonYr")


lowDensSoc <- summary(glmer(Survival ~ 
                             scale(strength_soc) +
                             scale(PSi) +
                             #HERD + 
                             scalePopDen + 
                             season + 
                             #factor(Year) + 
                             (1|ANIMAL_ID), 
                           family = "binomial",
                     data = nets[densityCat == "Low"]))$coefficients
summary(lowDensSoc)


highDensSoc <- summary(glmer(Survival ~ 
                          scale(strength_soc) +
                          scale(PSi) +
                          HERD + 
                          scalePopDen + 
                          season + 
                          factor(Year) + 
                          (1|ANIMAL_ID), 
                          family = "binomial",
                        data = nets[densityCat == "High"]))$coefficients

## Run random models for low density
coef_nulLowDens <- dynNets2[densityCat == "Low"][, as.list(summary(glmer(Survival ~ 
                                                                          scale(strength) + 
                                                                          scale(PSi) +
                                                                          #HERD + 
                                                                          scalePopDen + 
                                                                          season + 
                                                                          #Year +  
                                                                          (1|ANIMAL_ID),
                                                                         family = "binomial"))$coefficients), 
                                                 by = factor(iteration)]

colnames(coef_nulLowDens) <- c("iter", "intercept", "strength", "PSi", 
                               #"HERDGREY", "HERDLAPOILE", "HERDMIDRIDGE","HERDPOTHILL", 
                               "scalePopDen", "seasonwinter", 
                          "intercept_se", "strength_se", "PSi_se",  
                          #"HERDGREY_se", "HERDLAPOILE_se", "HERDMIDRIDGE_se", "HERDPOTHILL_se", 
                          "scalePopDen_se", "seasonwinter_se",
                          "intercept_z", "strength_z", "PSi_z", 
                          #"HERDGREY_z", "HERDLAPOILE_z", "HERDMIDRIDGE_z", "HERDPOTHILL_z", 
                          "scalePopDen_z", "seasonwinter_z",
                           "intercept_p","strength_p"  ,"PSi_p", 
                          #"HERDGREY_p", "HERDLAPOILE_p", "HERDMIDRIDGE_p", "HERDPOTHILL_p", 
                          "scalePopDen_p", "seasonwinter_p")


coef_nullHighDens <- dynNets2[densityCat == "High"][, as.list(summary(glmer(Survival ~ 
                                                                              scale(strength) + 
                                                                              scale(PSi) +
                                                                              HERD + 
                                                                              scalePopDen + 
                                                                              season + 
                                                                              as.factor(Year) +  
                                                                          (1|ANIMAL_ID),
                                                                          family = "binomial"))$coefficients), 
                                                 by = factor(iteration)]

colnames(coef_nullHighDens) <- c("iter","intercept", "strength", "PSi", 
                                 "HERDGREY", "HERDLAPOILE", "HERDMIDRIDGE", "HERDPOTHILL", "TOPSAILS" ,
                                 "scalePopDen", "seasonwinter", 
                                 "factor(Year)2008", "factor(Year)2009", "factor(Year)2010", 
                                 "factor(Year)2011", "factor(Year)2012", "factor(Year)2013",
                                 "intercept_se", "strength_se", "PSi_se", 
                                 "HERDGREY_se", "HERDLAPOILE_se", "HERDMIDRIDGE_se", "HERDPOTHILL_se", "TOPSAILS_se", 
                                 "scalePopDen_se", "seasonwinter_se",
                                 "factor(Year)2008_se", "factor(Year)2009_se", "factor(Year)2010_se", 
                                 "factor(Year)2011_se", "factor(Year)2012_se", "factor(Year)2013_se",
                                 "intercept_z", "strength_z", "PSi_z", 
                                 "HERDGREY_z", "HERDLAPOILE_z", "HERDMIDRIDGE_z", "HERDPOTHILL_z", "TOPSAILS_z", 
                                 "scalePopDen_z", "seasonwinter_z",
                                 "factor(Year)2008_z", "factor(Year)2009_z", "factor(Year)2010_z", 
                                 "factor(Year)2011_z", "factor(Year)2012_z", "factor(Year)2013_z",
                                 "intercept_p","strength_p", "PSi_p", 
                                 "HERDGREY_p", "HERDLAPOILE_p", "HERDMIDRIDGE_p", "HERDPOTHILL_p", "TOPSAILS_p", 
                                 "scalePopDen_p", "seasonwinter_p",
                                 "factor(Year)2008_p", "factor(Year)2009_p", "factor(Year)2010_p", 
                                 "factor(Year)2011_p", "factor(Year)2012_p", "factor(Year)2013_p")



## theme
themeMain <- theme(legend.position = 'none', 
                   axis.text=element_text(size=12, color = "black"),
                   axis.title=element_text(size=14),  
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),  
                   panel.border = element_rect(colour = "black", fill=NA, size=1), 
                   strip.background = element_rect(colour="black", fill = "white"))


png("graphics/FigS4.png", width= 6000, height = 3000, units = "px", res = 600)
aa <- ggplot(coef_nulLowDens) +
        geom_density(aes(strength), fill = "#56B4E9", alpha = 0.75) +
        geom_vline(xintercept = lowDensSoc[3,1], color = "black", lwd = 1) +
        geom_vline(xintercept = quantile(coef_nulLowDens$strength, c(0.975)), lty = 2, lwd = 1) +
        geom_vline(xintercept = quantile(coef_nulLowDens$strength, c(0.025)), lty = 2, lwd = 1) +
        ylab("Frequency") +
        xlab("Coefficient estimate for social strength") +
        ggtitle('A) Low density') +
        themeMain
bb <- ggplot(coef_nullHighDens) +
        geom_density(aes(strength), fill = "#E69F00", alpha = 0.75) +
        geom_vline(xintercept = highDensSoc[3,1], color = "black", lwd = 1) +
        geom_vline(xintercept = quantile(coef_nullHighDens$strength, c(0.975)), lty = 2, lwd = 1) +
        geom_vline(xintercept = quantile(coef_nullHighDens$strength, c(0.025)), lty = 2, lwd = 1) +
        ylab("Frequency") +
        xlab("Coefficient estimate for social strength") +
        ggtitle('B) High density') +
        themeMain
gridExtra::grid.arrange(aa,bb,nrow = 1)
dev.off()

## relationship between strength and PSi
SocPSi <- summary(lmer(scale(strength_soc) ~ 
                            scale(PSi) +
                            HERD + 
                            scalePopDen + 
                            season + 
                            #factor(Year) + 
                            (1|ANIMAL_ID), 
                             #family = "binomial",
                             data = nets))$coefficients
 

coef_nullSocPSi <- dynNets2[, as.list(summary(lmer(scale(strength) ~ 
                                                            scale(PSi) +
                                                            HERD + 
                                                            scalePopDen + 
                                                            season + 
                                                            #Year +  
                                                            (1|ANIMAL_ID)))$coefficients), 
                                                 by = factor(iteration)]

colnames(coef_nullSocPSi) <- c("iter", "intercept",  "PSi", 
                               "HERDGREY", "HERDLAPOILE", "HERDMIDRIDGE","HERDPOTHILL", "HERDTOPSAILS",
                               "scalePopDen", "seasonwinter", 
                               "intercept_se",  "PSi_se",  
                               "HERDGREY_se", "HERDLAPOILE_se", "HERDMIDRIDGE_se", "HERDPOTHILL_se", "HERDTOPSAILS_se",
                               "scalePopDen_se", "seasonwinter_se",
                               "intercept_t", "PSi_t", 
                               "HERDGREY_t", "HERDLAPOILE_t", "HERDMIDRIDGE_t", "HERDPOTHILL_t", "HERDTOPSAILS_t",
                               "scalePopDen_t", "seasonwinter_t")

png("graphics/FigS5.png", width= 4000, height = 4000, units = "px", res = 600)
ggplot(coef_nullSocPSi) +
  geom_density(aes(PSi), fill = "grey", alpha = 0.75) +
  geom_vline(xintercept = SocPSi[2,1], color = "black", lwd = 1) +
  geom_vline(xintercept = quantile(coef_nullSocPSi$PSi, c(0.975)), lty = 2, lwd = 1) +
  geom_vline(xintercept = quantile(coef_nullSocPSi$PSi, c(0.025)), lty = 2, lwd = 1) +
  xlim(-0.1, 0.25) +
  ylab("Frequency") +
  xlab("Coefficient estimate for proportional similarity index") +
  themeMain
dev.off()
