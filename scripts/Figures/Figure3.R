


###############################################
################### FIGURE 3 ################### 
###############################################

library(data.table)
library(MCMCglmm)
library(ggplot2)

#### EFFECTS OF SPECIALIZATION ON FITNESS ####

mod2 <- readRDS("output/models/2-ModLowDensityPSi.RDS")
summary(mod2)

## Low Density (mod 2)
df_adapt_PSi_low <- data.table(Trait = attr(colMeans(mod2$Sol), "names"),
                           Value = colMeans(mod2$Sol),
                           HPD = HPDinterval(mod2$Sol)) 
df_adapt_PSi_low$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt_PSi_low$Trait)) 
df_adapt_PSi_low$Trait <- gsub(pattern = "l.1",replacement = "", as.factor(df_adapt_PSi_low$Trait)) 
df_adapt_PSi_low2 <- df_adapt_PSi_low[29:416,]
df_adapt_PSi_low2[, c('Trait', 'ANIMAL_ID' ,'ID') := tstrsplit(Trait, '.', fixed = TRUE)]

df_adapt_PSi_low2$Trait[df_adapt_PSi_low2$Trait == "traitPSi"] <- 'IntPSi'  
df_adapt_PSi_low2$Trait[df_adapt_PSi_low2$Trait == "traitSurviva"] <- 'IntSurv'  
df_adapt_PSi_low2$Trait[df_adapt_PSi_low2$Trait == "traitPSi:scalePopDen"] <- 'PlastPSi'  
df_adapt_PSi_low2$Trait[df_adapt_PSi_low2$Trait == "traitSurvivalscalePopDen"] <- 'PlastSurv'  

df_adapt_PSi_low2[, c("ANIMAL_ID") := NULL]

IntPSiLow <- df_adapt_PSi_low2[Trait == "IntPSi"][,c("Trait") := NULL]
colnames(IntPSiLow)[1] <- "IntPSi"
colnames(IntPSiLow)[2] <- "IntPSi.lower"
colnames(IntPSiLow)[3] <- "IntPSi.upper"
IntPSiLow$density <- "Low density"

IntSurvLow <- df_adapt_PSi_low2[Trait == "IntSurv"][,c("Trait") := NULL]
colnames(IntSurvLow)[1] <- "IntSurv"
colnames(IntSurvLow)[2] <- "IntSurv.lower"
colnames(IntSurvLow)[3] <- "IntSurv.upper"
IntSurvLow$density <- "Low density"

## High Density (mod3)

mod3 <- readRDS("output/models/3-ModHighDensityPSi.RDS")
summary(mod3)

df_adapt_PSi_high <- data.table(Trait = attr(colMeans(mod3$Sol), "names"),
                            Value = colMeans(mod3$Sol),
                            HPD = HPDinterval(mod3$Sol)) 
df_adapt_PSi_high$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt_PSi_high$Trait)) 
df_adapt_PSi_high$Trait <- gsub(pattern = "l.1",replacement = "", as.factor(df_adapt_PSi_high$Trait)) 
df_adapt_PSi_high2 <- df_adapt_PSi_high[31:390,]
df_adapt_PSi_high2[, c('Trait', 'ANIMAL_ID' ,'ID') := tstrsplit(Trait, '.', fixed = TRUE)]

df_adapt_PSi_high2$Trait[df_adapt_PSi_high2$Trait == "traitPSi"] <- 'IntPSi'  
df_adapt_PSi_high2$Trait[df_adapt_PSi_high2$Trait == "traitSurviva"] <- 'IntSurv'  
df_adapt_PSi_high2$Trait[df_adapt_PSi_high2$Trait == "traitPSi:scalePopDen"] <- 'PlastPSi'  
df_adapt_PSi_high2$Trait[df_adapt_PSi_high2$Trait == "traitSurvivalscalePopDen"] <- 'PlastSurv'  

df_adapt_PSi_high2[, c("ANIMAL_ID") := NULL]


IntPSiHigh <- df_adapt_PSi_high2[Trait == "IntPSi"][,c("Trait") := NULL]
colnames(IntPSiHigh)[1] <- "IntPSi"
colnames(IntPSiHigh)[2] <- "IntPSi.lower"
colnames(IntPSiHigh)[3] <- "IntPSi.upper"
IntPSiHigh$density <- "High density"

IntSurvHigh <- df_adapt_PSi_high2[Trait == "IntSurv"][,c("Trait") := NULL]
colnames(IntSurvHigh)[1] <- "IntSurv"
colnames(IntSurvHigh)[2] <- "IntSurv.lower"
colnames(IntSurvHigh)[3] <- "IntSurv.upper"
IntSurvHigh$density <- "High density"

PSiIntDF <- rbind(IntPSiLow, IntPSiHigh)
SurvPSiDF <- rbind(IntSurvLow, IntSurvHigh)


#### EFFECTS OF SOCIAL STRENGTH ON FITNESS ####

mod4 <- readRDS("output/models/4-ModLowDensitySocial.RDS")
summary(mod4)

## Low Density (mod 2)
df_adapt_soc_low <- data.table(Trait = attr(colMeans(mod4$Sol), "names"),
                               Value = colMeans(mod4$Sol),
                               HPD = HPDinterval(mod4$Sol)) 
df_adapt_soc_low$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt_soc_low$Trait)) 
df_adapt_soc_low$Trait <- gsub(pattern = "l.1",replacement = "", as.factor(df_adapt_soc_low$Trait)) 
df_adapt_soc_low2 <- df_adapt_soc_low[29:388,]
df_adapt_soc_low2[, c('Trait', 'ANIMAL_ID' ,'ID') := tstrsplit(Trait, '.', fixed = TRUE)]

df_adapt_soc_low2$Trait[df_adapt_soc_low2$Trait == "traitstrength_soc"] <- 'IntSoc'  
df_adapt_soc_low2$Trait[df_adapt_soc_low2$Trait == "traitSurviva"] <- 'IntSurvSoc'  
df_adapt_soc_low2$Trait[df_adapt_soc_low2$Trait == "traitstrength_soc:scalePopDen"] <- 'PlastSoc'  
df_adapt_soc_low2$Trait[df_adapt_soc_low2$Trait == "traitSurvivalscalePopDen"] <- 'PlastSurvSoc'  

df_adapt_soc_low2[, c("ANIMAL_ID") := NULL]

IntSocLow <- df_adapt_soc_low2[Trait == "IntSoc"][,c("Trait") := NULL]
colnames(IntSocLow)[1] <- "IntSoc"
colnames(IntSocLow)[2] <- "IntSoc.lower"
colnames(IntSocLow)[3] <- "IntSoc.upper"
IntSocLow$density <- "Low density"

IntSurvSocLow <- df_adapt_soc_low2[Trait == "IntSurvSoc"][,c("Trait") := NULL]
colnames(IntSurvSocLow)[1] <- "IntSurvSoc"
colnames(IntSurvSocLow)[2] <- "IntSurvSoc.lower"
colnames(IntSurvSocLow)[3] <- "IntSurvSoc.upper"
IntSurvSocLow$density <- "Low density"

## High Density (mod3)

mod5 <- readRDS("output/models/5-ModHighDensitySocial.RDS")
summary(mod5)

df_adapt_soc_high <- data.table(Trait = attr(colMeans(mod5$Sol), "names"),
                                Value = colMeans(mod5$Sol),
                                HPD = HPDinterval(mod5$Sol)) 
df_adapt_soc_high$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt_soc_high$Trait)) 
df_adapt_soc_high$Trait <- gsub(pattern = "l.1",replacement = "", as.factor(df_adapt_soc_high$Trait)) 
df_adapt_soc_high2 <- df_adapt_soc_high[31:360,]
df_adapt_soc_high2[, c('Trait', 'ANIMAL_ID' ,'ID') := tstrsplit(Trait, '.', fixed = TRUE)]

df_adapt_soc_high2$Trait[df_adapt_soc_high2$Trait == "traitstrength_soc"] <- 'IntSoc'  
df_adapt_soc_high2$Trait[df_adapt_soc_high2$Trait == "traitSurviva"] <- 'IntSurvSoc'  
df_adapt_soc_high2$Trait[df_adapt_soc_high2$Trait == "traitstrength_soc:scalePopDen"] <- 'PlastSoc'  
df_adapt_soc_high2$Trait[df_adapt_soc_high2$Trait == "traitSurvivalscalePopDen"] <- 'PlastSurvSoc'  

df_adapt_soc_high2[, c("ANIMAL_ID") := NULL]


IntSocHigh <- df_adapt_soc_high2[Trait == "IntSoc"][,c("Trait") := NULL]
colnames(IntSocHigh)[1] <- "IntSoc"
colnames(IntSocHigh)[2] <- "IntSoc.lower"
colnames(IntSocHigh)[3] <- "IntSoc.upper"
IntSocHigh$density <- "High density"

IntSurvSocHigh <- df_adapt_soc_high2[Trait == "IntSurvSoc"][,c("Trait") := NULL]
colnames(IntSurvSocHigh)[1] <- "IntSurvSoc"
colnames(IntSurvSocHigh)[2] <- "IntSurvSoc.lower"
colnames(IntSurvSocHigh)[3] <- "IntSurvSoc.upper"
IntSurvSocHigh$density <- "High density"

SocIntDF <- rbind(IntSocLow, IntSocHigh)
SurvSocDF <- rbind(IntSurvSocLow, IntSurvSocHigh)



all <- cbind(SocIntDF, 
             SurvSocDF[,c("ID", "density") := NULL],
             PSiIntDF[,c("ID", "density") := NULL], 
             SurvPSiDF[,c("ID", "density") := NULL]) 


png("graphics/Fig3.png", width = 6000, height = 4000, res = 600, units = "px")
ggplot(all, aes(IntPSi, IntSurv, group = density)) +
  geom_errorbar(data = all, aes(ymin = IntSurv.lower, 
                                 ymax = IntSurv.upper),
                color = "grey") +
  geom_errorbarh(data = all, aes(xmin = IntPSi.lower, 
                                  xmax = IntPSi.upper),
                 color = "grey") +
  geom_point(color = "black", size = 2, alpha = 0.65) +
  geom_smooth(data = all[density == "High density"], aes(IntPSi, IntSurv, group = density),
              color = "black", 
              method = "lm", 
              se = F) +
  xlab(expression(Specialist %<->% Generalist)) +
  ylab(expression(`Low fitness` %<->% `High fitness`)) +
  theme(legend.position = c(0.1, 0.9),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        plot.title = element_text(size = 20),
        axis.title=element_text(size=20),
        strip.text = element_text(size=20, color = "black"),
        strip.background = element_rect(fill = "white", color = "black", size = 1),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~density, scale = "free")
dev.off()
