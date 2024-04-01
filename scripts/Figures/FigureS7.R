


###############################################
################### FIGURE S7 ################### 
###############################################

#### PHENOTYPIC COVARIANCE ####

#### All data combined ##### 

mod1 <- readRDS("output/models/1-ModAllData.RDS")
summary(mod1)

df_adapt <- data.table(Trait = attr(colMeans(mod1$Sol), "names"),
                       Value = colMeans(mod1$Sol)) 
df_adapt$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt$Trait)) 
df_adapt$Trait <- gsub(pattern = "_",replacement = "", as.factor(df_adapt$Trait)) 
df_adapt2 <- df_adapt[28:768,]
df_adapt2[, c('Trait', 'ANIMAL_ID' ,'ID', 'aa') := tstrsplit(Trait, '.', fixed = TRUE)]

df_adapt2$Trait[df_adapt2$Trait == "traitstrengthsoc"] <- 'IntCent'  
df_adapt2$Trait[df_adapt2$Trait == "traitPSi"] <- 'IntPSi'  
df_adapt2$Trait[df_adapt2$Trait == "traitSurvival"] <- 'IntSurv'  
df_adapt2$Trait[df_adapt2$Trait == "traitstrengthsoc:scalePopDen"] <- 'PlastCent'  
df_adapt2$Trait[df_adapt2$Trait == "traitPSi:scalePopDen"] <- 'PlastPSi'  
df_adapt2$Trait[df_adapt2$Trait == "traitSurvivalscalePopDen"] <- 'PlastSurv'  

df_adapt2[, c("ANIMAL_ID", 'aa') := NULL]

IntCentAll <- df_adapt2[Trait == "IntCent"][,c("Trait") := NULL]
colnames(IntCentAll)[1] <- "IntCent"

IntPSiAll <- df_adapt2[Trait == "IntPSi"][,c("Trait") := NULL]
colnames(IntPSiAll)[1] <- "IntPSi"

IntSurvAll <- df_adapt2[Trait == "IntSurv"][,c("Trait") := NULL]
colnames(IntSurvAll)[1] <- "IntSurv"

PlastCentAll <- df_adapt2[Trait == "PlastCent"][,c("Trait") := NULL]
colnames(PlastCentAll)[1] <- "PlastCent"

PlastPSiAll <- df_adapt2[Trait == "PlastPSi"][,c("Trait") := NULL]
colnames(PlastPSiAll)[1] <- "PlastPSi"

all2 <- cbind(IntCentAll, IntPSiAll[,c("ID") := NULL], 
              IntSurvAll[,c("ID") := NULL], 
              PlastCentAll[,c("ID") := NULL],
              PlastPSiAll[,c("ID") := NULL])

#### Plot covariance independent of density
png("graphics/FigS7.png", width = 4000, height = 4000, res = 600, units = "px")
ggplot(all2, aes(IntCent, IntSurv)) +
  geom_point(size = 2, alpha = 0.5, color = "black") +
  geom_smooth(method = "lm", se = F, color = "black") +
  ylab('Calf survival') +
  xlab('Social strength') +
  theme(legend.position = c(0.2,0.85), 
        legend.title = element_text('none'),
        legend.text = element_text(size = 12),
        legend.key = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=12,face = "bold"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

