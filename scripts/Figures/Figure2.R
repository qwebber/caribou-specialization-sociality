
###############################################
################### FIGURE 2 ###################
###############################################

#### PHENOTYPIC COVARIANCE ####

library(data.table)
library(ggplot2)
library(MCMCglmm)
#### All data combined #####

mod1 <- readRDS("output/models/1-ModAllData.RDS")
summary(mod1)

colSd <- function (x, na.rm=FALSE) apply(X=x, MARGIN=2, FUN=sd, na.rm=na.rm)

df_adapt <- data.table(Trait = attr(colMeans(mod1$Sol), "names"),
                       Value = colMeans(mod1$Sol),
                       HPD = HPDinterval(mod1$Sol))
df_adapt$Trait <- gsub(pattern = ".1:",replacement = "", as.factor(df_adapt$Trait))
df_adapt$Trait <- gsub(pattern = "_",replacement = "", as.factor(df_adapt$Trait))
df_adapt2 <- df_adapt[43:804,]
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
colnames(IntCentAll)[2] <- "IntCent.lower"
colnames(IntCentAll)[3] <- "IntCent.upper"

IntPSiAll <- df_adapt2[Trait == "IntPSi"][,c("Trait") := NULL]
colnames(IntPSiAll)[1] <- "IntPSi"
colnames(IntPSiAll)[2] <- "IntPSi.lower"
colnames(IntPSiAll)[3] <- "IntPSi.upper"

IntSurvAll <- df_adapt2[Trait == "IntSurv"][,c("Trait") := NULL]
colnames(IntSurvAll)[1] <- "IntSurv"
colnames(IntSurvAll)[2] <- "IntSurv.lower"
colnames(IntSurvAll)[3] <- "IntSurv.upper"

PlastCentAll <- df_adapt2[Trait == "PlastCent"][,c("Trait") := NULL]
colnames(PlastCentAll)[1] <- "PlastCent"
colnames(PlastCentAll)[2] <- "PlastCent.lower"
colnames(PlastCentAll)[3] <- "PlastCent.upper"

PlastPSiAll <- df_adapt2[Trait == "PlastPSi"][,c("Trait") := NULL]
colnames(PlastPSiAll)[1] <- "PlastPSi"
colnames(PlastPSiAll)[2] <- "PlastPSi.lower"
colnames(PlastPSiAll)[3] <- "PlastPSi.upper"


all2 <- cbind(IntCentAll, IntPSiAll[,c("ID") := NULL],
              IntSurvAll[,c("ID") := NULL],
              PlastCentAll[,c("ID") := NULL],
              PlastPSiAll[,c("ID") := NULL])

fwrite(all2, "output/figure 2 data/soc-hab.csv")

#### Plot covariance independent of density
png("graphics/Fig2.png", width = 4000, height = 4000, res = 600, units = "px")
ggplot(all2, aes(IntCent, IntPSi)) +
  geom_errorbar(data = all2, aes(ymin = IntPSi.lower,
                                   ymax = IntPSi.upper),
                  color = "grey") +
  geom_errorbarh(data = all2, aes(xmin = IntCent.lower,
                                 xmax = IntCent.upper),
                color = "grey") +
  geom_point(size = 2, color = "black") +
  geom_smooth(method = "lm", se = F, color = "black") +
  ylab(expression(Specialist %<->% Generalist)) +
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

