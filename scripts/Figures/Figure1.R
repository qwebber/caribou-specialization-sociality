
libs <- c('data.table', 'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

###############################################
################### FIGURE 1 ###################
###############################################



libs <- c('data.table', 'ggplot2', 'gridExtra',
          'MCMCglmm', 'reshape2', 'dplyr','tidyr')
lapply(libs, require, character.only = TRUE)


nets <- readRDS('output/5-combined-data.RDS')
nets$Year <- as.factor(nets$Year)

nets <- nets[Year != "2006"]

## scale population density by herd
nets[, scalePopDen := as.numeric(scale(pDen)), by = .(HERD)]

## group herds into density categories
nets[scalePopDen < 0, densityCat := "Low"]
nets[scalePopDen > 0, densityCat := "High"]
nets$densityCat <- as.factor(nets$densityCat)

## remove NAs from proportional similarity index
nets <- nets[!is.na(nets$densityCat),]
nets <- nets[!is.na(nets$PSi),]
nets <- nets[!is.na(nets$Survival),]


###########################################################
######################## FIGURES #########################
##########################################################

nitt=420000
burnin=20000
thin=100

##### FIGURE 1: BRNS #####
p.var_PSi<-var(nets$PSi,na.rm=TRUE)

prior_PSi<-list(G=list(G1=list(V=diag(2)*(p.var_PSi/2), nu=1,
                               alpha.V=diag(2)*p.var_PSi/2)),
                R=list(V=diag(1)*(p.var_PSi/2), nu=1))

mcmcPSi <- MCMCglmm(scale(PSi) ~ scale(strength_soc) +
                      scalePopDen +
                      Year + HERD + season,
                    random =~ us(1 + scalePopDen):ANIMAL_ID,
                    rcov = ~units,
                    family = "gaussian",
                    prior = prior_PSi,
                    nitt = nitt,
                    burnin = burnin,
                    thin = thin,
                    verbose = TRUE,
                    data = nets,
                    pr=TRUE,
                    saveX = TRUE,
                    saveZ = TRUE)

saveRDS(mcmcPSi, file = "output/figure 1 data/mod_PSi_BRN.Rds")

p.var_soc<-var(nets$strength_soc,na.rm=TRUE)

prior_soc<-list(G=list(G1=list(V=diag(2)*(p.var_soc/2), nu=1,
                               alpha.V=diag(2)*p.var_soc/2)),
                R=list(V=diag(1)*(p.var_soc/2), nu=1))

mcmcCentrality <- MCMCglmm(scale(strength_soc) ~ scale(PSi) +
                             scalePopDen +
                             Year + HERD + season,
                           random =~ us(1 + scalePopDen):ANIMAL_ID,
                           rcov = ~units,
                           family = "gaussian",
                           prior = prior_soc,
                           nitt = nitt,
                           burnin = burnin,
                           thin = thin,
                           verbose = TRUE,
                           data = nets,
                           pr=TRUE,
                           saveX = TRUE,
                           saveZ = TRUE)

saveRDS(mcmcCentrality, file = "output/figure 1 data/mod_cent_BRN.Rds")

#mcmcPSi <- readRDS("output/models/mod_PSi_BRN.Rds")
#mcmcCentrality <- readRDS("output/models/mod_cent_BRN.Rds")

### habitat specialization ###
df_PSi<- cbind(nets,
               fit = predict(mcmcPSi, marginal = NULL)) %>%
  group_by(ANIMAL_ID, HERD, scalePopDen) %>%
  summarise(fit = mean(fit.V1),
            PSi = mean(PSi)) %>%
  gather(Type, Value,
         fit:PSi)

df_fit_PSi = subset(df_PSi, Type == "fit")

### Centrality ###
df_centrality <- cbind(nets,
                       fit = predict(mcmcCentrality, marginal = NULL)) %>%
  group_by(ANIMAL_ID, HERD, scalePopDen) %>%
  summarise(fit = mean(fit.V1),
            Soc = mean(strength_soc)) %>%
  gather(Type, Value,
         fit:Soc)

df_fit_cent = subset(df_centrality, Type == "fit")

fwrite(df_fit_PSi, "output/figure 1 data/df_fit_PSi.csv")
fwrite(df_fit_cent, "output/figure 1 data/df_fit_cent.csv")


## load data without running above
df_fit_cent <- fread("output/figure 1 data/df_fit_cent.csv")
df_fit_PSi <- fread("output/figure 1 data/df_fit_PSi.csv")


png("graphics/Fig1.png", width = 6000, height = 3000, res=600, units="px")
aa <- ggplot(setDT(df_fit_cent)) + #aes(x = scalePopDen, y = Value, group = factor(ANIMAL_ID))) +
  geom_smooth(aes(scalePopDen,Value, group = ANIMAL_ID),
              color = "grey",
              size = 0.5,
              method=lm,
              se=FALSE) +
  geom_smooth(aes(scalePopDen,Value),
              color = "black",
              size = 1,
              method=lm,
              se=FALSE) +
  xlab("Mean-centred population density") +
  ylab("Social strength") +
  ggtitle('A) ') +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

bb <- ggplot(setDT(df_fit_PSi)) +  #aes(x = scalePopDen, y = Value, group = factor(ANIMAL_ID))) +
  geom_smooth(aes(scalePopDen,Value, group = ANIMAL_ID),
              color = "grey",
              size = 0.5,
              method=lm,
              se=FALSE) +
  geom_smooth(aes(scalePopDen,Value),
              color = "black",
              size = 1,
              method=lm,
              se=FALSE) +
  xlab("Mean-centred population density") +
  ylab(expression(Specialist %<->% Generalist)) +
  ggtitle('B)') +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

grid.arrange(aa,bb, ncol = 2)
dev.off()



