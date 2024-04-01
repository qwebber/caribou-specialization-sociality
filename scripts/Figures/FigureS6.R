

## load packages
libs <- c('data.table', 'ggplot2', 'gridExtra', 'lme4', 'dplyr', 'forcats')
lapply(libs, require, character.only = TRUE)

## load data
rsf <- fread("output/all-beta-rsfs.csv")
rsf2 <- dcast(rsf, IDSeasonYearHerd ~  term, value.var = c("estimate"))

nets <- readRDS('output/5-combined-data.RDS')

df <- merge(rsf2,
            nets[,c("PSi", "IDSeasonYearHerd", "Survival", "densityCat")],
            by = "IDSeasonYearHerd")

setDT(df)[, c('ANIMAL_ID', 'SEASON' ,'YEAR' ,'HERD') := tstrsplit(IDSeasonYearHerd, '_')]

df2 <- rbind(data.table(IDYr = paste(df$ANIMAL_ID, df$YEAR),
                        season = df$SEASON,
                        PSi = df$PSi,
                        selection = df$`bufLichen`,
                        Survival = df$Survival,
                        densityCat = df$densityCat,
                        Habitat = "Lichen"),
             data.table(IDYr = paste(df$ANIMAL_ID, df$YEAR),
                        season = df$SEASON,
                        PSi = df$PSi,
                        selection = df$`bufRocky`,
                        Survival = df$Survival,
                        densityCat = df$densityCat,
                        Habitat = "Rocky"),
             data.table(IDYr = paste(df$ANIMAL_ID, df$YEAR),
                        season = df$SEASON,
                        PSi = df$PSi,
                        selection = df$`bufWetland`,
                        Survival = df$Survival,
                        densityCat = df$densityCat,
                        Habitat = "Wetland"),
             data.table(IDYr = paste(df$ANIMAL_ID, df$YEAR),
                        season = df$SEASON,
                        PSi = df$PSi,
                        selection = df$`bufConiferScrub`,
                        Survival = df$Survival,
                        densityCat = df$densityCat,
                        Habitat = "Conifer"))


highFit_lowPSi <- df2[densityCat == "High" & PSi < quantile(PSi, 0.10) & Survival == 1]
highFit_lowPSi$fitness <- "High fitness, specialists"
lowFit_highPSi <- df2[densityCat == "High" & PSi > quantile(PSi, 0.90) & Survival == 0]
lowFit_highPSi$fitness <- "Low fitness, generalists"

all <- rbind(highFit_lowPSi, lowFit_highPSi)
all$IDYrSeasson <- as.factor(paste(all$IDYr, all$season, sep = "_"))

legend_title <- "PSi"

png("graphics/FigS6.png", width = 5000, height = 2500, res=600, units="px")
all %>%
  mutate(IDYrSeasson = forcats::fct_reorder(IDYrSeasson, PSi)) %>%
  ggplot(mapping = aes(x = IDYrSeasson,
                       y = selection,
                       shape = Habitat,
                       color = PSi)) +
  geom_point(alpha = 0.75,
             size = 3) +
  scale_color_viridis_c(legend_title) +
  ylim(-12, 7) +
  ylab("Selection coefficient") +
  xlab("Individual ID") +
  geom_hline(yintercept = 0, lty = 2) +
  theme(#legend.position = c(0.1, 0.9),
        #legend.title = element_blank(),
        legend.key = element_blank(),
        axis.text.x=element_blank(), #text(size=12, color = "black"),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour="black", fill = "white")) +
  facet_wrap(~fitness, scales = "free")
dev.off()

a1 <- lmer(PSi ~ `scale(bufLichen)` +
             `scale(bufRocky)`  +
             `scale(bufWetland)`  +
             `scale(bufConiferScrub)`  +
             SEASON +
             factor(YEAR) +
             (1|ANIMAL_ID/HERD), data = df)
Vcov <- vcov(a1, useScale = FALSE)
betas <- fixef(a1)
se <- sqrt(diag(Vcov))
zval <- betas / se
pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
round(cbind(betas, se, zval, pval), digits = 3)
summary(a1)
piecewiseSEM::rsquared(a1)

df4 <- df2[selection > 0]
df4$selectionAbs <- "Select"
df5 <- df2[selection < 0]
df5$selectionAbs <- "Avoid"

df6 <- rbind(df4, df5)

quantile(df6[habitat == "lichen"]$selection)

hh <- df6[habitat == "lichen" & selection > 0.7101335]

png("graphics/FigS5.1.png", width = 8000, height = 6000, res=600, units="px")

aa <- ggplot(hh[habitat == "lichen"]) +
  geom_histogram(aes(PSi,
                   fill = selectionAbs),
               alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('A) Lichen barrens selection') +
  xlab(expression(Specialist %<->% Generalist)) +
  ylab("Density distribution") +
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=14),
        #axis.text.x = element_blank(),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour="black", fill = "white")) +
  facet_wrap(~season*selectionAbs)


bb <- ggplot(df6[habitat == "rocky"]) +
  geom_histogram(aes(PSi,
                   fill = selectionAbs),
               alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('B) Rocky barrens selection') +
  xlab(expression(Specialist %<->% Generalist)) +
  ylab("Density distribution") +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=14),
        #axis.text.x = element_blank(),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour="black", fill = "white")) +
  facet_wrap(~season)


cc <- ggplot(df6[habitat == "wetland"]) +
  geom_histogram(aes(PSi,
                   fill = selectionAbs),
               alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('C) Wetland selection') +
  xlab(expression(Specialist %<->% Generalist)) +
  ylab("Density distribution") +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=14),
        #axis.text.x = element_blank(),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour="black", fill = "white")) +
  facet_wrap(~season)


dd <- ggplot(df6[habitat == "conifer"]) +
  geom_histogram(aes(PSi,
                   fill = selectionAbs),
               alpha = 0.5) +
  scale_fill_viridis_d() +
  ggtitle('D) Conifer selection') +
  xlab(expression(Specialist %<->% Generalist)) +
  ylab("Density distribution") +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=14),
        #axis.text.x = element_blank(),
        strip.text = element_text(size = 14),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(colour="black", fill = "white")) +
  facet_wrap(~season)

grid.arrange(aa,bb,cc,dd, nrow = 2, ncol = 2)
dev.off()


df$HERD[df$HERD == "MIDRIDGE"] <- "Middle Ridge"
df$HERD[df$HERD == "GREY"] <- "Grey River"
df$HERD[df$HERD == "LAPOILE"] <- "La Poile"
df$HERD[df$HERD == "BUCHANS"] <- "Buchans"
df$HERD[df$HERD == "POTHILL"] <- "Pot Hill"
df$HERD[df$HERD == "TOPSAILS"] <- "Topsails"

df$SEASON[df$SEASON == "calving"] <- "Calving"
df$SEASON[df$SEASON == "winter"] <- "Winter"


png("graphics/FigS5.png", width = 6000, height = 6000, res=600, units="px")
aa <- ggplot(df,
             aes(PSi, `scale(bufLichen)`)) +
  geom_point(aes(color = factor(SEASON)), alpha = 0.5) +
  ggtitle("A) Lichen barrens") +
  xlim(0.2, 1.0) +
  ylab("Selection coefficient") +
  xlab(expression(Specialist %<->% Generalist)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_color_manual(values=c("#d8b365", "#5ab4ac")) +
  theme(legend.position = c(0.15,0.9),
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 14),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~SEASON)
bb <- ggplot(df,
             aes(PSi, `scale(bufRocky)`)) +
  geom_point(aes(color = factor(SEASON)), alpha = 0.5) +
  ggtitle("B) Rocky barrens") +
  ylab("Selection coefficient") +
  xlab(expression(Specialist %<->% Generalist)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlim(0.2, 1.0) +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_color_manual(values=c("#d8b365", "#5ab4ac")) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 14),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~SEASON)
cc <- ggplot(df, aes(PSi, `scale(bufWetland)`)) +
  geom_point(aes(color = factor(SEASON)), alpha = 0.5) +
  ggtitle("C) Wetland") +
  xlim(0.2, 1.0) +
  ylab("Selection coefficient") +
  xlab(expression(Specialist %<->% Generalist)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_color_manual(values=c("#d8b365", "#5ab4ac")) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 14),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~SEASON)
dd <- ggplot(df, aes(scale(PSi), `scale(bufConiferScrub)`)) +
  geom_point(aes(color = factor(SEASON)), alpha = 0.5) +
  ggtitle("D) Conifer scrub") +
  ylab("Selection coefficient") +
  xlab(expression(Specialist %<->% Generalist)) +
  #xlim(0.2, 1.0) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_smooth(method = "lm", se = F, color = "black") +
  scale_color_manual(values=c("#d8b365", "#5ab4ac")) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(size = 14),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=12),
        strip.text = element_text(size=12),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~SEASON)
grid.arrange(aa,bb,cc,dd, nrow = 2, ncol = 2)
dev.off()

