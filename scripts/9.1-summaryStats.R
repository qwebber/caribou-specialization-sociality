

library(data.table)
library(ggplot2)

nets <- fread("output/netsFinal.csv")

#### summary stats
### number of unique individuals
length(unique(nets$ANIMAL_ID))

## number of years per individual
sum.stats <- nets[, .N, by = .(ANIMAL_ID, Year)]
mean(sum.stats[, .N, by = .(ANIMAL_ID)]$N)
sd(sum.stats[, .N, by = .(ANIMAL_ID)]$N)
range(sum.stats[, .N, by = .(ANIMAL_ID)]$N)

## number of measures per individual
mean(nets[, .N, by = .(ANIMAL_ID)]$N)
sd(nets[, .N, by = .(ANIMAL_ID)]$N)
range(nets[, .N, by = .(ANIMAL_ID)]$N)

nets[, median(PSi), by = .(season)]
nets[, sd(PSi), by = .(season)]
nets[, median(strength_soc), by = .(season)]
nets[, sd(strength_soc), by = .(season)]


## average habitat specialization
nets[, mean(LichenAll), by = .(season)]
nets[, mean(RockyAll), by = .(season)]
nets[, mean(WetlandAll), by = .(season)]
nets[, mean(ConiferScrubAll), by = .(season)]


nets <- readRDS('output/5-combined-data.RDS')

### Phase for each herd ###
netTime <- nets[, unique(Number), by = .(HERD, Year, season)]
netTime <- na.omit(netTime)

### check overall trend for pop size over time
data.table(coefs = c(coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "BUCHANS"]))[2],
                     coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "GREY"]))[2],
                     coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "LAPOILE"]))[2],
                     coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "MIDRIDGE"]))[2],
                     coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "POTHILL"]))[2],
                     coef(lm(V1 ~ as.integer(Year), data = netTime[HERD == "TOPSAILS"]))[2]),
           herds = c("BUCHANS", "GREY", "LAPOILE" , "MIDRIDGE",
                     "POTHILL", "TOPSAILS"))


netTime$HERD[netTime$HERD == "BUCHANS"] <- 'A) Buchans'
netTime$HERD[netTime$HERD == "GREY"] <- 'B) Grey River'
netTime$HERD[netTime$HERD == "LAPOILE"] <- 'C) La Poile'
netTime$HERD[netTime$HERD == "MIDRIDGE"] <- 'D) Middle Ridge'
netTime$HERD[netTime$HERD == "POTHILL"] <- 'E) Pot Hill'
netTime$HERD[netTime$HERD == "TOPSAILS"] <- 'F) Topsails'


png("graphics/supplementary/FigS2_phase.png", width = 8000, height = 5000, res = 600, units = "px")
ggplot(netTime, aes(Year, V1, group = HERD)) +
  geom_smooth(method = "lm", color = "black") +
  geom_point() +
  ylab("Population size") +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        axis.text=element_text(size = 14, color = "black"),
        axis.title=element_text(size = 18),
        strip.text = element_text(size = 18),
        strip.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  facet_wrap(~HERD, scale = "free")
dev.off()




