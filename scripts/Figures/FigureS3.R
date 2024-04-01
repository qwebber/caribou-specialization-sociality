
library(data.table)
library(ggplot2)

nets <- readRDS('output/5-combined-data.RDS')
nets$Year <- as.factor(nets$Year)

## year by season
num <- nets[, .N, by = c("Year", "season" ,"HERD", "Number")]

num$Herd <- num$HERD

num$Herd[num$Herd == "MIDRIDGE"] <- "Middle Ridge"
num$Herd[num$Herd == "GREY"] <- "Grey River"
num$Herd[num$Herd == "LAPOILE"] <- "La Poile"
num$Herd[num$Herd == "BUCHANS"] <- "Buchans"
num$Herd[num$Herd == "POTHILL"] <- "Pot Hill"
num$Herd[num$Herd == "TOPSAILS"] <- "Topsails"

num$season[num$season == "calving"] <- "Calving"
num$season[num$season == "winter"] <- "Winter"


png("graphics/FigS3.png", width = 4500, height = 3000, res=600, units="px")
ggplot(num) + 
  geom_point(aes(N, Number, color = Herd, size = 2), alpha = 0.5) +
  scale_color_viridis_d() +
  xlim(0,30) +
  ylab('Population size') +
  xlab('Number of collared animals ') +
  scale_size_continuous(guide = 'none') +
  theme(legend.key = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=14),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~season)
  dev.off()
  