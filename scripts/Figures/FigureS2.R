

library(data.table)
library(ggplot2)

density <- fread("input/densityCaribouHerds.csv")

density <- density[HERD == "MIDRIDGE" |
                   HERD == "LAPOILE" | 
                   HERD == "BUCHANS" | 
                   HERD == "TOPSAILS" |
                   HERD == "POTHILL" |
                   HERD == "GREY"]

density$HERD[density$HERD == "MIDRIDGE"] <- "Middle Ridge"
density$HERD[density$HERD == "GREY"] <- "Grey River"
density$HERD[density$HERD == "LAPOILE"] <- "La Poile"
density$HERD[density$HERD == "BUCHANS"] <- "Buchans"
density$HERD[density$HERD == "POTHILL"] <- "Pot Hill"
density$HERD[density$HERD == "TOPSAILS"] <- "Topsails"

png("graphics/FigS2.png", width = 4500, height = 3000, res=600, units="px")
ggplot(density, aes(Year, Number)) +
  geom_point(aes(color = HERD), alpha = 0.55) +
  geom_smooth(method = "gam", aes(color = HERD)) + 
  geom_vline(xintercept = 2007, lty = 2) +
  scale_color_viridis_d() +
  xlim(1960, 2020) +
  ylab("Population size") +
  theme(legend.position = 'none',
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(size = 20),
        axis.text=element_text(size=12, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=12,color = "black"),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    facet_wrap(~HERD, ncol = 3, nrow = 2, scale = "free")
dev.off()
