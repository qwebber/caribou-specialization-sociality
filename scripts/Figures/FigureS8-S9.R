

libs <- c('data.table', 'ggplot2', 'gridExtra')
lapply(libs, require, character.only = TRUE)

######################################################
################### FIGURE S6 and S7 ###################
######################################################


df_fit_cent <- fread("output/figure 1 data/df_fit_cent.csv")
df_fit_PSi <- fread("output/figure 1 data/df_fit_PSi.csv")



df_fit_cent$HERD[df_fit_cent$HERD == "MIDRIDGE"] <- "Middle Ridge"
df_fit_cent$HERD[df_fit_cent$HERD == "GREY"] <- "Grey River"
df_fit_cent$HERD[df_fit_cent$HERD == "LAPOILE"] <- "La Poile"
df_fit_cent$HERD[df_fit_cent$HERD == "BUCHANS"] <- "Buchans"
df_fit_cent$HERD[df_fit_cent$HERD == "POTHILL"] <- "Pot Hill"
df_fit_cent$HERD[df_fit_cent$HERD == "TOPSAILS"] <- "Topsails"


png("graphics/FigS8.png", width = 4500, height = 3000, res=600, units="px")
ggplot(setDT(df_fit_cent),
       aes(x = scalePopDen,
           y = Value,
           group = factor(ANIMAL_ID))) +
  geom_smooth(aes(scalePopDen,Value, group = ANIMAL_ID, color = HERD),
              size = 0.5,  method=lm, se=FALSE) +
  xlab("Mean-centred population density") +
  ylab("Social strength") +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=14),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~HERD)
dev.off()


df_fit_PSi$HERD[df_fit_PSi$HERD == "MIDRIDGE"] <- "Middle Ridge"
df_fit_PSi$HERD[df_fit_PSi$HERD == "GREY"] <- "Grey River"
df_fit_PSi$HERD[df_fit_PSi$HERD == "LAPOILE"] <- "La Poile"
df_fit_PSi$HERD[df_fit_PSi$HERD == "BUCHANS"] <- "Buchans"
df_fit_PSi$HERD[df_fit_PSi$HERD == "POTHILL"] <- "Pot Hill"
df_fit_PSi$HERD[df_fit_PSi$HERD == "TOPSAILS"] <- "Topsails"

png("graphics/FigS9.png", width = 4500, height = 3000, res=600, units="px")
ggplot(setDT(df_fit_PSi),
       aes(x = scalePopDen,
           y = Value,
           group = factor(ANIMAL_ID))) +
  geom_smooth(aes(scalePopDen,Value, group = ANIMAL_ID, color = HERD),
              size = 0.5,  method=lm, se=FALSE) +
  xlab("Mean-centred population density") +
  ylab(expression(Specialist %<->% Generalist)) +
  scale_color_viridis_d() +
  theme(legend.position = 'none',
        plot.title = element_text(size = 20),
        axis.text=element_text(size=14, color = "black"),
        axis.title=element_text(size=20),
        strip.text = element_text(size=14),
        strip.background = element_rect(colour="black", size = 1, fill = "white"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +
  facet_wrap(~HERD)
dev.off()
