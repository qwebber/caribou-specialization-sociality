

### Packages ----
libs <- c('data.table', 'adehabitatHR', 'sp', 'lubridate', 'dplyr', 'tidyr',
          'rgdal', 'raster', 'lme4', 'ggplot2', 'RInSp', 'glmmTMB')
lapply(libs, require, character.only = TRUE)


full.sample <- readRDS('output/full-sample')


## By ID RSFs
winter_mod <- glmmTMB(observed ~
                bufWetland +
                bufRocky +
                bufLichen +
                bufConiferScrub +
                elevScaled +
                (0 + bufWetland | IDSeasonYearHerd) +
                (0 + bufRocky | IDSeasonYearHerd) +
                (0 + bufLichen | IDSeasonYearHerd) +
                (0 + bufConiferScrub | IDSeasonYearHerd),
              family=poisson(),
              data = full.sample[season == "winter"],
              map = list(theta=factor(c(NA,1:3))),
              start = list(theta=c(log(1000), seq(0,0, length.out = 3))))


saveRDS(winter_mod, "output/RSF models/RSFglmmTMB-winter.RDS")


## By ID RSFs
calving_mod <- glmmTMB(observed ~
                        bufWetland +
                        bufRocky +
                        bufLichen +
                        bufConiferScrub +
                        elevScaled +
                        (0 + bufWetland | IDSeasonYearHerd) +
                        (0 + bufRocky | IDSeasonYearHerd) +
                        (0 + bufLichen | IDSeasonYearHerd) +
                        (0 + bufConiferScrub | IDSeasonYearHerd),
                      family=poisson(),
                      data = full.sample[season == "calving"],
                      map = list(theta=factor(c(NA,1:3))),
                      start = list(theta=c(log(1000), seq(0,0, length.out = 3))))


saveRDS(calving_mod, "output/RSF models/RSFglmmTMB-calving.RDS")

