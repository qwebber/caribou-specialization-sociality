


### Packages ----
libs <- c('data.table', 
          'sp', 'adehabitatHR',
          'igraph', 'spatsoc')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS('output/1-cleaned-locs.Rds')

locs$SeasonYearHerd <- as.factor(paste(locs$season, locs$Year, locs$HERD, sep = "_"))

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

source("functions/GetHRBy.R")

params = c(grid = 700, extent = 3)

herd.mcps <- locs[, GetHRBy(SeasonYearHerd, EASTING, NORTHING, utm21N, 100, 'mcp')]


vert.dt <- as.data.table(herd.mcps)

saveRDS(vert.dt, "output/4-home-range-area.RDS")
