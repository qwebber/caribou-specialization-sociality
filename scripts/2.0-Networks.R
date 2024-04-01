

### Packages ---
libs <- c('data.table','igraph','asnipe','spatsoc')
lapply(libs, require, character.only = TRUE)


### Input data ----
locs <- readRDS("output/1-cleaned-locs.Rds")

locs[JDate > 170 + 14]

### RUN NETWORKS ### 

### Dynamic Interactions Networks ----
locs <- group_times(locs, datetime = "roundtime", threshold = "1 hour")

group_pts(locs, threshold = 50, splitBy = c("Year", "season"),
                 timegroup = 'timegroup',
                 id = "ANIMAL_ID", coords = c('EASTING', 'NORTHING'))

source("functions/DynamicNetworkFunction.R")

nets <- locs[, DynamicNetwork(.SD, 'IDSeasonYearHerd'),
             by = .(season, Year)]

nets[, c('ANIMAL_ID', 'SEASON' ,'YEAR' ,'HERD') := tstrsplit(ID, '_')][, c('SEASON','YEAR')  := NULL]

saveRDS(nets, 'output/2-networks.RDS')

