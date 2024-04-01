


### Randomizations ====

### Packages ----
libs <- c('data.table', 'ggplot2', 'gridExtra',
          'lme4', 'spatsoc', 'igraph', 'gmodels')
lapply(libs, require, character.only = TRUE)

### Input data ----
locs <- readRDS("output/1-cleaned-locs.Rds")

### Variables ----
utm21N <- '+proj=utm +zone=21 ellps=WGS84'

### Proximity Based Social Networks ----
# Need to allocate columns since reading from .Rds
if (truelength(locs) == 0) alloc.col(locs)

group_times(locs, datetime = 'datetime', threshold = '5 minutes')

group_pts(
  locs,
  threshold = 50,
  splitBy = c('Year', 'season'),
  timegroup = 'timegroup',
  id = 'ANIMAL_ID',
  coords = c('EASTING', 'NORTHING')
)

source("functions/dynamic_network.R")

### Randomizations ----
# Number of iterations
N <-  2

lsDynNets <- lapply(1:N, FUN = function(i) {
  locsRandom <-
    randomizations(
      locs,
      id = 'ANIMAL_ID',
      type = 'trajectory',
      splitBy = c('Year', 'season'),
      group = 'group',
      datetime = 'datetime',
      iterations = 1,
      coords = c('EASTING', 'NORTHING')
    )[!(observed)]
  
  group_times(locsRandom, datetime = 'randomdatetime', threshold = '5 minutes')
  
  group_pts(
    locsRandom,
    threshold = 50,
    splitBy = c('Year', 'season'),
    timegroup = 'timegroup',
    id = 'ANIMAL_ID',
    coords = c('EASTING', 'NORTHING')
  )
  
  print(i)
  
  return(dynamic_network(locsRandom, id = 'ANIMAL_ID',
                         c('Year', 'season'))[, iteration := i])
}
)

dynNets <- rbindlist(lsDynNets)

### Output ----
saveRDS(dynNets, 'output/6-rdmNets-100.RDS')



