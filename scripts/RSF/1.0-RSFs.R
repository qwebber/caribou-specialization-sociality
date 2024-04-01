


### Packages ----
libs <- c('data.table', 'adehabitatHR', 'sp', 'lubridate', 
          'rgdal', 'raster', 'lme4', 'ggplot2', 'RInSp')
lapply(libs, require, character.only = TRUE)


# Set local directory to manage temporary files
rasterOptions(tmpdir = 'input/tmpDir')

### Input data ----
locs <- readRDS("output/1-cleaned-locs.Rds")

# Landcover
lc <- raster("../nl-landcover/input/FINAL_PRODUCT/FINAL_RC.tif")
legend <- fread('input/landcover/Legend.csv')


## Variables
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
crs = CRS("+proj=utm +zone=14 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

## read in functions
source("functions/ExtractPoints.R")

## extract habitat type at end step
locs[, Value := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2),
                            raster = lc)] 


source("functions/GetHRBy.R")

utm21N <- '+proj=utm +zone=21 ellps=WGS84'

## check to make sure no IDs < 100 points
locs[, .N, by = "IDSeasonYearHerd"]
locs[, .N, by = c("Year","HERD")]

grey.vertices.95 <- locs[HERD == 'GREY', 
                           GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                 utm21N, 95, 'mcp')] 
                              #params = list(extent = 12, grid = 3000, h = 400))]

top.vertices.95 <- locs[HERD == 'TOPSAILS', GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                    utm21N, 95, 'mcp')]
                                    #params = list(extent = 12, grid = 3000, h = 400))]

buc.vertices.95 <- locs[HERD == 'BUCHANS', GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                   utm21N, 95, 'mcp')]
                                      #params = list(extent = 12, grid = 3000, h = 400))]

pot.vertices.95 <- locs[HERD == 'POTHILL', GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                   utm21N, 95, 'mcp')]
                                                   #params = vert.params)]

mid.vertices.95 <- locs[HERD == 'MIDRIDGE', GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                    utm21N, 95, 'mcp')]
                                                    #params = vert.params)]

lap.vertices.95 <- locs[HERD == 'LAPOILE', GetHRBy(IDSeasonYearHerd, EASTING, NORTHING,
                                                   utm21N, 95, 'mcp')]
                                    #params = list(extent = 12, grid = 2000, h = 400))]

#saveRDS(pot.vertices.95, 'output/vertices/vertices95-blk-id-yr-pot')
#saveRDS(mid.vertices.95, 'output/vertices/vertices95-blk-id-yr-mid')
#saveRDS(lap.vertices.95, 'output/vertices/vertices95-blk-id-yr-lap')
#saveRDS(buc.vertices.95, 'output/vertices/vertices95-blk-id-yr-buc')
#saveRDS(top.vertices.95, 'output/vertices/vertices95-blk-id-yr-top')
#saveRDS(grey.vertices.95, 'output/vertices/vertices95-blk-id-yr-grey')


#ot.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-pot')
#mid.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-mid')
#lap.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-lap')
#buc.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-buc')
#top.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-top')
#grey.vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr-grey')


#vertices.95 <- rbind(mid.vertices.95, buc.vertices.95,
#                     top.vertices.95, grey.vertices.95, 
#                     pot.vertices.95, lap.vertices.95)

# Output to RDS 
#saveRDS(vertices.95, 'output/vertices/vertices95-blk-id-yr')

# And read them back in
vertices.95 <- readRDS('output/vertices/vertices95-blk-id-yr')

# These should match!
locs[, uniqueN(IDSeasonYearHerd)]
length(vertices.95)

# Finally, generate herd MCPs -- generate whole herd level MCPs instead of for the subset of individuals

#herd.mcps <- locs[, GetHRBy(HERD, EASTING, NORTHING, utm21N, 100, 'mcp')]

#saveRDS(herd.mcps, 'output/vertices/herd-mcps')
herd.mcps <- readRDS('output/vertices/herd-mcps')

### Generate Random Points ----
# Drop columns leaving only needed
cols <- c('EASTING', 'NORTHING', 'IDSeasonYearHerd', 'ANIMAL_ID', 'season', 'HERD')

# and set all locs as observed
observed.locs <- locs[, ..cols][, observed := 1]
observed.locs$iter <- 0

# Create identical for random with observed == 0
random.locs <- locs[, ..cols][, observed := 0]

# Generate 10 times the number of random points in vertices as observed
# (updating the observed locs EASTING, NORTHING columns)


out = c()
for(i in 1:50){ 
  
  aa <- random.locs[, c('EASTING', 'NORTHING') := as.data.table(spsample(
    vertices.95[vertices.95@data$id == .BY[[1]],],.N, iter = 100, type = "random")@coords),
                               by = IDSeasonYearHerd]
  aa$iter <- i 
  out[[i]] <- aa
  
  }

random.locs.out <- rbindlist(out)
  

# Combine the observed and random locs and assign a rowID
sample.locs <- rbindlist(list(random.locs.out, observed.locs))

# Add row ID
sample.locs[, rowID := .I]

saveRDS(sample.locs, 'output/sample-locs')
sample.locs <- readRDS('output/sample-locs')

### Extract land cover, DEM and focal rasters ----
# Extract raster values at points

source("functions/ExtractPoints.R")
source("functions/ExtractDiscreteCount.R")

### This next chunk is to calculate the proportion of each habitat type in a given radius
# TODO: why
# Set NAs in lc to class 10
lc[is.na(lc)] <- 10
focals <- lapply(legend$Value, function(val) {
  subs(lc, legend[, .(Value, Value == val)])
})

names(focals) <- legend$Value

# combine habitat types into groupings of your choice 
# using the Value numbers from the legend
openMove <- Reduce("+", focals[c(1, 6, 9)])
forest <- Reduce("+", focals[c(2, 3, 4, 5)])
lichen <- focals[[8]]

### This step makes new raster layers that are "proportion of habitat within a 100 m 
# buffer that is habitat x". Tends to make analyses more robust and less susceptible
# to problems with autocorrelation.-MPL

# Generate buffer size
# you can change this number, but right now it is a 100m circle around each point
buff <- 200

focweight <- focalWeight(lcFogo, d = buff, type = 'circle')

openMoveBuff200 <- focal(openMove, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)
ForestBuff200 <- focal(forest, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)
LichenBuff200 <- focal(lichen, focweight, na.rm = TRUE, pad = TRUE, padValue = 0)

# Proportion of habitat at each relocation
DT[, propOpenMove := extract(openMoveBuff200, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propForest := extract(ForestBuff200, matrix(c(EASTING, NORTHING), ncol = 2))]
DT[, propLichen := extract(LichenBuff200, matrix(c(EASTING, NORTHING), ncol = 2))]









## Sample focal rasters
# Set filepath for focal sum rasters
focal.sum.loc <- 'input/landcover/focal-rasters/'

# List rasters in that directory
ls.focal <- list.files(focal.sum.loc, '.tif', full.names = T)
ls.focal.names <- tstrsplit(list.files(focal.sum.loc, '.tif'), '.tif')[[1]]

# Add names of focal raster files as columns, filling with pt sampling
sample.locs[, (ls.focal.names) := lapply(ls.focal, FUN = function(r){
  ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2),
                raster(r))})]

## Landcover sampling
# Sample landcover at each point
sample.locs[, ptLandcover := ExtractPoints(matrix(c(EASTING, NORTHING), ncol = 2), landcover)]

# Loop over each value in the legend and if the above calculated landcover matches, assign a 1 to that column, otherwise a 0
# Essentially parsing out the landcover column
pt.legend.cover <- paste0('pt', legend$Cover)
sample.locs[, (pt.legend.cover) := lapply(legend$Value, FUN = function(val) ifelse(ptLandcover == val, 1, 0))]

# Sample landcover in a buffer
# 90 times faster if you use .grd (instead of .tif), still 23 minutes (for 350K points)
# ** Warning - if you use a .tif, this will take forever.
# In addition, if you use a huge raster (unnecessary coverage) this can take ~5 times longer
sample.landcover <- sample.locs[, ExtractDiscreteCount(matrix(c(EASTING, NORTHING), ncol = 2), landcover, 100)]

# There are some NAs sampled
# TODO: check if less NAs when full raster used. 
sum(sample.landcover[, `NA`])

# Here we're going to match up the legend$cover with the columns found by extract
# This needs a bit of human checking, to make sure the cover numbers and cover names provided match
buf.legend.cover <- paste0('buf', legend$Cover)
select.num.cols <- as.character(1:9)
sample.landcover[, (buf.legend.cover) := .SD, .SDcols = select.num.cols][, (select.num.cols) := NULL]

# And finally, we convert to proportions 
# If you wanted to keep the counts and proportions - use paste(prop, sub.legend.cover, sep = '') instead of sub.legend.cover
sample.landcover[, (buf.legend.cover) := lapply(.SD, FUN = function(col) col / total), 
                 .SDcols = buf.legend.cover] 

saveRDS(sample.landcover, 'output/sample-landcover')
# sample.landcover <- readRDS('output/sample-landcover')

## Output full observed sampling
full.sample <- merge(sample.locs, sample.landcover, by.x = 'rowID', by.y = 'id') 

nrow(full.sample)
nrow(sample.locs)

saveRDS(full.sample, 'output/full-sample')

###################################################
################## CALCULATE PSi ################## 
###################################################

all.out <- fread('output/all-beta-rsfs.csv')

#all.out2 <- data.table(all.out[,3:6])
#colnames(all.out2) <- c("LichenAll", "RockyAll", "WetlandAll", "ConiferScrubAll")
all.out[,3:6][all.out[,3:6] < -5] <- NA
all.out[,3:6][all.out[,3:6] > 5] <- NA

all.out$LichenRatio <- exp(all.out[,3]) ## Lichen
all.out$RockyRatio <- exp(all.out[,4]) ## Rocky
all.out$WetlandRatio <- exp(all.out[,5]) ## Wetland
all.out$ConiferScrubRatio <- exp(all.out[,6]) ## Conifere

all.out <- na.omit(all.out)

# Select a single spatial sampling site (site A)
rsfCoefs <- import.RInSp(all.out, row.names = 1,
                                  info.cols = c(1:7))

# Warning, the number of replicates is set low to speed up he example!
# Note, for real analyses we recommend to set replicates => 999
PSi <- PSicalc(rsfCoefs, exclude = FALSE, replicates = 100)

sumMC.RInSp(PSi)

PSidf <- data.table(rsfCoefs$info$IDSeasonYearHerd, PSi$PSi)
colnames(PSidf) <- c("IDSeasonYearHerd", "PSi")

hist(PSidf$PSi)

PSidf2 <- merge(PSidf, all.out, by = "IDSeasonYearHerd")

fwrite(PSidf2, file = "output/RSF.csv")
