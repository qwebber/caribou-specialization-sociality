


### Packages ----
libs <- c('data.table', 'adehabitatHR', 'sp',
          'rgdal', 'raster', 'RInSp')
lapply(libs, require, character.only = TRUE)

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

## rename habitat types
locs <- merge(locs, legend, by = 'Value')

locs <- locs[Cover != "Anthro"]

## locs by habitat types and herd
locs[, .N, by = c("HERD", "Cover")]

## locs by habitat types 
locs[, .N, by = c("Cover")]

## locs by herd
locs[, .N, by = c("HERD")]

## habitat types by ID
all.out <- locs[, .N, by = c("IDSeasonYearHerd", "Cover")]

## habitat with the most relocations per IDseasonYrHerd
all.out[, NHabitat := max(N), by = "IDSeasonYearHerd"]
all.out2 <- all.out[, hab := (N == NHabitat)][hab != "FALSE"]

## Convert DT to similary object
all.out3 <- dcast(all.out[,c("NHabitat", "hab") := NULL], IDSeasonYearHerd ~ Cover, value.var = "N")
all.out3[is.na(all.out3)] <- 0

psi <- import.RInSp(all.out3, row.names = 1,
                    info.cols = c(1))

# Warning, the number of replicates is set low to speed up he example!
# Note, for real analyses we recommend to set replicates => 999
PSi <- PSicalc(psi, exclude = FALSE, replicates = 10)

PSidf <- data.table(psi$info$IDSeasonYearHerd, PSi$PSi)
colnames(PSidf) <- c("IDSeasonYearHerd", "PSi")

PSidf2 <- merge(PSidf, all.out2[,c("NHabitat", "hab") := NULL], by = "IDSeasonYearHerd")

saveRDS(PSidf2, file = "output/3-PSi.RDS")
