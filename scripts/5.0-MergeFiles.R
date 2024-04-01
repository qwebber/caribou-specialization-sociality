
### Packages ----
libs <- c('data.table')
lapply(libs, require, character.only = TRUE)

### Load data -- individually-based network data
nets <- readRDS("output/2-networks.RDS")
nets$HERDYr <- as.factor(paste(nets$Year, nets$HERD, sep = "_"))
colnames(nets)[6] <- "netSize"


## Load data -- density data for each herd
density <- fread("input/densityCaribouHerds.csv")
density$HERDYr <- as.factor(paste(density$Year, density$HERD,  sep = "_"))
density <- density[, c("Number", "HERDYr")]

nets <- merge(nets, density, by = "HERDYr")
nets$scaleDens <- scale(nets$Number)
nets$id <- as.factor(paste(nets$season, nets$HERDYr, sep = "_"))

## home range area data for each herd
HRA <- readRDS("output/4-home-range-area.RDS")

## calculate density (animals/km2)
nets <- merge(nets, HRA, by = "id")
nets$pDen <- nets$Number/(nets$area/100)

nets$IDSeasonYearHerd <- nets$ID
nets[, c("id", "ID") := NULL] ## remove these rows since they are duplicate in next 

## RSF scores
PSi <- readRDS("output/3-PSi.RDS")

nets <- merge(nets, PSi, by = "IDSeasonYearHerd")
nets$IDyear <- paste(nets$HERD, nets$ANIMAL_ID, nets$Year,sep = "_")

## fitness data 
fitness <- fread("input/AllHerdsSurvival2hrMR.csv")

fitness[ , JDate_calved := yday(CalvingDate)]
fitness[ , JDate_loss := yday(LossDate)]

nets <- merge(nets,fitness, by = "IDyear", all = T)

nets = nets[,c("V1", "IDyear") := NULL]
nets$Calved[nets$Calved == "TRUE"] <- 1
nets$Calved[nets$Calved == "FALSE"] <- 0
nets$Survival[nets$Lost == "TRUE"] <- 0
nets$Survival[nets$Lost == "FALSE"] <- 1

nets[, .N, by = "Survival"]

nets[,c("CalvingDate", "LossDate", "HERDYr") := NULL]

## cut outlier years/individuals
nets <- nets[Year != "2006"] #
nets <- nets[Year != "2012" | HERD != "POTHILL"]

## scale population density by herd 
nets[, scalePopDen := as.numeric(scale(pDen)), by = .(HERD)]

## group herds into density categories
nets[scalePopDen < quantile(nets$scalePopDen)[2], densityCat := "Low"]
nets[scalePopDen > quantile(nets$scalePopDen)[2] & scalePopDen < quantile(nets$scalePopDen)[4], densityCat := "Med"]
nets[scalePopDen > quantile(nets$scalePopDen)[4], densityCat := "High"]
nets$densityCat <- as.factor(nets$densityCat)

## remove NAs from proportional similarity index
nets <- nets[!is.na(nets$densityCat),]
nets <- nets[!is.na(nets$PSi),]
nets <- nets[!is.na(nets$Survival),]

saveRDS(nets, 'output/5-combined-data.RDS')


