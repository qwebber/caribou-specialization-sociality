### Data cleaning ====

### Packages ----
libs <- c("data.table", "lubridate",
          "rgdal", "adehabitatLT")

lapply(libs, require, character.only = TRUE)


### Input data: 

## load NL data
clean <- fread('input/AllCaribouDataRaw.csv',
               drop = c("SPECIES", "EPSG_CODE", "Map_Quality",
                        "COLLAR_FILE_ID", "EXCLUDE", "VENDOR_CL", "DOP",
                        "NAV", "VALIDATED", "LOCQUAL", "COLLAR_ID",
                        "AGE", "Fix_Time_Delta", "V1", "FIX_ID"))

#clean <- rbind(fogo, clean, fill = T)

### Preprocessing ----
clean[, datetime := ymd_hms(paste(FIX_DATE, FIX_TIME))]
clean[, roundtime := round_date(datetime, unit = "hours")]

clean[, Year := year(datetime)]
clean[, JDate := yday(datetime)]

clean[JDate >= 335 | JDate <= 41, season := "winter"]
clean[JDate >= 141 & JDate <= 212, season := "calving"]

clean <- clean[!(is.na(season))]

## Loc fields
utm21N <- '+proj=utm +zone=21 ellps=WGS84'
clean[, c('EASTING', 'NORTHING') := as.data.table(project(cbind(X_COORD, Y_COORD), utm21N))]

# Add unique season, year, herd codes:
clean$IDSeasonYearHerd = paste(clean$ANIMAL_ID,clean$season, clean$Year,clean$HERD,sep = "_")

### Subset ----
# Only female GPS after 2002
clean <- clean[SEX == "F" & COLLAR_TYPE_CL == "GPS" & Year >= "2002" &
                 (HERD == "MIDRIDGE" | HERD == "TOPSAILS" | HERD == "BUCHANS" |
                  HERD == "LAPOILE" | HERD == "POTHILL" | HERD == "GREY")]

# Sub by bounding box
clean <- clean[NORTHING > 5250000 & NORTHING < 6000000 &
                 EASTING > 0 & EASTING < 800000]

## Check number of fixes that occur  +/- 5 minutes from  the hour (0.3%)
clean <- clean[!(minute(datetime) > 5 & minute(datetime) < 55)]

### data.table Step Length ----
# Set columns
time.col <- 'datetime'
coord.cols <- c('EASTING', 'NORTHING')

# Create lag and dif column names
lag.cols <- paste('lag', coord.cols, sep = '')
difference.cols <- c('difX', 'difY')

lag.time.col <- paste0('lag', time.col)
dif.time.col <- paste0('dif', time.col)

# Use shift  to create lagged cols
clean[order(get(time.col)), (lag.cols) := data.table::shift(.SD, 1, NA, 'lag'),
      by = .(ANIMAL_ID, Year),
      .SDcols = coord.cols]

# Find the difference squared between all points in each x,y separately
clean[, (difference.cols) := .((get(coord.cols[1]) - get(lag.cols[1])) ^2,
                               (get(coord.cols[2]) - get(lag.cols[2])) ^2)]

# Square root the summed difference for a simple step length
clean[, simpleStep := sqrt(rowSums(.SD)),
      .SDcols = difference.cols]

## Delta Time
clean[order(get(time.col)), (lag.time.col) := data.table::shift(.SD, 1, NA, 'lag'), 
      by = .(ANIMAL_ID, Year),
      .SDcols = time.col]

# difference in time in hours
clean[, (dif.time.col) := as.numeric(get(time.col) - get(lag.time.col), units = 'hours')]

# Simple step length divided by time difference
clean[, moveRate := simpleStep / (get(dif.time.col))]

# Drop more than 30km/hr movements
clean <- clean[moveRate < 30000]

## Drop ID, season, Herd, Years with <100 points 
clean[, N := .N, by = .(HERD, season, Year, ANIMAL_ID)][order(N)]

clean <- clean[N >= 70]


### Output ----
saveRDS(clean[, .(ANIMAL_ID, HERD, datetime, roundtime, Year, JDate, season,
                  EASTING, NORTHING, IDSeasonYearHerd, simpleStep, moveRate)],
        'output/1-cleaned-locs.Rds')
