



### Herd RSFs ----
# Read in focal sum on binary land class rasters (only those needed)
focalWetland <- raster('input/landcover/focal-rasters/focalWetland.tif')
focalBroadleaf <- raster('input/landcover/focal-rasters/focalBroadleaf.tif')
focalConiferScrub <- raster('input/landcover/focal-rasters/focalConiferScrub.tif')
focalMixedWood <- raster('input/landcover/focal-rasters/focalMixedWood.tif')
focalRocky <- raster('input/landcover/focal-rasters/focalRocky.tif')
focalLichen <- raster('input/landcover/focal-rasters/focalLichen.tif')

raster.ls = list(Wetland = focalWetland, Broadleaf = focalBroadleaf, ConiferScrub = focalConiferScrub,
                 MixedWood = focalMixedWood, Rocky = focalRocky, Lichen = focalLichen)

## By ID RSFs
mid.dt <- full.sample[HERD == 'MIDRIDGE'] 

## Model selection ##
## full model
mod1 <- glm(observed ~ ptWetland + ptRocky + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without Wetland
mod2 <- glm(observed ~  ptRocky + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without rocky  
mod3 <- glm(observed ~ ptWetland + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without Lichen  
mod4 <- glm(observed ~ ptWetland + ptRocky  + elevScaled +
              ptConiferScrub + ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without elevation  
mod5 <- glm(observed ~ ptWetland + ptRocky + ptLichen  +
              ptConiferScrub + ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without conifer scrub
mod6 <- glm(observed ~ ptWetland + ptRocky + ptLichen + elevScaled +
              ptConiferForest + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without conifer forest  
mod7 <- glm(observed ~ ptWetland + ptRocky + ptLichen + elevScaled +
              ptConiferScrub + ptMixedWood, family = 'binomial',
            data = mid.dt)
## without mixed wood
mod8 <- glm(observed ~ ptWetland + ptRocky + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest, family = 'binomial',
            data = mid.dt)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8) - AIC(mod8)

## remove Mixed wood ##
## without Wetland
mod9 <- glm(observed ~ ptRocky + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest, family = 'binomial',
            data = mid.dt)
## without rocky
mod10 <- glm(observed ~ ptWetland + ptLichen + elevScaled +
              ptConiferScrub + ptConiferForest, family = 'binomial',
            data = mid.dt)
## without lichen
mod11 <- glm(observed ~ ptWetland + ptRocky + elevScaled +
              ptConiferScrub + ptConiferForest, family = 'binomial',
            data = mid.dt)
## without elevation
mod12 <- glm(observed ~ ptWetland + ptRocky + ptLichen +
              ptConiferScrub + ptConiferForest, family = 'binomial',
            data = mid.dt)
## without conifer scrub
mod13 <- glm(observed ~ ptWetland + ptRocky + ptLichen + elevScaled +
               ptConiferForest, family = 'binomial',
            data = mid.dt)
## without conifer forest
mod14 <- glm(observed ~ bufWetland + bufRocky + bufLichen + elevScaled +
              bufConiferScrub, family = 'binomial',
            data = full.sample)

summary(mod14)

AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8,
    mod9, mod10, mod11, mod12, mod13, mod14) - AIC(mod1)

