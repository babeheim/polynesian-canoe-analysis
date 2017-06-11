
rm(list=ls())

# --------------------------------------
# LOOP OVER ALL SUBSETS OF CANOE TRAITS
# --------------------------------------
traittypes <- c("paddle", "hull", "decoration", "sailrigging", "doublecanoe", "outrigger", "all")

# for( j in 1:length(traittypes) ){

# if(j>1) rm(list <- unlist(res.str))


traitset <- traittypes[1] # can be "paddle", "hull", "decoration", "sailrigging", "doublecanoe", "outrigger", "all"

# ------------------------------------------------------------------
# READ IN DATA
# ------------------------------------------------------------------
trait_data <- read.csv( file <- "./inputs/traits.csv", stringsAsFactors=FALSE)
d <- read.csv( file <- "./inputs/islanddata.csv", stringsAsFactors=FALSE, na.strings="." )

d$sqkm <- d$sqkm * 1e6
# convert to square meters so everthing is >1 (for logging)

# ------------------------------------------------------------------
# CREATE ECOLOGICAL PREDICTORS
# ------------------------------------------------------------------
archipelago_list <- unique(d$archipelago) # list of unique island groups

# area covariates, all logged
# an educated guess since there are countless numbers of atolls with no data, and this is approximately their island size

area <- tapply(d$sqkm, d$archipelago, function(z) sum(z, na.rm=TRUE))
area <- area[archipelago_list]

log_total_area <- tapply(d$sqkm, d$archipelago, function(z) sum( log(z) , na.rm=TRUE))
log_total_area <- log_total_area[archipelago_list]

log_mean_area <- tapply(d$sqkm, d$archipelago, function(z) mean( log(z) , na.rm=TRUE))
log_mean_area <- log_mean_area[archipelago_list]

log_max_area <- tapply(d$sqkm, d$archipelago, function(z) max( log(z) , na.rm=TRUE))

tuamotors_island_sqkm <- c(170.0, 10.0, 7.0, 4.0, 4.0, 1.5, 1.5, 1.5, 
    1.5, 4.0, 8.0, 9.0, 600.0,  24.0, 3.0, rep( 10, 64 ) ) 

# keep in mind there's a big bug around here somewhere...
# island type covariates
reef_high <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z; sum( d[x,"high"]*d[x,"reef"]*d[x,"sqkm"], na.rm=TRUE )/area[z] } )
reef_low <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z; sum( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=TRUE )/area[z] } )
makatea <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z; sum( c(d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=TRUE )/area[z] } )

reeflow_mean_area <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z ; mean( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=TRUE ) } )
reeflow_max_area <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z ; max( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=TRUE ) } )

atoll <- sapply( archipelago_list, function(z) { x=d[,"archipelago"]==z; sum( d[x,"atoll"]*d[x,"sqkm"], na.rm=TRUE )/area[z] } )
noreef_high <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z; sum( d[x,"high"]*(1-d[x,"reef"])*d[x,"sqkm"], na.rm=TRUE )/area[z] } ) # no reef high island, covariate not used in models
noreef_low <- sapply( archipelago_list, function(z){ x=d[,"archipelago"]==z; sum( d[x,"makatea"]*(1-d[x,"reef"])*d[x,"sqkm"], na.rm=TRUE )/area[z] } ) # no reef high island, covariate not used in models

log_mean_area["Tuamotus"] <- mean( log( tuamotors_island_sqkm * 1000000 ) ) 
# correction converted to square meters
log_max_area["Tuamotus"] <- log(170) 
# correction
reeflow_mean_area["Tuamotus"] <- 5 # an educated guess since there are countless numbers of atolls with no data
reeflow_max_area["Tuamotus"] <- 170 # correction


# ------------------------------------------------------------------
# CREATE CULTURAL INHERITANCE PREDICTORS
# ------------------------------------------------------------------

# to use this gotta extract traits and matrix multiply thru?

# shit, the shape is wrong......! no wonder this leads to all sorts of weirdnessess
# this should be a LONG table

traits <- trait_data[,c("Hawaii", "Marq", "Tuam", "Soci", "Aust", "Cook", "Mani", "Newz", "Samo", "Tong", "Fiji")]
traits <- as.matrix(traits)

# along colonization sequence
inherit.islands <- t( sapply( archipelago_list, function(y){ x <- d[,"archipelago"]==y; d[x,21:31][1,] } ) )
inherit_mean1 <- sapply( archipelago_list, function(z)  rowMeans( t( as.numeric( inherit.islands[z,] ) * t(traits) ), na.rm=TRUE ) )
inherit_present <- sapply( archipelago_list, function(z)  ifelse( rowMeans( t( as.numeric( inherit.islands[z,] ) * t(traits) ), na.rm=TRUE )>0, 1, 0 ) )
# within spheres of interactions, according to Weisler
sphere.islands <- t( sapply( archipelago_list, function(y){ x <- d[,"archipelago"]==y; d[x,10:20][1,] } ) )
sphere_mean1 <- sapply( archipelago_list, function(z)  rowMeans( t( as.numeric( sphere.islands[z,] ) * t(traits) ), na.rm=TRUE ) )
sphere_present1 <- sapply( archipelago_list, function(z)  ifelse( rowMeans( t( as.numeric( sphere.islands[z,] ) * t(traits) ), na.rm=TRUE )> 0, 1, 0 ) )

# ------------------------------------------------------------------
# GENERATE INDICES OF SUBSETS OF TRAITS
# ------------------------------------------------------------------
all <- 1:nrow(trait_data)

hull <- which(trait_data$category=="hull")
decoration <- which(trait_data$category=="decoration")
sailrigging <- which(trait_data$category=="sailrigging")
doublecanoe <- which(trait_data$category=="doublecanoe")
outrigger <- which(trait_data$category=="outrigger")
paddle <- which(trait_data$category=="paddle")


# make this a predictor variable in the actual data

# narrow down the traits
set <- get( traitset )
Y <- as.matrix( traits[set,] )
sphere_mean <- as.matrix( sphere_mean1[set,] )
sphere_present <- as.matrix( sphere_present1[set,])
inherit_mean <- as.matrix( inherit_mean1[set,] )
inherit_present <- as.matrix( inherit_present[set,] )

I <- ncol( Y ) # number of islands
T <- nrow( Y ) # number of traits
a <- log_mean_area # average island area of each archipelago (log-transformed), vector that matches the columns of Y



library(rethinking)
n_iter <- 100 # length of chain
#----------------------------------------------------------
# ------------- MODELS AND ESTIMATION ---------------------
#----------------------------------------------------------
# -------- "Null" models ---------



# model mCoinFlip
dat_list <- list( T, I, Y )
mCoinFlip <- stan(file='./code/mCoinFlip.stan', data=dat_list, iter=n_iter)
save(mCoinFlip, file='./output/mCoinFlip.robj')

# model mBase checked
dat_list <- list( T, I, Y )
mBase <- stan(file='./code/mBase.stan', data=dat_list, iter=n_iter)
save(mBase, file='./output/mBase.robj')

#----------------------------------------------------------
# -------- Inheritance models -----------

# model mPastMean
inherit <- inherit_mean
dat_list <- list( T, I, Y, inherit )
mPastMean <- stan(file='./code/mPast.stan', data=dat_list, iter=n_iter)
save(mPastMean, file='./output/mPastMean.robj')

# model mPastPresent
inherit <- inherit_present
dat_list <- list( T, I, Y, inherit )
mPastPresent <- stan(file='./code/mPast.stan', data=dat_list, iter=n_iter)
save(mPastPresent, file='./output/mPastPresent.robj')

# model mPast2Mean 
inherit <- inherit_mean
dat_list <- list( T, I, Y, inherit )
mPast2Mean <- stan(file='./code/mPast2.stan', data=dat_list, iter=n_iter)
save(mPast2Mean, file='./output/mPast2Mean.robj')

# model mPast2Present 
inherit <- inherit_present
dat_list <- list( T, I, Y, inherit )
mPast2Present <- stan(file='./code/mPast2.stan', data=dat_list, iter=n_iter)
save(mPast2Present, file='./output/mPast2Present.robj')

# model mSphereMean 
sphere <- sphere_mean
dat_list <- list( T, I, Y, sphere )
mSphereMean <- stan(file='./code/mSphere.stan', data=dat_list, iter=n_iter)
save(mSphereMean, file='./output/mSphereMean.robj')

# model mSpherePresent 
sphere <- sphere_present
dat_list <- list( T, I, Y, sphere )
mSpherePresent <- stan(file='./code/mSphere.stan', data=dat_list, iter=n_iter)
save(mSpherePresent, file='./output/mSpherePresent.robj')

# model mPastPresentSphereMean 
sphere <- sphere_mean; inherit <- inherit_present
dat_list <- list( T, I, Y, sphere, inherit )
mPastPresentSphereMean <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=n_iter)
save(mPastPresentSphereMean, file='./output/mPastPresentSphereMean.robj')

# model mPastPresentSpherePresent
sphere <- sphere_present; inherit <- inherit_present 
dat_list <- list( T, I, Y, sphere, inherit )
mPastPresentSpherePresent <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=n_iter)
save(mPastPresentSpherePresent, file='./output/mPastPresentSpherePresent.robj')

# model mPastMeanSphereMean 
sphere <- sphere_mean; inherit <- inherit_mean
dat_list <- list( T, I, Y, sphere, inherit )
mPastMeanSphereMean <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=n_iter)
save(mPastMeanSphereMean, file='./output/mPastMeanSphereMean.robj')

# model mPastMeanSpherePresent
sphere <- sphere_present; inherit <- inherit_mean
dat_list <- list( T, I, Y, sphere, inherit )
mPastMeanSpherePresent <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=n_iter)
save(mPastMeanSpherePresent, file='./output/mPastMeanSpherePresent.robj')

# model mPast2PresentSphereMean 
sphere <- sphere_mean; inherit <- inherit_present
dat_list <- list( T, I, Y, sphere, inherit )
mPast2PresentSphereMean <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=n_iter)
save(mPast2PresentSphereMean, file='./output/mPast2PresentSphereMean.robj')

# model mPast2PresentSpherePresent
sphere <- sphere_present; inherit <- inherit_present 
dat_list <- list( T, I, Y, sphere, inherit )
mPast2PresentSpherePresent <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=n_iter)
save(mPast2PresentSpherePresent, file='./output/mPast2PresentSpherePresent.robj')

# model mPast2MeanSphereMean 
sphere <- sphere_mean; inherit <- inherit_mean
dat_list <- list( T, I, Y, sphere, inherit )
mPast2MeanSphereMean <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=n_iter)
save(mPast2MeanSphereMean, file='./output/mPast2MeanSphereMean.robj')

# model mPast2MeanSpherePresent
sphere <- sphere_present; inherit <- inherit_mean
dat_list <- list( T, I, Y, sphere, inherit )
mPast2MeanSpherePresent <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=n_iter)
save(mPast2MeanSpherePresent, file='./output/mPast2MeanSpherePresent.robj')


# #----------------------------------------------------------
# # ------------ Ecology models -----------

# model mAreaTotal
a <- log_total_area
dat_list <- list( T, I, Y, a )
mAreaTotal <- stan(file='./code/mArea.stan', data=dat_list, iter=n_iter)
save(mAreaTotal, file='./output/mAreaTotal.robj')

# model mAreaMean
a <- log_mean_area
dat_list <- list( T, I, Y, a )
mAreaMean <- stan(file='./code/mArea.stan', data=dat_list, iter=n_iter)
save(mAreaMean, file='./output/mAreaMean.robj')

# model mAreaMax
a <- log_max_area
dat_list <- list( T, I, Y, a )
mAreaMax <- stan(file='./code/mArea.stan', data=dat_list, iter=n_iter)
save(mAreaMax, file='./output/mAreaMax.robj')

# model mReefHigh
dat_list <- list( T, I, Y, rh=reef_high )
mReefHigh <- stan(file='./code/mReefHigh.stan', data=dat_list, iter=n_iter)
save(mReefHigh, file='./output/mReefHigh.robj')

# model mNoReefHigh
dat_list <- list( T, I, Y, nrh=noreef_high )
mNoReefHigh <- stan(file='./code/mNoReefHigh.stan', data=dat_list, iter=n_iter)
save(mNoReefHigh, file='./output/mNoReefHigh.robj')

# model mReefLow
dat_list <- list( T, I, Y, rl=reef_low )
mReefLow <- stan(file='./code/mReefLow.stan', data=dat_list, iter=n_iter)
save(mReefLow, file='./output/mReefLow.robj')

# model mNoReefLow
dat_list <- list( T, I, Y, nrl=noreef_low )
mNoReefLow <- stan(file='./code/mNoReefLow.stan', data=dat_list, iter=n_iter)
save(mNoReefLow, file='./output/mNoReefLow.robj')

# model mAreaTotalReefHigh
dat_list <- list( T, I, Y, a=log_total_area, rh=reef_high )
mAreaTotalReefHigh <- stan(file='./code/mAreaReefHigh.stan', data=dat_list, iter=n_iter)
save(mAreaTotalReefHigh, file='./output/mAreaTotalReefHigh.robj')

# model mAreaTotalNoReefHigh
dat_list <- list( T, I, Y, a=log_total_area, nrh=noreef_high )
mAreaTotalNoReefHigh <- stan(file='./code/mNoReefHigh.stan', data=dat_list, iter=n_iter)
save(mAreaTotalNoReefHigh, file='./output/mAreaTotalNoReefHigh.robj')

# model mAreaTotalReefLow
dat_list <- list( T, I, Y, a=log_total_area, rl=reef_low )
mAreaTotalReefLow <- stan(file='./code/mReefLow.stan', data=dat_list, iter=n_iter)
save(mAreaTotalReefLow, file='./output/mAreaTotalReefLow.robj')

# model mAreaTotalNoReefLow
dat_list <- list( T, I, Y, a=log_total_area, nrl=noreef_low )
mAreaTotalNoReefLow <- stan(file='./code/mNoReefLow.stan', data=dat_list, iter=n_iter)
save(mAreaTotalNoReefLow, file='./output/mAreaTotalNoReefLow.robj')

# model mAreaMeanReefHigh
dat_list <- list( T, I, Y, a=log_mean_area, rh=reef_high )
mAreaMeanReefHigh <- stan(file='./code/mAreaReefHigh.stan', data=dat_list, iter=n_iter)
save(mAreaMeanReefHigh, file='./output/mAreaMeanReefHigh.robj')

# model mAreaMeanNoReefHigh
dat_list <- list( T, I, Y, a=log_mean_area, nrh=noreef_high )
mAreaMeanNoReefHigh <- stan(file='./code/mNoReefHigh.stan', data=dat_list, iter=n_iter)
save(mAreaMeanNoReefHigh, file='./output/mAreaMeanNoReefHigh.robj')

# model mAreaMeanReefLow
dat_list <- list( T, I, Y, a=log_mean_area, rl=reef_low )
mAreaMeanReefLow <- stan(file='./code/mReefLow.stan', data=dat_list, iter=n_iter)
save(mAreaMeanReefLow, file='./output/mAreaMeanReefLow.robj')

# model mAreaMeanNoReefLow
dat_list <- list( T, I, Y, a=log_mean_area, nrl=noreef_low )
mAreaMeanNoReefLow <- stan(file='./code/mNoReefLow.stan', data=dat_list, iter=n_iter)
save(mAreaMeanNoReefLow, file='./output/mAreaMeanNoReefLow.robj')

# model mAreaTotalReefNoReefHighLow
dat_list <- list( T, I, Y, a=log_total_area, rh=reef_high, 
    rl=reef_low, nrh=noreef_high, nrl=noreef_low)
mAreaTotalReefNoReefHighLow <- stan(file='./code/mAreaReefNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mAreaTotalReefNoReefHighLow, file='./output/mAreaTotalReefNoReefHighLow.robj')

# model mAreaMeanReefNoReefHighLow
dat_list <- list( T, I, Y, a=log_mean_area, rh=reef_high, 
    rl=reef_low, nrh=noreef_high, nrl=noreef_low)
mAreaMeanReefNoReefHighLow <- stan(file='./code/mAreaReefNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mAreaMeanReefNoReefHighLow, file='./output/mAreaMeanReefNoReefHighLow.robj')


# #----------------------------------------------------------
# #----------- Inheritance and ecology models -----------


# model mPastMeanAreaMean
dat_list <- list( T, I, Y, inherit = inherit_mean, a = log_mean_area )
mPastMeanAreaMean <- stan(file='./code/mPastArea.stan', data=dat_list, iter=n_iter)
save(mPastMeanAreaMean, file='./output/mPastMeanAreaMean.robj')

# model mPastMeanAreaTotal
dat_list <- list( T, I, Y, inherit = inherit_mean, a = log_total_area )
mPastMeanAreaTotal <- stan(file='./code/mPastArea.stan', data=dat_list, iter=n_iter)
save(mPastMeanAreaTotal, file='./output/mPastMeanAreaTotal.robj')

# model mPastPresentAreaMean
dat_list <- list( T, I, Y, inherit = inherit_present, a = log_mean_area )
mPastPresentAreaMean <- stan(file='./code/mPastArea.stan', data=dat_list, iter=n_iter)
save(mPastPresentAreaMean, file='./output/mPastPresentAreaMean.robj')

# model mPastPresentAreaTotal
dat_list <- list( T, I, Y, inherit = inherit_present, a = log_total_area )
mPastPresentAreaTotal <- stan(file='./code/mPastArea.stan', data=dat_list, iter=n_iter)
save(mPastPresentAreaTotal, file='./output/mPastPresentAreaTotal.robj')

# model mPastMeanAreaMeanReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_mean, 
    a = log_mean_area, rh=reef_high, rl=reef_low)
mPastMeanAreaMeanReefHighLow <- stan(file='./code/mPastAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastMeanAreaMeanReefHighLow, file='./output/mPastMeanAreaMeanReefHighLow.robj')

# model mPastPresentAreaMeanReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    a = log_mean_area, rh=reef_high, rl=reef_low)
mPastPresentAreaMeanReefHighLow <- stan(file='./code/mPastAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastPresentAreaMeanReefHighLow, file='./output/mPastPresentAreaMeanReefHighLow.robj')

# model mPastMeanAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_mean, 
    a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mPastMeanAreaMeanNoReefHighLow <- stan(file='./code/mPastAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastMeanAreaMeanNoReefHighLow, file='./output/mPastMeanAreaMeanNoReefHighLow.robj')

# model mPastPresentAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mPastPresentAreaMeanNoReefHighLow <- stan(file='./code/mPastAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastPresentAreaMeanNoReefHighLow, file='./output/mPastPresentAreaMeanNoReefHighLow.robj')

# #--

# model mSphereMeanAreaMean
dat_list <- list( T, I, Y, sphere = sphere_mean, a = log_mean_area )
mSphereMeanAreaMean <- stan(file='./code/mSphereArea.stan', data=dat_list, iter=n_iter)
save(mSphereMeanAreaMean, file='./output/mSphereMeanAreaMean.robj')

# model mSphereMeanAreaTotal
dat_list <- list( T, I, Y, sphere = sphere_mean, a = log_total_area )
mSphereMeanAreaTotal <- stan(file='./code/mSphereArea.stan', data=dat_list, iter=n_iter)
save(mSphereMeanAreaTotal, file='./output/mSphereMeanAreaTotal.robj')

# model mSpherePresentAreaMean
dat_list <- list( T, I, Y, sphere = sphere_present, a = log_mean_area )
mSpherePresentAreaMean <- stan(file='./code/mSphereArea.stan', data=dat_list, iter=n_iter)
save(mSpherePresentAreaMean, file='./output/mSpherePresentAreaMean.robj')

# model mSpherePresentAreaTotal
dat_list <- list( T, I, Y, sphere = sphere_present, a = log_total_area )
mSpherePresentAreaTotal <- stan(file='./code/mSphereArea.stan', data=dat_list, iter=n_iter)
save(mSpherePresentAreaTotal, file='./output/mSpherePresentAreaTotal.robj')

# model mSphereMeanAreaMeanReefHighLow
dat_list <- list( T, I, Y, sphere = sphere_mean, 
    a = log_mean_area, rh=reef_high, rl=reef_low)
mSphereMeanAreaMeanReefHighLow <- stan(file='./code/mSphereAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mSphereMeanAreaMeanReefHighLow, file='./output/mSphereMeanAreaMeanReefHighLow.robj')

# model mSpherePresentAreaMeanReefHighLow
dat_list <- list( T, I, Y, sphere = sphere_present, 
    a = log_mean_area, rh=reef_high, rl=reef_low)
mSpherePresentAreaMeanReefHighLow <- stan(file='./code/mSphereAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mSpherePresentAreaMeanReefHighLow, file='./output/mSpherePresentAreaMeanReefHighLow.robj')

# model mSphereMeanAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, sphere = sphere_mean, 
    a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mSphereMeanAreaMeanNoReefHighLow <- stan(file='./code/mSphereAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mSphereMeanAreaMeanNoReefHighLow, file='./output/mSphereMeanAreaMeanNoReefHighLow.robj')

# model mSpherePresentAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, sphere = sphere_present, 
    a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mSpherePresentAreaMeanNoReefHighLow <- stan(file='./code/mSphereAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mSpherePresentAreaMeanNoReefHighLow, file='./output/mSpherePresentAreaMeanNoReefHighLow.robj')

# model mPastPresentSpherePresentAreaMeanReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    sphere = sphere_present, a = log_mean_area, rh=reef_high, rl=reef_low)
mPastPresentSpherePresentAreaMeanNoReefHighLow <- stan(file='./code/mPastSphereAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastPresentSpherePresentAreaMeanNoReefHighLow, file='./output/mPastPresentSpherePresentAreaMeanNoReefHighLow.robj')

# model mPastPresentSpherePresentAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    sphere = sphere_present, a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mPastPresentSpherePresentAreaMeanNoReefHighLow <- stan(file='./code/mPastSphereAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPastPresentSpherePresentAreaMeanNoReefHighLow, file='./output/mPastPresentSpherePresentAreaMeanNoReefHighLow.robj')

# model mPast2PresentSpherePresentAreaMeanReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    sphere = sphere_present, a = log_mean_area, rh=reef_high, rl=reef_low)
mPast2PresentSpherePresentAreaMeanNoReefHighLow <- stan(file='./code/mPast2SphereAreaReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPast2PresentSpherePresentAreaMeanNoReefHighLow, file='./output/mPast2PresentSpherePresentAreaMeanNoReefHighLow.robj')

# model mPast2PresentSpherePresentAreaMeanNoReefHighLow
dat_list <- list( T, I, Y, inherit = inherit_present, 
    sphere = sphere_present, a = log_mean_area, nrh=noreef_high, nrl=noreef_low)
mPast2PresentSpherePresentAreaMeanNoReefHighLow <- stan(file='./code/mPast2SphereAreaNoReefHighLow.stan', data=dat_list, iter=n_iter)
save(mPast2PresentSpherePresentAreaMeanNoReefHighLow, file='./output/mPast2PresentSpherePresentAreaMeanNoReefHighLow.robj')


# #----------------------------------------------------------
# # ------------- ORGANIZE RESULTS ---------------------
# #----------------------------------------------------------

# # ------ function to put model selection results together ------
# info.table <- function( res ){
# 	# find odds ratio
# 	or <- function(x, dig = 3) format( exp(x), digits = dig )
# 	# formatting
# 	fm <- function(z,x, dig=3) paste( or(z$summary[x,"mean"],dig)," (",or(z$summary[x,"2.5%"],dig),", ", or(z$summary[x,"97.5%"],dig),")", sep = "")
	
# 			delta <- function(ic)
# 			{
# 		noModels <- length(ic)
# 		i=1:noModels
# 		weight <- exp(-0.5*(ic[i]-min(ic)))/sum( exp(-0.5*(ic[i]-min(ic))) )
# 		names(weight) <- paste( "model", 1:noModels )
# 		weight
# 			}
	
# 	# table of results for single parameters, recover the estimates
# 	em.dic <- sapply( res, function(z) z$DIC )
# 	em.delta.dic <- delta( em.dic )
# 	em.beta <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="beta"), fm(z,"beta",dig=3), NA ) )
# 	em.kappa1 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa1"), fm(z,"kappa1"), NA ) )
# 	em.kappa2 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa2"), fm(z,"kappa2"), NA ) )
# 	em.kappa3 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa3"), fm(z,"kappa3"), NA ) )
# 	em.kappa4 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa4"), fm(z,"kappa4"), NA ) )
# 	em.lambda <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="lambda"), fm(z,"lambda"), NA ) )
# 	em.psi <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="psi"), fm(z,"psi"), NA ) )
# 	#em.alpha <- lapply( res, function(z) or( z$summary[1:T,1] ) )
# 	em.psi.tau <- lapply( res, function(z) if( any(row.names(z$summary)=="psi.tau[1]")){ z$summary[ (nrow(z$summary)-(T)):(nrow(z$summary)-1),1]}else{ NA } )
#   # put estimates in a table
# 	table <- data.frame( row.names <- names(res), em.delta.dic, em.dic, em.psi, em.lambda, em.beta, em.kappa1, em.kappa2, em.kappa3, em.kappa4 )
# 	names(table) <- c("dDIC", "DIC", "Past", "Sphere", "Area", "Reef.High", "Reef.Low", "No.Reef.High", "No.Reef.Low" )
# 	table <- table[ order(em.delta.dic, decreasing <- TRUE),]
# 	table
# }
# #----------------------------------------------------------	

# load models from directory
#res.na.dir <- lapply( dir(), function(z){ x <- strsplit(z,NULL); ifelse( x[[1]][1]=="m" & x[[1]][length(x[[1]])]=="a",z, NA ) })
#res.str.dir <- subset( res.na.dir, subset <- is.na(res.na.dir)==FALSE )
#for( i in 1:length(res.str.dir)) load(file <- res.str.dir[[i]][1])
#for( i in 1:length(res.str.dir)) print(res.str.dir[[i]][1])

# # list of results
# res.na <- lapply( ls(), function(z) ifelse( strsplit(z,NULL)[[1]][1]=="m",z, NA ) )
# res.str <- subset( res.na, subset <- is.na(res.na)==FALSE )
# res <- lapply( res.str, function(z) get(z) )
# names(res) <- res.str

# # call to organizing function
# table <- info.table( res )

# # save output to file
# write.table(table, file <- paste(getwd(),traitset,paste( saveas,".csv", sep="" ),sep <- "/"),row.names <- TRUE, col.names <- TRUE, sep=",")
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# }