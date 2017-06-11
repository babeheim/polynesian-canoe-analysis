
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
inheritmean1 <- sapply( archipelago_list, function(z)  rowMeans( t( as.numeric( inherit.islands[z,] ) * t(traits) ), na.rm=TRUE ) )
inheritpresent <- sapply( archipelago_list, function(z)  ifelse( rowMeans( t( as.numeric( inherit.islands[z,] ) * t(traits) ), na.rm=TRUE )>0, 1, 0 ) )
# within spheres of interactions, according to Weisler
sphere.islands <- t( sapply( archipelago_list, function(y){ x <- d[,"archipelago"]==y; d[x,10:20][1,] } ) )
spheremean1 <- sapply( archipelago_list, function(z)  rowMeans( t( as.numeric( sphere.islands[z,] ) * t(traits) ), na.rm=TRUE ) )
spherepresent1 <- sapply( archipelago_list, function(z)  ifelse( rowMeans( t( as.numeric( sphere.islands[z,] ) * t(traits) ), na.rm=TRUE )> 0, 1, 0 ) )

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
spheremean <- as.matrix( spheremean1[set,] )
spherepresent <- as.matrix( spherepresent1[set,])
inheritmean <- as.matrix( inheritmean1[set,] )
inheritpresent <- as.matrix( inheritpresent[set,] )

I <- ncol( Y ) # number of islands
T <- nrow( Y ) # number of traits
a <- log_mean_area # average island area of each archipelago (log-transformed), vector that matches the columns of Y



library(rstan)
iter <- 1000 # length of chain
#----------------------------------------------------------
# ------------- MODELS AND ESTIMATION ---------------------
#----------------------------------------------------------
# -------- "Null" models ---------



# model mCoinFlip
dat_list <- list( T, I, Y )
mCoinFlip <- stan(file='./code/mCoinFlip.stan', data=dat_list, iter=500)
save(mCoinFlip, file='./output/mCoinFlip.robj')

# model mBase checked
dat_list <- list( T, I, Y )
mBase <- stan(file='./code/mBase.stan', data=dat_list, iter=500)
save(mBase, file='./output/mBase.robj')

#----------------------------------------------------------
# -------- Inheritance models -----------

# model mPastMean
inherit <- inheritmean
dat_list <- list( T, I, Y, inherit )
mPastMean <- stan(file='./code/mPast.stan', data=dat_list, iter=500)
save(mPastMean, file='./output/mPastMean.robj')

# model mPastPresent
inherit <- inheritpresent
dat_list <- list( T, I, Y, inherit )
mPastPresent <- stan(file='./code/mPast.stan', data=dat_list, iter=500)
save(mPastPresent, file='./output/mPastPresent.robj')

# model mPast2Mean 
inherit <- inheritmean
dat_list <- list( T, I, Y, inherit )
mPast2Mean <- stan(file='./code/mPast2.stan', data=dat_list, iter=500)
save(mPast2Mean, file='./output/mPast2Mean.robj')

# model mPast2Present 
inherit <- inheritpresent
dat_list <- list( T, I, Y, inherit )
mPast2Present <- stan(file='./code/mPast2.stan', data=dat_list, iter=500)
save(mPast2Present, file='./output/mPast2Present.robj')

# model mSphereMean 
sphere <- spheremean
dat_list <- list( T, I, Y, sphere )
mSphereMean <- stan(file='./code/mSphere.stan', data=dat_list, iter=500)
save(mSphereMean, file='./output/mSphereMean.robj')

# model mSpherePresent 
sphere <- spherepresent
dat_list <- list( T, I, Y, sphere )
mSpherePresent <- stan(file='./code/mSphere.stan', data=dat_list, iter=500)
save(mSpherePresent, file='./output/mSpherePresent.robj')

# model mPastPresentSphereMean 
sphere <- spheremean; inherit <- inheritpresent
dat_list <- list( T, I, Y, sphere, inherit )
mPastPresentSphereMean <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=500)
save(mPastPresentSphereMean, file='./output/mPastPresentSphereMean.robj')

# model mPastPresentSpherePresent
sphere <- spherepresent; inherit <- inheritpresent 
dat_list <- list( T, I, Y, sphere, inherit )
mPastPresentSpherePresent <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=500)
save(mPastPresentSpherePresent, file='./output/mPastPresentSpherePresent.robj')

# model mPastMeanSphereMean 
sphere <- spheremean; inherit <- inheritmean
dat_list <- list( T, I, Y, sphere, inherit )
mPastMeanSphereMean <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=500)
save(mPastMeanSphereMean, file='./output/mPastMeanSphereMean.robj')

# model mPastMeanSpherePresent
sphere <- spherepresent; inherit <- inheritmean
dat_list <- list( T, I, Y, sphere, inherit )
mPastMeanSpherePresent <- stan(file='./code/mPastSphere.stan', data=dat_list, iter=500)
save(mPastMeanSpherePresent, file='./output/mPastMeanSpherePresent.robj')

# model mPast2PresentSphereMean 
sphere <- spheremean; inherit <- inheritpresent
dat_list <- list( T, I, Y, sphere, inherit )
mPast2PresentSphereMean <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=500)
save(mPast2PresentSphereMean, file='./output/mPast2PresentSphereMean.robj')

# model mPast2PresentSpherePresent
sphere <- spherepresent; inherit <- inheritpresent 
dat_list <- list( T, I, Y, sphere, inherit )
mPast2PresentSpherePresent <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=500)
save(mPast2PresentSpherePresent, file='./output/mPast2PresentSpherePresent.robj')

# model mPast2MeanSphereMean 
sphere <- spheremean; inherit <- inheritmean
dat_list <- list( T, I, Y, sphere, inherit )
mPast2MeanSphereMean <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=500)
save(mPast2MeanSphereMean, file='./output/mPast2MeanSphereMean.robj')

# model mPast2MeanSpherePresent
sphere <- spherepresent; inherit <- inheritmean
dat_list <- list( T, I, Y, sphere, inherit )
mPast2MeanSpherePresent <- stan(file='./code/mPast2Sphere.stan', data=dat_list, iter=500)
save(mPast2MeanSpherePresent, file='./output/mPast2MeanSpherePresent.robj')


# #----------------------------------------------------------
# # ------------ Ecology models -----------

# model mAreaTotal
a <- log_total_area
dat_list <- list( T, I, Y, a )
mAreaTotal <- stan(file='./code/mArea.stan', data=dat_list, iter=500)
save(mAreaTotal, file='./output/mAreaTotal.robj')

# # model mAreaMean
a <- log_mean_area
dat_list <- list( T, I, Y, a )
mAreaMean <- stan(file='./code/mArea.stan', data=dat_list, iter=500)
save(mAreaMean, file='./output/mAreaMean.robj')

# # model mAreaMax
a <- log_max_area
dat_list <- list( T, I, Y, a )
mAreaMax <- stan(file='./code/mArea.stan', data=dat_list, iter=500)
save(mAreaMax, file='./output/mAreaMax.robj')


# # model mReefHigh
# data <- list( "T","I","Y","reef_high" )
# parameters <- c("alpha", "kappa1"  )
# mReefHigh <- bugs(data, inits <- NULL, parameters, "mReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mReefLow
# data <- list( "T","I","Y","reef_low" )
# parameters <- c("alpha", "kappa2"  )
# mReefHigh <- bugs(data, inits <- NULL, parameters, "mReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mNoReefHigh
# data <- list( "T","I","Y","noreef_high" )
# parameters <- c("alpha", "kappa3"  )
# mNoReefHigh <- bugs(data, inits <- NULL, parameters, "mNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mNoReefHigh
# data <- list( "T","I","Y","noreef_low" )
# parameters <- c("alpha", "kappa4"  )
# mNoReefLow <- bugs(data, inits <- NULL, parameters, "mNoReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaTotalReefHigh 
# a <- log_total_area
# data <- list( "T","I","Y","reef_high", "a" )
# parameters <- c("alpha", "beta", "kappa1" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa1=rnorm(1,0,1) ) }
# mAreaTotalReefHigh <- bugs(data, inits <- inits, parameters, "mAreaReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaTotalReefLow
# a <- log_total_area
# data <- list( "T","I","Y","reef_low", "a" )
# parameters <- c("alpha", "beta", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa2=rnorm(1,0,1) ) }
# mAreaTotalReefLow <- bugs(data, inits <- inits, parameters, "mAreaReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaTotalNoReefHigh
# a <- log_total_area
# data <- list( "T","I","Y","noreef_high", "a" )
# parameters <- c("alpha", "beta", "kappa3" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa3=rnorm(1,0,1) ) }
# mAreaTotalNoReefHigh <- bugs(data, inits <- inits, parameters, "mAreaNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaTotalNoReefLow 
# a <- log_total_area
# data <- list( "T","I","Y","noreef_low", "a" )
# parameters <- c("alpha", "beta", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa4=rnorm(1,0,1) ) }
# mAreaTotalNoReefLow <- bugs(data, inits <- inits, parameters, "mAreaNoReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaMeanReefHigh 
# a <- log_mean_area
# data <- list( "T","I","Y","reef_high", "a" )
# parameters <- c("alpha", "beta", "kappa1" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa1=rnorm(1,0,1) ) }
# mAreaMeanReefHigh <- bugs(data, inits <- inits, parameters, "mAreaReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaMeanReefLow
# a <- log_mean_area
# data <- list( "T","I","Y","reef_low", "a" )
# parameters <- c("alpha", "beta", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa2=rnorm(1,0,1) ) }
# mAreaMeanReefLow <- bugs(data, inits <- inits, parameters, "mAreaReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaMeanNoReefHigh
# a <- log_mean_area
# data <- list( "T","I","Y","noreef_high", "a" )
# parameters <- c("alpha", "beta", "kappa3" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa3=rnorm(1,0,1) ) }
# mAreaMeanNoReefHigh <- bugs(data, inits <- inits, parameters, "mAreaNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaMeanNoReefLow
# a <- log_mean_area
# data <- list( "T","I","Y","noreef_low", "a" )
# parameters <- c("alpha", "beta", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa4=rnorm(1,0,1) ) }
# mAreaMeanNoReefLow <- bugs(data, inits <- inits, parameters, "mAreaNoReefLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaTotalReefNoReefHighLow
# a <- log_total_area
# data <- list( "T","I","Y","reef_high", "reef_low", "noreef_high", "noreef_low", "a" )
# parameters <- c("alpha", "beta", "kappa1", "kappa2", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), kappa3=rnorm(1,0,1) ), kappa4=rnorm(1,0,1) }
# mAreaTotalReefNoReefHighLow <- bugs(data, inits <- inits, parameters, "mAreaReefNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mAreaMeanReefNoReefHighLow
# a <- log_mean_area
# data <- list( "T","I","Y","reef_high", "reef_low", "noreef_high", "noreef_low", "a" )
# parameters <- c("alpha", "beta", "kappa1", "kappa2", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), beta <- 0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), kappa3=rnorm(1,0,1) ), kappa4=rnorm(1,0,1) }
# mAreaMeanReefNoReefHighLow <- bugs(data, inits <- inits, parameters, "mAreaReefNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# #----------------------------------------------------------
# #----------- Inheritance and ecology models -----------

# # model mPastMeanAreaMean
# inherit <- inheritmean; a <- log_mean_area
# data <- list( "T","I","Y","inherit","a" )
# parameters <- c("alpha", "beta", "psi", "psi.tau")
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
# mPastMeanAreaMean <- bugs(data, inits <- inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastMeanAreaTotal
# inherit <- inheritmean; a <- log_total_area
# data <- list( "T","I","Y","inherit","a" )
# parameters <- c("alpha", "beta", "psi", "psi.tau")
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
# mPastMeanAreaTotal <- bugs(data, inits <- inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentAreaMean
# inherit <- inheritpresent; a <- log_mean_area
# data <- list( "T","I","Y","inherit","a" )
# parameters <- c("alpha", "beta", "psi", "psi.tau")
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
# mPastPresentAreaMean <- bugs(data, inits <- inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentAreaTotal
# inherit <- inheritpresent; a <- log_total_area
# data <- list( "T","I","Y","inherit","a" )
# parameters <- c("alpha", "beta", "psi", "psi.tau")
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
# mPastPresentAreaTotal <- bugs(data, inits <- inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastMeanAreaMeanReefHighLow
# a <- log_mean_area; inherit=inheritmean
# data <- list( "T","I","Y","inherit","a", "reef_low", "reef_high" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
# mPastMeanAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mPastAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentAreaMeanReefHighLow
# a <- log_mean_area; inherit=inheritpresent
# data <- list( "T","I","Y","inherit","a", "reef_low", "reef_high" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
# mPastPresentAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mPastAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastMeanAreaMeanNoReefHighLow
# a <- log_mean_area; inherit=inheritmean
# data <- list( "T","I","Y","inherit","a", "noreef_low", "noreef_high" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
# mPastMeanAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mPastAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentAreaMeanNoReefHighLow
# a <- log_mean_area; inherit=inheritpresent
# data <- list( "T","I","Y","inherit","a", "noreef_low", "noreef_high" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
# mPastPresentAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mPastAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# #--

# # model mSphereMeanAreaMean
# sphere <- spheremean; a <- log_mean_area
# data <- list( "T","I","Y","sphere","a" )
# parameters <- c("alpha", "beta", "lambda")
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1) ) }
# mSphereMeanAreaMean <- bugs(data, inits <- inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSphereMeanAreaTotal
# sphere <- spheremean; a <- log_total_area
# data <- list( "T","I","Y","sphere","a" )
# parameters <- c("alpha", "beta", "lambda" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rbinom(1,0,1), beta=0 ) }
# mSphereMeanAreaTotal <- bugs(data, inits <- inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSpherePresentAreaMean
# sphere <- spherepresent; a <- log_mean_area
# data <- list( "T","I","Y","sphere","a" )
# parameters <- c("alpha", "beta", "lambda")
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1) ) }
# mSpherePresentAreaMean <- bugs(data, inits <- inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSpherePresentAreaTotal
# sphere <- spherepresent; a <- log_total_area
# data <- list( "T","I","Y","sphere","a" )
# parameters <- c("alpha", "beta", "lambda" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rbinom(1,0,1), beta=0 ) }
# mSpherePresentAreaTotal <- bugs(data, inits <- inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSphereMeanAreaMeanReefHighLow
# a <- log_mean_area; sphere=spheremean
# data <- list( "T","I","Y","sphere","a", "reef_low", "reef_high" )
# parameters <- c("alpha", "beta", "lambda", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
# mSphereMeanAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSpherePresentAreaMeanReefHighLow
# a <- log_mean_area; sphere=spherepresent
# data <- list( "T","I","Y","sphere","a", "reef_low", "reef_high" )
# parameters <- c("alpha", "beta", "lambda", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
# mSpherePresentAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSphereMeanAreaMeanNoReefHighLow
# a <- log_mean_area; sphere=spheremean
# data <- list( "T","I","Y","sphere","a", "noreef_low", "noreef_high" )
# parameters <- c("alpha", "beta", "lambda", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1), kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
# mSphereMeanAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mSpherePresentAreaMeanNoReefHighLow
# a <- log_mean_area; sphere=spherepresent
# data <- list( "T","I","Y","sphere","a", "noreef_low", "noreef_high" )
# parameters <- c("alpha", "beta", "lambda", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1), kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
# mSpherePresentAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentSpherePresentAreaMeanReefHighLow
# a <- log_mean_area; inherit=inheritpresent; sphere <- spherepresent
# data <- list( "T","I","Y","inherit","a", "reef_low", "reef_high", "sphere" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
# mPastPresentSpherePresentAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mPastSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentSpherePresentAreaMeanReefHighLow
# a <- log_mean_area; inherit=inheritpresent; sphere <- spherepresent
# data <- list( "T","I","Y","inherit","a", "reef_low", "reef_high", "sphere" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
# mPastPresentSpherePresentAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mPastSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentSpherePresentAreaMeanNoReefHighLow
# a <- log_mean_area; inherit=inheritpresent; sphere <- spherepresent
# data <- list( "T","I","Y","inherit","a", "noreef_low", "noreef_high", "sphere" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
# mPastPresentSpherePresentAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mPastSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPastPresentSpherePresentAreaMeanNoReefHighLow
# a <- log_mean_area; inherit=inheritpresent; sphere <- spherepresent
# data <- list( "T","I","Y","inherit","a", "noreef_low", "noreef_high", "sphere" )
# parameters <- c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
# mPastPresentSpherePresentAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mPastSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPast2PresentSpherePresentAreaMeanReefHighLow
# sphere <- spherepresent; inherit <- inheritpresent; a <- log_mean_area
# data <- list( "T","I","Y","sphere", "inherit", "a", "reef_low", "reef_high" )
# parameters <- c("alpha", "psi", "psi.tau", "lambda", "beta", "kappa1", "kappa2" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5), beta=0, lambda=rnorm(1,0,1), kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
# mPast2PresentSpherePresentAreaMeanReefHighLow <- bugs(data, inits <- inits, parameters, "mPast2SphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )

# # model mPast2PresentSpherePresentAreaMeanNoReefHighLow
# sphere <- spherepresent; inherit <- inheritpresent; a <- log_mean_area
# data <- list( "T","I","Y","sphere", "inherit", "a", "noreef_low", "noreef_high" )
# parameters <- c("alpha", "psi", "psi.tau", "lambda", "beta", "kappa3", "kappa4" )
# inits <- function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5), beta=0, lambda=rnorm(1,0,1), kappa3=rnorm(1,0,1), kappa4=rnorm(1,0,1) ) }
# mPast2PresentSpherePresentAreaMeanNoReefHighLow <- bugs(data, inits <- inits, parameters, "mPast2SphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD <- TRUE) 
# save.image( file <- paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep <- "/") )


#----------------------------------------------------------
# ------------- ORGANIZE RESULTS ---------------------
#----------------------------------------------------------

# ------ function to put model selection results together ------
info.table <- function( res ){
	# find odds ratio
	or <- function(x, dig = 3) format( exp(x), digits = dig )
	# formatting
	fm <- function(z,x, dig=3) paste( or(z$summary[x,"mean"],dig)," (",or(z$summary[x,"2.5%"],dig),", ", or(z$summary[x,"97.5%"],dig),")", sep = "")
	
			delta <- function(ic)
			{
		noModels <- length(ic)
		i=1:noModels
		weight <- exp(-0.5*(ic[i]-min(ic)))/sum( exp(-0.5*(ic[i]-min(ic))) )
		names(weight) <- paste( "model", 1:noModels )
		weight
			}
	
	# table of results for single parameters, recover the estimates
	em.dic <- sapply( res, function(z) z$DIC )
	em.delta.dic <- delta( em.dic )
	em.beta <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="beta"), fm(z,"beta",dig=3), NA ) )
	em.kappa1 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa1"), fm(z,"kappa1"), NA ) )
	em.kappa2 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa2"), fm(z,"kappa2"), NA ) )
	em.kappa3 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa3"), fm(z,"kappa3"), NA ) )
	em.kappa4 <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa4"), fm(z,"kappa4"), NA ) )
	em.lambda <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="lambda"), fm(z,"lambda"), NA ) )
	em.psi <- sapply( res, function(z) ifelse( any(row.names(z$summary)=="psi"), fm(z,"psi"), NA ) )
	#em.alpha <- lapply( res, function(z) or( z$summary[1:T,1] ) )
	em.psi.tau <- lapply( res, function(z) if( any(row.names(z$summary)=="psi.tau[1]")){ z$summary[ (nrow(z$summary)-(T)):(nrow(z$summary)-1),1]}else{ NA } )
  # put estimates in a table
	table <- data.frame( row.names <- names(res), em.delta.dic, em.dic, em.psi, em.lambda, em.beta, em.kappa1, em.kappa2, em.kappa3, em.kappa4 )
	names(table) <- c("dDIC", "DIC", "Past", "Sphere", "Area", "Reef.High", "Reef.Low", "No.Reef.High", "No.Reef.Low" )
	table <- table[ order(em.delta.dic, decreasing <- TRUE),]
	table
}
#----------------------------------------------------------	

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