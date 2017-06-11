# --------------------------------------
# LOOP OVER ALL SUBSETS OF CANOE TRAITS
# --------------------------------------
traittypes = c("paddle", "hull", "decor", "sailrigging", "doublecanoe", "outrigger", "all")
for( j in 1:length(traittypes) ){

if(j>1) rm(list = unlist(res.str))
traitset = traittypes[j] # can be "paddle", "hull", "decor", "sailrigging", "doublecanoe", "outrigger", "all"

# ------------------------------------------------------------------
# READ IN DATA
# ------------------------------------------------------------------
Y = traits = read.table( file = "traits.csv", sep = ",", header = TRUE, row.names = 1, nrows = 65 )
d =  read.csv( file = "islanddata.csv", header = TRUE, stringsAsFactors=FALSE, na.strings="." )
# direction = read.csv( file ="directionality.csv", header=TRUE, stringsAsFactors=FALSE, na.strings="." )
d$sqkm = d$sqkm * 1000000  # convert to square meters so everthing is >1 (for logging)
# ------------------------------------------------------------------
# CREATE ECOLOGICAL PREDICTORS
# ------------------------------------------------------------------
arch = unique(d[,"archipelago"]) # list of unique island groups

# area covariates, all logged
area = sapply( arch, function(z) sum( d[ d[,"archipelago"]==z, "sqkm"], na.rm=T ) )
logtotalarea = sapply( arch, function(z) sum( log( d[ d[,"archipelago"]==z, "sqkm"] ), na.rm=T ) )

logmnarea = sapply( arch, function(z) mean( log( d[ d[,"archipelago"]==z, "sqkm"] ), na.rm=T ) )
# an educated guess since there are countless numbers of atolls with no data, and this is approximately their island size
tua.is.sqkm = c(170.0, 10.0, 7.0,   4.0,   4.0,   1.5,   1.5,   1.5,   1.5,   4.0,   8.0,   9.0, 600.0,  24.0,   3.0, rep( 10, 64 ) ) 
logmnarea["Tuamotus"] = mean( log( tua.is.sqkm * 1000000 ) ) # correction converted to square meters

logmaxarea = sapply( arch, function(z) max( log(d[ d[,"archipelago"]==z, "sqkm"]), na.rm=T ) )
logmaxarea["Tuamotus"] = log(170) # correction

# island type covariates
rh = sapply( arch, function(z){ x=d[,"archipelago"]==z; sum( d[x,"high"]*d[x,"reef"]*d[x,"sqkm"], na.rm=T )/area[z] } )
rl = sapply( arch, function(z){ x=d[,"archipelago"]==z; sum( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=T )/area[z] } )
xmakatea = sapply( arch, function(z){ x=d[,"archipelago"]==z; sum( c(d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=T )/area[z] } )
rl.mn.area = sapply( arch, function(z){ x=d[,"archipelago"]==z ; mean( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=T ) } )
rl.mn.area["Tuamotus"] = 5 # an educated guess since there are countless numbers of atolls with no data
rl.max.area = sapply( arch, function(z){ x=d[,"archipelago"]==z ; max( c(d[x,"atoll"]*d[x,"reef"]*d[x,"sqkm"],d[x,"makatea"]*d[x,"reef"]*d[x,"sqkm"]), na.rm=T ) } )
rl.max.area["Tuamotus"] = 170 # correction
atoll = sapply( arch, function(z) { x=d[,"archipelago"]==z; sum( d[x,"atoll"]*d[x,"sqkm"], na.rm=T )/area[z] } )
nrh = sapply( arch, function(z){ x=d[,"archipelago"]==z; sum( d[x,"high"]*(1-d[x,"reef"])*d[x,"sqkm"], na.rm=T )/area[z] } ) # no reef high island, covariate not used in models
nrl = sapply( arch, function(z){ x=d[,"archipelago"]==z; sum( d[x,"makatea"]*(1-d[x,"reef"])*d[x,"sqkm"], na.rm=T )/area[z] } ) # no reef high island, covariate not used in models

# ------------------------------------------------------------------
# CREATE CULTURAL INHERITANCE PREDICTORS
# ------------------------------------------------------------------
# along colonization sequence
inherit.islands = t( sapply( arch, function(y){ x = d[,"archipelago"]==y; d[x,21:31][1,] } ) )
inheritmean1 = sapply( arch, function(z)  rowMeans( t( as.numeric( inherit.islands[z,] ) * t(Y) ), na.rm=T ) )
inheritpresent = sapply( arch, function(z)  ifelse( rowMeans( t( as.numeric( inherit.islands[z,] ) * t(Y) ), na.rm=T )>0, 1, 0 ) )
# within spheres of interactions, according to Weisler
sphere.islands = t( sapply( arch, function(y){ x = d[,"archipelago"]==y; d[x,10:20][1,] } ) )
spheremean1 = sapply( arch, function(z)  rowMeans( t( as.numeric( sphere.islands[z,] ) * t(Y) ), na.rm=T ) )
spherepresent1 = sapply( arch, function(z)  ifelse( rowMeans( t( as.numeric( sphere.islands[z,] ) * t(Y) ), na.rm=T )> 0, 1, 0 ) )

# ------------------------------------------------------------------
# GENERATE INDICES OF SUBSETS OF TRAITS
# ------------------------------------------------------------------
all = c(1:nrow(Y))
hull = c(1:18,30)
decor = c(19:22)
sailrigging = c(23:29, 64:65)
doublecanoe = c( 31:38 )
outrigger = c(39:54) 
paddle = c(55:63)

# narrow down the traits
set = get( traitset )
Y = as.matrix( traits[set,] )
spheremean = as.matrix( spheremean1[set,] )
spherepresent = as.matrix( spherepresent1[set,])
inheritmean = as.matrix( inheritmean1[set,] )
inheritpresent = as.matrix( inheritpresent[set,] )

I = ncol( Y ) # number of islands
T = nrow( Y ) # number of traits
a = logmnarea # average island area of each archipelago (log-transformed), vector that matches the columns of Y

saveas = paste( traitset,format( Sys.time(), "%b_%d_%H%M" ), sep="" ) # tag for files to be saved 
dir.create( traitset )  # create folder to put results in

library(arm)
iter = 1000 # length of chain
#----------------------------------------------------------
# ------------- MODELS AND ESTIMATION ---------------------
#----------------------------------------------------------
# -------- "Null" models ---------

# model mCoinFlip checked
data = list( "T","I","Y")
parameters = c("alpha")
# file.show("mCoinFlip.txt")
mCoinFlip = bugs(data, inits = NULL, parameters, "mCoinFlip.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
# save( mCoinFlip, file = paste("mCoinFlip",saveas,".rdata", sep = "") )
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mBase checked
data = list( "T","I","Y")
parameters = c("alpha")
mBase = bugs(data, inits = NULL, parameters, "mBase.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

#----------------------------------------------------------
# -------- Inheritance models -----------

# model mPastMean
inherit = inheritmean
data = list( "T","I","Y","inherit" )
parameters = c("alpha", "psi", "psi.tau")
mPastMean = bugs(data, inits = NULL, parameters, "mPast.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresent
inherit = inheritpresent
data = list( "T","I","Y","inherit" )
parameters = c("alpha", "psi", "psi.tau")
mPastPresent = bugs(data, inits = NULL, parameters, "mPast.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2Mean 
inherit = inheritmean
data = list( "T","I","Y","inherit" )
parameters = c("alpha", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5) ) }
mPast2Mean = bugs(data, inits = inits, parameters, "mPast2.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2Present 
inherit = inheritpresent
data = list( "T","I","Y","inherit" )
parameters = c("alpha", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5) ) }
mPast2Present = bugs(data, inits = inits, parameters, "mPast2.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSphereMean 
sphere = spheremean
data = list( "T","I","Y","sphere" )
parameters = c("alpha", "lambda")
mSphereMean = bugs(data, inits = NULL, parameters, "mSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSpherePresent 
sphere = spherepresent
data = list( "T","I","Y","sphere" )
parameters = c("alpha", "lambda")
mSpherePresent = bugs(data, inits = NULL, parameters, "mSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSphereMean 
sphere = spheremean; inherit = inheritpresent
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5),lambda=rnorm(1,0,1) ) }
mPastPresentSphereMean = bugs(data, inits = NULL, parameters, "mPastSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSpherePresent
sphere = spherepresent; inherit = inheritpresent 
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,0.01), psi=rnorm(1,0,0.01), psi.tau=rbinom(T,1,0.5), lambda=rnorm(1,0,0.1)  ) }
mPastPresentSpherePresent = bugs(data, inits = inits, parameters, "mPastSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastMeanSphereMean 
sphere = spheremean; inherit = inheritmean
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5),lambda=rnorm(1,0,1) ) }
mPastMeanSphereMean = bugs(data, inits = NULL, parameters, "mPastSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastMeanSpherePresent 
sphere = spherepresent; inherit = inheritmean
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,0.01), psi=rnorm(1,0,0.01), psi.tau=rbinom(T,1,0.5), lambda=rnorm(1,0,0.1)  ) }
mPastMeanSpherePresent = bugs(data, inits = inits, parameters, "mPastSphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2PresentSphereMean 
sphere = spheremean; inherit = inheritpresent
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5),lambda=rnorm(1,0,1) ) }
mPast2PresentSphereMean = bugs(data, inits = NULL, parameters, "mPast2Sphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2PresentSpherePresent 
sphere = spherepresent; inherit = inheritpresent
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,0.01), psi=rnorm(1,0,0.01), psi.tau=rbinom(T,1,0.5), lambda=rnorm(1,0,0.1)  ) }
mPast2PresentSpherePresent = bugs(data, inits = inits, parameters, "mPast2Sphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2MeanSphereMean 
sphere = spheremean; inherit = inheritmean
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5),lambda=rnorm(1,0,1) ) }
mPastMeanSphereMean = bugs(data, inits = NULL, parameters, "mPast2Sphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2MeanSpherePresent 
sphere = spherepresent; inherit = inheritmean
data = list( "T","I","Y","sphere", "inherit" )
parameters = c("alpha", "psi", "psi.tau", "lambda")
inits = function(){ list( alpha=rnorm(T,0,0.01), psi=rnorm(1,0,0.01), psi.tau=rbinom(T,1,0.5), lambda=rnorm(1,0,0.1)  ) }
mPastMeanSpherePresent = bugs(data, inits = inits, parameters, "mPast2Sphere.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )


#----------------------------------------------------------
# ------------ Ecology models -----------

# model mAreaTotal
a = logtotalarea
data = list( "T","I","Y","a" )
parameters = c("alpha", "beta")
# file.show("mArea.txt")
inits = function(){ list( alpha=rnorm(T,0,5), beta=0 ) }
mAreaTotal = bugs(data, inits = inits, parameters, "mArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMean
a = logmnarea
data = list( "T","I","Y","a" )
parameters = c("alpha", "beta")
# file.show("mArea.txt")
inits = function(){ list( alpha=rnorm(T,0,5), beta=0 ) }
mAreaMean = bugs(data, inits = inits, parameters, "mArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMean
a = logmxarea
data = list( "T","I","Y","a" )
parameters = c("alpha", "beta")
# file.show("mArea.txt")
inits = function(){ list( alpha=rnorm(T,0,5), beta=0 ) }
mAreaMax = bugs(data, inits = inits, parameters, "mArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mReefHigh
data = list( "T","I","Y","rh" )
parameters = c("alpha", "kappa1"  )
mReefHigh = bugs(data, inits = NULL, parameters, "mReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mReefLow
data = list( "T","I","Y","rl" )
parameters = c("alpha", "kappa2"  )
mReefHigh = bugs(data, inits = NULL, parameters, "mReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mNoReefHigh
data = list( "T","I","Y","nrh" )
parameters = c("alpha", "kappa3"  )
mNoReefHigh = bugs(data, inits = NULL, parameters, "mNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mNoReefHigh
data = list( "T","I","Y","nrl" )
parameters = c("alpha", "kappa4"  )
mNoReefLow = bugs(data, inits = NULL, parameters, "mNoReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaTotalReefHigh 
a = logtotalarea
data = list( "T","I","Y","rh", "a" )
parameters = c("alpha", "beta", "kappa1" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa1=rnorm(1,0,1) ) }
mAreaTotalReefHigh = bugs(data, inits = inits, parameters, "mAreaReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaTotalReefLow
a = logtotalarea
data = list( "T","I","Y","rl", "a" )
parameters = c("alpha", "beta", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa2=rnorm(1,0,1) ) }
mAreaTotalReefLow = bugs(data, inits = inits, parameters, "mAreaReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaTotalNoReefHigh
a = logtotalarea
data = list( "T","I","Y","nrh", "a" )
parameters = c("alpha", "beta", "kappa3" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa3=rnorm(1,0,1) ) }
mAreaTotalNoReefHigh = bugs(data, inits = inits, parameters, "mAreaNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaTotalNoReefLow 
a = logtotalarea
data = list( "T","I","Y","nrl", "a" )
parameters = c("alpha", "beta", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa4=rnorm(1,0,1) ) }
mAreaTotalNoReefLow = bugs(data, inits = inits, parameters, "mAreaNoReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMeanReefHigh 
a = logmnarea
data = list( "T","I","Y","rh", "a" )
parameters = c("alpha", "beta", "kappa1" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa1=rnorm(1,0,1) ) }
mAreaMeanReefHigh = bugs(data, inits = inits, parameters, "mAreaReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMeanReefLow
a = logmnarea
data = list( "T","I","Y","rl", "a" )
parameters = c("alpha", "beta", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa2=rnorm(1,0,1) ) }
mAreaMeanReefLow = bugs(data, inits = inits, parameters, "mAreaReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMeanNoReefHigh
a = logmnarea
data = list( "T","I","Y","nrh", "a" )
parameters = c("alpha", "beta", "kappa3" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa3=rnorm(1,0,1) ) }
mAreaMeanNoReefHigh = bugs(data, inits = inits, parameters, "mAreaNoReefHigh.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMeanNoReefLow
a = logmnarea
data = list( "T","I","Y","nrl", "a" )
parameters = c("alpha", "beta", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa4=rnorm(1,0,1) ) }
mAreaMeanNoReefLow = bugs(data, inits = inits, parameters, "mAreaNoReefLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaTotalReefNoReefHighLow
a = logtotalarea
data = list( "T","I","Y","rh", "rl", "nrh", "nrl", "a" )
parameters = c("alpha", "beta", "kappa1", "kappa2", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), kappa3=rnorm(1,0,1) ), kappa4=rnorm(1,0,1) }
mAreaTotalReefNoReefHighLow = bugs(data, inits = inits, parameters, "mAreaReefNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mAreaMeanReefNoReefHighLow
a = logmnarea
data = list( "T","I","Y","rh", "rl", "nrh", "nrl", "a" )
parameters = c("alpha", "beta", "kappa1", "kappa2", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), beta = 0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), kappa3=rnorm(1,0,1) ), kappa4=rnorm(1,0,1) }
mAreaMeanReefNoReefHighLow = bugs(data, inits = inits, parameters, "mAreaReefNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

#----------------------------------------------------------
#----------- Inheritance and ecology models -----------

# model mPastMeanAreaMean
inherit = inheritmean; a = logmnarea
data = list( "T","I","Y","inherit","a" )
parameters = c("alpha", "beta", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
mPastMeanAreaMean = bugs(data, inits = inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastMeanAreaTotal
inherit = inheritmean; a = logtotalarea
data = list( "T","I","Y","inherit","a" )
parameters = c("alpha", "beta", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
mPastMeanAreaTotal = bugs(data, inits = inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentAreaMean
inherit = inheritpresent; a = logmnarea
data = list( "T","I","Y","inherit","a" )
parameters = c("alpha", "beta", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
mPastPresentAreaMean = bugs(data, inits = inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentAreaTotal
inherit = inheritpresent; a = logtotalarea
data = list( "T","I","Y","inherit","a" )
parameters = c("alpha", "beta", "psi", "psi.tau")
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0 ) }
mPastPresentAreaTotal = bugs(data, inits = inits, parameters, "mPastArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastMeanAreaMeanReefHighLow
a = logmnarea; inherit=inheritmean
data = list( "T","I","Y","inherit","a", "rl", "rh" )
parameters = c("alpha", "beta", "psi", "psi.tau", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
mPastMeanAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mPastAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentAreaMeanReefHighLow
a = logmnarea; inherit=inheritpresent
data = list( "T","I","Y","inherit","a", "rl", "rh" )
parameters = c("alpha", "beta", "psi", "psi.tau", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
mPastPresentAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mPastAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastMeanAreaMeanNoReefHighLow
a = logmnarea; inherit=inheritmean
data = list( "T","I","Y","inherit","a", "nrl", "nrh" )
parameters = c("alpha", "beta", "psi", "psi.tau", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
mPastMeanAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mPastAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentAreaMeanNoReefHighLow
a = logmnarea; inherit=inheritpresent
data = list( "T","I","Y","inherit","a", "nrl", "nrh" )
parameters = c("alpha", "beta", "psi", "psi.tau", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
mPastPresentAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mPastAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

#--

# model mSphereMeanAreaMean
sphere = spheremean; a = logmnarea
data = list( "T","I","Y","sphere","a" )
parameters = c("alpha", "beta", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1) ) }
mSphereMeanAreaMean = bugs(data, inits = inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSphereMeanAreaTotal
sphere = spheremean; a = logtotalarea
data = list( "T","I","Y","sphere","a" )
parameters = c("alpha", "beta", "lambda" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rbinom(1,0,1), beta=0 ) }
mSphereMeanAreaTotal = bugs(data, inits = inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSpherePresentAreaMean
sphere = spherepresent; a = logmnarea
data = list( "T","I","Y","sphere","a" )
parameters = c("alpha", "beta", "lambda")
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1) ) }
mSpherePresentAreaMean = bugs(data, inits = inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSpherePresentAreaTotal
sphere = spherepresent; a = logtotalarea
data = list( "T","I","Y","sphere","a" )
parameters = c("alpha", "beta", "lambda" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rbinom(1,0,1), beta=0 ) }
mSpherePresentAreaTotal = bugs(data, inits = inits, parameters, "mSphereArea.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSphereMeanAreaMeanReefHighLow
a = logmnarea; sphere=spheremean
data = list( "T","I","Y","sphere","a", "rl", "rh" )
parameters = c("alpha", "beta", "lambda", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
mSphereMeanAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSpherePresentAreaMeanReefHighLow
a = logmnarea; sphere=spherepresent
data = list( "T","I","Y","sphere","a", "rl", "rh" )
parameters = c("alpha", "beta", "lambda", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
mSpherePresentAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSphereMeanAreaMeanNoReefHighLow
a = logmnarea; sphere=spheremean
data = list( "T","I","Y","sphere","a", "nrl", "nrh" )
parameters = c("alpha", "beta", "lambda", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1), kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
mSphereMeanAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mSpherePresentAreaMeanNoReefHighLow
a = logmnarea; sphere=spherepresent
data = list( "T","I","Y","sphere","a", "nrl", "nrh" )
parameters = c("alpha", "beta", "lambda", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), lambda=rnorm(1,0,1), beta=rnorm(1,0,1), kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1) ) }
mSpherePresentAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSpherePresentAreaMeanReefHighLow
a = logmnarea; inherit=inheritpresent; sphere = spherepresent
data = list( "T","I","Y","inherit","a", "rl", "rh", "sphere" )
parameters = c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
mPastPresentSpherePresentAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mPastSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSpherePresentAreaMeanReefHighLow
a = logmnarea; inherit=inheritpresent; sphere = spherepresent
data = list( "T","I","Y","inherit","a", "rl", "rh", "sphere" )
parameters = c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
mPastPresentSpherePresentAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mPastSphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSpherePresentAreaMeanNoReefHighLow
a = logmnarea; inherit=inheritpresent; sphere = spherepresent
data = list( "T","I","Y","inherit","a", "nrl", "nrh", "sphere" )
parameters = c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
mPastPresentSpherePresentAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mPastSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPastPresentSpherePresentAreaMeanNoReefHighLow
a = logmnarea; inherit=inheritpresent; sphere = spherepresent
data = list( "T","I","Y","inherit","a", "nrl", "nrh", "sphere" )
parameters = c("alpha", "beta", "psi", "psi.tau", "lambda", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(T,1,0.5), beta=0, kappa4=rnorm(1,0,1), kappa3=rnorm(1,0,1), lambda=rnorm(1,0,1) ) }
mPastPresentSpherePresentAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mPastSphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2PresentSpherePresentAreaMeanReefHighLow
sphere = spherepresent; inherit = inheritpresent; a = logmnarea
data = list( "T","I","Y","sphere", "inherit", "a", "rl", "rh" )
parameters = c("alpha", "psi", "psi.tau", "lambda", "beta", "kappa1", "kappa2" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5), beta=0, lambda=rnorm(1,0,1), kappa1=rnorm(1,0,1), kappa2=rnorm(1,0,1) ) }
mPast2PresentSpherePresentAreaMeanReefHighLow = bugs(data, inits = inits, parameters, "mPast2SphereAreaReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

# model mPast2PresentSpherePresentAreaMeanNoReefHighLow
sphere = spherepresent; inherit = inheritpresent; a = logmnarea
data = list( "T","I","Y","sphere", "inherit", "a", "nrl", "nrh" )
parameters = c("alpha", "psi", "psi.tau", "lambda", "beta", "kappa3", "kappa4" )
inits = function(){ list( alpha=rnorm(T,0,1), psi=rnorm(1,0,1), psi.tau=rbinom(1,1,0.5), beta=0, lambda=rnorm(1,0,1), kappa3=rnorm(1,0,1), kappa4=rnorm(1,0,1) ) }
mPast2PresentSpherePresentAreaMeanNoReefHighLow = bugs(data, inits = inits, parameters, "mPast2SphereAreaNoReefHighLow.txt", n.chains=3, n.iter=iter, clearWD = TRUE) 
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )


#----------------------------------------------------------
# ------------- ORGANIZE RESULTS ---------------------
#----------------------------------------------------------

# ------ function to put model selection results together ------
info.table = function( res ){
	# find odds ratio
	or = function(x, dig = 3) format( exp(x), digits = dig )
	# formatting
	fm = function(z,x, dig=3) paste( or(z$summary[x,"mean"],dig)," (",or(z$summary[x,"2.5%"],dig),", ", or(z$summary[x,"97.5%"],dig),")", sep = "")
	
			delta = function(ic)
			{
		noModels = length(ic)
		i=1:noModels
		weight = exp(-0.5*(ic[i]-min(ic)))/sum( exp(-0.5*(ic[i]-min(ic))) )
		names(weight) = paste( "model", 1:noModels )
		weight
			}
	
	# table of results for single parameters, recover the estimates
	em.dic = sapply( res, function(z) z$DIC )
	em.delta.dic = delta( em.dic )
	em.beta = sapply( res, function(z) ifelse( any(row.names(z$summary)=="beta"), fm(z,"beta",dig=3), NA ) )
	em.kappa1 = sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa1"), fm(z,"kappa1"), NA ) )
	em.kappa2 = sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa2"), fm(z,"kappa2"), NA ) )
	em.kappa3 = sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa3"), fm(z,"kappa3"), NA ) )
	em.kappa4 = sapply( res, function(z) ifelse( any(row.names(z$summary)=="kappa4"), fm(z,"kappa4"), NA ) )
	em.lambda = sapply( res, function(z) ifelse( any(row.names(z$summary)=="lambda"), fm(z,"lambda"), NA ) )
	em.psi = sapply( res, function(z) ifelse( any(row.names(z$summary)=="psi"), fm(z,"psi"), NA ) )
	#em.alpha = lapply( res, function(z) or( z$summary[1:T,1] ) )
	em.psi.tau = lapply( res, function(z) if( any(row.names(z$summary)=="psi.tau[1]")){ z$summary[ (nrow(z$summary)-(T)):(nrow(z$summary)-1),1]}else{ NA } )
  # put estimates in a table
	table = data.frame( row.names = names(res), em.delta.dic, em.dic, em.psi, em.lambda, em.beta, em.kappa1, em.kappa2, em.kappa3, em.kappa4 )
	names(table) = c("dDIC", "DIC", "Past", "Sphere", "Area", "Reef.High", "Reef.Low", "No.Reef.High", "No.Reef.Low" )
	table = table[ order(em.delta.dic, decreasing = TRUE),]
	table
	}
#----------------------------------------------------------	

# load models from directory
#res.na.dir = lapply( dir(), function(z){ x = strsplit(z,NULL); ifelse( x[[1]][1]=="m" & x[[1]][length(x[[1]])]=="a",z, NA ) })
#res.str.dir = subset( res.na.dir, subset = is.na(res.na.dir)==FALSE )
#for( i in 1:length(res.str.dir)) load(file = res.str.dir[[i]][1])
#for( i in 1:length(res.str.dir)) print(res.str.dir[[i]][1])

# list of results
res.na = lapply( ls(), function(z) ifelse( strsplit(z,NULL)[[1]][1]=="m",z, NA ) )
res.str = subset( res.na, subset = is.na(res.na)==FALSE )
res = lapply( res.str, function(z) get(z) )
names(res) = res.str

# call to organizing function
table = info.table( res )

# save output to file
write.table(table, file = paste(getwd(),traitset,paste( saveas,".csv", sep="" ),sep = "/"),row.names = TRUE, col.names = TRUE, sep=",")
save.image( file = paste(getwd(),traitset,paste( saveas,".rdata", sep="" ),sep = "/") )

}