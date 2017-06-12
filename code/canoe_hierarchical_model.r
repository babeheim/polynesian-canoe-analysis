
rm(list=ls())

trait_data <- read.csv( file <- "./inputs/traits.csv", stringsAsFactors=FALSE)
d <- read.csv( file <- "./inputs/islanddata.csv", stringsAsFactors=FALSE, na.strings="." )

islands_list <- sort(unique(d$archipelago))

trait_data$archipelago <- islands_list[match(substr(trait_data$islands, 1, 3), substr(islands_list, 1, 3))]

neighbor_cols <- c("sHawaii", "sMarq", "sTuam", "sSoci", "sAust",
    "sCook", "sMani", "sNewz", "sSamo", "sTong", "sFiji")

focal <- rep(islands_list, each=11)
neighbor <- rep(neighbor_cols, times=11)
neighbor_ties <- data.frame( focal, neighbor )
neighbor_ties$neighbor <- as.character(neighbor_ties$neighbor)
neighbor_ties$tie <- 0

for(i in 1:length(neighbor_cols)){
    nearby <- unique(d$archipelago[which(d[,neighbor_cols[i]]==1)])
    tar <- which(neighbor_ties$focal %in% nearby & neighbor_ties$neighbor==neighbor_cols[i])
    if(length(tar) > 0) neighbor_ties$tie[tar] <- 1
}

drop <- which(neighbor_ties$tie==0)
neighbor_ties <- neighbor_ties[-drop,]
neighbor_ties$neighbor <- islands_list[match(substr(neighbor_ties$neighbor, 2, 4), substr(islands_list, 1, 3))]
neighbor_ties <- neighbor_ties[,-which(colnames(neighbor_ties)=="tie")]

neighbor_ties$focal <- as.character(neighbor_ties$focal)
neighbor_ties$neighbor <- as.character(neighbor_ties$neighbor)

ancestor_cols <- c("iHawaii", "iMarq", "iTuam", "iSoci", "iAust",
    "iCook", "iMani", "iNewz", "iSamo", "iTong", "iFiji")

focal <- rep(islands_list, each=11)
ancestor <- rep(ancestor_cols, times=11)
ancestor_ties <- data.frame( focal, ancestor )
ancestor_ties$ancestor <- as.character(ancestor_ties$ancestor)
ancestor_ties$tie <- 0

for(i in 1:length(ancestor_cols)){
    nearby <- unique(d$archipelago[which(d[,ancestor_cols[i]]==1)])
    tar <- which(ancestor_ties$focal %in% nearby & ancestor_ties$ancestor==ancestor_cols[i])
    if(length(tar) > 0) ancestor_ties$tie[tar] <- 1
}

drop <- which(ancestor_ties$tie==0)
ancestor_ties <- ancestor_ties[-drop,]
ancestor_ties$ancestor <- islands_list[match(substr(ancestor_ties$ancestor, 2, 4), substr(islands_list, 1, 3))]

ancestor_ties <- ancestor_ties[,-which(colnames(ancestor_ties)=="tie")]

ancestor_ties$focal <- as.character(ancestor_ties$focal)
ancestor_ties$ancestor <- as.character(ancestor_ties$ancestor)

# these will be used to construct trait-level predictors...

trait_data$neighbor_trait_count <- 0
trait_data$ancestor_trait_count <- 0

for(i in 1:nrow(trait_data)){

    my_trait <- trait_data$traitID[i]
    my_archipelago <- trait_data$archipelago[i]

    my_neighbor_archipelagos <- neighbor_ties$neighbor[neighbor_ties$focal==my_archipelago]
    my_ancestor_archipelagos <- ancestor_ties$ancestor[ancestor_ties$focal==my_archipelago]

    neighbor_rows <- which(trait_data$archipelago %in% my_neighbor_archipelagos & trait_data$traitID==my_trait)
    if(length(neighbor_rows)>0) trait_data$neighbor_trait_count[i] <- sum(trait_data$present[neighbor_rows])

    ancestor_rows <- which(trait_data$archipelago %in% my_ancestor_archipelagos & trait_data$traitID==my_trait)
    if(length(ancestor_rows)>0) trait_data$ancestor_trait_count[i] <- sum(trait_data$present[ancestor_rows])

}



# need table to be one row per archipelago, to refrence the above

modal <- function(data){
    mode <- NA
    if(length(data) > 0 & !all(is.na(data))){
        mode <- names(sort(table(data),decreasing=T))[1]
        options(warn=-1)
        if(!is.na(as.numeric(mode))){
        mode <- as.numeric(mode)
        }
        options(warn=0)
    }
    return(mode)
}

group_reg <- data.frame( islands = islands_list )
group_reg$islands <- as.character(group_reg$islands)

group_reg$island_count <- tapply(d$high, d$archipelago, length)

group_reg$high <- tapply(d$high, d$archipelago, modal)
group_reg$makatea <- tapply(d$makatea, d$archipelago, modal)
group_reg$low <- as.numeric(!group_reg$high & !group_reg$makatea)
group_reg$reef_prop <- round(tapply(d$reef, d$archipelago, function(z) mean(z, na.rm=TRUE)), 2)

group_reg$area_mean <- round(tapply(d$sqkm, d$archipelago, function(z) mean(z, na.rm=TRUE)), 2)
group_reg$area_sum <- tapply(d$sqkm, d$archipelago, function(z) sum(z, na.rm=TRUE))



category_list <- sort(unique(trait_data$category))
trait_data$cat <- match(trait_data$category, category_list)


traitID_list <- sort(unique(trait_data$traitID))
trait_data$trait <- match(trait_data$traitID, traitID_list)

islands_list <- sort(unique(trait_data$islands))
trait_data$group <- match(trait_data$islands, islands_list)

link <- match(substr(trait_data$islands, 1,3), substr(group_reg$islands, 1, 3 ))
trait_data$islands <- group_reg$islands[link]
trait_data$low <- group_reg$low[link]
trait_data$area_sum <- group_reg$area_sum[link]

trait_data$log_area_sum <- log(trait_data$area_sum / 1e8)



library(rethinking)

model0 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a,
    a ~ normal(0, 10)
)

m0 <- map2stan(model0, data=trait_data)

model1 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a + a_trait[trait],
    a ~ normal(0, 10),
    a_trait[trait] ~ normal(0, 3)
)

m1 <- map2stan(model1, data=trait_data)

model2 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a + a_cat[cat] + a_group[group],
    a ~ normal(0, 4),
    a_cat[cat] ~ normal(0, 3),
    a_group[group] ~ normal(0, 3)
)

m2 <- map2stan(model2, data=trait_data)



model3 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a + a_cat[cat] + a_group[group] 
        + b_low*low + b_area*log_area_sum,
    a ~ normal(0, 10),
    a_cat[cat] ~ normal(0, 3),
    a_group[group] ~ normal(0, 3),
    b_low ~ normal(0, 1),
    b_area ~ normal(0, 1)
)

m3 <- map2stan(model3, data=trait_data, cores=3, chains=3)

# do different categories of trait have idff relationships with area?

# this is the full model:

model4 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a + a_cat[cat] + a_group[group] 
        + b_low*low + b_area*log_area_sum
        + b_ancestor * ancestor_trait_count
        + b_neighbor * neighbor_trait_count,
    a ~ normal(0, 1),
    a_cat[cat] ~ normal(0, 1),
    a_group[group] ~ normal(0, 1),
    b_low ~ normal(0, 1),
    b_area ~ normal(0, 1),
    b_ancestor ~ normal(0, 1),
    b_neighbor ~ normal(0, 1)
)

m4 <- map2stan(model4, data=trait_data)


model5 <- alist(
    present ~ bernoulli(p),
    logit(p) <- a + a_trait[trait] + a_cat[cat] + a_group[group] 
        + b_low*low + b_area*log_area_sum
        + b_ancestor * ancestor_trait_count
        + b_neighbor * neighbor_trait_count,
    a ~ normal(0, 10),
    a_trait[trait] ~ normal(0, 3),
    a_cat[cat] ~ normal(0, 3),
    a_group[group] ~ normal(0, 3),
    b_low ~ normal(0, 1),
    b_area ~ normal(0, 1),
    b_ancestor ~ normal(0, 1),
    b_neighbor ~ normal(0, 1)
)

m5 <- map2stan(model5, data=trait_data, chains=3, cores=3, iter=500)

# imputation: for fiji/samoa/tonga we dont know what their ancestors had, but we might be able to
# impute it...so, for ancestor_trait_count we need them to be NA
# and then do an imputation based on the distribution? i dunno...


# the next idea is that each canoe type has a different reationship with the outcome

# area is not showing up as predictive
# but we're using log area and including mega-island of NZ
# it could be interesting if there's no evidence smaller islands had less complex
# toolkits! 

