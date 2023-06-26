# here we calculate all the metrics to be cited in the paper and in the reviewers' replies
###############################################################################

if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive)

setwd("C:/Users/falchetta/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

##############

baseline <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

####

(sum(clusters$yearly_IRREQ)) / 1e9
((sum(clusters$yearly_IRREQ) * 0.68) / 0.26) / 1e9

(sum(clusters$yearly_IRREQ[!is.na(clusters$profit_yearly)])) / 1e9
((sum(clusters$yearly_IRREQ[!is.na(clusters$profit_yearly)]) * 0.68) / 0.26) / 1e9

# - how much harvested rainfed area there is in SSA in total

files = list.files(path = paste0("F:/Il mio Drive/MLED_database/input_folder/spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files_crops <- files_crops %in% c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")
files <- files[files_crops]
files <- lapply(files, raster)
files <- stack(files)

harv_area_rainfed <- sum(values(files), na.rm=T)

harv_area_rainfed / 1e6

######################

sum(baseline$A_total)/1e6 # million ha


#   - how much remained after filtering out large scale commercial agriculture: delta

(harv_area_rainfed - sum(baseline$A_total)) / 1e6


(harv_area_rainfed - sum(baseline$A_total)) / harv_area_rainfed

##############

#   - how much remained after considering water resources availability
#   •	Delta

envflow <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))


sum(baseline$A_total[!is.na(baseline$profit_yearly)])/1e6 # million ha
sum(envflow$A_total[!is.na(envflow$profit_yearly)])/1e6 # million ha

sum(baseline$yearly_IRREQ*(1-baseline$monthly_unmet_IRRIG_share_1))/1e9 # mkm
sum(envflow$yearly_IRREQ*(1-envflow$monthly_unmet_IRRIG_share_1))/1e9 # million ha

sum(envflow$A_total*(envflow$monthly_unmet_IRRIG_share_avg>=0.25))/sum(baseline$A_total) # million ha
sum(envflow$A_total*(envflow$monthly_unmet_IRRIG_share_avg>=0.25)) / 1e6

sum(envflow$A_total*(envflow$monthly_unmet_IRRIG_share_avg<=0.25))/sum(baseline$A_total) # million ha
sum(envflow$A_total*(envflow$monthly_unmet_IRRIG_share_avg<=0.25)) / 1e6

out <- vector()
for (i in seq(0, 1, 0.05)){
  out[as.character(i)] <- sum(envflow$A_total*(envflow$monthly_unmet_IRRIG_share_avg<=i)) / 1e6
}

png("new_figures/unsust_share.png")
plot(seq(0, 1, 0.05)*100, out, type="l", xlab="Share of env.flow. overpass (%)", ylab=c("Million ha of harvested land"))
dev.off()

###
#   •	Of which irrigated

files = list.files(path = paste0("F:/Il mio Drive/MLED_database/input_folder/spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'I.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_I.tif")))
files_crops <- files_crops %in% c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")
files <- files[files_crops]
files <- lapply(files, raster)
files <- stack(files)

harv_area_irrigated <- sum(values(files), na.rm=T)

harv_area_irrigated / 1e6


############

#   - how much remained finally after considering economic constraints (the balance between costs and benefits of solar irrigation).
#   •	Delta
  
sum(baseline$A_total[!is.na(baseline$profit_yearly)])/1e6 # million ha
sum(baseline$yearly_IRREQ[!is.na(baseline$profit_yearly)])/1e9 # bcm

sum(baseline$A_total[!is.na(baseline$profit_yearly)])/sum(baseline$A_total)


####################

rainfed <- list.files("risultati giacomo", full.names = T, pattern = "closure", recursive=T)
# library(mgsub)
# rainfed_n <- mgsub(rainfed, tolower(month.abb),as.character(1:12))
#file.rename(rainfed, rainfed_n)
rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)
for (i in 1:length(rainfed2)){
  crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

files <- stack(rainfed2)

mm_req <- vector()

for (i in 1:nlayers(files)){
  mm_req[i] <- sum(values(files[[i]]), na.rm=T)
}

mm_req <- sum(mm_req)

mm_req / harv_area_rainfed 


  
  