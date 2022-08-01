# Script to prepare data from Marta (monthly water requirements) to be read by script 2 (energy needs)

# data in mm/month
# actual <- list.files(paste0(input_folder, "20211122_irrigation"), full.names = T, pattern = "actual", recursive=T)
# actual2 <- mixedsort(actual)
# actual2 <- pblapply(actual2, raster)
# actual2 <- split(actual2,  unlist(qdapRegex::ex_between(actual, "irrigation/", "/I_")))
# actual <- pblapply(actual2, stack)

#

rainfed <- list.files(paste0("F:/MLED_database/", "risultati giacomo"), full.names = T, pattern = "closure", recursive=T)
# library(mgsub)
# rainfed_n <- mgsub(rainfed, tolower(month.abb),as.character(1:12))
#file.rename(rainfed, rainfed_n)
rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)
for (i in 1:length(rainfed2)){
crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

if(scenarios$field_size_contraint[scenario]==T){field_size <- projectRaster(field_size, mask_raster_to_polygon(rainfed2[[1]], st_as_sfc(st_bbox(clusters))), method = "bilinear") ; m <- field_size; m[m > 29] <- NA; field_size <- mask(field_size, m); rainfed2 <- pblapply(rainfed2, function(X){mask_raster_to_polygon(X, st_as_sfc(st_bbox(clusters)))}); for (i in 1:length(rainfed2)){crs(rainfed2[[i]]) <- crs(field_size)}; field_size <- projectRaster(field_size,rainfed2[[1]]); rainfed2 <- pblapply(rainfed2, function(X){mask(X, field_size)})}

rainfed2 <- split(rainfed2,  tolower(unlist(qdapRegex::ex_between(rainfed, "giacomo/", "/cl"))))

rainfed <- pblapply(rainfed2, stack)
  
for (i in 1:length(rainfed)){
rainfed[[i]][[2]] <- mean(rainfed[[i]][[1]], rainfed[[i]][[3]])
}

clusters$area <- as.numeric(st_area(clusters)) * 0.0001 # in hectares

# extract total bluewater demand in each cluster

names(rainfed) <- c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

files = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files_crops <- files_crops %in% names(rainfed)
files <- files[files_crops]

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

files <- pblapply(files, raster)
gc()

rainfed <- pblapply(1:length(files), function(X) {stack(rainfed[[X]] * files[[X]] * 10)})

# sum by month

rainfed_sum <- rainfed

for (m in 1:12){

  rainfed_sum[[m]] <- do.call("sum", c(pblapply(1:sum(files_crops), function(X){rainfed[[X]][[m]]}), na.rm = TRUE))

}

rainfed_sum <- stack(rainfed_sum)
rainfed_sum <- rainfed_sum[[1:12]]
rainfed <- rainfed_sum

#rasterVis::levelplot(rainfed, names.attr=month.abb, zscaleLog=TRUE)

for (i in 1:12){

  clusters[paste0('monthly_IRREQ' , "_" , as.character(i))] <- exact_extract(rainfed[[i]], clusters, "sum")
}


#

if (scenarios$rcp_ssp[scenario]!="baseline"){

markup <- stack(paste0("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost/other/markup_", scenarios$rcp_ssp[scenario], ".nc"))[[10]]

clusters$markup <- exact_extract(markup, clusters, "median")
clusters$markup <- ifelse(clusters$markup>1, 1, clusters$markup)

geom_bk <- clusters$x
clusters$x <- NULL

for (i in 1:12){

  clusters[,paste0('monthly_IRREQ' , "_" , as.character(i))] <- pull(clusters[paste0('monthly_IRREQ' , "_" , as.character(i))]) * (1 + clusters$markup)
}

clusters$x <- geom_bk
clusters <- st_as_sf(clusters)

}

#

aa <- clusters
aa$x=NULL

clusters$yearly_IRREQ <- rowSums(dplyr::select(aa, starts_with("monthly_IRREQ")))

#

if (scenarios$rcp_ssp[scenario]=="baseline"){
  
s <- stack(paste0(input_folder, "lpjml_gfdl-esm2m_ewembi_historical_histsoc_co2_qr_global_monthly_1861_2005.nc4"))
s <- s[[1501:1740]]
s <-stackApply(s, rep(1:12,times=20), fun = mean)
s <- s * 60*60*24*30  #convert to mm per month

}

if (scenarios$rcp_ssp[scenario]=="245"){
  
  s <- stack(paste0(input_folder, "lpjml_gfdl-esm2m_ewembi_rcp26_rcp26soc_co2_qr_global_monthly_2006_2099.nc4"))
  s <- s[[517:528]]
  s <- s * 60*60*24*30  #convert to mm per month
  
}

if (scenarios$rcp_ssp[scenario]=="585"){
  
  s <- stack(paste0(input_folder, "lpjml_gfdl-esm2m_ewembi_rcp60_rcp60soc_co2_qr_global_monthly_2006_2099.nc4"))
  s <- s[[517:528]]
  s <- s * 60*60*24*30  #convert to mm per month
  
}



# Apply sustainability constraint for groundwater depletion

for (i in 1:12){

  clusters[paste0('monthly_GQ' , "_" , as.character(i))] <- exact_extract(s[[i]], clusters, "mean") * clusters$area * 10
}

if(scenarios$groundwater_sustainability_contraint[scenario]==T){

for (i in 1:12){

  aa <- clusters
  aa$x=NULL

  clusters[paste0('monthly_unmet_IRRIG_share' , "_" , as.character(i))] <- as.numeric(ifelse((unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]) < unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]))==TRUE, (unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]) - unlist(aa[paste0('monthly_GQ' , "_" , as.character(i))]))/ unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]), 0))
  
}} else{
    
  
  clusters[paste0('monthly_unmet_IRRIG_share' , "_" , as.character(i))] <- 0
  
  }

#

clusters$maxflow <- exact_extract(maxflow, clusters, "mean")

clusters <- st_join(clusters, world %>% dplyr::select(sov_a3), largest = TRUE)

clusters$geometry <- clusters$x
clusters$x <- NULL
clusters <- st_as_sf(clusters)

#

clusters$cl_id <- 1:nrow(clusters)
clusters_bk_all <- clusters
clusters <- filter(clusters_bk_all, yearly_IRREQ>0)

#

write_rds(clusters_bk_all, paste0("clusters_bk_", paste(scenarios[scenario,], collapse="_"), ".Rds"))
write_rds(clusters, paste0("clusters_with_data_", paste(scenarios[scenario,], collapse="_"), ".Rds"))
#save.image("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost/before_energy.RData")
