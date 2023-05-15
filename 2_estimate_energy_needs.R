# Script to estimate, given monthly irrigation needs:
#1) Power of pump (W) given flow of pump (m3/s), or flow of pump given a fixed power of pump
#2) KWh/month required

#############

# Groundwater and surface water pumping module

# Use Google Earth Engine to extract the distance to the nearest source of surface water

geom <- ee$Geometry$Rectangle(c(as.vector(extent(clusters))[1], as.vector(extent(clusters))[3], as.vector(extent(clusters))[2], as.vector(extent(clusters))[4]))

# srtm = ee$Image('USGS/SRTMGL1_003');
# slope = ee$Terrain$slope(srtm);
# 
# img_01 <- ee_as_raster(
#   image = slope,
#   via = "drive",
#   region = geom,
#   scale = 500
# )

img_01 <- raster(paste0(input_folder, "slope.tif"))
 
# i = ee$FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers") #$filter(ee$Filter$lte('RIV_ORD', 7))
# i = i$map(function(f) {
#   f$buffer(20, 10);
# });
# 
# distance = i$distance(searchRadius = 50000, maxError = 25)$clip(geom)
# 
# img_02 <- ee_as_raster(
#   image = distance,
#   via = "drive",
#   region = geom,
#   scale = 500
# )

img_02 <- raster(paste0(input_folder, "groundwater_distance.tif"))

# # discharge of surface water
# i = ee$FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers")
# i = i$map(function(f) {
#   f$buffer(20, 10);
# });
# 
# i = i$reduceToImage(
#   properties = list('DIS_AV_CMS'),
#   reducer = ee$Reducer$sum())$clip(geom)
# 
# img_03 <- ee_as_raster(
#   image = i,
#   via = "drive",
#   region = geom,
#   scale = 500
# )
# 
# ###
# 
# i = ee$FeatureCollection("WWF/HydroSHEDS/v1/FreeFlowingRivers")
# i = i$map(function(f) {
#   f$buffer(20, 10);
# });
# 
# i = i$reduceToImage(
#   properties = list('VOLUME_TCM'),
#   reducer = ee$Reducer$sum())$clip(geom)
# 
# img_04 <- ee_as_raster(
#   image = i,
#   via = "drive",
#   region = geom,
#   scale = 500
# )

###

img_03 <- raster(paste0(input_folder, "surface_discharge.tif")) / 0.000277778 # convert m3/s to m3/h
img_04 <- raster(paste0(input_folder, "surface_volume.tif"))

######

# library(lwgeom)
#
# world_all_africa <- filter(rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"), region_un=="Africa") %>% st_transform(3395) %>%
#   st_snap_to_grid(size = 1000) %>%
#   st_make_valid() %>% st_union() %>% st_transform(4326) %>% st_as_sf()
#
# img_02 <- rgis::mask_raster_to_polygon(img_02, clusters)
#
# writeRaster(img_02,"//tsclient/D/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost/data/noid_image.tif", overwrite=T)

#
# # Calculate the mean distance from each cluster to the nearest source of surface water
clusters$surfw_dist <-  exact_extract(img_02, clusters, fun="mean")
clusters$slope <-  exact_extract(img_01, clusters, fun="mean")

# Groundwater depth
# Reclassify to numeric
DepthToGroundwater$depthwater = ifelse(DepthToGroundwater$DTWAFRICA_ == 'VS', 3.5, ifelse(DepthToGroundwater$DTWAFRICA_ == "S", 16, ifelse(DepthToGroundwater$DTWAFRICA_ == "SM", 37.5, ifelse(DepthToGroundwater$DTWAFRICA_ == "M", 75, ifelse(DepthToGroundwater$DTWAFRICA_ == "D", 175, ifelse(DepthToGroundwater$DTWAFRICA_ == "D", 250, 0))))))

DepthToGroundwater$depthwater <- as.numeric(DepthToGroundwater$depthwater)

groundwater_depth <- rasterFromXYZ(DepthToGroundwater[c("X", "Y", "depthwater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_depth <- exactextractr::exact_extract(groundwater_depth, clusters, fun="mean")

# Groundwater storage
GroundwaterStorage$storagewater = ifelse(GroundwaterStorage$GWSTOR_V2 == 'VL', 0, ifelse(GroundwaterStorage$GWSTOR_V2 == "L", 500, ifelse(GroundwaterStorage$GWSTOR_V2 == "LM", 5500, ifelse(GroundwaterStorage$GWSTOR_V2 == "M", 17500, ifelse(GroundwaterStorage$GWSTOR_V2 == "H", 37500, 50000)))))

GroundwaterStorage$storagewater <- as.numeric(GroundwaterStorage$storagewater)

groundwater_storage <- rasterFromXYZ(GroundwaterStorage[c("X", "Y", "storagewater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_storage <- exactextractr::exact_extract(groundwater_storage, clusters, fun="mean")

# Groundwate productivity
GroundwaterProductivity$Productivitywater = ifelse(GroundwaterProductivity$GWPROD_V2 == 'VH', 25, ifelse(GroundwaterProductivity$GWPROD_V2 == "H", 12.5, ifelse(GroundwaterProductivity$GWPROD_V2 == "M", 3, ifelse(GroundwaterProductivity$GWPROD_V2 == "LM", 0.75, ifelse(GroundwaterProductivity$GWPROD_V2 == "L", 0.3, 0.05)))))

GroundwaterProductivity$Productivitywater <- as.numeric(GroundwaterProductivity$Productivitywater)

groundwater_Productivity <- rasterFromXYZ(GroundwaterProductivity[c("X", "Y", "Productivitywater")], crs = 4326)

# Extract mean value within each cluster
clusters$gr_wat_productivity <- exactextractr::exact_extract(groundwater_Productivity, clusters, fun="mean")

##########

# To fix potential bugs in the data, delete negative values
clusters <- clusters %>% group_by(sov_a3) %>% mutate(gr_wat_depth=ifelse(is.na(gr_wat_depth), mean(gr_wat_depth, na.rm=T), gr_wat_depth)) %>% ungroup()

clusters <- clusters %>% group_by(sov_a3) %>% mutate(surfw_dist=ifelse(is.nan(surfw_dist), mean(surfw_dist, na.rm=T), surfw_dist)) %>% ungroup()

#clusters$surfw_dist <- ifelse(clusters$slope > slope_limit, Inf, clusters$surfw_dist) # an excessive slope renders groundwater pumping not feasible

clusters$surfw_dist = ifelse(clusters$surfw_dist>threshold_surfacewater_distance, Inf, clusters$surfw_dist)

clusters$gr_wat_depth = ifelse(clusters$gr_wat_depth>threshold_groundwater_pumping, Inf, clusters$gr_wat_depth)

#########

clusters$discharge_avg_m3h <-  exact_extract(img_03, clusters, fun="mean")

clusters$volume_m3 <-  exact_extract(img_04 * 1000, clusters, fun="mean")

########

# apply surface water abstraction constraint

for (i in 1:12){
  
  aa <- clusters
  aa = st_set_geometry(aa, NULL)
  
  aa$meanGQ = as.vector(rowMeans(as.matrix(aa[grepl("^monthly_GQ", colnames(aa)) & !grepl("surf", colnames(aa))])))
  
  aa[paste0('ratio' , "_" , as.character(i))] = pull(aa[paste0('monthly_GQ' , "_" , as.character(i))]) / aa$meanGQ
  
  clusters[paste0('discharge_avg_m3h' , "_" , as.character(i))] = clusters$discharge_avg_m3h * pull(aa[paste0('ratio' , "_" , as.character(i))])
  
  
}

if(scenarios$water_sustainability_contraint[scenario]==T){
  
  for (i in 1:12){
    
    aa <- clusters
    aa = st_set_geometry(aa, NULL)
    
    clusters[paste0('monthly_GQ_surf' , "_" , as.character(i))] = pull(aa[paste0('discharge_avg_m3h' , "_" , as.character(i))]) # share of discharge capturable 
    
    aa <- clusters
    aa = st_set_geometry(aa, NULL)
    
    clusters[paste0('monthly_unmet_IRRIG_share_surf' , "_" , as.character(i))] <- as.numeric(ifelse((unlist(aa[paste0('monthly_GQ_surf' , "_" , as.character(i))]) < unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]))==TRUE, (unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]) - unlist(aa[paste0('monthly_GQ_surf' , "_" , as.character(i))]))/ unlist(aa[paste0('monthly_IRREQ' , "_" , as.character(i))]), 0))
    
  }} else{
  
  for (i in 1:12){
    
    aa <- clusters
    aa = st_set_geometry(aa, NULL)
    
    clusters[paste0('monthly_GQ_surf' , "_" , as.character(i))] = pull(aa[paste0('discharge_avg_m3h' , "_" , as.character(i))])
    
    aa <- clusters
    aa = st_set_geometry(aa, NULL)
    
    clusters[paste0('monthly_unmet_IRRIG_share_surf' , "_" , as.character(i))] <- 0
    
  }
}

######################

# Calculate average water pumps flow rate required in m3/h in each month
for (i in c(1:12)){

  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  if(scenarios$water_sustainability_contraint[scenario]==F){
  
  clusters[paste0("q" , as.character(i))] = aa[paste0('monthly_IRREQ' , "_" , as.character(i))] / (30/irrigation_frequency_days)/nhours_irr}
  
  else{
    
    clusters[paste0("q" , as.character(i))] = (aa[paste0('monthly_IRREQ' , "_" , as.character(i))]     * (1- aa[paste0('monthly_unmet_IRRIG_share' , "_" , as.character(i))])
)/ (30/irrigation_frequency_days)/nhours_irr
    
  }
  
}

# npumps required
aa <- clusters
aa= st_set_geometry(aa, NULL)
clusters$maxq =NULL

clusters$maxq <- as.numeric(as.vector(rowMax(as.matrix(aa[grepl("^q", colnames(aa))]))))

clusters$npumps_gw <- ceiling(clusters$maxq / clusters$maxflow)

clusters$npumps_gw <- ifelse((is.infinite(clusters$gr_wat_depth)), 0, clusters$npumps_gw)

# ground water pumping

for (i in 1:12){
  print(i)
  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  # RGH to estimate power for pump (in kW), missing the head losses
  clusters[paste0('powerforpump', as.character(i))] = ifelse(clusters$npumps_gw>0, (((rho* g * clusters$gr_wat_depth* pull(aa[paste0("q", as.character(i))])/ clusters$npumps_gw)/(3.6*10^6))/eta_pump)/eta_motor, 0)

  aa <- clusters
  aa= st_set_geometry(aa, NULL)
  
  clusters$powerforpump =NULL
  clusters$powerforpump <- as.vector(rowMax(as.matrix(aa[grepl("^powerforpump", colnames(aa))])))
  
  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  #Calculate monthly electric requirement
  clusters[paste0('wh_monthly', as.character(i))] = pull(aa[paste0('powerforpump')])*nhours_irr*(30/irrigation_frequency_days)

  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  clusters[paste0('er_kwh' , as.character(i))] = aa[paste0('wh_monthly', as.character(i))]

  aa <- clusters
  aa= st_set_geometry(aa, NULL)

}

# surface water pumping

for (i in c(1:12)){
  aa <- clusters
  aa= st_set_geometry(aa, NULL)
  
  if(scenarios$water_sustainability_contraint[scenario]==F){
    
    clusters[paste0("q" , as.character(i))] = aa[paste0('monthly_IRREQ' , "_" , as.character(i))] / (30/irrigation_frequency_days)/nhours_irr}
  
  else{
    
    clusters[paste0("q" , as.character(i))] = (aa[paste0('monthly_IRREQ' , "_" , as.character(i))]     * (1- aa[paste0('monthly_unmet_IRRIG_share_surf' , "_" , as.character(i))])
    )/ (30/irrigation_frequency_days)/nhours_irr
    
  }
  
}

# npumps required
aa <- clusters
aa= st_set_geometry(aa, NULL)
clusters$maxq <- NULL

clusters$maxq <- as.numeric(as.vector(rowMax(as.matrix(aa[grepl("^q", colnames(aa))]))))

clusters$npumps_sw <- ceiling(clusters$maxq / clusters$maxflow)

clusters$npumps_sw <- ifelse((is.infinite(clusters$surfw_dist)), 0, clusters$npumps_sw)

for (i in 1:12){
  print(i)
  aa <- clusters
  aa= st_set_geometry(aa, NULL)
  
  v = (pull(aa[paste0("q", as.character(i))]) / aa$npumps_sw) / (3600 * pi * (pipe_diameter/2)^2)
  
  delta_p_psi <- ((v^2 * pull(aa["surfw_dist"]) * (1+(pull(aa["slope"]))/100)) / (2 * pipe_diameter)) 
  
  clusters[paste0("surfw_w", as.character(i))] = ifelse((clusters$npumps_sw>0 & is.finite(clusters$surfw_dist)), (((delta_p_psi * (pull(aa[paste0("q", as.character(i))]) / aa$npumps_sw))/(3.6*10^6))/eta_pump)/eta_motor, 0)
  
  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  clusters$surfw_w =NULL
  clusters$surfw_w <- as.vector(rowMax(as.matrix(aa[grepl("^surfw_w", colnames(aa))])))
  
  aa <- clusters
  aa= st_set_geometry(aa, NULL)

  clusters[paste0('surface_er_kwh', as.character(i))] = aa[paste0('surfw_w', as.character(i))]*nhours_irr*(30/irrigation_frequency_days)

  }

######################
# select less energy intensive option between surface and groundwater pumping

aa <- clusters
aa= st_set_geometry(aa, NULL)

clusters[paste0('er_kwh_tt')] <- NULL
clusters[paste0('surface_er_kwh_tt')] <- NULL
  
clusters[paste0('er_kwh_tt')] <- as.numeric(rowSums(aa[,grep("^er_kwh", colnames(aa))], na.rm = T))
clusters[paste0('surface_er_kwh_tt')] <- as.numeric(rowSums(aa[,grep("^surface_er_kwh", colnames(aa))], na.rm = T))

clusters$er_kwh_tt <- ifelse(clusters$npumps_gw==0 | clusters$powerforpump==0, NA, clusters$er_kwh_tt)

clusters$surface_er_kwh_tt <- ifelse(clusters$npumps_sw==0 | clusters$surfw_w==0, NA, clusters$surface_er_kwh_tt)

# make less energy intensive source the default

clusters$which_pumping <- NA
clusters$which_pumping[clusters$er_kwh_tt<clusters$surface_er_kwh_tt | (!is.na(clusters$er_kwh_tt) & is.na(clusters$surface_er_kwh_tt))] <- "Ground water pumping"
clusters$which_pumping[clusters$surface_er_kwh_tt<clusters$er_kwh_tt | (!is.na(clusters$surface_er_kwh_tt) & is.na(clusters$er_kwh_tt))] <- "Surface water pumping"
clusters$which_pumping[(is.na(clusters$which_pumping) | (is.infinite(clusters$er_kwh_tt) & is.infinite(clusters$surface_er_kwh_tt))| (clusters$er_kwh_tt==0 & clusters$surface_er_kwh_tt==0))] <- "Neither possible"

table(clusters$which_pumping)

#

summary(clusters$powerforpump[clusters$npumps_gw>0])
summary(clusters$surfw_w[clusters$npumps_sw>0])

summary(clusters$er_kwh_tt[clusters$npumps_gw>0])
summary(clusters$surface_er_kwh_tt[clusters$npumps_sw>0])

#

for (i in 1:12){
  
  aa <- clusters
  aa= st_set_geometry(aa, NULL)
  
  clusters[paste0("er_kwh", as.character(i))] = ifelse(aa["which_pumping"]=="Ground water pumping", pull(aa[paste0("er_kwh", as.character(i))]), ifelse(aa["which_pumping"]=="Surface water pumping", pull(aa[paste0("surface_er_kwh", as.character(i))]), NA))
  
}

clusters$er_kwh_tt <- NULL

aa <- clusters
aa= st_set_geometry(aa, NULL)

clusters[paste0('er_kwh_tt')] <- as.numeric(rowSums(aa[,grep("^er_kwh", colnames(aa))], na.rm = T))
clusters$er_kwh_tt <- ifelse(clusters$npumps_gw==0 | is.infinite(clusters$gr_wat_depth), NA, clusters$er_kwh_tt)

clusters$powerforpump = ifelse(clusters$which_pumping=="Ground water pumping", clusters$powerforpump, ifelse(clusters$which_pumping=="Surface water pumping", clusters$surfw_w, NA))

clusters$npumps = ifelse(clusters$which_pumping=="Ground water pumping", clusters$npumps_gw, ifelse(clusters$which_pumping=="Surface water pumping", clusters$npumps_sw, 0))

summary(clusters$powerforpump[clusters$npumps>0])
summary(clusters$er_kwh_tt[clusters$npumps>0])

sum(clusters$powerforpump * clusters$npumps, na.rm=T)/1e3 # MW
sum(clusters$er_kwh_tt * clusters$npumps, na.rm=T)/1e9 # TWh

clusters <- filter(clusters, npumps>0 & er_kwh_tt>0)

write_rds(clusters, paste0("clusters_with_data_2_", paste(scenarios[scenario,], collapse="_"), ".Rds"))
