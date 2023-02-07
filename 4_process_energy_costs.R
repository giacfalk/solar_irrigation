# 4.1 import the PV cost layers

rr <- list.files(paste0(input_folder, "pv_cost"), full.names = T, pattern = "tif")

df <- data.frame(rr)
df$name <- gsub(".tif", "", df$rr)
df$name <- sub('.*/', '', df$name)

df$crop <- sub("\\&.*", "", df$name)
df$irr <- sub('.*&', '', df$name)

# 4.2 cluster the MapSPAM crops to match the crop-technology couplets from the Earth's future paper to all the crops
crops = readxl::read_xlsx(paste0("D:/MLED_database/" , 'crops_cfs_ndays_months.xlsx')) %>% dplyr::select(crop, eta_irr)

crops$most_sim <- c("Wheat", "Banana", "Maize", "Banana", "Maize", "Maize", "Onions", "Wheat", "Maize", "Commonbeans", "Sugarcane", "Wheat", "Onions")

crops$irr_type <- ifelse(crops$eta_irr==0.6, "flood", "drip")
crops <- merge(df, crops, by.x=c("crop"), by.y=c("most_sim"))

# prevalent crop in each grid cell
files = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)

files <- files[tolower(as.character(substr(basename(files), 20, 23))) %in% crops$crop.y]

for (X in files){
  a = paste0("A_" , tolower(as.character(substr(basename(files), 20, 23))))
  clusters[a] <- exactextractr::exact_extract(raster(X), clusters, fun="sum")
}

aa <- clusters
aa= st_set_geometry(aa, NULL)
clusters$maxq <-  colnames(aa)[grepl("A_", colnames(aa))][apply(aa[grepl("A_", colnames(aa))],1,which.max)]
clusters$maxq <- gsub("A_", "", clusters$maxq)
clusters$maxq <- gsub("_r", "", clusters$maxq)

clusters <- merge(clusters, data.frame(crops[,c(2, 5)]), by.y="crop.y", by.x="maxq", all.x=T)

clusters <- clusters[!duplicated(clusters$cl_id), ]

clusters$rr <- ifelse(is.na(clusters$rr), names(sort(-table(clusters$rr)))[1], clusters$rr)

##

list <- as.character(unique(clusters$rr))
list <- ifelse(is.na(list), "D:/MLED_database/input_folder/pv_cost/Maize&flood.tif", list)
list2 <- pblapply(list, raster)

clusters$pvcost <- NA

for (i in 1:length(list2)){
  clusters[names(list2[[i]])] <- exact_extract(list2[[i]], clusters, fun="mean")
}

for (i in 1:nrow(clusters)){
  clusters$pvcost[i] <-   pull(clusters[i,(which(grepl("pvcost", colnames(clusters))))+match(as.character(clusters$rr[i]), list)] %>% st_set_geometry(NULL))
}

# extrapolate where NA
# extract solar radiation

files <- list.files(path=paste0(input_folder, "pv_cost"), pattern="asc", full.names = T)

for (i in 1:length(files)){
sr <- raster(files[i])
clusters[paste0("sr_", i)] <- exact_extract(sr, clusters, "mean")
}

# regress on solar radiation and prevalent crop and X and Y and predict

clusters <- bind_cols(clusters, data.frame(st_coordinates(st_centroid(clusters))))

model <- lm(pvcost ~ sr_1 + sr_2 + sr_3 + sr_4 + sr_5 + sr_6 + sr_7 + sr_8 + sr_9 + sr_10 + sr_11 + sr_12 + X*Y , data=clusters)
#summary(model)
pred <- predict(model, clusters)

clusters$pvcost <- ifelse(is.nan(clusters$pvcost), pred, clusters$pvcost)

###

if(nrow(clusters)!=nrow(read.csv("pvout_t.csv"))){pvout_t <- stack(pblapply(list.files(paste0(input_folder, "monthly"), full.names = T, pattern = "asc"), raster)) ; pvout_t <-  exact_extract(pvout_t, clusters, "mean"); pvout_t <- (pvout_t/30) * 0.65; write.csv(pvout_t, "pvout_t.csv")} else{
  pvout_t <- read.csv("pvout_t.csv")
  pvout_t$X<- NULL
}

##

clusters <- merge(clusters, vat_import, by.x="sov_a3", by.y="ISO3", all.x=T)
clusters$VAT_PV <- ifelse(is.na(clusters$VAT_PV), mean(vat_import$VAT_PV), clusters$VAT_PV)
clusters$VAT_BATTERY <- ifelse(is.na(clusters$VAT_BATTERY), mean(vat_import$VAT_BATTERY), clusters$VAT_BATTERY)
clusters$IMPORT_PV <- ifelse(is.na(clusters$IMPORT_PV), mean(vat_import$IMPORT_PV), clusters$IMPORT_PV)
clusters$IMPORT_BATTERY <- ifelse(is.na(clusters$IMPORT_BATTERY), mean(vat_import$IMPORT_BATTERY), clusters$IMPORT_BATTERY)

clusters$totalcost = NA
clusters$landuse_m2 = NA
clusters$pvsize_kw = NA
clusters$batterysize_kwh = NA
clusters$share_pvout_used_for_pumping = NA
clusters$residual_pvout_notused_for_pumping = NA

library(insol)

dem_t <- clusters %>% dplyr::select(starts_with("er_kwh")) %>% st_set_geometry(NULL)
dem_t <- dem_t[,c(1:12)]

set_obs <- which(clusters$er_kwh_tt>0)

for (ncls in set_obs){

lat = clusters$Y[ncls]
long =  clusters$X[ncls]
powerpump <- clusters$powerforpump[ncls] # power of the pump, in KW

dem <- as.numeric(dem_t[ncls,]) / (30/irrigation_frequency_days) # daily demand, in kwh for each day of each month
pvout <- as.numeric(pvout_t[ncls,]) # daily generation potential (kWh/kWp) for each day of each month
sunlighthours <- (daylength(lat=lat,long=long,JD(seq(ISOdate(2019,1,1),ISOdate(2019,12,31),by='month')),tmz=1)[,3]) * 0.65 # number of sunlight hours at 100% potential equivalent ("peak sun") per month

usehours_which <- which(load_curve_irrig!=0) # number of hours when the pump is operating
usehours <- length(usehours_which)

mat <- daylength(lat=lat,long=long,JD(seq(ISOdate(2019,1,1),ISOdate(2019,12,31),by='month')),tmz=1)

out <- matrix(ncol=length(usehours_which), nrow=12)

k = 1
for (i in usehours_which){
  out[,k] <-  cbind( mat[,1] <= i & mat[,2] >= i )
  k = k+1
}

if (scenarios$water_tank_storage[scenario]==T){
  
  out <- ifelse(out==FALSE, TRUE, TRUE)
  
}

demand_no_supply_hours <- usehours - rowSums(out) # number of hours when the pump use is not in sunlight hours

######################

if (scenarios$water_tank_storage[scenario]==T){
  
toprovided_withbatteries_daily <- 0.2 * dem # demand to be supplied when no generation potential is available

} else{ toprovided_withbatteries_daily <- (demand_no_supply_hours/usehours) * dem 

}

########

if (scenarios$no_battery[scenario]==F){
  
  toprovided_withbatteries_daily <- 0 
  
} else{ toprovided_withbatteries_daily <- (demand_no_supply_hours/usehours) * dem 

}

if (scenarios$no_battery[scenario]==F){
  
  toprovided_directly_from_pv_daily <- dem # demand to be supplied directly via PV
  
} else{ toprovided_directly_from_pv_daily <- (1-(demand_no_supply_hours/usehours)) * dem 

}

# pv size
kwh_per_hour_direct_use <- toprovided_directly_from_pv_daily / (usehours - demand_no_supply_hours)

wp_direct_use <- (kwh_per_hour_direct_use / (pvout/sunlighthours)) * 1000 * 1.1 # energy lost in the system

kwh_per_hour_battery <- toprovided_withbatteries_daily / (sunlighthours - (usehours - demand_no_supply_hours))

wp_to_charge_battery <- (kwh_per_hour_battery / (pvout/(sunlighthours - (usehours - demand_no_supply_hours)))) * 1000 * 1.2 # energy lost in the system

wp <- max(wp_direct_use, wp_to_charge_battery) # required pv size
kwp <- wp/1000

# actual use of power for pumping

share_pvout_used_for_pumping  <- sum((kwh_per_hour_direct_use * usehours), na.rm=T) / sum(((pvout * kwp) - (kwh_per_hour_direct_use * usehours)), na.rm=T)

clusters$share_pvout_used_for_pumping[ncls] = ifelse(!is.finite(share_pvout_used_for_pumping), NA, share_pvout_used_for_pumping)

residual_pvout_notused_for_pumping <- (pvout * kwp) - (kwh_per_hour_direct_use * usehours)

clusters$residual_pvout_notused_for_pumping[ncls] = sum(residual_pvout_notused_for_pumping * 30, na.rm=T)

# battery size

kwh_battery <- max((toprovided_withbatteries_daily / 0.85 / 0.8)) # required battery size, net of battery efficiency, depth of discharge, extra size for unexpected low generation buffer

kwh_battery <- kwh_battery + (max(dem)*1) # extra days of autonomy

# inverter size

inverter_size <- powerpump * 1.3 #W

## plots

# plot(dem, type="l", col="red")
# plot(pvout, type="l", col="blue")
# plot(sunlighthours, type="l", col="red")
# plot(toprovided_directly_from_pv_daily, type="l", col="red")
# plot(toprovided_withbatteries_daily, type="l", col="red")
# plot(kwp, kwh_battery)

################
# 2. estimate costs of installing the system and O&M

usdperkwhbattery <- 500

if (scenarios$instalments_business_model[scenario]==4 | scenarios$instalments_business_model[scenario]==3){

  pvcost <- npv(discount_rate, rep(((wp * clusters$pvcost[ncls] ) / lifetimepump)), 0:(lifetimepump-1))
  
  storcost <- npv(discount_rate, rep(((kwh_battery * usdperkwhbattery) / lifetimepump)), 0:(lifetimepump-1))
  
  
} else{

  pvcost <- wp * clusters$pvcost[ncls]
  
  storcost <- kwh_battery * usdperkwhbattery
  
}

if (scenarios$VAT_import_costs[scenario]==T){

  pvcost <- pvcost * ((1 + clusters$VAT_PV[ncls]/100)) + (1*(clusters$IMPORT_PV[ncls]/100))

  storcost <- storcost * ((1 + clusters$VAT_BATTERY[ncls]/100)) + (1 + (clusters$IMPORT_BATTERY[ncls]/100))

}

if (scenarios$no_battery[scenario]==F){
  
  storcost <- 0 
  
}

om_cost <- npv(discount_rate, rep((pvcost*0.1), lifetimepump), 0:(lifetimepump-1))

inverter_cost <- inverter_size * 0.2 #usd per watt

inverter_cost <- ifelse(clusters$gr_wat_depth[ncls]<=10 | clusters$which_pumping[ncls]=="Surface water pumping", 0, inverter_cost) #DC used if water is easily accessible, so no need for an inverter

ch_contr_cost <- (pvcost + om_cost + storcost  + inverter_cost) * 0.1

totalcost <- (pvcost + om_cost + storcost + ch_contr_cost  + inverter_cost) * clusters$npumps[ncls]

#

landuse_m2 <- (kwp * 5 + ifelse(kwp>0, 9, 0)) * clusters$npumps[ncls] # m2 + space for charge controller, inverter, pump

clusters$totalcost[ncls] = totalcost
clusters$landuse_m2[ncls] = landuse_m2
clusters$pvsize_kw[ncls] = kwp
clusters$batterysize_kwh[ncls] = kwh_battery
}

clusters$totalcost = ifelse(clusters$yearly_IRREQ==0, NA, clusters$totalcost)
clusters$pvsize_kw = ifelse(clusters$yearly_IRREQ==0, NA, clusters$pvsize_kw)
clusters$batterysize_kwh = ifelse(clusters$yearly_IRREQ==0, NA, clusters$batterysize_kwh)
clusters$landuse_m2 = ifelse(clusters$yearly_IRREQ==0, NA, clusters$landuse_m2)

summary(clusters$totalcost[clusters$totalcost>0]/clusters$npumps[clusters$totalcost>0], na.rm=T)
summary(clusters$pvsize_kw[clusters$pvsize_kw!=0])
summary(clusters$batterysize_kwh[clusters$batterysize_kwh!=0])

write_rds(clusters, paste0("clusters_with_data_4_", paste(scenarios[scenario,], collapse="_"), ".Rds"))
