# ------------------------------------------------------------------------------
## Define interpolating function over the aproximated surface
# ------------------------------------------------------------------------------

# Mobilization & Demobilization costs: raw range, proportional to city travel time

img_02 <- raster(paste0(input_folder, "accessibility.tif"))

clusters$traveltime_market = exact_extract(img_02, clusters, "mean")

cost_inv_mob_range <- c(500, 1600)
clusters$cost_inv_mob <- scales::rescale(clusters$traveltime_market, to = cost_inv_mob_range)
clusters$cost_inv_mob <- ifelse(is.na(clusters$cost_inv_mob), mean(clusters$cost_inv_mob, na.rm=T), clusters$cost_inv_mob)

##########################

# Find nearest neighbor interpolating function.
nninterp <- function(q, h, type, yearly_IRREQ, index) {
  
  # Read xlsx of spline surface.
  x = read_xlsx("interp_surface_cost/smooth_q.xlsx", col_names = T)
  y = read_xlsx("interp_surface_cost/smooth_h.xlsx", col_names = T)
  z = read_xlsx("interp_surface_cost/smooth_c.xlsx", col_names = T)
  
  df = data.frame(x, y, z)
  
  # As numeric.
  x = as.vector(x$`spline[["xyz.est"]][["x"]]`)
  y = as.vector(y$`spline[["xyz.est"]][["y"]]`)
  z = as.matrix(z)
  
  # Apply NN.
  near_q = which.min(abs(x-q))
  near_h = which.min(abs(y-h))
  
  # Find pump cost.
  # Convert from GBP to USD.
  
  if (scenarios$instalments_business_model[scenario]==4 | scenarios$instalments_business_model[scenario]==2){
  
  cost_inv_pump = as.numeric(z[near_h,near_q]) * 1.38
  
  } else{
    
    cost_inv_pump =  npv(discount_rate, rep(((as.numeric(z[near_h,near_q]) * 1.38) / lifetimepump)), 0:(lifetimepump-1)) 
    
  }
  
  if (scenarios$water_tank_storage[scenario]==T){
   
  st_tank_cost <-  yearly_IRREQ * 0.075 #- proportional to water requirements; 0.075 usd / liter
     
  } else{st_tank_cost <- 0}
  
  # Fixed costs
  # Siting costs: mean between the only values we have
  cost_inv_sit = mean(c(563, 1000))
  
  # Mobilization & Demobilization costs: raw range, proportional to city travel time
  cost_inv_mob = clusters$cost_inv_mob[index]
  
  ## Variable costs: drilling and casing costs
  # from regression in "groundwater_pumps_costs_FS.r".
  cost_var_dril_cas = 122.1362 * h - 1661.8328
  
  # Variable costs: failure costs.
  # Mean value from Xenarios (see "failure_cost_FS.r"), can refine selecting costs
  # by location (country), but needs country or coordinates as input.

  cost_fail_tot = npv(discount_rate, rep(((31.55 * h) / lifetimepump)), 0:(lifetimepump-1))
  
  # Total cost.
    
  if (type == "Ground water pumping") {
    
    cost_tot = cost_inv_sit + cost_inv_pump + cost_inv_mob + cost_var_dril_cas + cost_fail_tot + st_tank_cost

    } 
  
  if (type == "Surface water pumping") {
      
      if (scenarios$instalments_business_model[scenario]==4 | scenarios$instalments_business_model[scenario]==2){
        
        cost_fail_tot = npv(discount_rate, rep(((31.55 * 15) / lifetimepump)), 0:(lifetimepump-1))
        
        cost_inv_pump = as.numeric(z[which.min(y),near_q]) * 1.38
        cost_tot = cost_inv_pump + cost_inv_mob + cost_fail_tot + st_tank_cost
        
      } else{
        
        cost_fail_tot = npv(discount_rate, rep(((31.55 * 15) / lifetimepump)), 0:(lifetimepump-1))
        
        cost_inv_pump =  npv(discount_rate, rep(((as.numeric(z[which.min(y),near_q]) * 1.38) / lifetimepump)), 0:(lifetimepump-1)) 
        cost_tot = cost_inv_pump + cost_inv_mob + cost_fail_tot + st_tank_cost
        
      }}
    
    return(cost_tot)
  
}

cl <- clusters %>% dplyr::select(starts_with("q")) %>% st_set_geometry(NULL)
cl <- apply(cl, 1, max)

clusters$totalpumpcost <- NA
where_index <- (is.finite(cl/clusters$npumps) & !is.na(cl/clusters$npumps) & clusters$er_kwh_tt>0)

clusters$totalpumpcost[where_index] <- pbapply::pbmapply(nninterp, cl[where_index]/clusters$npumps[where_index], clusters$gr_wat_depth[where_index], clusters$which_pumping[where_index], scales::rescale(clusters$yearly_IRREQ, to=range_tank)[where_index], which(where_index)) * clusters$npumps[where_index]

clusters$totalpumpcost <- ifelse(is.infinite(clusters$totalpumpcost), NA, clusters$totalpumpcost)

write_rds(clusters, paste0("clusters_with_data_3_", paste(scenarios[scenario,], collapse="_"), ".Rds"))

