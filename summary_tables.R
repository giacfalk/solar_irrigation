# here we generate summary tables by country and major crops to be included in the paper and in the reviewers' replies

if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive)

setwd("C:/Users/falchetta/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

######

baseline <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
baseline$geometry <- NULL

baseline_summary_iso3 <- baseline %>% group_by(sov_a3) %>% dplyr::summarise(ha_rainfed_harvested_area = sum(A_total, na.rm=T)/1e6, ha_rainfed_harvested_area_econ_feas = sum(A_total[!is.na(profit_yearly)], na.rm=T)/1e6, water_irreq  = sum(yearly_IRREQ, na.rm=T)/1e9, water_irreq_per_ha_mm = (sum(yearly_IRREQ, na.rm=T)/sum(A_total, na.rm=T))/10, twh_pumping = sum(er_kwh_tt*npumps, na.rm=T)/1e9, kwh_per_m3 = sum(er_kwh_tt*npumps, na.rm=T) / sum(yearly_IRREQ, na.rm=T), feasible_pumps_thousands = sum(npumps[!is.na(profit_yearly)], na.rm=T)/1e3, tot_sys_disc_million_yrly = sum(total_system_cost_discounted_yeary[!is.na(profit_yearly)], na.rm=T)/1e6, total_revenues_discounted_discounted_yearly = sum(total_revenues_discounted_discounted_yearly[!is.na(profit_yearly)], na.rm=T)/1e6, tot_profit_yearly = sum(profit_yearly[!is.na(profit_yearly)], na.rm=T)/1e6)

baseline_summary_iso3 <- na.omit(baseline_summary_iso3)

baseline_summary_iso3[nrow(baseline_summary_iso3)+1, ] <- NA
baseline_summary_iso3[nrow(baseline_summary_iso3), 5] <-  (sum(baseline_summary_iso3$water_irreq *1e9, na.rm=T) / sum(baseline_summary_iso3$ha_rainfed_harvested_area*1e6, na.rm=T))/10

baseline_summary_iso3[nrow(baseline_summary_iso3), 7] <-  sum(baseline$er_kwh_tt*baseline$npumps, na.rm=T) / sum(baseline$yearly_IRREQ, na.rm=T)

baseline_summary_iso3[nrow(baseline_summary_iso3), 1] <- "SSA total"
baseline_summary_iso3[nrow(baseline_summary_iso3), 2] <- sum(baseline_summary_iso3$ha_rainfed_harvested_area, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 3] <- sum(baseline_summary_iso3$ha_rainfed_harvested_area_econ_feas, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 4] <- sum(baseline_summary_iso3$water_irreq, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 6] <- sum(baseline_summary_iso3$twh_pumping, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 8] <- sum(baseline_summary_iso3$feasible_pumps_thousands, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 9] <- sum(baseline_summary_iso3$tot_sys_disc_million_yrly, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 10] <- sum(baseline_summary_iso3$total_revenues_discounted_discounted_yearly, na.rm=T)
baseline_summary_iso3[nrow(baseline_summary_iso3), 11] <- sum(baseline_summary_iso3$tot_profit_yearly, na.rm=T)

baseline_summary_iso3$share_cropland_feasible <- (baseline_summary_iso3$ha_rainfed_harvested_area_econ_feas / baseline_summary_iso3$ha_rainfed_harvested_area) * 100

baseline_summary_iso3 <- baseline_summary_iso3[,c(1,2,3,12, 4, 5, 6, 7, 8, 9, 10, 11)]

colnames(baseline_summary_iso3) <- c("Country ISO3", "Rainfed harvested area (million ha)", "in which solar irrig. is econ. feas. (million ha)", "% econ. feas. area", "Total irigation (blue) water gap (bmc)", "Irrigation (blue) water gap (mm)", "Pumping energy (TWh/yr.)", "Pumping energy (kWh/m3)", "Econ. feas. pumps (million)", "Tot. sys. disc. costs (mil. USD/yr.)", "Tot. sys. disc. revenues (mil. USD/yr.)", "Tot. sys. disc. profits (mil. USD/yr.)")

library(stargazer)

baseline_summary_iso3 <- mutate_if(baseline_summary_iso3, is.numeric, ~round(., 2))

rownames(baseline_summary_iso3) <- NULL

stargazer::stargazer(baseline_summary_iso3, summary = F, out = "new_figures/Table_SI_country.html", type = "html")

######
######

clusters <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

rainfed <- list.files("risultati giacomo", full.names = T, pattern = "closure", recursive=T)
# library(mgsub)
# rainfed_n <- mgsub(rainfed, tolower(month.abb),as.character(1:12))
#file.rename(rainfed, rainfed_n)
rainfed2 <- mixedsort(rainfed)
rainfed2 <- pblapply(rainfed2, raster)
for (i in 1:length(rainfed2)){
  crs(rainfed2[[i]]) <- as.character(CRS("+init=epsg:4236"))
}

rainfed2 <- split(rainfed2,  tolower(unlist(qdapRegex::ex_between(rainfed, "giacomo/", "/cl"))))

rainfed <- pblapply(rainfed2, stack)

for (i in 1:length(rainfed)){
  rainfed[[i]][[2]] <- mean(rainfed[[i]][[1]], rainfed[[i]][[3]])
}

# extract total bluewater demand in each cluster

names(rainfed) <- c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

files = list.files(path = paste0("F:/Il mio Drive/MLED_database/input_folder/spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files_crops <- files_crops %in% names(rainfed)
files <- files[files_crops]

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

files <- pblapply(files, raster)
gc()

### 
# apply crop irrigation efficiencies

crops_effs = readxl::read_xlsx(paste0(input_folder, "country_studies/zambia/mled_inputs/crops_cfs_ndays_months_ZMB.xlsx")) %>% dplyr::select(crop, eta_irr)

for (i in 1:length(files)){
  files[[i]] = files[[i]] / crops_effs$eta_irr[match(tolower(unlist(qdapRegex::ex_between(names(stack(files)), "SSA_H_", "_R")))[i], crops_effs$crop)]
}

rainfed <- pblapply(1:length(files), function(X) {stack(rainfed[[X]] * files[[X]] * 10)})

for (m in 1:length(rainfed)){
  
  rainfed[[m]] <- stackApply(rainfed[[m]], 1, fun = sum)
  
}

rainfed_sum <- stack(rainfed)

clusters_crops <- dplyr::select(clusters, sov_a3, geometry)

clusters_crops_ex = exact_extract(rainfed_sum, clusters_crops, "sum")
colnames(clusters_crops_ex) <- c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

clusters_crops <- bind_cols(clusters_crops, clusters_crops_ex)

###

files = list.files(path = paste0("F:/Il mio Drive/MLED_database/input_folder/spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files_crops <- files_crops %in% c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")
files <- files[files_crops]

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

files <- pblapply(files, raster)
gc()

files <- stack(files)

clusters_crops_ex = exact_extract(files, clusters_crops, "sum")

colnames(clusters_crops_ex) <- paste0("A_", c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams"))

colnames(clusters_crops)[1:19] <- paste0("WR_", colnames(clusters_crops)[1:19])

clusters_crops <- bind_cols(clusters_crops, clusters_crops_ex)

clusters_crops$geometry <- NULL

clusters_crops <- dplyr::summarise_all(clusters_crops, sum)

clusters_crops_pw <- pivot_longer(clusters_crops, cols = 1:38, names_to = c("Variable", "Crop"), names_sep = "_")

library(modelsummary)

clusters_crops_pw$value <- ifelse(clusters_crops_pw$Variable=="WR", clusters_crops_pw$value/1e9, clusters_crops_pw$value/1e6)

clusters_crops_pw$Variable <- ifelse(clusters_crops_pw$Variable=="WR", "Total irigation (blue) water gap (bmc)", "Rainfed harvested area (million ha)")


datasummary(Crop ~ Variable * value * mean, data = clusters_crops_pw, output = "new_figures/Table_SI_crop.docx")

################

baseline <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
baseline$geometry <- NULL

baseline_summary_iso3 <- baseline %>% group_by(sov_a3) %>% dplyr::summarise(calories = sum(calories_gain_total[!is.na(profit_yearly)], na.rm=T)/1e9, calories_capita_day = (sum(calories_gain_total[!is.na(profit_yearly)], na.rm=T)/sum(pop, na.rm=T))/365, deficit=mean(Calories_gap, na.rm=T))

baseline_summary_iso3 <- filter(baseline_summary_iso3, !is.na(baseline_summary_iso3$sov_a3))

baseline_summary_iso3$gap_share <- (baseline_summary_iso3$calories_capita_day / baseline_summary_iso3$deficit)*100

View(baseline_summary_iso3)

baseline_summary_iso3 <- baseline_summary_iso3[,c(1,4,2,3,5)]

colnames(baseline_summary_iso3) <- c("Country ISO3", "National caloric deficit (Kcal/capita/day)", "Food generation potential from economically feasible solar pumps (billion Kcal/yr.)", "Food generation potential from economically feasible solar pumps (billion Kcal/capita/day)", "Food generation potential as a share of the national caloric deficit (%)")

library(stargazer)

baseline_summary_iso3 <- mutate_if(baseline_summary_iso3, is.numeric, ~round(., 2))

rownames(baseline_summary_iso3) <- NULL

stargazer::stargazer(baseline_summary_iso3, summary = F, out = "new_figures/Table_SI_food_country.html", type = "html")

