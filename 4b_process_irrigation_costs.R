#clusters <- read_rds(paste0("clusters_with_data_4_", paste(scenarios[scenario,], collapse="_"), ".Rds"))

# irrigation system costs depending on irrigation type, which depend on the crop, and the related efficiency 

# extracted rainfed cropland area

files = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
files_crops <- tolower(unlist(qdapRegex::ex_between(files, "SSA_H_", "_R.tif")))
files_crops <- files_crops %in% c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")
files <- files[files_crops]

# mm to m3 -> 1 mm supplies 0.001 m3 per m^2 of soil

files <- pblapply(files, raster)
gc()

out <- list()

for (i in 1:length(files)){
out[[i]] <- exact_extract(files[[i]], clusters, "sum")
}

out <- bind_cols(out)
colnames(out) <- paste0("A_", c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams"), "_r")

clusters <- bind_cols(clusters, out)

# import costs

#1400 * 2.47105 # USD/ha for drip irrigation kit
irr_cost <- read.csv("irr_cost.csv") # https://ars.els-cdn.com/content/image/1-s2.0-S146290112200140X-mmc1.pdf table SI3

crops_effs_irrtype <- read_xlsx("crops_irrtype.xlsx")
crops_effs_irrtype$irr_type[crops_effs_irrtype$irr_type=="surface"] <- "flood"

crops_effs_irrtype <- merge(crops_effs_irrtype, irr_cost, by.x="irr_type", by.y="irr_tech")

# multiply rainfed ha, by crop, per irr_cost

aa <- clusters
aa= st_set_geometry(aa, NULL)

clusters$monthly_unmet_IRRIG_share_avg = as.vector(rowMeans(as.matrix(aa[grepl("^monthly_unmet_IRRIG_share_", colnames(aa)) & !grepl("surf", colnames(aa))])))
clusters$monthly_unmet_IRRIG_share_surf_avg = as.vector(rowMeans(as.matrix(aa[grepl("^monthly_unmet_IRRIG_share_", colnames(aa)) & grepl("surf", colnames(aa))])))

aa <- clusters
aa= st_set_geometry(aa, NULL)

for (crop in c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")){
  
  if(scenarios$water_sustainability_contraint[scenario]==T){
  
clusters[paste0("inv_c_", crop)] <- pull(aa[paste0("A_", crop, "_r")])  * ifelse(clusters$which_pumping=="Ground water pumping", (1-clusters$monthly_unmet_IRRIG_share_avg), (1-clusters$monthly_unmet_IRRIG_share_surf_avg)) * crops_effs_irrtype$inv_per_ha[crops_effs_irrtype$crop==crop]  #* 1.25 # deflator

  } else{
  
    clusters[paste0("inv_c_", crop)] <- pull(aa[paste0("A_", crop, "_r")]) * crops_effs_irrtype$inv_per_ha[crops_effs_irrtype$crop==crop]  #* 1.25 # deflator
}

}

aa <- clusters
aa= st_set_geometry(aa, NULL)

for (crop in c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")){
  
  if(scenarios$water_sustainability_contraint[scenario]==T){
  
  clusters[paste0("yrl_c_", crop)] <- (pull(aa[paste0("A_", crop, "_r")]) * crops_effs_irrtype$fix_per_ha_yr[crops_effs_irrtype$crop==crop]) + (pull(aa[paste0("A_", crop, "_r")])  * ifelse(clusters$which_pumping=="Ground water pumping", (1-clusters$monthly_unmet_IRRIG_share_avg), (1-clusters$monthly_unmet_IRRIG_share_surf_avg)) * crops_effs_irrtype$var_per_ha_yr[crops_effs_irrtype$crop==crop])# * 1.25)
  
  } else{
    
    clusters[paste0("yrl_c_", crop)] <- (pull(aa[paste0("A_", crop, "_r")]) * crops_effs_irrtype$fix_per_ha_yr[crops_effs_irrtype$crop==crop]) + (pull(aa[paste0("A_", crop, "_r")])  * crops_effs_irrtype$var_per_ha_yr[crops_effs_irrtype$crop==crop])
    
    
  }
  
}

##### Production cost and its delta from rainfed to irrigated
# crop production costs (USD/ha)
# seeds, labor to irrigate, fertilizer, pest management, etc. (operational costs induced by irrigation)

#devtools::install_github('https://github.com/USDA-ERS/MTED-HARr.git')
require(HARr)

data = read_har('G:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/CGE/DB_eco_GTAP10_2014.har')

dimnames(data[["sf01"]])

ctrs <- dimnames(data[["sf01"]])$reg[toupper(dimnames(data[["sf01"]])$reg) %in% clusters$sov_a3]
crps <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr")
  
# 1	pdr	Paddy rice
# 2	wht	Wheat
# 3	gro	Cereal grains nec
# 4	v_f	Vegetables, fruit, nuts
# 5	osd	Oil seeds
# 6	c_b	Sugar cane, sugar beet
# 7	pfb	Plant-based fibers
# 8	ocr	Crops nec

# data[["sf01"]]
# 
# In USD (nominal)
# 
# 1)	Factors for production
# 2)	Sector
# 3)	Regions
# 4)	Domestic/import
# 5)	Spesa/taxed input

x <- rep(NA, length(ctrs)*length(crps))

m <- matrix(x, nrow = length(ctrs), byrow = TRUE, 
            dimnames = list(ctrs, crps))

for (i in ctrs){
  
  for (j in crps){
  
    m[i,j] <- sum(data[["sf01"]][,j,i,1,1]) * 1e6  #convert to USD (2014 year)
    
  }}

data2 = read_har('G:/.shortcut-targets-by-id/13znqeVDfPULc4J_lQLbyW_Kmfa03o63F/3-Research/GIACOMO/CGE/DB_AEZ_GTAP10_2014.har')

dimnames(data2[["area"]])

x <- rep(NA, length(ctrs)*length(crps))

m2 <- matrix(x, nrow = length(ctrs), byrow = TRUE, 
            dimnames = list(ctrs, crps))

for (i in ctrs){
  
  for (j in crps){
    
    m2[i,j] <- sum(data2[["area"]][,j,i]) * 1e3 #convert to ha
    
  }}

marginal_cost <- m/m2 #cost per ha
marginal_cost[is.infinite(marginal_cost)] <- NA
marginal_cost

mapper <- data.frame(crop=c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams"), category=NA)

mapper$category <- c("gro", "ocr", "v_f", "pfb", "v_f", "gro", "gro", "gro", "osd", "ocr", "osd", "pdr", "gro", "ocr", "c_b", "c_b", "osd", "wht", "ocr")

marginal_cost <- as.data.frame(marginal_cost)
marginal_cost$sov_a3 <- rownames(marginal_cost)
marginal_cost <- melt(marginal_cost, "sov_a3")
colnames(marginal_cost) <- c("sov_a3", "category", "marginal_prod_cost")

mapper <- merge(mapper, marginal_cost, "category")
mapper$category <- NULL
mapper = mapper %>% group_by(crop) %>% mutate(marginal_prod_cost=median(marginal_prod_cost, na.rm=T))
mapper$sov_a3 <- toupper(mapper$sov_a3)

library(tidyr)

mapper_w_r <- spread(mapper, key = crop, value = marginal_prod_cost)
colnames(mapper_w_r)[-1] <- paste0("marg_prod_cost_", colnames(mapper_w_r)[-1])

### how to derive markup of when scaling up to irrigated cropland?
# https://data.worldbank.org/indicator/AG.LND.IRIG.AG.ZS

ctrs <- c("bgd", "pak")
crps <- c("pdr", "wht", "gro", "v_f", "osd", "c_b", "pfb", "ocr")

x <- rep(NA, length(ctrs)*length(crps))

m <- matrix(x, nrow = length(ctrs), byrow = TRUE, 
            dimnames = list(ctrs, crps))

for (i in ctrs){
  
  for (j in crps){
    
    m[i,j] <- sum(data[["sf01"]][,j,i,1,1]) * 1e6  #convert to USD (2014 year)
    
  }}

x <- rep(NA, length(ctrs)*length(crps))

m2 <- matrix(x, nrow = length(ctrs), byrow = TRUE, 
             dimnames = list(ctrs, crps))

for (i in ctrs){
  
  for (j in crps){
    
    m2[i,j] <- sum(data2[["area"]][,j,i]) * 1e3 #convert to ha
    
  }}

marginal_cost_irr <- m/m2 #cost per ha
marginal_cost_irr[is.infinite(marginal_cost_irr)] <- NA

marginal_cost_irr <- colMeans(marginal_cost_irr)
marginal_cost_irr <- matrix(marginal_cost_irr, nrow = 1)
rownames(marginal_cost_irr) <- "avg_irr"
colnames(marginal_cost_irr) <- crps

mapper <- data.frame(crop=c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams"), category=NA)

mapper$category <- c("gro", "ocr", "v_f", "pfb", "v_f", "gro", "gro", "gro", "osd", "ocr", "osd", "pdr", "gro", "ocr", "c_b", "c_b", "osd", "wht", "ocr")

marginal_cost_irr <- as.data.frame(marginal_cost_irr)
marginal_cost_irr$sov_a3 <- rownames(marginal_cost_irr)
marginal_cost_irr <- melt(marginal_cost_irr, "sov_a3")
colnames(marginal_cost_irr) <- c("sov_a3", "category", "marginal_prod_cost")

mapper <- merge(mapper, marginal_cost_irr, "category")
mapper$category <- NULL
mapper = mapper %>% group_by(crop) %>% mutate(marginal_prod_cost=mean(marginal_prod_cost, na.rm=T))
mapper$sov_a3 <- toupper(mapper$sov_a3)

mapper_w_i <- spread(mapper, key = crop, value = marginal_prod_cost)
colnames(mapper_w_i)[-1] <- paste0("marg_prod_cost_irr_", colnames(mapper_w_i)[-1])

mapper_w_diff <- mapper_w_r

for (i in 1:nrow(mapper_w_r)){
mapper_w_diff[i,-1] <- mapper_w_i[1,-1] - mapper_w_r[i,-1]
}

mapper_w_diff[mapper_w_diff<0] <- 0

# # replace outliers with median in each column
# for (i in 2:ncol(mapper_w_diff)){
# high <- mean(pull(mapper_w_diff[,i])) + sd(pull(mapper_w_diff[,i])) * 3
# low <- mean(pull(mapper_w_diff[,i])) - sd(pull(mapper_w_diff[,i])) * 3
# outliers <- (pull(mapper_w_diff[,i]) < low | pull(mapper_w_diff[,i]) > high)
# mapper_w_diff[outliers,i] = median(pull(mapper_w_diff[!outliers,i]))
# }
# 
# mapper_w_diff <-  mapper_w_diff %>%  mutate(across(.cols = marg_prod_cost_barl:marg_prod_cost_yams, 
#                            .fns = ~ifelse(.x == 0, mean(.x), .x)))

# colmeans
# for (i in 2:ncol(mapper_w_diff)){
# high <- mean(pull(mapper_w_diff[,i])) + sd(pull(mapper_w_diff[,i])) * 3
# low <- mean(pull(mapper_w_diff[,i])) - sd(pull(mapper_w_diff[,i])) * 3
# outliers <- (pull(mapper_w_diff[,i]) < low | pull(mapper_w_diff[,i]) > high)
# mapper_w_diff[outliers,i] = median(pull(mapper_w_diff[!outliers,i]))
# }

clusters <- merge(clusters, mapper_w_diff, "sov_a3", all.x=T)

aa <- clusters
aa= st_set_geometry(aa, NULL)

for (crop in mapper$crop){
  clusters[,paste0("marg_prod_cost_irr_", crop)] <- ifelse(is.na(aa[,paste0("marg_prod_cost_", crop)]), median(aa[,paste0("marg_prod_cost_", crop)], na.rm=T), aa[,paste0("marg_prod_cost_", crop)]) * 1.13 # deflator and to PPP
}

aa <- clusters
aa= st_set_geometry(aa, NULL)

for (crop in mapper$crop){
  
  if(scenarios$water_sustainability_contraint[scenario]==T){
  
clusters[,paste0("yearly_prod_cost_irr_", crop)] <- aa[,paste0("marg_prod_cost_", crop)] * aa[,paste0("A_", crop, "_r")]  * ifelse(clusters$which_pumping=="Ground water pumping", (1-clusters$monthly_unmet_IRRIG_share_avg), (1-clusters$monthly_unmet_IRRIG_share_surf_avg)) }

else{
  
  clusters[,paste0("yearly_prod_cost_irr_", crop)] <- aa[,paste0("marg_prod_cost_", crop)] * aa[,paste0("A_", crop, "_r")]
  
}
}

write_rds(clusters, paste0("clusters_with_data_4b_", paste(scenarios[scenario,], collapse="_"), ".Rds"))


#############################################
##### Installation

# water infrastructure development for surface water irrigation (e.g. costs for reservoir, canal and pond development and operation): 8000 to 80000 USD/ha --> dependant on distance and slope to reservoir 

# https://www.fao.org/aquastat/ru/overview/archive/investment-costs

# https://www.gbcma.vic.gov.au/downloads/Farm_Water_Program/2019%20-%20Comparison%20of%20irrigation%20system%20costs.pdf

# https://www.researchgate.net/publication/257574275_Why_invest_in_minor_projects_in_sub-Saharan_Africa_An_exploration_of_the_scale_economy_and_diseconomy_of_irrigation_projects

# https://www.researchgate.net/publication/272484017_comparison_of_estimated_maize_and_cabbage_enterprise_budgets_of_ideal_small-scale_commercial_and_subsistence_farms_in_the_Eastern_Cape_province_of_South_Africa

# https://www.researchgate.net/publication/268045569_Lessons_from_Irrigation_Investment_Experiences_Cost-reducing_and_Performance-enhancing_Options_for_sub-Saharan_Africa

# https://www.pnas.org/doi/10.1073/pnas.1203597110

# https://www.nature.com/articles/s41893-020-00670-7

# https://www.gov.mb.ca/agriculture/farm-management/production-economics/pubs/cop-crop-production.pdf
# file:///C:/Users/falchetta/Downloads/587.pdf
# https://sci-hub.st/https://acsess.onlinelibrary.wiley.com/doi/abs/10.2134/practicalmath2017.0032#:~:text=Cost%20of%20production%20is%20the,%2C%20labor%2C%20machinery%20and%20land.

