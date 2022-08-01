gadm <- read_sf((paste0(input_folder, "gadm_africa.shp")))

# import farmgate prices

prices <- read.csv(paste0(input_folder, "country_studies/zambia/mled_inputs/FAOSTAT_data_8-11-2021.csv"))
prices <- filter(prices, (Area!="Sudan (former)" & Area!="Ethiopia PDR"))

# min_max_norm <- function(x) {
#   (x - min(x, na.rm=T)) / (max(x, na.rm=T) - min(x, na.rm=T))
# }
# 
# prices_var <- prices %>% group_by(Area, Item) %>% mutate(Value=min_max_norm(Value)) %>%  dplyr::summarise(PSI=sd(Value, na.rm=T)) %>% ungroup() %>% group_by(Area, Item) %>%  dplyr::summarise(PSI=mean(PSI, na.rm=T)) %>% ungroup()

prices <- prices %>% filter(Year>2000) %>% group_by(Area, Item) %>% dplyr::summarise(Value=median(Value, na.rm=T)) %>% ungroup()

#prices <- merge(prices, prices_var, by=c("Area", "Item"), all.x=T)

# prices$Value_upper <- prices$Value * (1+prices$PSI)
# prices$Value_lower <- prices$Value - (1+prices$PSI)
# 
# prices$Value_upper <- ifelse(is.nan(prices$PSI), prices$Value, prices$Value_upper)
# prices$Value_lower <- ifelse(is.nan(prices$PSI), prices$Value, prices$Value_lower)

###########

parser <- read.csv(paste0(input_folder, "country_studies/zambia/mled_inputs/parser.csv"))

prices <- merge(prices, parser, all.x=T, by="Item")
prices <- na.exclude(prices)
prices <- prices %>% group_by(spam, Area) %>% summarise(Value=mean(Value, na.rm=T)) %>% ungroup()

for(i in 3:ncol(prices)){
  prices[,i][is.na(prices[,i])] <- mean(pull(prices[,i]), na.rm = TRUE)
}

files <- list.files(paste0(input_folder, "watercrop"), full.names = T, pattern = "variation.txt", recursive=T)
nomi <- unlist(qdapRegex::ex_between(files, "watercrop/", "/yield_percentage"))
files <- pblapply(files, raster)
files <- stack(files)
names(files) <-  c("barl", "cass", "coco", "cott", "grou", "maiz", "pmil", "smil", "oilp", "pota", "rape", "rice", "sorg", "soyb", "sugb", "sugc", "sunf", "whea", "yams")

crs(files) <- as.character(CRS("+init=epsg:4236"))

#

files2 <- list.files(path=paste0(input_folder, "spam_folder/spam2017v2r1_ssa_yield.geotiff"), pattern="R.tif", full.names=T)
nomi <- tolower(as.character(substr(basename(files2), 20, 23)))
files2 <- pblapply(files2, raster)
files2 <- stack(files2)
names(files2) <- nomi
files2 <- subset(files2, names(files))

files3 = list.files(path = paste0(input_folder, "spam_folder/spam2017v2r1_ssa_harv_area.geotiff") , pattern = 'R.tif', full.names = T)
nomi <- tolower(as.character(substr(basename(files3), 20, 23)))
files3 <- pblapply(files3, raster)
files3 <- stack(files3)
names(files3) <- nomi
files3 <- subset(files3, names(files))

#

clusters <- dplyr::select(clusters, -starts_with("A_"))

for (X in 1:nlayers(files3)){
  a = paste0("A_" , names(files3)[X])
  clusters[a] <- exactextractr::exact_extract(files3[[X]], clusters, fun="sum")
}

aa <- clusters
aa$geometry=NULL
aa$geom=NULL
clusters$A_total <- as.vector(rowSums(as.matrix(aa[grepl("^A_", colnames(aa))])))

#

for (i in 1:nlayers(files)){
print(i)
files[[i]] <- (files[[i]] / 100) * (files2[[i]] * files3[[i]])
}
names(files) <- names(files2)

#

for (i in 1:nlayers(files)){
  sr <- files[[i]]
  name <- names(files[[i]])
  e <- exact_extract(sr, clusters, "sum")
  clusters[paste0("yg_", name)] <- ifelse(e<0, 0, e)
}

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("yg_")) %>% st_set_geometry(NULL) %>% mutate(yg_total = rowSums(., na.rm=T))
clusters$yg_total <- aa$yg_total

# obtain economic benefit (additional yield * price)

prices$ISO3 <- countrycode(prices$Area, 'country.name', 'iso3c')
prices <- pivot_wider(prices, names_from = spam, values_from = c(Value), names_prefix = "pri_")

indexes <- c((nrow(prices) + 1):(nrow(prices) + length(setdiff(gadm$ISO3, prices$ISO3))))

prices[indexes,] <- NA

prices[indexes,]$ISO3 <-setdiff(gadm$ISO3, prices$ISO3)

prices$Area <- as.character(prices$Area)

prices[indexes,]$Area <-countrycode(prices[indexes,]$ISO3, 'iso3c', 'country.name')

prices <-prices %>%  mutate_at(vars(contains('pri_')), ~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))

clusters <- merge(clusters, prices, by.x="sov_a3", by.y="ISO3", all.x=T)

for (i in names(files)){

aa <- clusters
aa$geometry=NULL
aa$geom=NULL

clusters[paste0('add_val_', as.character(i))] = aa[paste0('pri_', as.character(i))] * (aa[paste0('yg_', as.character(i))] / 1000)

}

# calculate total added value

aa <- clusters
aa <- aa %>% dplyr::select(starts_with("add_val_")) %>% st_set_geometry(NULL) %>% mutate(tt_ddvl = rowSums(., na.rm=T))
clusters$tt_ddvl <- aa$tt_ddvl

#

clusters$totalpumpcost <- ifelse(clusters$er_kwh_tt!=0, clusters$totalpumpcost, 0)
clusters$totalcost <- ifelse(clusters$er_kwh_tt!=0, clusters$totalcost, 0)

clusters$tt_ddvl <- ifelse(clusters$totalcost>0 & clusters$totalpumpcost>0, clusters$tt_ddvl, 0)
clusters$yg_total<- ifelse(clusters$totalcost>0 & clusters$totalpumpcost>0, clusters$yg_total, 0)

# replicate little model of transport costs to market
# Calculate transportation cost for crops
# Formula: TC = 2 * (TTM x fuelcost x lpermin) * n

clusters$diesel_price = exact_extract(diesel_price, clusters, 'mean')

# geom <- ee$Geometry$Rectangle(c(as.vector(extent(clusters))[1], as.vector(extent(clusters))[3], as.vector(extent(clusters))[2], as.vector(extent(clusters))[4]))
#
# img_02 <- ee_as_raster(
#    image = traveltime_market,
#    via = "drive",
#    region = geom,
#    scale = 1000
#  )

img_02 <- raster(paste0(input_folder, "accessibility.tif"))

clusters$traveltime_market = exact_extract(img_02, clusters, "mean")

# impose limit travel time to market
#clusters$remote_from_market = ifelse(clusters$traveltime_market>360, 1, 0)

clusters$transp_costs = 2 * (clusters$traveltime_market/60*fuel_consumption*clusters$diesel_price) * ceiling( (clusters$yg_total/1000/truck_bearing_t))

for (i in 1:nrow(clusters)){
  clusters$transp_costs[i] = npv(discount_rate, rep( clusters$transp_costs[i], lifetimepump), 0:(lifetimepump-1))
}

# obtain economic cost (pump, energy and transport)

clusters$total_system_cost_discounted <- clusters$totalcost + clusters$totalpumpcost + clusters$transp_costs

#clusters$tt_ddvl = ifelse(clusters$total_system_cost>clusters$tt_ddvl, 0, clusters$tt_ddvl)

# NPV of revenues

for (i in 1:nrow(clusters)){
  clusters$total_revenues_discounted[i] = npv(discount_rate, rep(clusters$tt_ddvl[i], lifetimepump), 0:(lifetimepump-1))
}

clusters$total_system_cost_discounted_yeary <- clusters$total_system_cost_discounted / lifetimepump

clusters$total_revenues_discounted_discounted_yearly <- clusters$total_revenues_discounted / lifetimepump

clusters$profit_yearly <- clusters$total_revenues_discounted_discounted_yearly - clusters$total_system_cost_discounted_yeary

#

summary(clusters$total_system_cost_discounted_yeary / clusters$npumps)
summary(clusters$total_revenues_discounted_discounted_yearly / clusters$npumps)

table(clusters$total_revenues_discounted_discounted_yearly>clusters$total_system_cost_discounted_yeary)

table(clusters$profit_yearly>0)

#


clusters$profit_yearly <- ifelse(clusters$profit_yearly>0, clusters$profit_yearly, NA)

clusters$pop <- exact_extract(population, clusters, "sum")

clusters$profit_yearly_capita <- clusters$profit_yearly / clusters$pop
clusters$profit_yearly_capita <- ifelse(is.infinite(clusters$profit_yearly_capita), NA, clusters$profit_yearly_capita)

# 9) Paybacktime of investment in each cluster (in years)
clusters$PBT = clusters$total_system_cost_discounted / clusters$total_revenues_discounted_discounted_yearly

clusters$PBT <- ifelse(is.infinite(clusters$PBT), NA, clusters$PBT)

summary(clusters$PBT)
summary(clusters$PBT<5) 
summary(clusters$PBT<10) 
summary(clusters$PBT<20) 

write_rds(clusters, "clusters_with_data_5.Rds")
