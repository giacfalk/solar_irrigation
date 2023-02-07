if (!require("pacman")) install.packages("pacman"); library(pacman)

pacman::p_load(sf, raster, exactextractr, dplyr, readxl, cowplot, ggplot2, scales, tidyr, tidyverse, rgeos, chron, nngeo, strex, rgee, data.table, gdata, FactoMineR, factoextra, maps  , mapdata, maptools, grid, randomForestSRC, countrycode, remotes, stars, gdistance, rgl, rasterVis, qlcMatrix, stars, tvm, gtools, wbstats, stars, patchwork, ggrepel, terra, pbapply, googledrive)

tmpDir(create=TRUE)

if (!require("qgisprocess")) remotes::install_github("paleolimbot/qgisprocess"); library(qgisprocess)
qgis_configure()

if (!require("rgis")) remotes::install_github("JGCRI/rgis"); library(rgis)

ee_Initialize(user = email, drive = TRUE)

rasterOptions(tmpdir=input_folder)

mask_raster_to_polygon <- function (raster_object, polygon) 
{
  if (class(polygon)[[1]] != "sf") 
    polygon <- st_as_sf(polygon)
  r_crs <- st_crs(projection(raster_object))
  polys <- polygon %>% st_transform(crs = r_crs)
  n_lcs <- crop(raster_object, polys) %>% mask(polys)
  return(n_lcs)
}

#

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf"); world <- filter(world, region_wb=="Sub-Saharan Africa" & continent=="Africa" & sov_a3!="MDG" & sov_a3!="STP" & sov_a3!="CPV" & sov_a3!="SOL")

world <- st_make_grid(world)

if (read_existing_clusters == T){
  clusters <- read_rds("clusters_with_data.Rds")
} else{

 clusters <- st_make_grid(x= world, what="polygons", cellsize = .25); st_crs(clusters) <- st_crs(world); clusters <- st_as_sf(clusters); clusters <- st_filter(clusters, world, .predicate = st_intersects)}

#

# Planning horizon parameters
today = 2020
planning_year = 2050
planning_horizon = planning_year - today

# Groundwater pump technical parameters
rho = 1000 # density of water (1000 kg / m3)
g = 9.81 # gravitational constant (m / s2)
c = 3.6 * (10^6) # differential pressure, (Pa)

#Threshold parameters
threshold_surfacewater_distance = 1500 # (m): distance threshold which discriminates if groundwater pumping is necessary or a surface pump is enough # REF:
threshold_groundwater_pumping = 75 # (m): maximum depth at which the model allows for water pumping: beyond it, no chance to install the pump # REF:

water_speed = 2 #m/s, https://www.engineeringtoolbox.com/flow-velocity-water-pipes-d_385.html
water_viscosity = 0.00089 #https://www.engineersedge.com/physics/water__density_viscosity_specific_weight_13146.htm
pipe_diameter = 0.025 # m

slope_limit <- 5 # average %, for surface pumping

field_size <- raster(paste0(input_folder, "global_field_size/field_size_10_40_cropland.img"))
field_size <- mask_raster_to_polygon(field_size,  st_as_sfc(st_bbox(clusters)))
gc()

# maxflow of a pump in m3/h
maxflow_boundaries <- c(5, 25) #i.e. 1-25 m3/h
maxflow <- field_size
gc()
v <- scales::rescale(values(maxflow), to = maxflow_boundaries)
values(maxflow) <- v
rm(v); gc()

# water storage tank range 
range_tank <- c(1000, 20000) #liters

# Surface water parameters
# water_speed = 2 #m/s, https://www.engineeringtoolbox.com/flow-velocity-water-pipes-d_385.html
# water_viscosity = 0.00089 #https://www.engineersedge.com/physics/water__density_viscosity_specific_weight_13146.htm
# pipe_diameter = 0.8 # m

#number of hours of pumping, 3 during the morning and 3 during the evening
nhours_irr = 6
irrigation_frequency_days <- 2
eta_pump = 0.75
eta_motor = 0.75 

# Transportation costs
fuel_consumption = 15 # (l/h) # REF: OnSSET
fuel_cost = 1 # (USD/l) # baseline, then adapted based on distance to city
truck_bearing_t = 15 # (tons) # REF: https://en.wikipedia.org/wiki/Dump_truck

#Pumo economic parameters
lifetimepump = 20

traveltime_market = ee$Image("Oxford/MAP/accessibility_to_cities_2015_v1_0")
diesel_price <- raster(paste0(input_folder, "diesel_price_baseline_countryspecific_0505.tif"))

#
DepthToGroundwater = read.delim(paste0(input_folder , 'DepthToGroundwater/xyzASCII_dtwmap_v1.txt'), sep='\t')
GroundwaterStorage = read.delim(paste0(input_folder , 'GroundwaterStorage/xyzASCII_gwstor_v1.txt'), sep='\t')
GroundwaterProductivity = read.delim(paste0(input_folder , 'GroundwaterProductivity/xyzASCII_gwprod_v1.txt'), sep='\t')

population <- raster(paste0(input_folder, "GHS_POP_E2015_GLOBE_R2019A_4326_30ss_V1_0.tif"))

load_curve_irrig = c(0, 0, 0, 0, 0, 0.166, 0.166, 0.166, 0.166, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.166, 0.166)
load_curve_irr = load_curve_irrig

#

# mask for areas near to existing LV grid
# lv_grid_density <- raster(paste0(input_folder, "targets.tif"))
# lv_grid_density <- mask_raster_to_polygon(lv_grid_density, st_as_sfc(st_bbox(clusters)))
# lv_grid_density <- terra::aggregate(lv_grid_density, fun=sum, fact=20)
# writeRaster(lv_grid_density, file=paste0(input_folder, "targets_10km.tif"), overwrite=T)
lv_grid_density <- raster(paste0(input_folder, "targets_10km.tif"))
crs(lv_grid_density) <- crs(population)

ref <- raster(list.files(paste0("D:/MLED_database/", "risultati giacomo"), full.names = T, pattern = "actual", recursive=T)[[1]])

crs(ref) <- crs(population)
lv_grid_density <- lv_grid_density>=1
lv_grid_density <- projectRaster(lv_grid_density, ref, method = "ngb")

vat_import <- read.csv(paste0(input_folder, "vat_import.csv"), stringsAsFactors = F)
vat_import$ISO3 <- countrycode::countrycode(vat_import[,1], 'country.name', 'iso3c')

write_rds(clusters, "clusters_backend.Rds")
