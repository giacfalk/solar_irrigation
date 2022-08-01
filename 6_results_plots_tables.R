#######################################
# ggplots of input data and results

library(tmap)
data(World)

# clusters <- read_rds("clusters_with_data_5.Rds")
# clusters <- subset(clusters, clusters$Y > -38)
world_all <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# fun_plot <- function(var_to_plot, br_lab="auto", title_plot="", tra="identity"){
#
#   ggplot()+
#     geom_sf(data = clusters,
#                aes(fill = ifelse(var_to_plot==0, NA, var_to_plot)), colour=NA) +
#     scale_fill_viridis_c(name="", na.value = NA, trans=tra)+
#     geom_sf(data=world_all, fill=NA, size =.3)+
#     theme_classic()+
#     coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
#     theme(legend.position = c(0.2, 0.32), legend.direction = "vertical", axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=8, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
#           axis.text.y=element_blank(),axis.ticks=element_blank(),
#           axis.title.x=element_blank(),
#           axis.title.y=element_blank())+
#     #guides(fill = guide_colorsteps(barwidth = 1, barheight = 5))+
#     ggtitle(title_plot)+
#     xlab("")+
#     ylab("")
#
# }

#

fun_plot <- function(var_to_plot, title_plot="", style="quantile"){

  tm_shape ( clusters ) + tm_fill ( col=as.character(var_to_plot), title=title_plot, style=style, legend.reverse = TRUE,  colorNA = "white", showNA = F, n=5,
                                    legend.hist = TRUE) +  tm_layout( frame = FALSE , scale = 1) + tm_shape(World) + tm_borders("black", lwd = .5)

}

library(lwgeom)

world_all_africa <- filter(world_all, region_un=="Africa") %>% st_transform(3395) %>%
  st_snap_to_grid(size = 1000) %>%
  st_make_valid() %>% st_union() %>% st_transform(4326) %>% st_as_sf()

#
sum(clusters$pop[!is.na(clusters$profit_yearly)]) / sum(clusters$pop)

(sum(clusters$pop[!is.na(clusters$profit_yearly)]) / sum(clusters$pop)) * exact_extract(population, world_all_africa, "sum")


#

library(qgisprocess)
qgis_configure()

out <- qgisprocess::qgis_run_algorithm( "native:clip",
                                 INPUT = st_as_sf(clusters$geometry), OVERLAY= st_as_sf(world_all_africa$x))

clusters$geometry <- read_sf(out$OUTPUT)$geom

# Descriptive data plots #

png("figures/grwatwdist.png")
fun_plot("gr_wat_depth", "Groundwater well depth (m)")
dev.off()

png("figures/pv_cost_data.png")
fun_plot("pvcost", "Breakeven PV cost (USD/Wp)")
dev.off()

png("figures/gr_wat_storage.png")
fun_plot("gr_wat_storage", "Groundwater storage (m)")
dev.off()

png("figures/gr_wat_productivity.png")
fun_plot("gr_wat_productivity", "Groundwater productivity (mm/h)")
dev.off()

png("figures/total_cropland.png")
fun_plot("A_total", "Rainfed cropland (ha)")
dev.off()

clusters$lv_grid_density <- exact_extract(lv_grid_density, clusters, "sum")

png("figures/LV_grid_density.png")
fun_plot("lv_grid_density", "LV grid density (km)")
dev.off()


# Results plots #

my.at <- c(0, 15, 30, 60, 120, 240, 480)

myColorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at ## where to print labels
                   ))

# plot irrigation requirements by month
png("figures/irreq.png", width=1200/1.5, height=1200/1.5, res=100)
r <- raster(); res(r)<-2;
irreq <- stack(lapply(grep("monthly_IRREQ_", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e6
values(irreq) <- ifelse(values(irreq)==0, NA, values(irreq) )
ext <- as.vector(extent(irreq))
boundaries <- map('worldLores', fill=TRUE,
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(irreq)))
names(irreq) <- month.abb
print(rasterVis::levelplot(irreq, xlim=c(-30, 55), ylim=c(-40, 45),
                     main="Water requirements to close the irrigation gap (million m3/month)", xlab="Longitude", ylab="Latitude", ncol=4, at=my.at, colorkey=myColorkey, par.settings=YlOrRdTheme) + layer(sp.polygons(bPols)))
dev.off()

clusters_irr <- dplyr::select(clusters, starts_with("monthly_IRREQ_"), ISO3)
clusters_irr$geometry<-NULL
clusters_irr[,1:12] <- clusters_irr[,1:12]/clusters$A_total
clusters_irr <- clusters_irr %>% group_by(ISO3) %>% summarise_all(.funs = "sum", na.rm=T) %>% ungroup()
clusters_irr <- reshape2::melt(clusters_irr, 1)
clusters_irr <- merge(clusters_irr, world_all %>% filter(region_un=="Africa"), by.x="ISO3", by.y="iso_a3", all.x=T)
clusters_irr <- dplyr::select(clusters_irr, variable, value, subregion)
clusters_irr <- clusters_irr %>% group_by(subregion, variable) %>% summarise(value=sum(value, na.rm=T))

levels(clusters_irr$variable) <- month.abb

ggplot(clusters_irr)+
  geom_col(aes(x=subregion, y=value, fill=subregion))+
  facet_wrap(vars(variable), nrow=2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position="bottom", legend.direction = "horizontal")+
  ylab("Water requirements to close the irrigation gap \n(m3/ha cropland/month)")
ggsave("figures/irreq2.png")


png("figures/enreq.png", width=1200/1.5, height=1200/1.5, res=100)
r <- raster(); res(r)<-2;
irreq <- stack(lapply(grep("er_kwh", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)}))
values(irreq) <- ifelse(values(irreq)==0, NA, values(irreq) )
ext <- as.vector(extent(irreq))
boundaries <- map('worldLores', fill=TRUE,
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(irreq)))
names(irreq) <- month.abb
print(rasterVis::levelplot(irreq, xlim=c(-30, 55), ylim=c(-40, 45),
                           main="Electricity requirements to pump water (kWh/pump/month)", xlab="Longitude", ylab="Latitude", ncol=4, at=my.at, colorkey=myColorkey, par.settings=YlOrRdTheme) + layer(sp.polygons(bPols)))
dev.off()

clusters_irr <- dplyr::select(clusters, starts_with("er_kwh"), ISO3)
clusters_irr$geometry<-NULL
clusters_irr[,1:12] <- clusters_irr[,1:12]*clusters$npumps
clusters_irr <- clusters_irr %>% group_by(ISO3) %>% summarise_all(.funs = "sum", na.rm=T) %>% ungroup()
clusters_irr <- reshape2::melt(clusters_irr, 1)
clusters_irr <- merge(clusters_irr, world_all %>% filter(region_un=="Africa"), by.x="ISO3", by.y="iso_a3", all.x=T)
clusters_irr <- dplyr::select(clusters_irr, variable, value, subregion)
clusters_irr <- clusters_irr %>% group_by(subregion, variable) %>% summarise(value=sum(value, na.rm=T))

levels(clusters_irr$variable) <- month.abb

ggplot(clusters_irr)+
  geom_col(aes(x=subregion, y=value/1e6, fill=subregion))+
  facet_wrap(vars(variable), nrow=2)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position="bottom", legend.direction = "horizontal")+
  ylab("Electricity requirements to close the irrigation gap \n(GWh/month)")
ggsave("figures/enreq2.png")

# plot elrate

clusters$npumps_th <- clusters$npumps/1e3

png("figures/npumps.png")
fun_plot("npumps_th", "Number or required pumps (thousands)")
dev.off()

clusters$pumps_ha <- clusters$npumps / clusters$A_total

png("figures/npumps_hectare.png")
fun_plot("pumps_ha", "Required pumps /cropland ha")
dev.off()


clusters$total_cost_pumps <- clusters$totalpumpcost/1e6

png("figures/total_cost_pumps.png")
fun_plot("total_cost_pumps", "Total costs for pumps installation (million USD)")
dev.off()

clusters$average_cost_pump <- clusters$totalpumpcost/clusters$npumps

png("figures/average_cost_pumps.png")
fun_plot("average_cost_pump", "Average pump cost installation (USD)")
dev.off()

clusters$pvsize_mwh_total <- clusters$pvsize_kw * clusters$npumps / 1000

png("figures/pvsize_mw.png")
fun_plot("pvsize_mwh_total", "PV installation requirements (MW)")
dev.off()

clusters$battery_mwh <- clusters$batterysize_kwh* clusters$npumps / 1000

png("figures/battery_mwh.png")
fun_plot("battery_mwh", "Battery installation requirements (MWh)")
dev.off()

clusters$total_energy_costs <- clusters$totalcost/clusters$npumps

png("figures/totalcost_energy.png")
fun_plot("total_energy_costs", "Average energy system cost per pump (USD)")
dev.off()

clusters$costs_ratio <- (clusters$totalcost)/(clusters$totalpumpcost)

png("figures/costs_ratio.png")
fun_plot("costs_ratio", "Ratio beween PV system total costs and pump total costs")
dev.off()

clusters$tt_ddvl_mln <- clusters$tt_ddvl/1e6


png("figures/tt_ddvl.png")
fun_plot("tt_ddvl_mln", "Estimated yearly revenues (million USD)")
dev.off()

clusters$economic_feas <- !is.na(clusters$profit_yearly)

png("figures/economic_feasibility.png")
fun_plot("economic_feas", "Areas where PV groundwater pumps are economically feasible")
dev.off()

png("figures/PBT.png")
fun_plot("PBT", "Estimated payback time (years)")
dev.off()

png("figures/PBT_hist.png")
histogram(~ PBT | ISO3, data = clusters, xlab="Number of years", main="Payback time of total costs", col="lightgrey")
dev.off()

clusters$profit_yearly_pop <- ifelse(clusters$profit_yearly/clusters$pop>0,clusters$profit_yearly/clusters$pop , NA)

png("figures/profit_yearly.png")
fun_plot("profit_yearly_pop", "Estimated yearly profit per capita (USD)")
dev.off()

clusters$average_system_cost_discounted_yeary <- clusters$total_system_cost_discounted_yeary / clusters$npumps
clusters$average_system_cost_discounted_yeary <- ifelse(!is.finite(clusters$average_system_cost_discounted_yeary), NA, clusters$average_system_cost_discounted_yeary)

png("figures/total_system_cost.png")
fun_plot("average_system_cost_discounted_yeary", "Estimated average system costs (pump+energy+transport)  per pump")
dev.off()

clusters$average_revenues_discounted_discounted_yearly <- clusters$total_revenues_discounted_discounted_yearly / clusters$npumps
clusters$average_revenues_discounted_discounted_yearly <- ifelse(!is.finite(clusters$average_revenues_discounted_discounted_yearly), NA, clusters$average_revenues_discounted_discounted_yearly)

png("figures/total_system_cost.png")
fun_plot("average_revenues_discounted_discounted_yearly", "Estimated yearly average discounted revenues per pump")
dev.off()

clusters$transp_costs_avg <- clusters$transp_costs / lifetimepump
clusters$transp_costs_avg <- ifelse(!is.finite(clusters$transp_costs_avg), NA, clusters$transp_costs_avg)

png("figures/transp_cost.png")
fun_plot("transp_costs_avg", "Estimated yearly average discounted transport costs")
dev.off()

clusters$landuse_ha <- clusters$landuse_m2 * 0.0001

png("figures/landuse.png")
fun_plot("landuse_ha", "Estimated system land use (ha)")
dev.off()

png("figures/landuse_hist.png")
barchart(~ landuse_ha | ISO3, data = clusters, xlab="Land use (ha)", main="", col="lightgrey")
dev.off()

###

ggplot(clusters[!is.na(clusters$PBT),])+
  geom_bar(aes(x=ISO3, y=PBT),  position = "dodge", stat = "summary", fun = "mean", na.rm = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/PBT_barplot.png")

ggplot(clusters[!is.na(clusters$profit_yearly),])+
  geom_bar(aes(x=ISO3, y=ifelse(profit_yearly/pop>0,profit_yearly/pop , NA)),  position = "dodge", stat = "summary", fun = "mean", na.rm = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/average_capita_profit_barplot.png")

ggplot(clusters[!is.na(clusters$npumps),])+
  geom_bar(aes(x=ISO3, y=total_system_cost_discounted/npumps),  position = "dodge", stat = "summary", fun = "mean", na.rm = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/average_pump_cost_barplot.png")

ggplot(clusters[!is.na(clusters$npumps) & is.finite(clusters$profit_yearly),])+
  geom_bar(aes(x=ISO3, y=npumps),  position = "dodge", stat = "summary", fun = "mean", na.rm = TRUE)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")
ggsave("figures/npumps_ec_feasible_barplot.png")

#

cl <- dplyr::select(clusters, ISO3, total_system_cost_discounted_yeary, total_revenues_discounted_discounted_yearly) %>% group_by(ISO3) %>% summarise_all(., "mean", na.rm=T) %>% ungroup()
cl$total_system_cost_discounted_yeary <- -cl$total_system_cost_discounted_yeary
cl$geometry<-NULL
cl <- melt(cl, 1)

ggplot(cl)+
  geom_col(aes(x=ISO3, y=value/lifetimepump, fill=variable))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "none")+
  ggtitle("Average yearly discounted costs/benefits")

ggsave("figures/CBA.png")


