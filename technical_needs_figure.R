clusters_bk <- read_rds(paste0("clusters_bk_", paste(scenarios[scenario,], collapse="_"), ".Rds"))

lifetimepump=20

clusters_bk <- filter(clusters_bk, !(clusters_bk$cl_id %in% clusters$cl_id))

clusters <- bind_rows(clusters, clusters_bk)
clusters <- st_as_sf(clusters)

library(tmap)
data(World)

world_all <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

fun_plot <- function(var_to_plot, title_plot="", style="quantile"){
  
  tm_shape ( clusters ) + tm_fill ( col=as.character(var_to_plot), title=title_plot, style=style, legend.reverse = TRUE,  colorNA = "white", showNA = F, n=5,
                                    legend.hist = TRUE) +  tm_layout( frame = FALSE , scale = 1) + tm_shape(World) + tm_borders("black", lwd = .5)
  
}

#

#output = st_intersection(clusters, st_union(world_all$geometry))

######

# Figure 2
library(lwgeom)
library(ggsci)

regions <- filter(world_all, region_un=="Africa") %>% st_transform(3395) %>%
  st_snap_to_grid(size = 1000) %>%
  st_make_valid() %>%
  group_by(subregion) %>% 
  summarize() %>% 
  st_transform(4326)

regions <- filter(regions, subregion!="Northern Africa")

r <- raster(); res(r)<-.25;
irreq <- stack(lapply(grep("monthly_IRREQ_", colnames(clusters), value = TRUE), function(X){fasterize::fasterize(clusters, r, X)})) / 1e6

regions_data <- exact_extract(irreq, regions, "sum")
regions_data$region <- regions$subregion
regions_data <- melt(regions_data, 13)

regions_data$variable <- gsub("sum.layer.", "", regions_data$variable)

regions_data$region[regions_data$region=="Middle Africa"] <- "Central Africa"

fig2a <- ggplot(regions_data)+
  theme_classic()+
  geom_point(aes(x=as.numeric(variable), y=value/1e3, group=region, colour=region), size=2)+
  geom_line(aes(x=as.numeric(variable), y=value/1e3, group=region, colour=region), linetype="dashed", size=1)+
  scale_x_continuous(labels=month.abb, breaks=c(1:12))+
  ylab("Water requirements to close \nthe irrigation gap (cubic km / month)")+
  xlab("Month")+
  scale_colour_npg(name="Region")

enreq <- stack(lapply(grep("^er_kwh", colnames(clusters), value = TRUE)[1:12], function(X){fasterize::fasterize(clusters, r, X)})) 

npumps <- fasterize::fasterize(clusters, r, "npumps")

regions_data <- exact_extract(enreq*npumps, regions, "sum") / 1e6
regions_data$region <- regions$subregion
regions_data <- melt(regions_data, 13)

regions_data$variable <- gsub("sum.layer.", "", regions_data$variable)

regions_data$region[regions_data$region=="Middle Africa"] <- "Central Africa"

fig2b <- ggplot(regions_data)+
  theme_classic()+
  geom_point(aes(x=as.numeric(variable), y=value, group=region, colour=region), size=2)+
  geom_line(aes(x=as.numeric(variable), y=value, group=region, colour=region), linetype="dashed", size=1)+
  scale_x_continuous(labels=month.abb, breaks=c(1:12))+
  ylab("Electricity requirements to close \nthe irrigation gap (GWh)")+
  xlab("Month")+
  scale_colour_npg(name="Region")

library(patchwork)

(fig2a + fig2b + plot_layout(guides = "collect"))
