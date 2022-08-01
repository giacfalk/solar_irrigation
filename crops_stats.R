clusters <- read_rds("clusters_backend.Rds")

input_folder <- "F:/MLED_database/input_folder/"

library(pbapply)
library(raster)
library(sf)
#

files2 <- list.files(path=paste0(input_folder, "spam_folder/spam2010v1r0_global_yield.geotiff"), pattern="r.tif", full.names=T)
files2 <- pblapply(files2, raster)
files2 <- stack(files2)
names(files2) <- unlist(qdapRegex::ex_between(names(files2), "spam2010v1r0_global_yield_", "_r"))

files3 <- list.files(path=paste0(input_folder, "spam_folder/spam2010v1r0_global_harv_area.geotiff"), pattern="r.tif", full.names=T)
files3 <- pblapply(files3, raster)
files3 <- stack(files3)
names(files3) <- unlist(qdapRegex::ex_between(names(files3), "spam2010v1r0_global_harvested.area_", "_r"))

library(exactextractr)

for (i in 1:nlayers(files3)){
  sr <- files3[[i]]
  name <- names(files3[[i]])
  e <- exact_extract(sr, clusters, "sum")
  clusters[paste0("A_", name)] <- ifelse(e<0, 0, e)
}

clusters <- dplyr::select(clusters, 40:84)

clusters$geometry<- NULL
clusters$cl_id<- NULL

clusters_s <- dplyr::group_by(clusters, sov_a3) %>% summarise_all(., "sum", na.rm=T)
clusters$sov_a3 <- NULL
clusters_t <- dplyr::summarise_all(clusters, "sum", na.rm=T)
clusters_t <- reshape2::melt(clusters_t)

#
clusters_t$variable<- gsub("A_", "", clusters_t$variable)

ggplot(clusters_t)+
  geom_col(aes(x=reorder(variable, -value), y=value))
