library(readr)
library(tidyverse)
library(reshape2)
setwd("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

# discount rate

dr_15 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
dr_075 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[13,], collapse="_"), ".Rds"))
dr_25 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[25,], collapse="_"), ".Rds"))
dr_40 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[37,], collapse="_"), ".Rds"))

#

a <- sum(dr_15$total_system_cost_discounted_yeary[!is.na(dr_15$profit_yearly)], na.rm=T) / 1e9 
b <- sum(dr_075$total_system_cost_discounted_yeary[!is.na(dr_075$profit_yearly)], na.rm=T) / 1e9 
c <- sum(dr_25$total_system_cost_discounted_yeary[!is.na(dr_25$profit_yearly)], na.rm=T) / 1e9 
d <- sum(dr_40$total_system_cost_discounted_yeary[!is.na(dr_40$profit_yearly)], na.rm=T) / 1e9 

e <- sum(dr_15$total_revenues_discounted_discounted_yearly[!is.na(dr_15$profit_yearly)], na.rm=T) / 1e9 
f <- sum(dr_075$total_revenues_discounted_discounted_yearly[!is.na(dr_075$profit_yearly)], na.rm=T) / 1e9 
g <- sum(dr_25$total_revenues_discounted_discounted_yearly[!is.na(dr_25$profit_yearly)], na.rm=T) / 1e9 
h <- sum(dr_40$total_revenues_discounted_discounted_yearly[!is.na(dr_40$profit_yearly)], na.rm=T) / 1e9 

i <- sum(dr_15$profit_yearly[!is.na(dr_15$profit_yearly)], na.rm=T) / 1e9 
j <- sum(dr_075$profit_yearly[!is.na(dr_075$profit_yearly)], na.rm=T) / 1e9 
k <- sum(dr_25$profit_yearly[!is.na(dr_25$profit_yearly)], na.rm=T) / 1e9 
l <- sum(dr_40$profit_yearly[!is.na(dr_40$profit_yearly)], na.rm=T) / 1e9 

m <- sum(dr_15$npumps[!is.na(dr_15$profit_yearly)], na.rm=T) / 1e6 
n <- sum(dr_075$npumps[!is.na(dr_075$profit_yearly)], na.rm=T) / 1e6 
o <- sum(dr_25$npumps[!is.na(dr_25$profit_yearly)], na.rm=T) / 1e6 
p <- sum(dr_40$npumps[!is.na(dr_40$profit_yearly)], na.rm=T) / 1e6 

sens_dr <- data.frame(dr= c("7.5%", "15%", "25%", "40%"), costs=c(b,a,c,d), revenues=c(f,e,g,h), profits=c(j,i,k,l), npumps=c(n,m,o,p))
sens_dr <- reshape2::melt(sens_dr, 1)
sens_dr$dr <- factor(sens_dr$dr, c("7.5%", "15%", "25%", "40%"))

plot_a <- ggplot(sens_dr)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=dr), position = "dodge")+
  ggsci::scale_fill_npg(name="Discount rate")+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: discount rate")

# field size contraint

all_cropland <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[7,], collapse="_"), ".Rds"))
only_smallholder <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

a <- sum(all_cropland$total_system_cost_discounted_yeary[!is.na(all_cropland$profit_yearly)], na.rm=T) / 1e9 
b <- sum(only_smallholder$total_system_cost_discounted_yeary[!is.na(only_smallholder$profit_yearly)], na.rm=T) / 1e9 

c <- sum(all_cropland$total_revenues_discounted_discounted_yearly[!is.na(all_cropland$profit_yearly)], na.rm=T) / 1e9 
d <- sum(only_smallholder$total_revenues_discounted_discounted_yearly[!is.na(only_smallholder$profit_yearly)], na.rm=T) / 1e9 

e <- sum(all_cropland$profit_yearly[!is.na(all_cropland$profit_yearly)], na.rm=T) / 1e9 
f <- sum(only_smallholder$profit_yearly[!is.na(only_smallholder$profit_yearly)], na.rm=T) / 1e9 

g <- sum(all_cropland$npumps[!is.na(all_cropland$profit_yearly)], na.rm=T) / 1e6
h <- sum(only_smallholder$npumps[!is.na(only_smallholder$profit_yearly)], na.rm=T) / 1e6 

sens_fs <- data.frame(fs= c("All cropland", "Smallholder only"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h))
sens_fs <- reshape2::melt(sens_fs, 1)
sens_fs$fs <- factor(sens_fs$fs, c("All cropland", "Smallholder only"))

plot_b <- ggplot(sens_fs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=fs), position = "dodge")+
  scale_fill_manual(name="Scenario", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7, 6)])+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: cropland consideration")


# groundwater_sustainability_contraint

env_flow <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
unbounded <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))

a <- sum(env_flow$total_system_cost_discounted_yeary[!is.na(env_flow$profit_yearly)], na.rm=T) / 1e9 
b <- sum(unbounded$total_system_cost_discounted_yeary[!is.na(unbounded$profit_yearly)], na.rm=T) / 1e9 

c <- sum(env_flow$total_revenues_discounted_discounted_yearly[!is.na(env_flow$profit_yearly)], na.rm=T) / 1e9 
d <- sum(unbounded$total_revenues_discounted_discounted_yearly[!is.na(unbounded$profit_yearly)], na.rm=T) / 1e9 

e <- sum(env_flow$profit_yearly[!is.na(env_flow$profit_yearly)], na.rm=T) / 1e9 
f <- sum(unbounded$profit_yearly[!is.na(unbounded$profit_yearly)], na.rm=T) / 1e9 

g <- sum(env_flow$npumps[!is.na(env_flow$profit_yearly)], na.rm=T) / 1e6
h <- sum(unbounded$npumps[!is.na(unbounded$profit_yearly)], na.rm=T) / 1e6 

sens_gs <- data.frame(gs= c("Env flows", "Unbounded"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h))
sens_gs <- reshape2::melt(sens_gs, 1)
sens_gs$gs <- factor(sens_gs$gs, c("Env flows", "Unbounded"))

plot_c <- ggplot(sens_gs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=gs), position = "dodge")+
  ggsci::scale_fill_npg(name="Scenario")+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: environmental flows")


####

library(patchwork)

m1 <- plot_b  + plot_c + plot_layout(ncol=1) + patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'right',
        legend.direction = 'vertical')

ggsave("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Manuscript/sens_an_1.png", m1, scale = 1.5, height = 4, width = 4)


ggsave("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Manuscript/sens_an_2.png", plot_a, scale = 1.5, height = 2.5, width = 3)
