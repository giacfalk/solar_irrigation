library(readr)
library(tidyverse)
library(reshape2)

# discount rate

dr_15 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
dr_075 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[193,], collapse="_"), ".Rds"))
dr_25 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[385,], collapse="_"), ".Rds"))
dr_40 <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[577,], collapse="_"), ".Rds"))

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

sens_dr$variable = as.factor(sens_dr$variable)
levels(sens_dr$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")

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

sens_fs$variable = as.factor(sens_fs$variable)
levels(sens_fs$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")

plot_b <- ggplot(sens_fs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=fs), position = "dodge")+
  scale_fill_manual(name="Scenario", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7, 6)])+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: cropland consideration")

# water_sustainability_contraint

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

sens_gs <- data.frame(gs= c("Unbounded", "Env flows"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h))
sens_gs <- reshape2::melt(sens_gs, 1)
sens_gs$gs <- factor(sens_gs$gs, c("Env flows", "Unbounded"))

sens_gs$variable = as.factor(sens_gs$variable)
levels(sens_gs$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")


plot_c <- ggplot(sens_gs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=gs), position = "dodge")+
  ggsci::scale_fill_npg(name="Scenario")+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: environmental flows")

#

# VAT and import costs exemption

exemption <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[13,], collapse="_"), ".Rds"))
no_exemption <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

a <- sum(exemption$total_system_cost_discounted_yeary[!is.na(exemption$profit_yearly)], na.rm=T) / 1e9 
b <- sum(no_exemption$total_system_cost_discounted_yeary[!is.na(no_exemption$profit_yearly)], na.rm=T) / 1e9 

c <- sum(exemption$total_revenues_discounted_discounted_yearly[!is.na(exemption$profit_yearly)], na.rm=T) / 1e9 
d <- sum(no_exemption$total_revenues_discounted_discounted_yearly[!is.na(no_exemption$profit_yearly)], na.rm=T) / 1e9 

e <- sum(exemption$profit_yearly[!is.na(exemption$profit_yearly)], na.rm=T) / 1e9 
f <- sum(no_exemption$profit_yearly[!is.na(no_exemption$profit_yearly)], na.rm=T) / 1e9 

g <- sum(exemption$npumps[!is.na(exemption$profit_yearly)], na.rm=T) / 1e6
h <- sum(no_exemption$npumps[!is.na(no_exemption$profit_yearly)], na.rm=T) / 1e6 

sens_fs <- data.frame(fs= c("Exemption", "No exemption"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h))
sens_fs <- reshape2::melt(sens_fs, 1)
sens_fs$fs <- factor(sens_fs$fs, c("Exemption", "No exemption"))

sens_fs$variable = as.factor(sens_fs$variable)
levels(sens_fs$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")


plot_d <- ggplot(sens_fs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=fs), position = "dodge")+
  scale_fill_manual(name="Scenario", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7, 6)])+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: solar VAT + import costs exemption")

# incentives - business model

no_upfront <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
only_pv <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[25,], collapse="_"), ".Rds"))
only_pump <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[49,], collapse="_"), ".Rds"))
all_upfront <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[73,], collapse="_"), ".Rds"))

#

a <- sum(no_upfront$total_system_cost_discounted_yeary[!is.na(no_upfront$profit_yearly)], na.rm=T) / 1e9 
b <- sum(only_pv$total_system_cost_discounted_yeary[!is.na(only_pv$profit_yearly)], na.rm=T) / 1e9 
c <- sum(only_pump$total_system_cost_discounted_yeary[!is.na(only_pump$profit_yearly)], na.rm=T) / 1e9 
d <- sum(all_upfront$total_system_cost_discounted_yeary[!is.na(all_upfront$profit_yearly)], na.rm=T) / 1e9 

e <- sum(no_upfront$total_revenues_discounted_discounted_yearly[!is.na(no_upfront$profit_yearly)], na.rm=T) / 1e9 
f <- sum(only_pv$total_revenues_discounted_discounted_yearly[!is.na(only_pv$profit_yearly)], na.rm=T) / 1e9 
g <- sum(only_pump$total_revenues_discounted_discounted_yearly[!is.na(only_pump$profit_yearly)], na.rm=T) / 1e9 
h <- sum(all_upfront$total_revenues_discounted_discounted_yearly[!is.na(all_upfront$profit_yearly)], na.rm=T) / 1e9 

i <- sum(no_upfront$profit_yearly[!is.na(no_upfront$profit_yearly)], na.rm=T) / 1e9 
j <- sum(only_pv$profit_yearly[!is.na(only_pv$profit_yearly)], na.rm=T) / 1e9 
k <- sum(only_pump$profit_yearly[!is.na(only_pump$profit_yearly)], na.rm=T) / 1e9 
l <- sum(all_upfront$profit_yearly[!is.na(all_upfront$profit_yearly)], na.rm=T) / 1e9 

m <- sum(no_upfront$npumps[!is.na(no_upfront$profit_yearly)], na.rm=T) / 1e6 
n <- sum(only_pv$npumps[!is.na(only_pv$profit_yearly)], na.rm=T) / 1e6 
o <- sum(only_pump$npumps[!is.na(only_pump$profit_yearly)], na.rm=T) / 1e6 
p <- sum(all_upfront$npumps[!is.na(all_upfront$profit_yearly)], na.rm=T) / 1e6 

sens_dr <- data.frame(dr= c("PV purchase incentive", "Whole system incentive", "No incentive", "Pump purchase incentive"), costs=c(a,b,c,d), revenues=c(e,f,g,h), profits=c(i,j,k,l), npumps=c(m,n,o,p))
sens_dr <- reshape2::melt(sens_dr, 1)

sens_dr$variable = as.factor(sens_dr$variable)
levels(sens_dr$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")

plot_e <- ggplot(sens_dr)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=dr), position = "dodge")+
  ggsci::scale_fill_npg(name="Business model")+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: incentives")

# VAT and import costs no_water_tank

no_water_tank <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[97,], collapse="_"), ".Rds"))
water_tank <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

a <- sum(no_water_tank$total_system_cost_discounted_yeary[!is.na(no_water_tank$profit_yearly)], na.rm=T) / 1e9 
b <- sum(water_tank$total_system_cost_discounted_yeary[!is.na(water_tank$profit_yearly)], na.rm=T) / 1e9 

c <- sum(no_water_tank$total_revenues_discounted_discounted_yearly[!is.na(no_water_tank$profit_yearly)], na.rm=T) / 1e9 
d <- sum(water_tank$total_revenues_discounted_discounted_yearly[!is.na(water_tank$profit_yearly)], na.rm=T) / 1e9 

e <- sum(no_water_tank$profit_yearly[!is.na(no_water_tank$profit_yearly)], na.rm=T) / 1e9 
f <- sum(water_tank$profit_yearly[!is.na(water_tank$profit_yearly)], na.rm=T) / 1e9 

g <- sum(no_water_tank$npumps[!is.na(no_water_tank$profit_yearly)], na.rm=T) / 1e6
h <- sum(water_tank$npumps[!is.na(water_tank$profit_yearly)], na.rm=T) / 1e6 

sens_fs <- data.frame(fs= c("No water storage", "Water storage"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h))
sens_fs <- reshape2::melt(sens_fs, 1)
sens_fs$fs <- factor(sens_fs$fs, c("No water storage", "Water storage"))

sens_fs$variable = as.factor(sens_fs$variable)
levels(sens_fs$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)")

plot_f <- ggplot(sens_fs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=fs), position = "dodge")+
  scale_fill_manual(name="Scenario", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7, 6)])+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: water storage tank")

#

# No battery

no_battery <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[769,], collapse="_"), ".Rds"))
battery <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

a <- sum(no_battery$total_system_cost_discounted_yeary[!is.na(no_battery$profit_yearly)], na.rm=T) / 1e9 
b <- sum(battery$total_system_cost_discounted_yeary[!is.na(battery$profit_yearly)], na.rm=T) / 1e9 

c <- sum(no_battery$total_revenues_discounted_discounted_yearly[!is.na(no_battery$profit_yearly)], na.rm=T) / 1e9 
d <- sum(battery$total_revenues_discounted_discounted_yearly[!is.na(battery$profit_yearly)], na.rm=T) / 1e9 

e <- sum(no_battery$profit_yearly[!is.na(no_battery$profit_yearly)], na.rm=T) / 1e9 
f <- sum(battery$profit_yearly[!is.na(battery$profit_yearly)], na.rm=T) / 1e9 

g <- sum(no_battery$npumps[!is.na(no_battery$profit_yearly)], na.rm=T) / 1e6
h <- sum(battery$npumps[!is.na(battery$profit_yearly)], na.rm=T) / 1e6 


i <- sum(no_battery$npumps[!is.na(battery$profit_yearly)]*no_battery$powerforpump[!is.na(no_battery$profit_yearly)], na.rm=T) / 1e6
j <- sum(battery$npumps[!is.na(battery$profit_yearly)]*battery$powerforpump[!is.na(battery$profit_yearly)], na.rm=T) / 1e6 

sens_fs <- data.frame(fs= c("No battery", "Battery"), costs=c(a,b), revenues=c(c,d), profits=c(e,f), npumps=c(g,h), installedcapacity=c(i,j))
sens_fs <- reshape2::melt(sens_fs, 1)
sens_fs$fs <- factor(sens_fs$fs, c("No battery", "Battery"))

sens_fs$variable = as.factor(sens_fs$variable)
levels(sens_fs$variable) <- c("Yr. disc. costs (bn.)", "Yr. disc. revenues (bn.)", "Yr. disc. profits (bn.)", "Econo. feas. pumps (mil.)", "PV cap. (GW)")

plot_g <- ggplot(sens_fs)+
  theme_classic()+
  geom_col(aes(x=variable, y=value, fill=fs), position = "dodge")+
  scale_fill_manual(name="Scenario", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7, 6)])+
  xlab("")+
  ylab("")+
  ggtitle("Sensitivity analysis: battery")



####

library(patchwork)

m1 <- plot_b  + plot_c + plot_layout(ncol=1) + patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'right',
        legend.direction = 'vertical')

ggsave("new_figures/batterysens_an_1.png", m1, scale = 2, height = 4, width = 4)

ggsave("new_figures/batterysens_an_2.png", plot_a + theme(legend.position = "bottom", legend.direction = "horizontal"), scale = 2, height = 3, width = 4)

m2 <- plot_e  + plot_d + plot_layout(ncol=1) + patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'right',
        legend.direction = 'vertical')

ggsave("new_figures/batterysens_an_3.png", m2, scale = 2, height = 4, width = 4)

m3 <- plot_g  + plot_f + plot_layout(ncol=1) + patchwork::plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'right',
        legend.direction = 'vertical')

ggsave("new_figures/batterysens_an_4.png", m3, scale = 2, height = 4, width = 5)
