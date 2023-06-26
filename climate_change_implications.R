library(readr)
library(tidyverse)
library(reshape2)

setwd("C:/Users/falchetta/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

rcp_ssp <- c("baseline", "245", "585")

water_sustainability_contraint <- c(F, T)

field_size_contraint <- c(T, F)

VAT_import_costs <- c(T,F)

instalments_business_model <- c(4,3,2,1) # 1. all upfront (system) costs 2. only pump + irr system unfront costs 3. only PV upfront costs 4. no upfront costs

no_battery <- c(T,F)

water_tank_storage <- c(T,F)

discount_rate = c(0.15, 0.075, 0.25, 0.4)

prices_sens <- c("median", "min", "max")

pvbatterycosts_sens <- c("baseline", "minus10pct", "minus25pct") 

scenarios <- expand.grid(rcp_ssp, water_sustainability_contraint, field_size_contraint, VAT_import_costs, instalments_business_model, water_tank_storage, discount_rate, no_battery, stringsAsFactors = F)

colnames(scenarios) <- c("rcp_ssp", "water_sustainability_contraint", "field_size_contraint", "VAT_import_costs", "instalments_business_model", "water_tank_storage", "discount_rate", "no_battery")
####

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

# baseline_T <- subset(baseline_T, !is.na(baseline_T$profit_yearly))
# s_245_T <- subset(s_245_T, !is.na(s_245_T$profit_yearly))
# s_585_T <- subset(s_585_T, !is.na(s_585_T$profit_yearly))
# 
# baseline_F <- subset(baseline_F, !is.na(baseline_F$profit_yearly))
# s_245_F <- subset(s_245_F, !is.na(s_245_F$profit_yearly))
# s_585_F <- subset(s_585_F, !is.na(s_585_F$profit_yearly))

#

summary(s_245_T$yearly_IRREQ) / summary(baseline_T$yearly_IRREQ) - 1
summary(s_585_T$yearly_IRREQ) / summary(baseline_T$yearly_IRREQ) - 1

i_n <- c(sum(baseline_T$yearly_IRREQ), sum(s_245_T$yearly_IRREQ), sum(s_585_T$yearly_IRREQ))
rcp_ssp <- c("Baseline", "SSP 245", "SSP 585")

t1 <- data.frame(rcp_ssp, i_n, stringsAsFactors = F)

t1$rcp_ssp <- factor(t1$rcp_ssp, levels = c("Baseline", "SSP 245", "SSP 585"))

uno <- ggplot(t1, aes(x=rcp_ssp, y=i_n/1e9, fill=rcp_ssp))+
  theme_classic()+
  geom_col()+
  xlab("")+
  ylab("Cubic km/yr.")+
  ggsci::scale_fill_npg(name="Scenario")+
  ggtitle("Rainfed crop evapotrans. needs in 2050")

#ggsave("//tsclient/D/OneDrive - IIASA/Conferences 2022/Scenarios Forum/LEAPRE submission/fig1.png", uno, scale = 1.5)

###

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

# baseline_T <- subset(baseline_T, !is.na(baseline_T$profit_yearly))
# s_245_T <- subset(s_245_T, !is.na(s_245_T$profit_yearly))
# s_585_T <- subset(s_585_T, !is.na(s_585_T$profit_yearly))
#
# baseline_F <- subset(baseline_F, !is.na(baseline_F$profit_yearly))
# s_245_F <- subset(s_245_F, !is.na(s_245_F$profit_yearly))
# s_585_F <- subset(s_585_F, !is.na(s_585_F$profit_yearly))

kwh_n <- c(sum(baseline_T$er_kwh_tt*baseline_T$npumps, na.rm=T), sum(s_245_T$er_kwh_tt*s_245_T$npumps, na.rm=T), sum(s_585_T$er_kwh_tt*s_585_T$npumps, na.rm=T), sum(baseline_F$er_kwh_tt*baseline_F$npumps, na.rm=T), sum(s_245_F$er_kwh_tt*s_245_F$npumps, na.rm=T), sum(s_585_F$er_kwh_tt*s_585_F$npumps, na.rm=T))
rcp_ssp <- c("Baseline", "SSP 245", "SSP 585", "Baseline", "SSP 245", "SSP 585")
scen <- c("Unbounded", "Unbounded", "Unbounded", "Env flows", "Env flows", "Env flows")

t1 <- data.frame(kwh_n, rcp_ssp, scen, stringsAsFactors = F)

t1$rcp_ssp <- factor(t1$rcp_ssp, levels = c("Baseline", "SSP 245", "SSP 585"))

due <- ggplot(t1, aes(x=rcp_ssp, y=kwh_n/1e9, fill=rcp_ssp))+
  theme_classic()+
  geom_col()+
  facet_wrap(vars(scen))+
  xlab("")+
  ylab("TWh/year")+
  ggsci::scale_fill_npg(name="Scenario")+
  ggtitle("Solar pumping electricity demand in 2050")

#ggsave("//tsclient/D/OneDrive - IIASA/Conferences 2022/Scenarios Forum/LEAPRE submission/fig2.png", due, scale = 1.5)

#

###

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

baseline_T$which_pumping <- ifelse(is.na(baseline_T$profit_yearly), "Neither", baseline_T$which_pumping)
s_245_T$which_pumping <- ifelse(is.na(s_245_T$profit_yearly), "Neither", s_245_T$which_pumping)
s_585_T$which_pumping <- ifelse(is.na(s_585_T$profit_yearly), "Neither", s_585_T$which_pumping)
baseline_F$which_pumping <- ifelse(is.na(baseline_F$profit_yearly), "Neither", baseline_F$which_pumping)
s_245_F$which_pumping <- ifelse(is.na(s_245_F$profit_yearly), "Neither", s_245_F$which_pumping)
s_585_F$which_pumping <- ifelse(is.na(s_585_F$profit_yearly), "Neither", s_585_F$which_pumping)

s1 <- as.vector(table(baseline_T$which_pumping))
s2 <- as.vector(table(s_245_T$which_pumping))
s3 <- as.vector(table(s_585_T$which_pumping))
s4 <- as.vector(table(baseline_F$which_pumping))
s5 <- as.vector(table(s_245_F$which_pumping))
s6 <- as.vector(table(s_585_F$which_pumping))

shares <- as.data.frame(rbind(s1, s2, s3, s4, s5, s6))

rcp_ssp <- c("Baseline", "SSP 245", "SSP 585", "Baseline", "SSP 245", "SSP 585")
scen <- c("Unbounded", "Unbounded", "Unbounded", "Env flows", "Env flows", "Env flows")

shares$rcp_ssp <- rcp_ssp
shares$scen <- scen

rownames(shares) <- NULL
colnames(shares)[1:3] <- names(table(baseline_T$which_pumping)) 

shares$sum_cat <- rowSums(shares[,1:3])

shares <- reshape2::melt(shares, 4:6)

shares$rcp_ssp <- factor(shares$rcp_ssp, levels = c("Baseline", "SSP 245", "SSP 585"))

shares$variable <- factor(shares$variable, levels = rev(c("Neither", "Ground water pumping", "Surface water pumping")))

tre <- ggplot(shares, aes(x=rcp_ssp, y=value/sum_cat, fill=variable))+
  theme_classic()+
  geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ scen)+
  xlab("Climate change scenario")+
  ylab("% of pumping water source")+
  scale_fill_manual(name="Source", values=ggsci::pal_npg("nrc", alpha = .8)(9)[c(7,  9, 6)])+
  ggtitle("Water source for solar pumps in 2050")+
  scale_y_continuous(labels=scales::label_percent())

#ggsave("//tsclient/D/OneDrive - IIASA/Conferences 2022/Scenarios Forum/LEAPRE submission/fig3.png", tre, scale = 1.5)

###

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

baseline_F_wg <- dplyr::select(baseline_F, contains("monthly_unmet_IRRIG_share_") & !contains("surf") & !contains("avg"))
s_245_F_wg <- dplyr::select(s_245_F, contains("monthly_unmet_IRRIG_share_") & !contains("surf") & !contains("avg"))
s_585_F_wg <- dplyr::select(s_585_F, contains("monthly_unmet_IRRIG_share_") & !contains("surf") & !contains("avg"))

baseline_F_wg$geometry <- NULL
s_245_F_wg$geometry <- NULL
s_585_F_wg$geometry <- NULL

#

baseline_F_wd <- dplyr::select(baseline_F, contains("monthly_IRREQ") & !contains("surf") & !contains("avg"))
s_245_F_wd <- dplyr::select(s_245_F, contains("monthly_IRREQ") & !contains("surf") & !contains("avg"))
s_585_F_wd <- dplyr::select(s_585_F, contains("monthly_IRREQ") & !contains("surf") & !contains("avg"))

baseline_F_wd$geometry <- NULL
s_245_F_wd$geometry <- NULL
s_585_F_wd$geometry <- NULL

##

baseline_F_wg <- baseline_F_wg * baseline_F_wd
s_245_F_wg <- s_245_F_wg * s_245_F_wd
s_585_F_wg <- s_585_F_wg * s_585_F_wd

baseline_F_wg <- summarise_all(baseline_F_wg,.funs = "sum")
s_245_F_wg <- summarise_all(s_245_F_wg,.funs = "sum")
s_585_F_wg <- summarise_all(s_585_F_wg,.funs = "sum")

#

baseline_F_wg <- melt(baseline_F_wg)
baseline_F_wg$variable <- month.abb
baseline_F_wg$scen <- "Baseline"

s_245_F_wg <- melt(s_245_F_wg)
s_245_F_wg$variable <- month.abb
s_245_F_wg$scen <- "SSP 245"

s_585_F_wg <- melt(s_585_F_wg)
s_585_F_wg$variable <- month.abb
s_585_F_wg$scen <- "SSP 585"

all <- bind_rows(baseline_F_wg, s_245_F_wg, s_585_F_wg)

all$scen <- factor(all$scen, levels = c("Baseline", "SSP 245", "SSP 585"))
all$variable <- factor(all$variable, levels = month.abb)

all %>% group_by(scen) %>% dplyr::summarise(value=sum(value)/1e9)
all %>% filter(variable=="Aug") %>%  group_by(scen) %>% dplyr::summarise(value=sum(value)/1e9)

quattro <- ggplot(all, aes(x=variable, y=value/1e9, colour=scen, group=scen))+
 geom_line(show.legend=F)+
  theme_classic()+
  xlab("Month of the year")+
  ylab("Km^3 groundwater requir.")+
  ggsci::scale_colour_npg(name="Climate change scenario")+
  ggtitle("Unmet groundwater demand  in 2050")

#ggsave("//tsclient/D/OneDrive - IIASA/Conferences 2022/Scenarios Forum/LEAPRE submission/fig4.png", quattro, scale = 1.5)

######################

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

i_n <- c(sum(baseline_T$calories_gain_total[baseline_T$profit_yearly>0], na.rm=T), sum(s_245_T$calories_gain_total[s_245_T$profit_yearly>0], na.rm=T), sum(s_585_T$calories_gain_total[s_585_T$profit_yearly>0], na.rm=T))

rcp_ssp <- c("Baseline", "SSP 245", "SSP 585")

t1 <- data.frame(rcp_ssp, i_n, stringsAsFactors = F)

t1$rcp_ssp <- factor(t1$rcp_ssp, levels = c("Baseline", "SSP 245", "SSP 585"))

cinque <- ggplot(t1, aes(x=rcp_ssp, y=i_n/1e12, fill=rcp_ssp))+
  theme_classic()+
  geom_col()+
  xlab("")+
  ylab("Trillion calories")+
  ggsci::scale_fill_npg(name="Scenario")+
  ggtitle("Food growth potential from (economically \nfeasible) solar pumps adoption")

#

baseline_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))
s_245_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[2,], collapse="_"), ".Rds"))
s_585_T <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[3,], collapse="_"), ".Rds"))

baseline_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[4,], collapse="_"), ".Rds"))
s_245_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[5,], collapse="_"), ".Rds"))
s_585_F <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[6,], collapse="_"), ".Rds"))

i_n <- c(sum(baseline_T$A_total[baseline_T$profit_yearly>0], na.rm=T), sum(s_245_T$A_total[s_245_T$profit_yearly>0], na.rm=T), sum(s_585_T$A_total[s_585_T$profit_yearly>0], na.rm=T))
rcp_ssp <- c("Baseline", "SSP 245", "SSP 585")

t1 <- data.frame(rcp_ssp, i_n, stringsAsFactors = F)

t1$rcp_ssp <- factor(t1$rcp_ssp, levels = c("Baseline", "SSP 245", "SSP 585"))

sei <- ggplot(t1, aes(x=rcp_ssp, y=i_n/1e6, fill=rcp_ssp))+
  theme_classic()+
  geom_col()+
  xlab("")+
  ylab("Million of hectares equivalent")+
  ggsci::scale_fill_npg(name="Scenario")+
  ggtitle("Harvested area economically \nsuitable for solar irrigation")

library(patchwork)

m <- uno + tre + due + quattro + cinque + sei + patchwork::plot_annotation(tag_levels = 'A') + plot_layout(guides="collect", nrow=3) &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')

m[[1]]$data
m[[2]]$data
m[[3]]$data
m[[4]]$data
m[[5]]$data
m[[6]]$data

ggsave("new_figures/fig_scens.png", m, scale = 1.25, height = 7, width = 7)

