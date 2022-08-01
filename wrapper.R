# Quantifying the economic feasibility of solar irrigation in sub-Saharan Africa
# Giacomo Falchetta, Francesco Semeria and Marta Tuninetti
# Version: July 2022

############

# NB: plug external disk F

#

setwd("D:/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

input_folder <- "F:/MLED_database/input_folder/"
email<- "giacomo.falchetta@gmail.com"

read_existing_clusters = F

#

#rcp_ssp <- c("baseline")
rcp_ssp <- c("baseline", "245", "585")
groundwater_sustainability_contraint <- c(T, F)
field_size_contraint <- c(T, F)
VAT_import_costs <- c(T)
instalments_business_model <- c(T)
water_tank_storage <- c(T)
discount_rate = c(0.15, 0.075, 0.25, 0.4)

scenarios <- expand.grid(rcp_ssp, groundwater_sustainability_contraint, field_size_contraint, VAT_import_costs, instalments_business_model, water_tank_storage, discount_rate, stringsAsFactors = F)

colnames(scenarios) <- c("rcp_ssp", "groundwater_sustainability_contraint", "field_size_contraint", "VAT_import_costs", "instalments_business_model", "water_tank_storage", "discount_rate")

#scenario <- 1

scenarios_selected <- c(1:3, 4:6, 7, 13, 25, 37)

#View(scenarios[scenarios_selected,])

#

for (scenario in scenarios_selected){
print(scenario)
  
discount_rate <- scenarios$discount_rate[scenario]
  
source("backend.R", echo=F)
if(read_existing_clusters==FALSE){source("1_process_irrigation_water_requirements.R", echo=F)} else{
  clusters <- read_rds("clusters_with_data.Rds")
}
source("2_estimate_energy_needs.R" , echo=F)
source("3_pumps_installation_costs.R" , echo=F)
source("4_process_energy_costs.R"     , echo=F)
source("5_estimate_economic_revenues.R" , echo=F)
#source("6_results_plots_tables.R", echo=F)
source("7_food_security_implications.R", echo=F)

save <- c("scenarios", "read_existing_clusters", "email", "input_folder", "scenarios_selected")
rm(list=setdiff(ls(), save))  
gc()

}

source("figures_for_paper.R", echo=F)
source("merge_scenarios_produce_joint_figures.R", echo=F)
