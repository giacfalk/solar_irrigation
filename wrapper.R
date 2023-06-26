# Quantifying the economic feasibility of solar irrigation in sub-Saharan Africa
# Giacomo Falchetta, Francesco Semeria and Marta Tuninetti
# Version: May 2023

############

#

setwd("C:/Users/falchetta/OneDrive - IIASA/Current papers/Groundwater_economic_feasibility/Groundwater-Cost/Groundwater-Cost")

input_folder <- "H:/ECIP/Falchetta/MLED_database/input_folder/"

email<- "giacomo.falchetta@gmail.com"

read_existing_clusters = F

#

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

scenarios <- expand.grid(rcp_ssp, water_sustainability_contraint, field_size_contraint, VAT_import_costs, instalments_business_model, water_tank_storage, discount_rate, no_battery, prices_sens, pvbatterycosts_sens, stringsAsFactors = F)

colnames(scenarios) <- c("rcp_ssp", "water_sustainability_contraint", "field_size_contraint", "VAT_import_costs", "instalments_business_model", "water_tank_storage", "discount_rate", "no_battery", "prices_sens", "pvbatterycosts_sens")

scenarios_selected <- c(1:3, 4:6, 7, 13, 25, 49, 73, 97, 193, 385, 577, 769, 1537, 3073, 4609, 9217)
#View(scenarios[scenarios_selected,])

for (scenario in scenarios_selected){
  
  print(timestamp())
  print(scenario)
  
  discount_rate <- scenarios$discount_rate[scenario]
  
  source("backend.R", echo=F)
  if(read_existing_clusters==FALSE){source("1_process_irrigation_water_requirements.R", echo=F)} else{
    clusters <- read_rds("clusters_with_data.Rds"); clusters_bk_all <- read_rds(paste0("clusters_bk_", paste(scenarios[scenario,], collapse="_"), ".Rds"))
  }
  
  source("2_estimate_energy_needs.R" , echo=F)
  
  source("3_pumps_installation_costs.R" , echo=F)
  
  source("4_process_energy_costs.R"     , echo=F)
  
  source("4b_process_irrigation_costs.R"     , echo=F)
  
  source("5_estimate_economic_revenues.R" , echo=F)
  
  #source("6_results_plots_tables.R", echo=F)
  
  source("7_food_security_implications.R", echo=F)
  
  print(timestamp())
  
  save <- c("scenarios", "read_existing_clusters", "email", "input_folder", "scenarios_selected")
  rm(list=setdiff(ls(), save))  
  gc()
  
}

source("figures_for_paper.R", echo=F)
source("supplementary_figs.R", echo=F)
source("analyse_sensitivity_runs.R", echo=F)
source("climate_change_implications.R", echo=F)

###

source("summary_stats_for_paper.R", echo=F)
source("summary_tables.R", echo=F)
