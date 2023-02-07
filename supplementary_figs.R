# istogramma costi (componenti)  per pompa

scenario <- 1

clusters <-  read_rds(paste0("clusters_with_data_7_", paste(scenarios[scenario,], collapse="_"), ".Rds"))

clusters_si <- clusters

clusters_si <- filter(clusters_si, profit_yearly>0 & !is.na(profit_yearly))

library(r2spss)

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean((irrig_sys_costs_discounted[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]/npumps[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0])+(prod_cost_irr_tot_discounted[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]/npumps[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]), trim = 0.1)) +

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(totalcost[totalcost!=0]/npumps[totalcost!=0], trim = 0.1)) +

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(totalpumpcost[totalpumpcost!=0]/npumps[totalpumpcost!=0], trim = 0.1)) +

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(transp_costs[transp_costs!=0]/npumps[transp_costs!=0], trim = 0.1))


f_s_1 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=totalcost/npumps, fill="PV"), alpha=0.75)+
  geom_histogram(aes(x=totalpumpcost/npumps, fill="Pump"), alpha=0.75)+
  geom_histogram(aes(x=transp_costs/npumps, fill="Transport"), alpha=0.75)+
  geom_histogram(aes(x=(irrig_sys_costs_discounted/npumps)+(prod_cost_irr_tot_discounted/npumps), fill="Irr. sys. & farming"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean((irrig_sys_costs_discounted[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]/npumps[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0])+(prod_cost_irr_tot_discounted[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]/npumps[irrig_sys_costs_discounted!=0 & prod_cost_irr_tot_discounted!=0]), trim = 0.1)), alpha=0.3, colour="red")+
  geom_vline(aes(xintercept=trimmed_mean(totalcost[totalcost!=0]/npumps[totalcost!=0], trim = 0.1)), alpha=0.3, colour="blue")+
    geom_vline(aes(xintercept=trimmed_mean(totalpumpcost[totalpumpcost!=0]/npumps[totalpumpcost!=0], trim = 0.1)), alpha=0.3, colour="green")+
    geom_vline(aes(xintercept=trimmed_mean(transp_costs[transp_costs!=0]/npumps[transp_costs!=0], trim = 0.1)), alpha=0.3, colour="purple")+
    scale_x_continuous(limits = c(0, 10000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Lifetime costs (USD)")+
  ylab("Count of sites")+
  scale_fill_discrete(name="")
  

# istogramma revenues/profits per pompa

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(tt_ddvl[!is.na(tt_ddvl)]/npumps[!is.na(tt_ddvl)], trim = 0.1))

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(profit_yearly[!is.na(tt_ddvl)]/npumps[!is.na(tt_ddvl)], trim = 0.1))

f_s_2 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=tt_ddvl/npumps, fill="Revenues"), alpha=0.75)+
  geom_histogram(aes(x=profit_yearly/npumps, fill="Profits"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean(tt_ddvl/npumps, trim = 0.1)), alpha=0.3, colour="blue")+
  geom_vline(aes(xintercept=trimmed_mean(profit_yearly/npumps, trim = 0.1)), alpha=0.3, colour="red")+
  scale_x_continuous(limits = c(0, 10000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Yearly revenues / profits (USD)")+
  ylab("")+
  scale_fill_discrete(name="")

# istogramma pump size, PV size, battry size, pump energy consumption

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(pvsize_kw[pvsize_kw>0], trim = 0.1))

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(batterysize_kwh[batterysize_kwh>0], trim = 0.1))

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(powerforpump[powerforpump>0], trim = 0.1))

f_s_3 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=pvsize_kw, fill="PV (kW)"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean(pvsize_kw[pvsize_kw>0], trim = 0.1)), alpha=0.3, colour="blue")+
  geom_histogram(aes(x=batterysize_kwh, fill="Battery (kWh)"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean(batterysize_kwh[batterysize_kwh>0], trim = 0.1)), alpha=0.3, colour="red")+
  geom_histogram(aes(x=powerforpump, fill="Pump (kW)"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean(powerforpump[powerforpump>0], trim = 0.1)), alpha=0.3, colour="green")+
  scale_x_continuous(limits = c(0, 10))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Capacity")+
  ylab("")+
  scale_fill_discrete(name="")

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(er_kwh_tt[er_kwh_tt>0], trim = 0.1))

f_s_4 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=er_kwh_tt, fill="kWh/pump/year"), alpha=0.75)+
  geom_vline(aes(xintercept=trimmed_mean(er_kwh_tt, trim = 0.1)), alpha=0.3, colour="red")+
  scale_x_continuous(limits = c(0, 2000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Electricity consumption")+
  ylab("Count of sites")+
  scale_fill_discrete(name="")

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(er_kwh_tt/6/powerforpump, trim = 0.1))

f_s_5 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=er_kwh_tt/6/powerforpump, fill="Days of pump use / year"), alpha=0.75)+
  scale_fill_manual(name="", values="forestgreen")+
    geom_vline(aes(xintercept=trimmed_mean(er_kwh_tt/6/powerforpump, trim = 0.1)), alpha=0.3, colour="forestgreen")+
  scale_x_continuous(limits = c(14, 100))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Pump use frequency")+
  ylab("")

clusters_si %>% st_set_geometry(NULL) %>% dplyr::summarise(a=trimmed_mean(PBT, trim = 0.1))


f_s_6 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=PBT, fill="Years"), alpha=0.75)+
  scale_fill_manual(name="", values="gold")+
  geom_vline(aes(xintercept=trimmed_mean(PBT, trim = 0.1)), alpha=0.3, colour="gold")+
  scale_x_continuous(limits = c(0, 20))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("System payback time")+
  ylab("")

library(patchwork)

f_s_1 + f_s_2 +f_s_3 +f_s_4 +f_s_5+f_s_6 + plot_annotation(title="Distribution of metrics across solar pumping economic feasibility sites")

ggsave("new_figures/fig_si.png", last_plot(), scale=3.7, height = 2, width = 3)

#############

