# istogramma costi (componenti)  per pompa

clusters <- read_rds(paste0("clusters_with_data_7_", paste(scenarios[1,], collapse="_"), ".Rds"))

clusters_si <- clusters

clusters_si <- filter(clusters_si, profit_yearly>0 & !is.na(profit_yearly))

f_s_1 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=totalcost/npumps, fill="PV"), alpha=0.75)+
  geom_histogram(aes(x=totalpumpcost/npumps, fill="Pump"), alpha=0.75)+
  geom_histogram(aes(x=transp_costs/npumps, fill="Transport"), alpha=0.75)+
  geom_vline(aes(xintercept=median(totalcost/npumps, na.rm=T)), alpha=0.3, colour="green")+
    geom_vline(aes(xintercept=median(totalpumpcost/npumps, na.rm=T)), alpha=0.3, colour="red")+
    geom_vline(aes(xintercept=median(transp_costs/npumps, na.rm=T)), alpha=0.3, colour="blue")+
    scale_x_continuous(limits = c(0, 10000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Lifetime costs (USD)")+
  ylab("Count of sites")+
  scale_fill_discrete(name="")
  

# istogramma revenues/profits per pompa

f_s_2 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=tt_ddvl/npumps, fill="Revenues"), alpha=0.75)+
  geom_histogram(aes(x=profit_yearly/npumps, fill="Profits"), alpha=0.75)+
  geom_vline(aes(xintercept=median(tt_ddvl/npumps, na.rm=T)), alpha=0.3, colour="blue")+
  geom_vline(aes(xintercept=median(profit_yearly/npumps, na.rm=T)), alpha=0.3, colour="red")+
  scale_x_continuous(limits = c(0, 10000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Yearly revenues / profits (USD)")+
  ylab("")+
  scale_fill_discrete(name="")

# istogramma pump size, PV size, battry size, pump energy consumption

f_s_3 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=pvsize_kw, fill="PV (kW)"), alpha=0.75)+
  geom_vline(aes(xintercept=median(pvsize_kw, na.rm=T)), alpha=0.3, colour="blue")+
  geom_histogram(aes(x=batterysize_kwh, fill="Battery (kWh)"), alpha=0.75)+
  geom_vline(aes(xintercept=median(batterysize_kwh, na.rm=T)), alpha=0.3, colour="red")+
  geom_histogram(aes(x=powerforpump, fill="Pump (kW)"), alpha=0.75)+
  geom_vline(aes(xintercept=median(powerforpump, na.rm=T)), alpha=0.3, colour="green")+
  scale_x_continuous(limits = c(0, 10))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Capacity")+
  ylab("")+
  scale_fill_discrete(name="")

f_s_4 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=er_kwh_tt, fill="kWh/pump/year"), alpha=0.75)+
  geom_vline(aes(xintercept=median(er_kwh_tt, na.rm=T)), alpha=0.3, colour="red")+
  scale_x_continuous(limits = c(0, 2000))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Electricity consumption")+
  ylab("Count of sites")+
  scale_fill_discrete(name="")

f_s_5 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=er_kwh_tt/6/powerforpump, fill="Days of pump use / year"), alpha=0.75)+
  scale_fill_manual(name="", values="forestgreen")+
    geom_vline(aes(xintercept=median(er_kwh_tt/6/powerforpump, na.rm=T)), alpha=0.3, colour="forestgreen")+
  scale_x_continuous(limits = c(14, 100))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("Pump use frequency")+
  ylab("")

f_s_6 <- ggplot(clusters_si)+theme_classic()+
  geom_histogram(aes(x=PBT, fill="Years"), alpha=0.75)+
  scale_fill_manual(name="", values="gold")+
  geom_vline(aes(xintercept=median(PBT, na.rm=T)), alpha=0.3, colour="gold")+
  scale_x_continuous(limits = c(0, 20))+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  xlab("System payback time")+
  ylab("")

library(patchwork)

f_s_1 + f_s_2 +f_s_3 +f_s_4 +f_s_5+f_s_6 + plot_annotation(title="Distribution of metrics across solar pumping economic feasibility sites")

ggsave("new_figures/fig_si.png", last_plot(), scale=3.3, height = 2, width = 3)


#############





