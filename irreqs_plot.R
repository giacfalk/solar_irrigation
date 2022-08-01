clusters$yearly_IRREQ_cut <- cut(clusters$yearly_IRREQ/1000,
                      breaks=c(0, 1, 10, 100, 1000, Inf),
                      labels=c('<1', '1-10', '10-100', '100-1000', '>1000'))

fig_wr <-  ggplot() + geom_sf(data = clusters,
                             aes(fill = yearly_IRREQ_cut), colour=NA)+
  scale_fill_viridis_d(name="", na.value = NA, na.translate = F)+
  geom_sf(data=world_all, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-34, 36), xlim=c(-20, 51))+
  theme(legend.position = c(0.2, 0.28), legend.direction = "vertical", legend.text=element_text(size=8), axis.line=element_blank(),axis.text.x=element_blank(), legend.title=element_text(size=7, face = "bold"), panel.border = element_rect(colour = "black", fill=NA, size=1),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(), plot.title = element_text(size = 10, hjust = 0.5))+
  ggtitle("Water needs to close irrigation gap, thousand m3 / year")+
  xlab("")+
  ylab("")

ggsave("new_figures/fig_wr.png", fig_wr, scale=1.35, height = 4, width = 4)
