##bar_graphs_for_serotypes
library(readxl)
library(ggsignif)
data<-read_xlsx("PN3_PN5_serotypes.xlsx")

plot<-ggplot(data, aes(fill=Groups, y=log, x=Serotypes)) + 
  geom_bar(position="dodge", stat="identity") + 
  #facet_wrap(~organ)+
  scale_fill_manual(values=c("blue", "red", "darkgreen")) +
  ylab(" log (CFU/ml)")
plot

Plot_theme_adjusted<-plot + theme_classic() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
                                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                                  axis.title = element_text( size = 15, face="bold", colour = "black"),
                                  axis.text.y = element_text(size=12, face="bold", colour = "black"),
                                  axis.text.x = element_text(size=10, face="bold", colour = "black", angle =45, vjust = 1, hjust = 1),
                                  axis.title.x = element_text(size=12, face="bold", colour = "black"),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  legend.text = element_text(colour="black", size=10, face="bold"),
                                  legend.title = element_text(colour="black", size=15, face="bold"))
  ##guides(color = guide_legend(override.aes = list(size = 5))) +

Plot_theme_adjusted
Plot_theme_adjusted_1 <-Plot_theme_adjusted + scale_y_continuous(expand = c(0, 0), limits = c(0,10))
Plot_theme_adjusted_1 

ggsave("serotypes_inhibition_pn3_pn5_1.tiff", Plot_theme_adjusted_1 , width = 7, height = 5, dpi = 300)


## bar graph for biofilm
data<-read_xlsx("MBEC_results.xlsx")

plot<-ggplot(data, aes(y=log_cfu_ml, x=Groups, fill=Groups)) + 
  geom_bar(position="dodge", stat="identity", width=0.5) + 
  geom_errorbar(aes(ymin = log_cfu_ml - SD, ymax = log_cfu_ml + SD), width = 0.2, color = 'black') +
  #facet_wrap(~organ)+
  scale_fill_manual(values=c("darkgreen", "blue", "red", "orange")) +
  ylab(" log (CFU/ml)")
plot

Plot_theme_adjusted<-plot + theme_classic() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size =12, face="bold", colour = "black"),
        axis.text.y = element_text(size=10, face="bold", colour = "black"),
        axis.text.x = element_text(size=10, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

       ## geom_signif(comparisons = list( c("NC", "PC"), c("NC", "PN3_18mM"), c("NC", "PN5_21mM")),
              map_signif_level = TRUE, annotations = c("**"),
              size = 1, textsize=8, y_position = c(8.5, 9.5, 10.5)) 
##guides(color = guide_legend(override.aes = list(size = 5))) 

Plot_theme_adjusted
Plot_theme_adjusted_1 <-Plot_theme_adjusted + scale_y_continuous(expand = c(0, 0), limits = c(0,8))+
  theme(legend.position = "none")
Plot_theme_adjusted_1 

ggsave("biofilm_PN3_PN5.tiff", Plot_theme_adjusted_1 , width = 4, height = 3, dpi = 300)


## graphs for intracellular assay HD-11
data<-read_xlsx("HD_11_results.xlsx")

pn3<-data %>% 
  filter(peptide=="PN3")
pn3$Groups <- ordered(pn3$Groups, levels = c('NC','9mM','13.5mM', '18mM', '27mM'))
                    

pn5<-data %>% 
  filter(peptide=="PN5")
pn5$Groups <- ordered(pn5$Groups, levels = c('NC','10.5mM','15.8mM', '21mM', '31.5mM'))
                     
                               
plot<-ggplot(pn5, aes(y=log_cfu, x=Groups, fill=Groups))+ 
  geom_bar(position="dodge", stat="identity", width=0.5) + 
  geom_errorbar(aes(ymin = log_cfu - SD, ymax = log_cfu + SD), width = 0.2, color = 'black') +
  scale_fill_manual(values=c("darkgreen", "blue", "red", "orange", "purple")) +
  #facet_wrap(~peptide)+
  ylab(" log (CFU/ml)")
plot

## Apply anova to see which groups are different
data_anova<-read_excel("HD_11_anova.xlsx")
pn3_anova<-data_anova %>% 
  filter(peptide=="PN3")

pn5_anova<-data_anova %>% 
  filter(peptide=="PN5")


#3Apply anova to see if there is significant difference
anova <- aov(log_cfu ~ Groups, data = pn5_anova)
summary<-summary(anova)
print(summary)
tukey<-TukeyHSD(anova)
print(tukey)

Plot_theme_adjusted<-plot + theme_classic() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size =15, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
        geom_signif(comparisons = list( c("NC", "21mM"), c("NC", "31.5mM")),
        map_signif_level = TRUE, annotations = c("***","***" ),
size = 0.5, textsize=5, y_position = c(6.5, 7.5)) 
##guides(color = guide_legend(override.aes = list(size = 5))) 

Plot_theme_adjusted
Plot_theme_adjusted_1 <-Plot_theme_adjusted + scale_y_continuous(expand = c(0, 0), limits = c(0,8.5))+
  theme(legend.position = "none") #+ ggtitle("PN3_HD-11")
Plot_theme_adjusted_1 

ggsave("PN5_HD_11_intracellular.tiff", Plot_theme_adjusted_1 , width = 4, height = 3.5, dpi = 300)

## graphs for intracellular assay caco-2
data<-read_xlsx("caco_2_intracellular.xlsx")

pn3_anova<-data %>% filter(peptide=="PN3")
  
  
pn3_plot<-pn3_anova %>% 
  group_by(Groups) %>% 
  summarise(mean = mean(log_cfu),
            sd = sd(log_cfu))

pn3_plot$Groups <- ordered(pn3_plot$Groups, levels = c('NC','13.5mM','18mM', '27mM', '36mM', '45mM'))

pn5_anova<-data %>% filter(peptide=="PN5")


pn5_plot<-pn5_anova %>% 
  group_by(Groups) %>% 
  summarise(mean = mean(log_cfu),
            sd = sd(log_cfu))

pn5_plot$Groups <- ordered(pn5_plot$Groups, levels = c('NC','15.8mM','21mM', '31.5mM', '42mM', '52.5mM'))


plot<-ggplot(pn3_plot, aes(y=mean, x=Groups, fill=Groups))+ 
  geom_bar(position="dodge", stat="identity", width=0.5) + 
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = 'black') +
  scale_fill_manual(values=c("darkgreen", "blue", "red", "orange", "purple", "brown")) +
  #facet_wrap(~peptide)+
  ylab(" log (CFU/ml)")
plot

## Apply anova to see which groups are different
data_anova<-read_excel("HD_11_anova.xlsx")
pn3_anova<-data_anova %>% 
  filter(peptide=="PN3")

#3Apply anova to see if there is significant difference
anova <- aov(log_cfu ~ Groups, data = pn3_anova)
summary<-summary(anova)
print(summary)
tukey<-TukeyHSD(anova)
print(tukey)

Plot_theme_adjusted<-plot + theme_classic() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size =15, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_signif(comparisons = list( c("NC", "21mM"), c("NC", "31.5mM"), c("NC", "42mM"), c("NC", "52.5mM")),
              map_signif_level = TRUE, annotations = c("**","***", "***", "***" ),
              size = 0.5, textsize=5, y_position = c(6.5, 7.5, 8.5, 9.5)) 
##guides(color = guide_legend(override.aes = list(size = 5))) 

Plot_theme_adjusted<-plot + theme_classic() +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size =15, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_signif(comparisons = list( c("NC", "27mM"), c("NC", "36mM"), c("NC", "45mM")),
              map_signif_level = TRUE, annotations = c("***", "***", "***" ),
              size = 0.5, textsize=5, y_position = c(6.5, 7.5, 8.5)) 
##guides(color = guide_legend(override.aes = list(size = 5))) 

Plot_theme_adjusted
Plot_theme_adjusted_1 <-Plot_theme_adjusted + scale_y_continuous(expand = c(0, 0), limits = c(0,9.5))+
  theme(legend.position = "none") #+ ggtitle("PN5_cac0-2-11")
Plot_theme_adjusted_1 

ggsave("PN3_cac0-2_intracellular.tiff", Plot_theme_adjusted_1 , width = 4.5, height = 3.5, dpi = 300)

