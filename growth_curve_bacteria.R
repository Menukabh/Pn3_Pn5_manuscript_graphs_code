#3MIC and MBC plot
setwd("C:/Users/bhandari.72/Desktop/Hayes_poster/PN3 and PN5 results")
library(ggplot2)
library(readxl)
library(RColorBrewer)
##Import_data
data<-read_xlsx("PN3_PN5_MIC_for_graphs.xlsx")
##plot the graph
p <- ggplot(data, aes(x=Time, y=OD, color=Groups, shape=Groups))+
  geom_point() +
  geom_line()+
  theme_bw()+
  theme(legend.position = "right", 
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size = 15, face="bold"),
        axis.text = element_text(face="bold")) +
  facet_wrap(~ Peptide, scales='free') +
  ylab("OD 600nm") +
  xlab("Time (hrs)")
  
plot<-p +scale_color_brewer(palette="Set1")+
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9))+
  scale_x_continuous(breaks = seq(0, 12.5, by = 1))+
  scale_y_continuous(limits=c(0.0, NA)) +
  theme(panel.spacing = unit(2, "lines"))

plot

ggsave("MIC_plot_with_different_shapes.tiff", plot, width = 8.5, height = 4.5, dpi = 300)

##graphs for the inhibition of the different serotype of Salmonella by PN3 and PN5 peptides
#load_data
a<-read.csv("liver_first_second_necropsy.csv")
theme_set(theme_bw(16))
#3make grouped bar graph
a$Groups <- ordered(a$Groups,
                    levels = c("LS", "CS", "PC"))

plot<-ggplot(a, aes(fill=Necropsy, y=Pecent_positive, x=Groups)) + 
  geom_bar(position="dodge", stat="identity") + 
  facet_wrap(~organ)+
  scale_fill_manual(values=c("darkblue", "red")) +
  ylab(" Salmonella positive organs(%)")

plot
Plot_theme_adjusted<-plot + theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
                                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                                  axis.title = element_text( size = 15, face="bold", colour = "black"),
                                  axis.text = element_text(size=12, face="bold", colour = "black"),
                                  axis.title.x = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  legend.text = element_text(colour="black", size=10, face="bold"),
                                  legend.title = element_text(colour="black", size=15, face="bold"))+
  guides(color = guide_legend(override.aes = list(size = 5)))



## make plot bacterial growth for thermal stability

data<-read_excel("thermal_stability.xlsx")
data$Groups <- ordered(data$Groups,
                       levels = c( "80 °C" ,"100 °C"  , "121 °C", "Con_PN5", "Con_PN3", "PC", "NC"))


p <- ggplot(data, aes(x=Time, y=OD, color=Groups, shape=Groups))+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin =  OD - SD, ymax = OD + SD), width = 0.1, color = 'black') +
  theme_bw()+
  theme(legend.position = "right", 
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size = 15, face="bold"),
        axis.text = element_text(face="bold")) +
  facet_wrap(~ Peptide, scales='free') +
  ylab("OD 600nm") +
  xlab("Time (hrs)")

p
plot<-p +scale_color_brewer(palette="Dark2")
  #scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9))+
  ##scale_x_continuous(breaks = seq(0, 12.5, by = 1))
  #scale_y_continuous(limits=c(0.0, 0.55, by =0.2)) 
plot
plot_1<-plot +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size = 15, face="bold", colour = "black"),
        axis.text = element_text(size=12, face="bold", colour = "black"),
        legend.text = element_text(colour="black", size=10, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))

ggsave("Pn3_pn5_thermal_stability_1.tiff", plot_1, width = 8.5, height = 4.5, dpi = 300)

## make plot bacterial growth for proteinase k stability

data<-read_excel("proteinase_K_results.xlsx")
data$Groups <- ordered(data$Groups,
                       levels = c( "PK-PN3" , "Con_PN3", "PK-PN5" , "Con_PN5", "PK-water", "PC", "NC"))


p <- ggplot(data, aes(x=Time, y=OD, color=Groups, shape=Groups))+
  geom_point() +
  geom_line()+
  geom_errorbar(aes(ymin =  OD - SD, ymax = OD + SD), width = 0.1, color = 'black') +
  theme_bw()+
  theme(legend.position = "right", 
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size = 15, face="bold"),
        axis.text = element_text(face="bold")) +
  facet_wrap(~ Peptide, scales='free') +
  ylab("OD 600nm") +
  xlab("Time (hrs)") 

p
plot<-p +scale_color_brewer(palette="Dark2")

plot_1<-plot +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
        axis.title = element_text( size = 15, face="bold", colour = "black"),
        axis.text = element_text(size=12, face="bold", colour = "black"),
        legend.text = element_text(colour="black", size=10, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))
#scale_shape_manual(values = c(0,1,2,3,4,5,6,7,8,9))+
##scale_x_continuous(breaks = seq(0, 12.5, by = 1))
#scale_y_continuous(limits=c(0.0, 0.55, by =0.2)) 
plot_1

ggsave("Pn3_pn5_proteinase_k_stability_1.tiff", plot_1, width = 8.5, height = 4.5, dpi = 300)


## alanine scanning results-PN3
data<-read_excel("alanine_scanning_results_PN3_15mM.xlsx")
theme_set(theme_bw(16))
#3make grouped bar graph
data$PN3 <- ordered(data$PN3,
                    levels = c('NC', 'Original','V14A', 'E13A', 'I12A',  'D8A', 'Q5A','Q2A', 
                               'V1A'))

mycols <- c("#a74bb4", "#62b54f", "#7064d3", "#b5b348", "#dd6fc5",
            "#4db18a", "#ce416e", "#45aecf", "#d55035", "#7784cb",
            "#cc8e3e", "#ac6090", "#647934", "#df837e", "#9c5736")

plot<-ggplot(data, aes(fill=PN3, x=PN3, y=Average)) + 
  geom_bar(stat="identity") + 
  #facet_wrap(~organ)+
  #scale_fill_manual(values=c("darkblue", "red")) +
  ylab(" Log(CFU/ml)") +
  #geom_errorbar(aes(ymin =  Average- St_dev, ymax = Average + St_dev), width = 0.1, color = 'black') +
  coord_flip()+
  scale_fill_manual(values=mycols)



plot
Plot_theme_adjusted<-plot + theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
                                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                                  axis.title = element_text( size = 15, face="bold", colour = "black"),
                                  axis.text = element_text(size=12, face="bold", colour = "black"),
                                  axis.title.y = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  legend.text = element_blank(),
                                  legend.title = element_blank())

final_plot<-Plot_theme_adjusted+ guides(fill="none")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10.5))


final_plot

ggsave("alanine_scanning_pn3_15mM.tiff", final_plot, width = 4.5, height = 4.5, dpi = 300)

## alanine scanning results-PN5
data<-read_excel("alanine_scanning_results_PN5_21mM.xlsx")
theme_set(theme_bw(16))
#3make grouped bar graph
data$PN5 <- ordered(data$PN5,
                    levels = c('NC', 'Original','V16A','N15A','S14A','T11A', 
                               'T10A', 'K7A', 'S5A', 'T4A','D3A', 'T2A', 'V1A'))
                               

mycols <- c("#a74bb4", "#62b54f", "#7064d3", "#b5b348", "#dd6fc5",
            "#4db18a", "#ce416e", "#45aecf", "#d55035", "#7784cb",
            "#cc8e3e", "#ac6090", "#647934", "#df837e", "#9c5736")

plot<-ggplot(data, aes(fill=PN5, x=PN5, y=Average)) + 
  geom_bar(stat="identity") + 
  #facet_wrap(~organ)+
  #scale_fill_manual(values=c("darkblue", "red")) +
  ylab(" Log(CFU/ml)") +
  geom_errorbar(aes(ymin =  Average- St_dev, ymax = Average + St_dev), width = 0.1, color = 'black') +
  coord_flip()+
  scale_fill_manual(values=mycols)



plot
Plot_theme_adjusted<-plot + theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
                                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                                  axis.title = element_text( size = 15, face="bold", colour = "black"),
                                  axis.text = element_text(size=12, face="bold", colour = "black"),
                                  axis.title.y = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  legend.text = element_blank(),
                                  legend.title = element_blank())

final_plot<-Plot_theme_adjusted+ guides(fill="none")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10.5))


final_plot

ggsave("alanine_scanning_pn5_21mM.tiff", final_plot, width = 4.5, height = 4.5, dpi = 300)


## alanine scanning results-PN5-15mM
setwd("C:/Users/bhandari.72/Desktop/Hayes_poster/PN3 and PN5 results/alanine_scanning")
data<-read_excel("alanine_scanning_results_PN5_18mM.xlsx")
theme_set(theme_bw(16))
#3make grouped bar graph
data$PN5 <- ordered(data$PN5,
                    levels = c('NC', 'Original','T11A', 
                               'S5A', 'T4A','D3A'))


mycols <- c("#a74bb4", "#62b54f", 
            "#4db18a", 
            "#cc8e3e", "#ac6090")

plot<-ggplot(data, aes(fill=PN5, x=PN5, y=Average)) + 
  geom_bar(stat="identity") + 
  #facet_wrap(~organ)+
  #scale_fill_manual(values=c("darkblue", "red")) +
  ylab(" Log(CFU/ml)") +
  geom_errorbar(aes(ymin =  Average- St_dev, ymax = Average + St_dev), width = 0.1, color = 'black') +
  coord_flip()+
  scale_fill_manual(values=mycols)



plot
Plot_theme_adjusted<-plot + theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
                                  axis.line = element_line(colour = "black", size = 1, linetype = "solid"),
                                  axis.title = element_text( size = 15, face="bold", colour = "black"),
                                  axis.text = element_text(size=12, face="bold", colour = "black"),
                                  axis.title.y = element_blank(),
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(), 
                                  legend.text = element_blank(),
                                  legend.title = element_blank())

final_plot<-Plot_theme_adjusted+ guides(fill="none")+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10.5))


final_plot

ggsave("alanine_scanning_pn5_18mM.tiff", final_plot, width = 4.5, height = 3.5, dpi = 300)

