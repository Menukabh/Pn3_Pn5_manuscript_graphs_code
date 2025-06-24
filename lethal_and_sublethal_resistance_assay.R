setwd("C:/Users/bhandari.72/Desktop/Hayes_poster/PN3 and PN5 results")
library(ggplot2)
library(readxl)
library(RColorBrewer)
##Import_data
data<-read.xlsx("lethal_resistance_assay.xlsx", sheet=2)
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

p
ggsave("lethal_resistance_assay.tiff", p, height=4, width=7,  dpi=300)
