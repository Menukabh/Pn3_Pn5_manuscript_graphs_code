setwd("C:/Users/bhandari.72/Desktop/lab data/wax moth data/3_27-2023_salmonella_peptide")

library(readxl)
library(ggplot2)
library(ggsignif)
#read csv file
b<-read_xlsx("salmonella_load_first_and_second_trial_wax_moth.xlsx")

#calculate mean of the group to make the line from it
group_mean <- aggregate(load ~ Groups, data = b, mean) 
#display plot
c<-ggplot(b, aes(x=Groups, y=load, shape=Groups)) +  
  geom_jitter(width=0.1, size = 3) +
  geom_crossbar(data=group_mean, aes(ymin = load, ymax =load),
                size=0.5, col="red", width = .3)+ 
  scale_fill_manual('Groups')+
  theme_classic()+
  scale_y_continuous(limits = c(0, 11.5), breaks = seq(0, 10, by = 2))+
  theme(axis.title = element_text(face = "bold"), 
        axis.text = element_text(face = "bold"))+
  labs(y= "Log(CFU/larva)", x = "")
 

d<-c +
  theme(legend.position = "right", legend.key.size = unit(0.5, 'cm'),
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        axis.title = element_text( size = 14, face="bold", colour = "black"),
        axis.text.y = element_text(size=12, face="bold", colour = "black"),
        axis.text.x = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.text = element_text(colour="black", size=14, face="bold"),
        legend.title = element_text(colour="black", size=15, face="bold"))+
        geom_signif(comparisons = list(c("PN3", "PC"), c("PN5", "PC")),
              map_signif_level = TRUE, annotations = "***", size = 1, textsize=8,
              y_position = c(9.5, 10.5))

d

ggsave("salmonella-load_inside_wax_moth_3_17_2023.png", d, width = 5.5, height = 4.5, dpi = 300) 




#One way ANOVA
one.way <- aov(log10 ~ Treatment, data = b)
summary(one.way)
#Tukey test
tukey.two.way<-TukeyHSD(one.way)

tukey.two.way




bp<-geom_jitter(b, aes(x=Treatment, y=log10, fill=Treatment,color=Treatment )) 
q <- ggplot(b, aes(x=Treatment, y=log10, fill=Treatment)) 
  
q+ geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+ theme_classic()+
  geom_signif(comparisons = list(c("P2-100", "PC")), map_signif_level=TRUE)+labs(y= "Log10(CFU/ml)", x = "Treatment")
+ geom_crossbar(group_mean, aes(ymin = log10, ymax = log10), size=1, col="red", width = .5)

#Compute unpaired two-samples t-test

res <- t.test(log10 ~ Treatment, data = a, var.equal = TRUE)
res
