setwd("C:/Users/bhandari.72/Documents/desktop_10_2_2023/lab data/wax moth data/5-17-2023 peptide and salmonella")

library(ggplot2)
library(readxl)
install.packages("survminer")
library("survminer")
install.packages("ggfortify")
library("ggfortify")
library("survival")

#survival for computing survival analyses
#survival for computing survival analyses

a<-read_xlsx("wax_moth_data_05_17_2023.xlsx")
attach(a)
km_fit<- survfit(Surv(time=Time, event=Death) ~ Group, data=a) 
#where time represent the survival time and event= censoring status
print(km_fit)
summary(km_fit)
a$Group<-factor(a$Group, levels=c("NC", "MgSO4", "PN3", "PN5", "PC"))

unique(a$Group)

p<-ggsurvplot(km_fit, data = a,
           linetype = "dashed",
           xlab= "Time Points",#axis label of x
           ylab= "Survival Probability", # axis label of y
           pval = FALSE,
           font.x=c(16, "bold", "black"),
           font.y=c(16, "bold", "black"),
           font.legend = c(14, "bold", "black"),
           font.tickslab = c(14, "bold", "black"),
           break.y.by = 0.1, # Y axis breaks
           break.x.by=12,
           size = 1, # line width
           #risk.table = TRUE, # to count the number of deaths in the bottom
           legend = "right",
           palette=c("orange", "red", "green", "black", "blue"),
           legend.title="Groups",
           xlim = c(0,72))
p        

ggplot_plot <- p$plot


ggsave("survival_plot_changed_font.png", height=5, width=7, ggplot_plot, dpi=300)


## Most commonly used statistics for survivality
#it assumes that the survival curves for each group are proportional over time. 
#This means that the difference in survival between two groups is assumed to be constant over time, 
#and the log-rank test is used to test whether this assumption is valid.
logrank <- survdiff(Surv(time=Time, event=Death) ~ Group, data=a)
summary(logrank)
