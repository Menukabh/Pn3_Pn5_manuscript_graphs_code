##make the helical wheel of the antimicrobial peptides

setwd("C:/Users/bhandari.72/Desktop/Hayes_poster/PN3 and PN5 results")
library("helixvis")
library(gridExtra)

##you can save the plot in the higher resolution using the dev.off function in R

data<-read.table('PN3_PN5_seq_helical_wheel.txt', header = TRUE, stringsAsFactors = FALSE)


tiff('pn5.tiff', units="in", width=5, height=6, res=300)

## Make helical wheel plot using the default color function
plot<-draw_wheel(data$Seq[1], labels = TRUE, label.col = "black",
                 fixed= TRUE,
                 legend=TRUE)


##If you would like to change the color of the circle
plot<-draw_wheel(data$Seq[1], col = c("purple",  "orange", "red", "lightgreen"),
           labels = TRUE, label.col = "black",
           fixed= TRUE,
           legend=TRUE)
plot



plot_pn5<-draw_wheel(data$Seq[2], col = c("purple",  "orange", "red", "lightgreen"),
                 labels = TRUE, label.col = "black",
                 fixed= TRUE,
                 legend=TRUE)
plot_pn5

##to save graph
dev.off

## tovisualize graph in R
dev.new()


