library(ggplot2) 
Sys.setlocale("LC_MESSAGES", 'french')

en <- TRUE

bench <- "GST-B_qualities/parameterPValues.csv"

h <- 4
w <- 8

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ";", strip.white=TRUE)
  names(df) <- c("P", "GAIN", "LOSS")
  df
}


print <- function(data){
dtemp<-data
ntemp <- data.frame(P= dtemp[2:(nrow(dtemp)),1]-0.0000001, GAIN= dtemp[1:nrow(dtemp)-1,2], LOSS= dtemp[1:nrow(dtemp)-1,3])
ntemp[1,]<-c(1,1,1)
dtemp<-rbind(dtemp, ntemp)
xlabel<- "Paramètre p"
ylabel<- "Amplitude des mesures de qualité"
legend<- "Amplitude des mesures de qualité en fonction du parametre p"
plot<-ggplot(dtemp, aes(x=P))
plot<-plot + geom_line(aes(y=GAIN, colour = "Réduction de complexité"))
plot<-plot + geom_line(aes(y=LOSS, colour = "Perte d'information"))
plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_colour_manual(name="Mesures de qualité",values = c("red","green"))
plot<-plot + annotate("rect", xmin=0.963, xmax=1, ymin=0, ymax=1, fill='black', alpha=0.2)
plot<-plot + annotate("text",x=0.973, y=0.5,hjust=.2,label="D")
plot<-plot + annotate("rect", xmin=0.375, xmax=0.963, ymin=0, ymax=1, fill='grey', alpha=0.2)
plot<-plot + annotate("text",x=0.67, y=0.5,hjust=.2,label="C")
plot<-plot + annotate("rect", xmin=0.180, xmax=0.375, ymin=0, ymax=1, fill='black', alpha=0.2)
plot<-plot + annotate("text",x=0.27, y=0.5,hjust=.2,label="B")
plot<-plot + annotate("rect", xmin=0, xmax=0.180, ymin=0, ymax=1, fill='grey', alpha=0.2)
plot<-plot + annotate("text",x=0.09, y=0.5,hjust=.2,label="A")
plot
}


args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
output <- paste(args[2],'/',"GST-B_qualities.pdf", sep="")
ggsave(output, plot = print(data), width = w, height = h)


