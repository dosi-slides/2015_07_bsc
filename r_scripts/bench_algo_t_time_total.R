library(ggplot2) 
Sys.setlocale("LC_MESSAGES", 'french')

bench <- "benchmark_olpaggreg3/results/bench_tall_res"

h <- 2.7
w <- 4.6

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ",", strip.white=TRUE)
  names(df) <- c("N", "DIM1", "DIM2", "DIM3", "DICHO", "P", "DENSITY", 
"COUNTERQ", "COUNTERBC", "COUNTERBP", "TIMEQ", "TIMEDICHO", "TIMEBC", "TIMEBP", 
"PRETRIEVED")
  df
}

printTotal <- function(data){
dtemp<-data
dtemp$TIMETOTAL<-dtemp$TIMEQ+dtemp$TIMEDICHO+dtemp$TIMEBC
dtemp<-dtemp[(dtemp$DIM2 %in% 1000),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEQ/1000))
#, color=DIM1))
plot<-plot + geom_point()
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^2), alpha=0.7, colour="red")
#plot<-plot + theme_bw()
#plot<-plot + labs(x=xlabel,y=ylabel,title=legend)
plot<-plot + labs(x=xlabel,y=ylabel)
#plot<-plot + scale_color_gradient2(
#name="|T|",midpoint=1000,low='blue',mid='green', high='red')
plot
}

printQualitiesT <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$DIM2 %in% 1000),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEQ/1000))
#, color=DIM1))
plot<-plot + geom_point()
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^2), alpha=0.7, colour="red")
#plot<-plot + theme_bw()
#plot<-plot + labs(x=xlabel,y=ylabel,title=legend)
plot<-plot + labs(x=xlabel,y=ylabel)
#plot<-plot + scale_color_gradient2(
#name="|T|",midpoint=1000,low='blue',mid='green', high='red')
plot
}

printQualitiesS <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$DIM1 %in% 1000),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|S|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM2, y=TIMEQ/1000))
#, color=DIM1))
plot<-plot + geom_point()
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x), alpha=0.7, colour="blue")
#plot<-plot + theme_bw()
#plot<-plot + labs(x=xlabel,y=ylabel,title=legend)
plot<-plot + labs(x=xlabel,y=ylabel)
#plot<-plot + scale_color_gradient2(
#name="|T|",midpoint=1000,low='blue',mid='green', high='red')
plot
}

printQualitiesBK <- function(data){
dtemp<-data
dtemp$DIMN<-dtemp$DIM1^2*dtemp$DIM2*dtemp$DIM3
xlabel<- "|T|^2 * |S| * |J(E)|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIMN, y=TIMEQ/1000, color=DIM1))
plot<-plot + geom_point()
plot<-plot + theme_bw()
#plot<-plot + labs(x=xlabel,y=ylabel,title=legend)
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_gradient2(
name="|T|",midpoint=1000,low='blue',mid='green', high='red')
#plot<-plot + theme(legend.position=c(1,0.4),legend.justification=c(1,1))
plot
}

printDichoT <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$DIM2 %in% 1000),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEDICHO/1000)) + labs(list(x=xlabel,y=ylabel))
plot<-plot + geom_point()
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^2), alpha=0.7, colour="red")
plot
}

printDichoS <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$DIM1 %in% 1000),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|S|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM2, y=TIMEDICHO/1000)) + labs(list(x=xlabel,y=ylabel))
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x), alpha=0.7, colour="blue")
plot<-plot + geom_point()
plot
}

printParts <- function(data){
dtemp<-data
dtemp$TIMEPARTS<-dtemp$TIMEBC
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEPARTS/1000)) + labs(list(x=xlabel,y=ylabel))
plot<-plot + geom_point()
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^2), alpha=0.7, colour="red")
plot
}

args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
outputQualitiesT <- paste(args[2],'/',"quality_t_t.pdf", sep="")
ggsave(outputQualitiesT, plot = printQualitiesT(data), width = w, height = h)
outputQualitiesS <- paste(args[2],'/',"quality_t_s.pdf", sep="")
ggsave(outputQualitiesS, plot = printQualitiesS(data), width = w, height = h)
outputDichoT <- paste(args[2],'/',"dicho_t_t.pdf", sep="")
ggsave(outputDichoT, plot = printDichoT(data), width = w, height = h)
outputDichoS <- paste(args[2],'/',"dicho_t_s.pdf", sep="")
ggsave(outputDichoS, plot = printDichoS(data), width = w, height = h)
outputParts <- paste(args[2],'/',"parts_t.pdf", sep="")
ggsave(outputParts, plot = printParts(data), width = w, height = h)
outputTotal <- paste(args[2],'/',"total_t.pdf", sep="")
ggsave(outputTotal, plot = printTotal(data), width = w, height = h)


