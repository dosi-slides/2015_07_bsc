library(ggplot2) 
Sys.setlocale("LC_MESSAGES", 'french')

bench <- "benchmark_cache/20150218.csv"

h <- 4
w <- 8

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ";", strip.white=TRUE)
  names(df) <- c("TRACE", "PRODUCERS", "LEAVES", "START", "END", "EVENTS", "TRACESIZE", "TS", "QUERY_TYPE", "QUERY", "MICROMODEL", "TOTAL_TIME")
  df
}

printCache <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$TS < 110 & dtemp$TS > 80 ),]
xlabel<- "Nombre d'évènements de la trace"
ylabel<- "Temps d'exécution (s)"
legend<- "Temps de construction du modèle microsopique"
plot<-ggplot(dtemp, aes(x=EVENTS, y=TOTAL_TIME/1000, color=QUERY_TYPE, 
shape=QUERY_TYPE, group=QUERY_TYPE))
plot<-plot + geom_point(alpha = 2/3)
plot<-plot + geom_line()
#plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Stratégie de reconstruction",
breaks=c("NOCACHE", "CLEAN", "APPROX"),
labels=c("Requête DB", "Cache Parfait", "Cache Approximation"))
plot<-plot+ scale_shape_manual(name="Stratégie de reconstruction",
values=c(0,5,2),
breaks=c("NOCACHE", "CLEAN", "APPROX"),
labels=c("Requête DB", "Cache Parfait", "Cache Approximation"))
plot
}

printTS <- function(data, string){
dtemp<-data
dtemp<-dtemp[(dtemp$EVENTS > 1000000),]
dtemp<-dtemp[(dtemp$QUERY_TYPE %in% string),]
dtemp$EV<-as.character(dtemp$EVENTS)
xlabel<- "|T| (échelle logarithmique)"
ylabel<- "Temps d'exécution (s)"
legend<- "Temps de construction du modèle microsopique en fonction de |T|"
plot<-ggplot(dtemp, aes(x=TS, y=TOTAL_TIME/1000, color=EV, 
shape=EV))
plot<-plot + geom_point()
plot<-plot + geom_line()
plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_x_log10()
plot<-plot + scale_color_discrete(name="Trace (nombre d'évènements)",
breaks=c("10000000", "100000000", "500000000", "1000000000", "1500000000", "2000000000", "2147483646"),
labels=c("1E7", "1E8", "5E8", "1E9", "1.5E9", "2E9", "2.147483646E9"))
plot<-plot+ scale_shape_manual(name="Trace (nombre d'évènements)",
values=c(0,1,2,3,4,5,6),
breaks=c("10000000", "100000000", "500000000", "1000000000", "1500000000", "2000000000", "2147483646"),
labels=c("1E7", "1E8", "5E8", "1E9", "1.5E9", "2E9", "2.147483646E9"))
plot
}


args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
outputCache <- paste(args[2],'/',"ocelotl_cache.pdf", sep="")
ggsave(outputCache, plot = printCache(data), width = w, height = h)
outputCache <- paste(args[2],'/',"ocelotl_nocache.pdf", sep="")
ggsave(outputCache, plot = printTS(data, "NOCACHE"), width = w, height = h)
#outputCache <- paste(args[2],'/',"ocelotl_cacheclean.pdf", sep="")
#ggsave(outputCache, plot = printTS(data, "CLEAN"), width = w, height = h)
#outputCache <- paste(args[2],'/',"ocelotl_cacheapprox.pdf", sep="")
#ggsave(outputCache, plot = printTS(data, "APPROX"), width = w, height = h)



