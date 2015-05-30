library(ggplot2) 
Sys.setlocale("LC_MESSAGES", 'french')

bench <- "benchmark_cache/20150218.csv"

h <- 4
w <- 11

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ";", strip.white=TRUE)
  names(df) <- c("TRACE", "PRODUCERS", "LEAVES", "START", "END", "EVENTS", "TRACESIZE", "TS", "QUERY_TYPE", "QUERY", "MICROMODEL", "TOTAL_TIME")
  df
}

printCache <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$TS < 110 & dtemp$TS > 80 ),]
xlabel<- "Trace event number"
ylabel<- "Execution time (s)"
legend<- "Microscopic model building time"
plot<-ggplot(dtemp, aes(x=EVENTS, y=TOTAL_TIME/1000, color=QUERY_TYPE, 
shape=QUERY_TYPE, group=QUERY_TYPE))
plot<-plot + geom_point(alpha = 2/3)
plot<-plot + geom_line()
#plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Building strategy",
breaks=c("NOCACHE", "CLEAN", "APPROX"),
labels=c("Database query", "Cache (perfect)", "Cache (approximate"))
plot<-plot+ scale_shape_manual(name="Stratégie de reconstruction",
values=c(0,5,2),
breaks=c("NOCACHE", "CLEAN", "APPROX"),
labels=c("Database query", "Cache (perfect)", "Cache (approximate"))
plot
}


args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
outputCache <- paste(args[2],'/',"ocelotl_cache.pdf", sep="")
ggsave(outputCache, plot = printCache(data), width = w, height = h)



