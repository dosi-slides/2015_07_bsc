library(ggplot2)
Sys.setlocale("LC_MESSAGES", 'french')


bench <- "benchmark_query/20150105.csv"

h <- 4
w <- 10

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ";", strip.white=TRUE)
  names(df) <- c("TRACE", "PRODUCERS", "LEAVES", "START", "END", "EVENTS", "TRACESIZE", "TS", "QUERY_TYPE", "QUERY", "MICROMODEL", "TOTAL_TIME")
  df
}


printQuery <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$TS %in% 1000),]
xlabel<- "Trace event number"
ylabel<- "Execution time (s)"
plot<-ggplot(dtemp, aes(x=EVENTS, y=TOTAL_TIME/1000, color=QUERY_TYPE, 
shape=QUERY_TYPE))
plot<-plot + geom_point()
plot<-plot + geom_line()
#plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Query Optimization",
breaks=c("NOPT", "T", "ET", "TET", "EP", "TEP", "EPET", "OPT"),
labels=c("No Optmization", "Time", "Event Types", "Time + Event Types", "Space", "Time + Space", "Space + Event Types", "Full Optimization" ))
plot<-plot+ scale_shape_manual(name="Query Optimization",
values=c(0,1,2,3,4,5,6,7),
breaks=c("NOPT", "T", "ET", "TET", "EP", "TEP", "EPET", "OPT"),
labels=c("No Optmization", "Time", "Event Types", "Time + Event Types", "Space", "Time + Space", "Space + Event Types", "Full Optimization" ))
plot
}


args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
outputQuery <- paste(args[2],'/',"ocelotl_query.pdf", sep="")
ggsave(outputQuery, plot = printQuery(data), width = w, height = h)



