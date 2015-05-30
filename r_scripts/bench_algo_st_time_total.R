library(ggplot2) 
Sys.setlocale("LC_MESSAGES", 'french')

bench <- "benchmark_dlpaggreg2/results/bench_st_nall_res"

h <- 2.7
w <- 4.6

read_data <- function(file) {
  df <- read.csv(file, header=TRUE, sep = ",", strip.white=TRUE)
  names(df) <- c("N", "DIM1", "LVL", "DIM3", "DICHO", "P", "DENSITY", "NODES", 
"LEAVES", "TREE", "COUNTERQ", "COUNTERBC", "COUNTERBP", "TIMEQ", "TIMEDICHO", 
"TIMEBC", "TIMEBP", "PRETRIEVED")
  df
}

printTotal2 <- function(data){
dtemp<-data
dtemp$DIMN<-dtemp$DIM1^3*dtemp$NODES
dtemp$TIMETOTAL<-dtemp$TIMEQ+dtemp$TIMEDICHO+dtemp$TIMEBC+dtemp$TIMEBP
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
xlabel<- "|T|^3 * |H(S)|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIMN, y=TIMETOTAL/1000, shape=TREE, color=DIM3))
plot<-plot + geom_point()
plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_gradient2(
name="Event Type number",midpoint=100,low='blue',mid='green', high='red')
plot<-plot + scale_shape_manual(name="Arbre",values=c(0,1,2,3))
plot
}

printTotal <- function(data){
dtemp<-data
dtemp$TIMETOTAL<-dtemp$TIMEQ+dtemp$TIMEDICHO+dtemp$TIMEBC+dtemp$TIMEBP
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$LVL %in% 4),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMETOTAL/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^3+x^2), alpha=0.5)
plot<-plot + scale_color_discrete(name="Arité (4 niv.)",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot<-plot+ scale_shape_manual(name="Arité (4 niv.)",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot
}

#30 120 780 1111



printQualitiesT <- function(data){
dtemp<-data
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$LVL %in% 4),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEQ/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^2), alpha=0.5)
plot<-plot + scale_color_discrete(name="Arité (4 niv.)",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot<-plot+ scale_shape_manual(name="Arité (4 niv.)",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot
}

printQualitiesS <- function(data){
dtemp<-data
dtemp<-dtemp[(dtemp$NODES < 5000),]
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$DIM1 %in% 30),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|H(S)|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=NODES, y=TIMEQ/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Arité",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2", "3", "5", "10"))
plot<-plot+ scale_shape_manual(name="Arité",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2", "3", "5", "10"))
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x), alpha=0.5)
plot
}

printQualitiesCounter <- function(data){
dtemp<-data
dtemp$DIMN<-dtemp$DIM1^2*(dtemp$NODES)*dtemp$DIM3
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
xlabel<- "|T|^3 * |H(S)| * |J(E)|"
ylabel<- "Compteur d'assignation"
plot<-ggplot(dtemp, aes(x=DIMN, y=COUNTERQ/1000, color=TREE))
plot<-plot + geom_point()
plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Arbre")
plot
}

printDicho <- function(data){
dtemp<-data
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$LVL %in% 4),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEDICHO/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^3), alpha=0.5)
plot<-plot + scale_color_discrete(name="Arité (4 niv.)",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot<-plot+ scale_shape_manual(name="Arité (4 niv.)",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot
}

printPartsT <- function(data){
dtemp<-data
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$LVL %in% 4),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|T|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=DIM1, y=TIMEBC/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x^3), alpha=0.5)
plot<-plot + scale_color_discrete(name="Arité (4 niv.)",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot<-plot+ scale_shape_manual(name="Arité (4 niv.)",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot
}

printPartsS <- function(data){
dtemp<-data
dtemp[dtemp$TREE %in% 2, "TREE"]<-"Binaire"
dtemp[dtemp$TREE %in% 3, "TREE"]<-"Ternaire"
dtemp[dtemp$TREE %in% 5, "TREE"]<-"Quinquénaire"
dtemp[dtemp$TREE %in% 10, "TREE"]<-"Décennaire"
dtemp<-dtemp[(dtemp$DIM1 %in% 30),]
dtemp<-dtemp[(dtemp$DIM3 %in% 10),]
xlabel<- "|H(S)|"
ylabel<- "Temps d'exécution (s)"
plot<-ggplot(dtemp, aes(x=NODES, y=TIMEBC/1000, color=TREE, group=TREE, shape=TREE))
plot<-plot + geom_point()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + stat_smooth(method="lm", formula=y ~ I(x), alpha=0.5)
plot<-plot + scale_color_discrete(name="Arité",
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot<-plot+ scale_shape_manual(name="Arité",
values=c(0,1,2,3),
breaks=c("Binaire", "Ternaire", "Quinquénaire", "Décennaire"),
labels=c("2","3","5","10"))
plot
}

args <- commandArgs(trailingOnly = TRUE)
input <- paste(args[1],'/',bench, sep="")
data <- read_data(input)
outputQualitiesT <- paste(args[2],'/',"quality_st_t.pdf", sep="")
ggsave(outputQualitiesT, plot = printQualitiesT(data), width = w, height = h)
outputQualitiesS <- paste(args[2],'/',"quality_st_s.pdf", sep="")
ggsave(outputQualitiesS, plot = printQualitiesS(data), width = w, height = h)
outputQualitiesCounter <- paste(args[2],'/',"quality_c_st.pdf", sep="")
ggsave(outputQualitiesCounter, plot = printQualitiesCounter(data), width = w, height = h)
outputDicho <- paste(args[2],'/',"dicho_st.pdf", sep="")
ggsave(outputDicho, plot = printDicho(data), width = w, height = h)
outputPartsT <- paste(args[2],'/',"parts_st_t.pdf", sep="")
ggsave(outputPartsT, plot = printPartsT(data), width = w, height = h)
outputPartsS <- paste(args[2],'/',"parts_st_s.pdf", sep="")
ggsave(outputPartsS, plot = printPartsS(data), width = w, height = h)
outputTotal <- paste(args[2],'/',"total_st.pdf", sep="")
ggsave(outputTotal, plot = printTotal(data), width = w, height = h)
outputTotal <- paste(args[2],'/',"total_st2.pdf", sep="")
ggsave(outputTotal, plot = printTotal2(data), width = w, height = h)

