library(ggplot2)
Sys.setlocale("LC_MESSAGES", 'french')

bench <- "lpaggreg_speedup"

h <- 4
w <- 10


#Tracage des courbes de l'acceleration en fonction du nombre de processeurs

#Numero de test
NT = 2

#Nombre de processeurs utilises pour les tests
NP = 4

printSU <- function(data){
dtemp<-data
xlabel<- "CPU number"
ylabel<- "Speed up"
plot<-ggplot(dtemp, aes(x=NUM, y=VALUE, color=MOD, 
shape=MOD))
plot<-plot + geom_point()
plot<-plot + geom_line()
#plot<-plot + theme_bw()
plot<-plot + labs(x=xlabel,y=ylabel)
plot<-plot + scale_color_discrete(name="Microscopic Model",
breaks=c("LOW","MID","HIGH"),
labels=c("10,1,1", "50,10,10", "90,100,100"))
plot<-plot+ scale_shape_manual(name="Microscopic Model",
values=c(0,1,2),
breaks=c("LOW","MID","HIGH"),
labels=c("10,1,1", "50,10,10", "90,100,100"))
plot
}



args <- commandArgs(trailingOnly = TRUE)
input = paste(args[1],"/", bench ,sep="")
output <- paste(args[2],'/',"lpaggreg_speedup.pdf", sep="")


#Fichier d'extraction
fextract = paste0(paste0(paste0(paste0(input,'/'),"donnees_courbe_t"),as.character(NT)),".RData")

#Recuperation des temps du meilleur algo sequentiel
load(fextract)

Tseq1 = md1["TIME.Q"]
Tseq2 = md2["TIME.Q"]
Tseq3 = md3["TIME.Q"]

#Recuperation des temps de 1 a NP processeurs
Tpara1 = rep(0,NP)
Tpara2 = rep(0,NP)
Tpara3 = rep(0,NP)

for(i in 1:NP){
  #Construction du nom du fichier d'extraction
  fextract= paste0(paste0(paste0(input,'/'),"donnees_courbe_t"),as.character(NT))
  fextract = paste0(fextract,"_p")
  fextract = paste0(paste0(fextract,as.character(i)),".RData")
  
  load(fextract)
  
  Tpara1[i] = md1["TIME.Q"] 
  Tpara2[i] = md2["TIME.Q"]
  Tpara3[i] = md3["TIME.Q"]
}

#Affichage graphique

#Donnees pour tracer les 3 courbes
dcourbe1 = rep(0,NP)
dcourbe2 = rep(0,NP)
dcourbe3 = rep(0,NP)
for(i in 1:NP){
  dcourbe1[i] = colMeans(Tseq1/Tpara1[i])
  dcourbe2[i] = colMeans(Tseq2/Tpara2[i])
  dcourbe3[i] = colMeans(Tseq3/Tpara3[i])
}

dataframe1 = data.frame(NUM=seq(1,NP), VALUE=dcourbe1, MOD="LOW")
dataframe2 = data.frame(NUM=seq(1,NP), VALUE=dcourbe2, MOD="MID")
dataframe3 = data.frame(NUM=seq(1,NP), VALUE=dcourbe3, MOD="HIGH")
dataframe=rbind(dataframe1,dataframe2)
dataframe=rbind(dataframe,dataframe3)
print(dataframe)

ggsave(output, plot = printSU(dataframe), width = w, height = h)
