#Tracage des courbes de l'acceleration en fonction du nombre de processeurs

#Numero de test
NT = 2

#Nombre de processeurs utilises pour les tests
NP = 4

#Chemin ou se trouve les fichiers
chemin = "/home/mrakotom/StageL3/tototl/samples"
setwd(chemin) 

#Fichier d'extraction
fextract = paste0(paste0("donnees_courbe_t",as.character(NT)),".RData")

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
  fextract= paste0("donnees_courbe_t",as.character(NT))
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

plot(dcourbe1,col="red",xlim = c(0,NP+1),ylim = c(0,NP), 
     xlab = "nombre de processeurs", ylab = "acceleration")
lines(dcourbe1,col="red")
points(dcourbe2,col="lightblue")
lines(dcourbe2,col="lightblue")
points(dcourbe3,col="purple")
lines(dcourbe3,col="purple")

lines(c(0:NP),c(0:NP),col="blue") # fonction x

#Traces des intervalles de confiances associes     
for(i in 2:NP){
  # on recupere les bornes min et max des intervalles de confiance 
  mic1 = t.test(Tseq1/Tpara1[i])$conf.int[1]
  mac1 = t.test(Tseq1/Tpara1[i])$conf.int[2]
  
  mic2 = t.test(Tseq2/Tpara2[i])$conf.int[1]
  mac2 = t.test(Tseq2/Tpara2[i])$conf.int[2]
  
  mic3 = t.test(Tseq3/Tpara3[i])$conf.int[1]
  mac3 = t.test(Tseq3/Tpara3[i])$conf.int[2]
  
  # on represente les intervalles de confiance avec des fleches     
  arrows(i,mic1,i,mac1,col="black",length=0.05,angle=75,code=3)
  arrows(i,mic2,i,mac2,col="black",length=0.05,angle=75,code=3)
  arrows(i,mic3,i,mac3,col="black",length=0.05,angle=75,code=3)
}

#Ajout titre
title("Calcul des qualites", cex.main = 0.8)

#Legende

#Dimensions choisies
d1 = paste0(as.character(md1[1,"DIM1"]),",")
d1 = paste0(paste0(d1,as.character(md1[1,"DIM2"])),",")
d1 = paste0(d1,as.character(md1[1,"DIM3"]))

d2 = paste0(as.character(md2[1,"DIM1"]),",")
d2 = paste0(paste0(d2,as.character(md2[1,"DIM2"])),",")
d2 = paste0(d2,as.character(md2[1,"DIM3"]))

d3 = paste0(as.character(md3[1,"DIM1"]),",")
d3 = paste0(paste0(d3,as.character(md3[1,"DIM2"])),",")
d3 = paste0(d3,as.character(md3[1,"DIM3"]))

legend("topright", legend = c(d1,d2,d3),col = c("red","lightblue","purple"),pch = 15)

