#Wyjście:
#  1) np. nadwyżka glukozy (przyrost tkanki tłuszczowej)
#  2) skład mikrobiomu → zmiany w składzie diety  (opracować przelicznik do modyfikacji diety przez mikrobiom

library(BacArena)
library(R.matlab)
library(parallel)

#PARAMETRY WEJŚCIA
diet_type <- "fat" # fat/fiber/Mediterranean/protein
microbiom <- "healthy" # healthy/unhealthy


#WEJŚCIE 1: dieta
diet <- read.csv2(file = paste("diet/",diet_type,".csv",sep =""), sep = ';', header = TRUE)


#WEJŚCIE 2: skład mikrobiomu (narazie jedna bakteria)

#bacteriaH <- c("Akkermansia_muciniphila_ATCC_BAA_835.mat",
#               "Bacteroides_sp_2_1_7.mat",
#               "Bacteroides_thetaiotaomicron_VPI_5482.mat",
#               "Bacteroides_vulgatus_ATCC_8482.mat",
#               "Bifidobacterium_mongoliense_DSM_21395.mat",
#               "Clostridium_sp_SS2_1.mat",
#               "Faecalibacterium_prausnitzii_SL3_3.mat",
#               "Lactobacillus_amylovorus_GRL_1112.mat",
#               "Roseburia_intestinalis_L1_82.mat",
#               "Ruminococcus_sp_SR1_5.mat")
#bacteria_ammountH <- c(10,10,10,10,10,10,10,10,10,10)

bacteriaH <- c("Akkermansia_muciniphila_ATCC_BAA_835.mat", "Bacteroides_sp_2_1_7.mat")
bacteria_ammountH <- c(10,10)

bacteriaUnH <- c("Bifidobacterium_mongoliense_DSM_21395.mat")
bacteria_ammountUnH <- c(20)

if (microbiom == "healthy") {
  bacteria <- bacteriaH
  bacteria_ammount <- bacteria_ammountH
} else {
  bacteria <- bacteriaUnH
  bacteria_ammount <- bacteria_ammountUnH
}

#STWORZENIE ARENY I DODANIE BAKTERII
arena <- Arena(n=100,m=100)

for (i in 1:length(bacteria)) {
  bac <- readMATmod(paste("bacteria/",bacteria[i],sep=""))
  arena <- addOrg(arena,Bac(bac),amount=bacteria_ammount[i])
  arena <- addEssentialMed(arena, Bac(bac))  
  #arena <- addDeafultMed(arena, Bac(bac))
}


#DODANIE SKŁADNIKÓW Z DIETY DO ARENY
for(i in 1:nrow(diet)) {
  row <- diet[i,]
  subst <- row$Reaction
  subst_ammount <- row$Flux.Value
  arena <- addSubs(arena, smax=subst_ammount, mediac=subst, unit="mM")
}


#SYMULACJA
sim_no <- 10
simulation <- simEnv(arena,time=sim_no)
#zapisywanie symulacji
#save(simulation,file = "simulationManyOnlyEssentials.RData")
#wczytywanie symulacji
#load("simulation4.RData")


#STWORZENIE CSV Z ILOŚCIĄ BAKTERII
list <- lapply(simulation@simlist, function(x){
  occ <- table(x$type)
  unlist(lapply(seq_along(simulation@specs), function(i){ifelse(i %in% names(occ),occ[paste(i)], 0)})) 
})
mat_bac  <- do.call(cbind, list)
rownames(mat_bac) <- names(simulation@specs)

write.csv2(mat_bac, file = "output.csv", row.names = TRUE)


#ANALIZA WYNIKÓW,...
par(mfrow=c(1,2))
plotCurves2(simulation, legendpos = "bottomleft")

par(mfrow=c(2,5))
evalArena(simulation, show_legend = FALSE, time=1:10)




