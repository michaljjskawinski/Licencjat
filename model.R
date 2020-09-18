#Wyjście:
#  1) np. nadwyżka glukozy (przyrost tkanki tłuszczowej)
#  2) skład mikrobiomu → zmiany w składzie diety  (opracować przelicznik do modyfikacji diety przez mikrobiom

library(BacArena)
library(R.matlab)
library(parallel)

#PARAMETRY WEJŚCIA
diet_type <- "fiber" # fat/fiber/Mediterranean/protein
microbiom <- "healthy" # healthy/unhealthy


#WEJŚCIE 1: dieta
diet <- read.csv2(file = paste("diet/",diet_type,".csv",sep =""), sep = ';', header = TRUE)


#WEJŚCIE 2: skład mikrobiomu
bacteriaH <- c("bacteria/Bifidobacterium_mongoliense_DSM_21395.mat")
#, "bacteria/Lactobacillus_amylovorus_GRL_1112.mat", "bacteria/Roseburia_intestinalis_L1_82.mat")
bacteria_ammountH <- c(20)
#, 20, 20)

bacteriaUnH <- c("bacteria/Bifidobacterium_mongoliense_DSM_21395.mat")
bacteria_ammountUnH <- c(20)

if (microbiom == "healthy") {
  bacteria <- bacteriaH
  bacteria_ammount <- bacteria_ammountH
} else {
  bacteria <- bacteriaUnH
  bacteria_ammount <- bacteria_ammountUnH
}

#STWORZENIE ARENY I DODANIE BAKTERII
arena <- Arena(n=20,m=20)

for (i in 1:length(bacteria)) {
  bac <- readMATmod(bacteria[i])
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
#save(simulation,file = "simulation.RData")
#wczytywanie symulacji
#load("simulation.RData")
#simulation <- simulation


#ANALIZA WYNIKÓW,...
par(mfrow=c(1,2))
plotCurves2(simulation, legendpos = "topleft")

#par(mfrow=c(2,5))
#evalArena(simulation, show_legend = FALSE, time=1:10)


#WYJŚCIE
#varSubs <- getVarSubs(simulation)
#getSubHist(simulation, "EX_lac_L(e)")

bacteria_change <- data.frame("Bacteria" = bacteria, "Biomass_0" = bacteria_ammount, "Biomass_end" = bacteria_ammount)
for (i in 1:nrow(bacteria_change)) {
  biomass_end <- sum(simulation@simlist[[sim_no+1]]$biomass)
  bacteria_change[i,]$Biomass_end <- biomass_end
}
#diet_out <- diet

