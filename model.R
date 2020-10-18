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
bacteriaH <- c("Akkermansia_muciniphila_ATCC_BAA_835.mat",
               "Bacteroides_sp_2_1_7.mat",
               "Bacteroides_thetaiotaomicron_VPI_5482.mat",
               "Bacteroides_vulgatus_ATCC_8482.mat",
               "Bifidobacterium_mongoliense_DSM_21395.mat",
               "Clostridium_sp_SS2_1.mat",
               "Faecalibacterium_prausnitzii_SL3_3.mat",
               "Lactobacillus_amylovorus_GRL_1112.mat",
               "Roseburia_intestinalis_L1_82.mat",
               "Ruminococcus_sp_SR1_5.mat")
bacteria_ammountH <- c(10,10,10,10,10,10,10,10,10,10)


bacteriaUnH <- c("Bifidobacterium_mongoliense_DSM_21395.mat")
bacteria_ammountUnH <- c(20)

if (microbiom == "healthy") {
  bacteria <- bacteriaH
  bacteria_ammount <- bacteria_ammountH
} else {
  bacteria <- bacteriaUnH
  bacteria_ammount <- bacteria_ammountUnH
}

substances <- c()
for(i in 1:nrow(diet)) {
  substances <- append(substances, as.character(diet[i,]$Reaction))
}
bacteria_change <- data.frame("Bacteria" = bacteria, "Biomass_0" = bacteria_ammount, "Biomass_end" = bacteria_ammount) 
for(s in substances) {
  tmp <- data.frame(rep(FALSE,length(bacteria)))
  colnames(tmp) = s
  bacteria_change <- cbind(bacteria_change,tmp)
}





#STWORZENIE ARENY I DODANIE BAKTERII
arena <- Arena(n=100,m=100)

for (i in 1:length(bacteria)) {
  bac <- readMATmod(paste("bacteria/",bacteria[i],sep=""))
  arena <- addOrg(arena,Bac(bac),amount=bacteria_ammount[i])
  #arena <- addEssentialMed(arena, Bac(bac))
  #arena <- addDeafultMed(arena, Bac(bac))
  
  for(j in Bac(bac)@medium) {
    #bacteria_change[[which(colnames(bacteria_change) == j)]][i] <- TRUE
    try(bacteria_change[[which(colnames(bacteria_change) == j)]][i] <- TRUE,silent = TRUE)
  }
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
save(simulation,file = "simulationManyFatWithoutEssentials.RData")
#wczytywanie symulacji
#load("simulation4.RData")


for (i in 1:nrow(bacteria_change)) { #count czy biomasa ? początkowa i końcowa
  biomass_end <- sum(simulation@simlist[[sim_no+1]]$biomass)
  bacteria_change[i,]$Biomass_end <- biomass_end
}

#ANALIZA WYNIKÓW,...
par(mfrow=c(1,2))
plotCurves2(simulation, legendpos = "bottomleft")

#par(mfrow=c(2,5))
#evalArena(simulation, show_legend = FALSE, time=1:10)


#WYJŚCIE
#varSubs <- getVarSubs(simulation)
#getSubHist(simulation, "EX_lac_L(e)")



#diet_out <- diet

