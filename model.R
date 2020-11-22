#Wyjście:
#  1) np. nadwyżka glukozy (przyrost tkanki tłuszczowej)
#  2) skład mikrobiomu → zmiany w składzie diety  (opracować przelicznik do modyfikacji diety przez mikrobiom

setwd("/Users/michalskawinski/Desktop/Licencjat")
library(BacArena)
library(R.matlab)
library(parallel)

#PARAMETRY WEJŚCIA
diet_type <- "fiber" # fat/fiber/Mediterranean/protein  (unhealthy?)
microbiom <- "healthy" # healthy/unhealthy


#WEJŚCIE 1: dieta
diet <- read.csv2(file = paste("diet/",diet_type,".csv",sep =""), sep = ';', header = TRUE)


#WEJŚCIE 2: skład mikrobiomu (narazie jedna bakteria)

#Phylum|Class|Order
bacteriaH <- c(#"Anaerostipes_caccae_DSM_14662.mat", #Firmicutes|Clostridia|Clostridiales
  "Bacteroides_thetaiotaomicron_VPI_5482.mat", #Bacteroidetes|Bacteroidia|Bacteroidales
  "Blautia_producta_DSM_2950.mat", #Firmicutes|Clostridia|Clostridiales
  "Escherichia_coli_str_K_12_substr_MG1655.mat", #Proteobacteria|Gammaproteobacteria|Enterobacteriales
  "Clostridium_ramosum_VPI_0427_DSM_1402.mat", #Firmicutes|Erysipelotrichia|Erysipelotrichales
  "Lactobacillus_plantarum_subsp_plantarum_ATCC_14917.mat", #Firmicutes|Bacilli|Lactobacillales
  "Bifidobacterium_longum_NCC2705.mat", #Actinobacteria|Actinobacteria|Bifidobacteriales
  "Akkermansia_muciniphila_ATCC_BAA_835.mat") #Verrucomicrobia|Verrucomicrobiae|Verrucomicrobiales
bacteria_ammountH <- c(5,5,5,5,5,5,5)


bacteriaUnH <- c()
bacteria_ammountUnH <- c()

if (microbiom == "healthy") {
  bacteria <- bacteriaH
  bacteria_ammount <- bacteria_ammountH
} else {
  bacteria <- bacteriaUnH
  bacteria_ammount <- bacteria_ammountUnH
}

#STWORZENIE ARENY I DODANIE BAKTERII
arena <- Arena(n=100,m=100,Lx=0.025, Ly=0.025, stir=F, tstep=1) 

for (i in 1:length(bacteria)) {
  bac <- readMATmod(paste("bacteria/",bacteria[i],sep=""))
  arena <- addOrg(arena,Bac(bac,growtype="exponential", speed=10),amount=bacteria_ammount[i]) #?
  arena <- addEssentialMed(arena, Bac(bac))  
  #arena <- addDeafultMed(arena, Bac(bac))
}

diet <- diet[which(diet$Reaction %in% arena@mediac),]

#DODANIE SKŁADNIKÓW Z DIETY DO ARENY
arena <- addSubs(arena, smax=diet$Flux.Value, mediac=diet$Reaction, unit="mM")


#SYMULACJA
sim_no <- 12
simulation <- simEnv(arena,time=sim_no)

#ZAPISANIE SYMULACJI
#save(simulation,file = "simulations/simulationFiber7bac.RData")
#WCZYTANIE SYMULACJI
load("simulations/simulationFiber7bac.RData")


#STWORZENIE CSV Z ILOŚCIĄ BAKTERII
list <- lapply(simulation@simlist, function(x){
  occ <- table(x$type)
  unlist(lapply(seq_along(simulation@specs), function(i){ifelse(i %in% names(occ),occ[paste(i)], 0)})) 
})
mat_bac  <- do.call(cbind, list)
rownames(mat_bac) <- names(simulation@specs)
mat_bac <- t(mat_bac)

#mat_reversed_bac <- data.frame(t(mat_bac[-1]))
#colnames(mat_reversed_bac) <- mat_bac[, 1]

write.csv2(mat_bac, file = "output/outputFiber7bac.csv", row.names = TRUE)

#CSV\ka Z PROPORCJAMI
bac_csv <- read.csv2(file = "output/outputFiber7bac.csv", header=TRUE)
bac_csv <- bac_csv[-c(1)]
bac_prop_csv <- data.frame()
sum <- 0
for (i in names(bac_csv)) {
  sum <- sum + bac_csv[nrow(bac_csv),i]
}
for (i in names(bac_csv)) {
  bac_prop_csv[1,i] <- bac_csv[[nrow(bac_csv),i]]/sum
}
write.csv2(mat_bac, file = "output/outputFiber7bac.csv", row.names = FALSE)

#ANALIZA WYNIKÓW...
par(mfrow=c(1,2))
plotCurves2(simulation, legendpos = "bottomleft")

par(mfrow=c(2,5))
evalArena(simulation, show_legend = FALSE, time=1:10)






