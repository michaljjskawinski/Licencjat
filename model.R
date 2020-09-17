library(BacArena)
library(R.matlab)

sim_no <- 6
#WCZYTANIE DIETY
diet <- read.csv2(file = 'diet/fiber1h.csv', sep = ';', header = TRUE)
#diet <- read.csv2(file = 'diet/Mediterranean1h.csv', sep = ';', header = TRUE)


#STWORZENIE ARENY I DODANIE BAKTERII
arena <- Arena(n=20,m=20)

bacteria <- c("bacteria/Bifidobacterium_mongoliense_DSM_21395.mat", "bacteria/Lactobacillus_amylovorus_GRL_1112.mat", "bacteria/Roseburia_intestinalis_L1_82.mat")
bacteria_ammount <- c(20, 20, 20)

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
  #cat(paste(subst," ",subst_ammount,"\n"))
}

#SYMULACJA
simulation <- simEnv(arena,time=sim_no)

#ANALIZA WYNIKÓW,...

