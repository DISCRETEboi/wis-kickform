#args <- commandArgs(TRUE)
args <- c("C:\\DISCRETEfold_1\\Rprojs\\WIS_test", 18, 23, 25)
leaguePath <-args[1]
numberOfTeams <-as.numeric(args[2])
afterMatchday <-as.numeric(args[3])
tillMatchday <-as.numeric(args[4])

setwd(leaguePath)
library(skellam)
library(jsonlite)

### Anzahl Teams:
Teams_2 <- numberOfTeams
Spiele_ges <- Teams_2 * (Teams_2 - 1)
Anzahl_Spieltage <- (Teams_2 - 1) * 2
Jahr <- 2021
#### Teams einlesen
Alleteams <- read.table("../teams.txt")
###

Saisons <- read.table("seasons-archiv.txt", skip = 1)[-1]
Saisons <- Saisons[(dim(Saisons)[1] - (4 * Spiele_ges) +1):dim(Saisons)[1] ,]
colnames(Saisons) <- c("Saison","Spieltag","Heimteam", "Auswartsteam","Tore_heim", "Tore_aus")

Saison19 <- read.table("last-season.txt", skip = 1)[,-c(1,8:9)]
if(dim(Saison19)[2] == 5){
     Saison19 <- cbind(Jahr - 1, Saison19)
}
colnames(Saison19) <- c("Saison","Spieltag","Heimteam", "Auswartsteam","Tore_heim", "Tore_aus")

#### Matrix mit allen Teams:
Kombis <- read.table("current-season.txt", header =T)
colnames(Kombis) <- c("Saison","Spieltag","Heimteam", "Auswartsteam","Tore_heim", "Tore_aus")

### Koeffizienten
load("coefficients.RData")

z <- Saisons[,c(5,6)]
Saisons$Toregesamt <- apply(z, 1, sum)
Torekomplett <- tapply(Saisons$Toregesamt, Saisons$Saison, sum)
Torekomplettn <- Torekomplett/Spiele_ges

alleTeams <- tapply(Saisons$Heimteam, Saisons$Saison, unique)
alleTeams <- do.call("rbind", alleTeams)
alleTeams <- t(apply(alleTeams, 1, sort))

Saisons$Tordiffheim <- Saisons$Tore_heim - Saisons$Tore_aus  ### Tordifferenz aus Sicht der Heimmannschaft
Tordiffkomplettheim <- (tapply(Saisons$Tordiffheim, Saisons$Saison, sum))/Spiele_ges

Heimteams<- Kombis$Heimteam
Austeams <- Kombis$Auswartsteam
alleTeams <- tapply(Saisons$Heimteam, Saisons$Saison, unique)
alleTeams <- do.call("rbind", alleTeams)
alleTeams <- t(apply(alleTeams, 1, sort))
aktuelle_Teams <- sort(cbind(unique(Heimteams)))
alleTeams <-  rbind(alleTeams, aktuelle_Teams)

Saisons$Tordiffheim <- Saisons$Tore_heim - Saisons$Tore_aus  ### Tordifferenz aus Sicht der Heimmannschaft
Saisons$Chancenheimdiff <- Saisons$Tordiffheim
Saisons$Chancengastdiff <- (-1)* Saisons$Chancenheimdiff

Chancen_heim <- tapply(Saisons$Chancenheimdiff[1:1520], Saisons[1:1520,c("Saison","Heimteam")], mean)
Chancen_gast <- tapply(Saisons$Chancengastdiff[1:1520], Saisons[1:1520,c("Saison","Auswartsteam")], mean)
TCdiff <- (Chancen_heim + Chancen_gast)/2

l = Jahr - 1

histTCD <- sapply(alleTeams[(l - (Jahr - 5)),], function(i)  sum(0.5*TCdiff[as.character(l-1),as.character(i)],0.35*TCdiff[as.character(l-2),as.character(i)],0.15*TCdiff[as.character(l-3),as.character(i)],na.rm =T))

FaktorTore <- 1 + 0.0224*histTCD
FaktorGT <- 1- 0.0224*histTCD

CHeim_alle <- tapply(Saisons$Tore_heim, Saisons[,c("Saison", "Heimteam")], sum)
CHeimaus_alle <- tapply(Saisons$Tore_aus, Saisons[,c("Saison", "Heimteam")], sum)
CGast_alle <- tapply(Saisons$Tore_aus, Saisons[,c("Saison", "Auswartsteam")], sum)
CGastaus_alle <- tapply(Saisons$Tore_heim, Saisons[,c("Saison", "Auswartsteam")], sum)

CTeamskomplett <-(1/Anzahl_Spieltage) * sapply(alleTeams[4,], function(x) (CHeim_alle[as.character(l), as.character(x)] + CGast_alle[as.character(l), as.character(x)])*FaktorTore[which(alleTeams[4,] == x)]
-(CHeimaus_alle[as.character(l), as.character(x)] + CGastaus_alle[as.character(l), as.character(x)])*FaktorGT[which(alleTeams[4,] == x)])

Abpos <- which(!alleTeams[4,] %in% alleTeams[5,])


Chancenges_heim <- tapply(Saisons$Toregesamt, Saisons[,c("Saison","Heimteam")], sum)
Mittel_heim <- rowMeans(Chancenges_heim, na.rm =T)
Chancenges_heim <-(Chancenges_heim - Mittel_heim)/(Anzahl_Spieltage/2)
Chancenges_gast <- tapply(Saisons$Toregesamt, Saisons[,c("Saison","Auswartsteam")], sum)
Mittel_gast <- rowMeans(Chancenges_gast, na.rm =T)
Chancenges_gast <-(Chancenges_gast - Mittel_gast)/(Anzahl_Spieltage/2)
l <- Jahr
gemittelteCl3J <- sapply(alleTeams[4,], function(i)  (mean(Chancenges_heim[as.character(c((l -3):(l - 1))), as.character(i)] ,na.rm =T) +
mean(Chancenges_gast[as.character(c((l -3):(l - 1))), as.character(i)] ,na.rm =T))/2)
gemittelteCl3J[Abpos] <- 0

CAufsteiger <- ifelse(length(Abpos) == 2,(-1)*sum(CTeamskomplett[-Abpos])/2, (-1)*sum(CTeamskomplett[-Abpos])/3)
CTeamskomplett[Abpos] <- CAufsteiger
alleTeams[4,Abpos] <- aktuelle_Teams[!aktuelle_Teams %in% alleTeams[4,]]
aktuelle_Teams <- alleTeams[4,]

mToreaus <- tapply(Saison19$Tore_aus, Saison19$Auswartsteam, mean)
mToreheim <- tapply(Saison19$Tore_heim, Saison19$Heimteam, mean)
mTore <- (mToreaus + mToreheim)/2
mTore[Abpos] <- mean(mTore)
names(mTore) <- aktuelle_Teams

mGToreaus <- tapply(Saison19$Tore_aus, Saison19$Heimteam, mean)
mGToreheim <- tapply(Saison19$Tore_heim, Saison19$Auswartsteam, mean)
mGTore <- (mGToreaus + mGToreheim)/2
mGTore[Abpos] <- mean(mGTore)
names(mGTore) <- aktuelle_Teams


Spielvorhersage <- function(N,MW = F, Heimvorteil = 0, Gastvorteil = 0, Tore = 0, GTore = 0 ,Ende = 19, Lieblingsmannschaft = F, Absteiger = FALSE, Favoriten = FALSE,Hassmannschaft =F,Torekomplettn, FaktorTore, FaktorGT, gemittelteCl3J, Teams = Teams_2, Heimteams, Austeams,Kombis, CTeamskomplett, mTore, mGTore, Jahr){
# Saison2 <- rbind(Saisons, Kombis)
load("coefficients.RData")
Kombis$Tordiffheim <- Kombis$Tore_heim - Kombis$Tore_aus  ### Tordifferenz berechnen
Kombis$Toregesamt <- Kombis$Tore_heim + Kombis$Tore_aus   ### Anzahl Tore bestimmen
Team_heim <- sapply((N*(Teams/2) + 1) :(Ende*(Teams/2)), function(x) which(Heimteams[x] == aktuelle_Teams))
Team_aus <- sapply((N*(Teams/2) + 1):(Ende*(Teams/2)),function(x) which(Austeams[x] == aktuelle_Teams))

HV <- ifelse(N == 0, mean(Tordiffkomplettheim[2:4]) * beta1[1] ,mean(Tordiffkomplettheim[2:4]) * beta1[N + 1] + beta2[N + 1] *  mean(Kombis[1:(N*(Teams/2)),]$Tordiffheim)  )


if(N != 0){### Effizienter machen!
l = Jahr
CTeamsN <- c()
CSTeamsN <- c()
j <- 1
for(i in aktuelle_Teams){
Heim <- Kombis[Kombis$Heimteam == i &Kombis$Spieltag < (N + 1),]
Gast <- Kombis[Kombis$Auswartsteam == i &Kombis$Spieltag < (N + 1),]
effTC <- (sum(Heim$Tore_heim) + sum(Gast$Tore_aus))
effGTC <- ((sum(Heim$Tore_aus) + sum(Gast$Tore_heim)))
CSTeamsN[j] <- (effTC + effGTC)/N
if(dim(Heim)[1] > dim(Gast)[1]){
CTeamsN[j] <- (effTC - effGTC +  HV) /N
j <- j + 1
}else{
CTeamsN[j] <- (effTC - effGTC) /N
j <- j + 1
}
}
Mittel <- mean(CSTeamsN)
CSTeamsN <- CSTeamsN - Mittel
}


eAnzahlTore <- if(N == 0){beta7[1] * gemittelteCl3J}else{beta7[N + 1] * gemittelteCl3J + beta8[N + 1]* CSTeamsN} ### Warum klappt ifelse nicht?
if(MW){
LS <- if(N == 0){beta3.1[1] * CTeamskomplett + beta9[1] * zen_MW}else{beta3.1[N + 1] * CTeamskomplett + beta4.1[N + 1] * CTeamsN+ beta9[N + 1] * zen_MW }
}else{
LS <- if(N == 0){beta3[1] * CTeamskomplett}else{beta3[N + 1] * CTeamskomplett + beta4[N + 1] * CTeamsN }
}

Tordiff <- sapply((N*(Teams/2) +1):(Ende*(Teams/2)), function(i) LS[which(aktuelle_Teams == Heimteams[i])]/(Anzahl_Spieltage/2) - LS[which(aktuelle_Teams == Austeams[i])]/(Anzahl_Spieltage/2) + HV )
eToranzahl <- sapply((N*(Teams/2) +1):(Ende*(Teams/2)), function(i)  eAnzahlTore[which(aktuelle_Teams == Heimteams[i])] + eAnzahlTore[which(aktuelle_Teams == Austeams[i])])

Anzahltore <- if(N == 0){mean(Torekomplettn[2:4]) * beta5[1]}else{mean(Torekomplettn[2:4]) * beta5[N + 1] + beta6[N + 1] * (mean(Kombis[Kombis$Spieltag < (N +1),]$Toregesamt))}

eToregast <- sapply(1:(Ende*(Teams/2) - N*(Teams/2)), function(i) (Anzahltore + eToranzahl[i]  - Tordiff[i])/2)
eToreheim <- sapply(1:(Ende*(Teams/2) - N*(Teams/2)), function(i) Anzahltore + eToranzahl[i] - eToregast[i])


if(Heimvorteil - Gastvorteil > 0){
eToreheim <- eToreheim + (Heimvorteil- Gastvorteil)/200
}
if(Heimvorteil - Gastvorteil < 0){
eToregast <- eToregast + (Gastvorteil - Heimvorteil)/200
}
if(all(Favoriten != FALSE)){
Index <- sapply(1:3,function(i) which(Favoriten[i] == aktuelle_Teams))
for(i in 1:3){
eToreheim[which(Team_heim == Index[i])] <-eToreheim[which(Team_heim == Index[i])] + 0.5
eToregast[which(Team_aus == Index[i])] <-eToregast[which(Team_aus == Index[i])] + 0.5
}
}
if(all(Absteiger != FALSE)){
Index <- sapply(1:3,function(i) which(Absteiger[i] == aktuelle_Teams))
for(i in 1:3){
eToreheim[which(Team_heim == Index[i])] <-eToreheim[which(Team_heim == Index[i])] - 0.5
eToregast[which(Team_aus == Index[i])] <-eToregast[which(Team_aus == Index[i])] - 0.5
}
}
if(Lieblingsmannschaft != FALSE & !any(Favoriten == Lieblingsmannschaft)){
Index <- which(Lieblingsmannschaft == aktuelle_Teams)
eToreheim[which(Team_heim == Index)] <-eToreheim[which(Team_heim == Index)] + 0.5
eToregast[which(Team_aus == Index)] <-eToregast[which(Team_aus == Index)] + 0.5
}
if(Hassmannschaft != FALSE & !any(Absteiger == Hassmannschaft)){
Index <- which(Hassmannschaft == aktuelle_Teams)
eToreheim[which(Team_heim == Index)] <-eToreheim[which(Team_heim == Index)] + 0.5
eToregast[which(Team_aus == Index)] <-eToregast[which(Team_aus == Index)] + 0.5
}

if(any(eToreheim< 0 )){
index <- which(eToreheim < 0)
Abstand <- abs(eToreheim[index])
eToreheim[index] <- 0
eToregast[index] <- eToregast[index ] + Abstand

}
if(any(eToregast< 0 )){
index <- which(eToregast < 0)
Abstand <- abs(eToregast[index])
eToregast[index] <- 0
eToreheim[index] <- eToreheim[index ] + Abstand
}
Tordiff <- eToreheim - eToregast

#Toreheim <- sapply(1:(Ende*(Teams/2) - N*(Teams/2)), function(s) if(abs(Tordiff[s]) > 0.3 & abs(Tordiff[s]) < 1){round(eToreheim[s])}else{which.max(dpois(0:8, eToreheim[s])) - 1 })
#Toregast <- sapply(1:(Ende*(Teams/2) - N*(Teams/2)), function(s) if(abs(Tordiff[s]) > 0.3 & abs(Tordiff[s]) < 1){round(eToregast[s])}else{which.max(dpois(0:8, eToregast[s])) - 1 })

### 3 Wahrscheinlichsten Ergebnisse einbauen: -> Edit:alle Wahrscheinlichkeiten ausgeben

Tore <- c()
BTS <- list()
U1.5 <- list()
U2.5 <- list()
U3.5 <- list()
mErg <-  c("0:0", "1:0", "2:0", "3:0", "4:0", "5:0", "6:0", "0:1", "1:1", "2:1", "3:1","4:1", "5:1", "6:1", "0:2", "1:2", "2:2", "3:2",  "4:2", "5:2", "6:2",
"0:3", "1:3", "2:3", "3:3","4:3", "5:3", "6:3", "0:4", "1:4", "2:4", "3:4", "4:4", "5:4", "6:4", "0:5", "1:5", "2:5", "3:5","4:5", "5:5", "6:5", "0:6", "1:6", "2:6", "3:6", "4:6", "5:6", "6:6")
#### Neu
Score <- c(0, 1, 2 , 3, 4, 5, 6, -1, 0, 7, 8,9, 10, 11, -2, -3, 0, 12,  13, 14, 15,
-4, -5, -6, 0,16, 17, 18, -7, -8, -9, -10, 0, 19, 20, -11, -12, -13, -14,-15, 0, 21, -16, -17, -18, -19, -20, -21, 0)
Topergebnis <- c()### Neu
Kicktip <-c()
Ergebnisse <- matrix(mErg, byrow =F, ncol = 7)
WahrscheinlichsteErgeb <- list()
PUnentschieden <- dskellam(0,eToreheim, eToregast) * 1.1
PUnentschieden <- ifelse(PUnentschieden == "NaN",0, PUnentschieden )
Korrektur     <- PUnentschieden* 0.05
PAuswartssieg <- pskellam(-1,eToreheim, eToregast ) - Korrektur
PAuswartssieg <- ifelse(PAuswartssieg == "NaN",0, PAuswartssieg)
PHeimsieg    <-  1 - round(PAuswartssieg,2) - round(PUnentschieden,2)
PUnentschieden <- round(PUnentschieden,2)
PAuswartssieg <- round(PAuswartssieg,2)
PHeimsieg <- round(PHeimsieg,2)
for(s in 1:(Ende*(Teams/2) - N*(Teams/2))){
if(abs(Tordiff[s]) > 0.1 & abs(Tordiff[s]) < 1){
eToreheim[s] <- eToreheim[s] + 0.5
eToregast[s] <- eToregast[s] + 0.5}
Wahrscheinlichkeiten <-sapply(0:6, function(j) sapply(0:6, function(i) round(dpois(j, eToregast[s]),2) * round(dpois(i, eToreheim[s]),2)))
#### Both Teams to score:
BTS_n <- sum(Wahrscheinlichkeiten[1,], Wahrscheinlichkeiten[-1,1])
BTS[[s]] <- c(BTS_n, 1 - BTS_n) ### Nein, Ja
U1.51 <- sum(Wahrscheinlichkeiten[1,1], Wahrscheinlichkeiten[2,1], Wahrscheinlichkeiten[1,2])
U1.5[[s]] <- c(U1.51, 1- U1.51)
U2.51 <-  sum(U1.51,Wahrscheinlichkeiten[2,2], Wahrscheinlichkeiten[1,3], Wahrscheinlichkeiten[3,1])
U2.5[[s]] <- c(U2.51, 1- U2.51)
U3.51 <-  sum(U2.51, Wahrscheinlichkeiten[1,4], Wahrscheinlichkeiten[4,1],  Wahrscheinlichkeiten[2,3], Wahrscheinlichkeiten[3,2])
U3.5[[s]] <- c(U3.51, 1- U3.51)
Pmax <- unique(sort(as.vector(Wahrscheinlichkeiten), decreasing = T))
Pos <- as.vector(unlist(sapply(1:length(Pmax), function(u) rev(which(Wahrscheinlichkeiten == Pmax[u])))))
WahrscheinlichsteErgeb[[s]] <- rbind(Ergebnisse[Pos], Wahrscheinlichkeiten[Pos])
Tore[s] <- Ergebnisse[Pos[1]]
if(PHeimsieg[s] > 0.4){  ### Also ab hier wäre es neu
Platz <- which(Score[Pos] > 0)[1]
Topergebnis[s] <- Ergebnisse[which(Score == Score[Pos][Platz])]
}else{
if(PAuswartssieg[s] > 0.4){
Platz <- which(Score[Pos] < 0)[1]
Topergebnis[s] <- Ergebnisse[which(Score == Score[Pos][Platz])]
}else{### ab hier geändert!!!!!!
Topergebnis[s] <- WahrscheinlichsteErgeb[[s]][1,1]  ### Wenn Unentschieden am "wahrscheinlichsten" ist, ist es ja immer an erster Stelle, von daher kann ich einfach auf die Liste zugreifen
}
}
if(PHeimsieg[s] > PAuswartssieg[s]){
if(PHeimsieg[s] > 0.55){  ### Also ab hier wäre es neu
Kicktip[s] <- '1'
}else{
if(PHeimsieg[s] < 0.56 & PHeimsieg[s] > 0.38 & PAuswartssieg[s] > PUnentschieden[s]){
Kicktip[s] <- '1/2'
}
else{
if(PHeimsieg[s] < 0.56 & PHeimsieg[s] > 0.38 & PAuswartssieg[s] <= PUnentschieden[s]){
Kicktip[s] <- '1/X'
}else{
if(PHeimsieg[s] < 0.39){
Kicktip[s] <- 'X'
}
}
}
}
}
if(PHeimsieg[s]< PAuswartssieg[s]){
if(PAuswartssieg[s] > 0.55){
Kicktip[s] <- '2'
}else{
if(PAuswartssieg[s] < 0.56 & PAuswartssieg[s] > 0.38 & PHeimsieg[s] > PUnentschieden[s]){
Kicktip[s] <- '2/1'
}else{
if(PAuswartssieg[s] < 0.56 & PAuswartssieg[s] > 0.38 & PHeimsieg[s] <= PUnentschieden[s]){
Kicktip[s] <- '2/X'
}else{
if(PAuswartssieg[s] < 0.39){
Kicktip[s] <- 'X'
}
}
}
}
}
}

Handicapkandidaten <- which(PHeimsieg > 0.5| PAuswartssieg > 0.5)
i <- 1
Handicaps <- c()
Empfehlung <- c()
for(s in Handicapkandidaten){
Wahrscheinlichkeiten <-sapply(0:6, function(j) sapply(0:6, function(i) round(dpois(j, eToregast[s]),2) * round(dpois(i, eToreheim[s]),2)))
if(PHeimsieg[s] > 0.5){
k <- 1
l <- k
Summe <- 0
while(l < 6){
Summe <- Summe + Wahrscheinlichkeiten[(l + 1), (l)]
l <- l + 1
}
Handicaps[i] <- PHeimsieg[s] - Summe
while(Handicaps[i] > (0.5 - k*0.05) & k < 5){
k <- k + 1
l <- 1
Summe <- 0
while((l + k) < 6){
Summe <- Summe + Wahrscheinlichkeiten[(l + k), (l)]
l <- l + 1
}
Handicaps[i] <- Handicaps[i] - Summe
}
}
if(PAuswartssieg[s] > 0.5){
k <- 1
l <- k
Summe <- 0
while(l < 6){
Summe <- Summe + Wahrscheinlichkeiten[l , (l + 1)]
l <- l + 1
}
Handicaps[i] <- PAuswartssieg[s] - Summe
while(Handicaps[i] > (0.5 - k*0.05) & k < 5){
k <- k + 1
Summe <- 0
l <- 1
while((l + k) < 7){
Summe <- Summe + Wahrscheinlichkeiten[l , (l + k)]
l <- l + 1
}
Handicaps[i] <- Handicaps[i] - Summe
}
}
if(PHeimsieg[s] > 0.5){
Empfehlung[i] <- -(k - 1)
}else{
Empfehlung[i] <- k - 1
}
i <-  i + 1
}
Tendenzen <- list()
for(x in 1:(Ende*(Teams/2) - (N*(Teams/2) ))){
Tendenzen[[x]] <- cbind(PHeimsieg[x], PUnentschieden[x], PAuswartssieg[x])
}

#names(WahrscheinlichsteErgeb) <- Alleteams[Heimteams[(N*(Teams/2) +1):(Ende*(Teams/2))],3]
Liste <-paste(sep="-", Kombis[(N*(Teams/2) +1):(Ende*(Teams/2)),3], Kombis[(N*(Teams/2) +1):(Ende*(Teams/2)),4])
names(Tendenzen) <- Liste
names(WahrscheinlichsteErgeb) <- Liste
Spieltag <- rbind(as.character(Alleteams[Heimteams[(N*(Teams/2) +1):(Ende*(Teams/2))],3]),as.character(Alleteams[Austeams[(N*(Teams/2) +1):(Ende*(Teams/2))],3]), Tore)
#Spieltag <- rbind(as.character(Alleteams[Heimteams[(N*(Teams/2) +1):(Ende*(Teams/2))],3]),as.character(Alleteams[Austeams[(N*(Teams/2) +1):(Ende*(Teams/2))],3]), Toreheim, Toregast)
Resultat <- list()
Resultat[[1]] <- Tendenzen
Resultat[[2]] <- WahrscheinlichsteErgeb
Resultat[[3]] <- HV
Resultat[[4]] <- Anzahltore
LS <- as.list(LS/(Anzahl_Spieltage/2))
Indexing <- c()
for(i in 1:Teams){
Indexing[i] <- which(aktuelle_Teams[i]== Alleteams[,1])
}
names(LS) <- Alleteams[Indexing,3]
Resultat[[5]] <- LS
eToranzahl <- as.list(eToranzahl + Anzahltore )
names(eToranzahl) <- Liste
Resultat[[6]] <- eToranzahl
names(BTS) <- Liste
Resultat[[7]] <- BTS
names(U1.5) <- Liste
names(U2.5) <- Liste
names(U3.5) <- Liste
Resultat[[8]] <- U1.5
Resultat[[9]] <- U2.5
Resultat[[10]] <- U3.5
Empfehlung <- as.list(Empfehlung)
names(Empfehlung) <-Liste[Handicapkandidaten]
Resultat[[11]] <- Empfehlung
names(Resultat) <- c("Tendenzen", "Wahrscheinlichste", "Heimvorteil", "AnzahlTore", "Leistungsstxrke", "Individuelle Torsumme", "Both Teams to score", "Unter 1.5", "Unter 2.5", "Unter 3.5", "Handicap")
#Spieltag2 <- toJSON(Spieltag, pretty=TRUE)
#### Ab hier neu
names(Topergebnis) <- Liste
Topergebnis <- as.list(Topergebnis)
Resultat[["Topergebnis"]] <- Topergebnis
names(Kicktip) <- Liste
Kicktip <- as.list(Kicktip)
Resultat[['Kicktip']] <- Kicktip
### bis hierhin
Spieltag2 <- toJSON(Resultat, pretty =T)
#Spieltag <- toJSON(WahrscheinlichsteErgeb, pretty =T)
return(Spieltag2)
}


Spieltage <- Spielvorhersage(N = afterMatchday, Ende = tillMatchday,Torekomplettn = Torekomplettn, Heimteams = Heimteams, Austeams = Austeams, gemittelteCl3J = gemittelteCl3J,CTeamskomplett = CTeamskomplett, FaktorTore = FaktorTore, FaktorGT = FaktorGT,Kombis = Kombis, Jahr = Jahr)
#einSpieltag <- Spielvorhersage(N = 2,Ende =4,Torekomplettn = Torekomplettn, Heimteams = Heimteams, Austeams = Austeams, gemittelteCl3J = gemittelteCl3J,CTeamskomplett = CTeamskomplett, FaktorTore = FaktorTore, FaktorGT = FaktorGT,Kombis = Kombis,  MW = T, Form = Form_Form, Jahr = Jahr)
#Spieltagextrem <- Spielvorhersage(N = 19,Ende = 20, Heimvorteil = 100, Gastvorteil = 100,Tore = 100,GTore = 100, MW =TRUE, Favoriten = c(7,8,16), Absteiger = c(25,49,54), Torekomplettn = Torekomplettn,Kombis = Kombis, Heimteams = Heimteams, Austeams = Austeams, gemittelteCl3J = gemittelteCl3J,CTeamskomplett = CTeamskomplett, FaktorTore = FaktorTore, FaktorGT = FaktorGT , mTore = mTore, mGTore = mGTore)
#Spieltageantirb <- Spielvorhersage(Abst

#Marktwerte <- read.table("market-values.txt", header = T)
#Marktwerte <- Marktwerte[order(Marktwerte[,1]),]
#zen_MW <- log(Marktwerte[,2]) - mean(log(Marktwerte[,2]))
#Index <- sapply(aktuelle_Teams,function(x) which(x == Marktwerte[,1]))
#zen_MW <- zen_MW[Index]
Spieltage
