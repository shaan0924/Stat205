library(ggplot2)
library(dplyr)

Data_Frame_1 <- read_excel("C:/Users/shaan/OneDrive/Desktop/Stanford/Freshman Spring/Non-Parametric Stat/Project/Data Frame 1.xlsx")
NFL <- slice_head(Data_Frame_1,n = 160)
Wins <- NFL$W

#Point Differential Analysis

PDEW <- NFL$`This Year EW`


pddf <- data.frame(W = Wins, PD = PDEW)
  
pdgraph <- ggplot(pddf, aes(x = PD, y = W)) + geom_point()

bwid <- sd(PDEW)
gsvals <- 0
weights <- matrix(0, nrow = 160, ncol = 160)
u <- matrix(0, nrow = 160, ncol = 160)
wu <- matrix(0, nrow = 160, ncol = 160)
wutwo <- matrix(0, nrow = 160, ncol = 160)
bs <- matrix(0, nrow = 160, ncol = 160)
l <- matrix(0, nrow = 160, ncol = 160)
val <- 0
sortvals <- arrange(pddf, PD)
Wval <- select(sortvals, W)
PD <- sortvals$PD

for(i in 1:160) {
  for(j in 1:160) {
    u[i,j] <- (PD[j] - PD[i])
    pt <- u[i,j]/bwid
    weights[i,j] <- ((2*pi)^(-0.5))*exp((-0.5)*pt^2)
    wu[i,j] <- weights[i,j]*u[i,j]
    wutwo[i,j] <- weights[i,j]*u[i,j]*u[i,j]
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    bs[i,j] <- weights[i,j]*(sum(wutwo[i,]) - u[i,j]*sum(wu[i,]))
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    l[i,j] <- bs[i,j]/sum(bs[i,])
    val[j] <- l[i,j]*Wval$W[j]
  }
  gsvals[i] <- sum(val)
}

gsdf <- data.frame(xval = PD, value = gsvals)

pdgraph <- pdgraph + geom_line(data = gsdf, aes(x = xval, y = value), color = "red")
pdgraph

#SOS Analysis

SOS <- NFL$SoS

sdf <- data.frame(W = Wins, SoS = SOS)

sosgraph <- ggplot(sdf, aes(x = SoS, y = W)) + geom_point()

bwid <- sd(SOS)
gsvals2 <- 0
weights <- matrix(0, nrow = 160, ncol = 160)
u <- matrix(0, nrow = 160, ncol = 160)
wu <- matrix(0, nrow = 160, ncol = 160)
wutwo <- matrix(0, nrow = 160, ncol = 160)
bs <- matrix(0, nrow = 160, ncol = 160)
l <- matrix(0, nrow = 160, ncol = 160)
val <- 0
sortvals <- arrange(sdf, SoS)
Wval <- select(sortvals, W)
SoS <- sortvals$SoS

for(i in 1:160) {
  for(j in 1:160) {
    u[i,j] <- (SoS[j] - SoS[i])
    pt <- u[i,j]/bwid
    weights[i,j] <- ((2*pi)^(-0.5))*exp((-0.5)*pt^2)
    wu[i,j] <- weights[i,j]*u[i,j]
    wutwo[i,j] <- weights[i,j]*u[i,j]*u[i,j]
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    bs[i,j] <- weights[i,j]*(sum(wutwo[i,]) - u[i,j]*sum(wu[i,]))
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    l[i,j] <- bs[i,j]/sum(bs[i,])
    val[j] <- l[i,j]*Wval$W[j]
  }
  gsvals2[i] <- sum(val)
}

gsdf2 <- data.frame(xval = SoS, value = gsvals2)

sosgraph <- sosgraph + geom_line(data = gsdf2, aes(x = xval, y = value), color = "red")
sosgraph

#Draft Analysis

Picks <- NFL$`% of Picks`

pickdf <- data.frame(W = Wins, Picks = Picks)

pickgraph <- ggplot(pickdf, aes(x = Picks, y = W)) + geom_point()

bwid <- sd(Picks)
gsvals3 <- 0
weights <- matrix(0, nrow = 160, ncol = 160)
u <- matrix(0, nrow = 160, ncol = 160)
wu <- matrix(0, nrow = 160, ncol = 160)
wutwo <- matrix(0, nrow = 160, ncol = 160)
bs <- matrix(0, nrow = 160, ncol = 160)
l <- matrix(0, nrow = 160, ncol = 160)
val <- 0
sortvals <- arrange(pickdf, Picks)
Wval <- select(sortvals, W)
Picks <- sortvals$Picks

for(i in 1:160) {
  for(j in 1:160) {
    u[i,j] <- (Picks[j] - Picks[i])
    pt <- u[i,j]/bwid
    weights[i,j] <- ((2*pi)^(-0.5))*exp((-0.5)*pt^2)
    wu[i,j] <- weights[i,j]*u[i,j]
    wutwo[i,j] <- weights[i,j]*u[i,j]*u[i,j]
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    bs[i,j] <- weights[i,j]*(sum(wutwo[i,]) - u[i,j]*sum(wu[i,]))
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    l[i,j] <- bs[i,j]/sum(bs[i,])
    val[j] <- l[i,j]*Wval$W[j]
  }
  gsvals3[i] <- sum(val)
}

gsdf3 <- data.frame(xval = Picks, value = gsvals3)

pickgraph <- pickgraph + geom_line(data = gsdf3, aes(x = xval, y = value), color = "red") + xlim(0,0.1)
pickgraph


#Weighted Pick Analysis
Wpicks <- NFL$`Weighted %`

wpickdf <- data.frame(W = Wins, Picks = Wpicks)

wpickgraph <- ggplot(wpickdf, aes(x = Picks, y = W)) + geom_point()

bwid <- sd(Wpicks)
gsvals4 <- 0
weights <- matrix(0, nrow = 160, ncol = 160)
u <- matrix(0, nrow = 160, ncol = 160)
wu <- matrix(0, nrow = 160, ncol = 160)
wutwo <- matrix(0, nrow = 160, ncol = 160)
bs <- matrix(0, nrow = 160, ncol = 160)
l <- matrix(0, nrow = 160, ncol = 160)
val <- 0
sortvals <- arrange(wpickdf, Picks)
Wval <- select(sortvals, W)
Wpicks <- sortvals$Picks

for(i in 1:160) {
  for(j in 1:160) {
    u[i,j] <- (Wpicks[j] - Wpicks[i])
    pt <- u[i,j]/bwid
    weights[i,j] <- ((2*pi)^(-0.5))*exp((-0.5)*pt^2)
    wu[i,j] <- weights[i,j]*u[i,j]
    wutwo[i,j] <- weights[i,j]*u[i,j]*u[i,j]
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    bs[i,j] <- weights[i,j]*(sum(wutwo[i,]) - u[i,j]*sum(wu[i,]))
  }
}

for(i in 1:160) {
  for(j in 1:160) {
    l[i,j] <- bs[i,j]/sum(bs[i,])
    val[j] <- l[i,j]*Wval$W[j]
  }
  gsvals4[i] <- sum(val)
}

gsdf4 <- data.frame(xval = Wpicks, value = gsvals4)

wpickgraph <- wpickgraph + geom_line(data = gsdf4, aes(x = xval, y = value), color = "red") + xlim(0,0.1)
wpickgraph
