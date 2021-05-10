
# install.packages("PerformanceAnalytics")

library(dplyr)
library(plyr)
library(stringr)
library(scales)
library(caret)
#library(psych)
#library(corrplot)
library(PerformanceAnalytics)
library(Hmisc)


# ---- prepare ----
rm(list = ls())
dev.off()

bonding <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/all_bonding(top).csv",header = T,as.is = T)
connecting <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/all_connecting(top).csv",header = T,as.is = T)
external <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/coop_crossing_list(top10per)_external.csv",header = T,as.is = T)
internal <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/coop_crossing_list(top10per)_internal.csv",header = T,as.is = T)
outdegree <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/mobi_outdegree (top10).csv",header = T,as.is = T)
indegree <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/mobi_indegree (top10).csv",header = T,as.is = T)

# ---- table ----
  
min.max.norm <- function(x){((x-min(x))/(max(x)-min(x)))}

c1_bonding <- bonding[,c(1,8,14)] 
c1_bonding$degree2 <- min.max.norm(c1_bonding$degree)
colnames(c1_bonding)[3] <- "role"
colnames(c1_bonding)[2] <- "measure"
colnames(c1_bonding)[4] <- "normalization"

c2_connecting <- connecting[,c(1,11,15)]
c2_connecting$betweenness2 <- min.max.norm(c2_connecting$betweenness)
colnames(c2_connecting)[3] <- "role"
colnames(c2_connecting)[2] <- "measure"
colnames(c2_connecting)[4] <- "normalization"

c3_external <- external[,c(1,3)]
c3_external$role <- "external"
c3_external$external_relation2 <- min.max.norm(c3_external$external_relation)
colnames(c3_external)[2] <- "measure"
colnames(c3_external)[4] <- "normalization"

c4_internal <- internal[,c(1,2)]
c4_internal$role <- "internal"
c4_internal$internal_relation2 <- min.max.norm(c4_internal$internal_relation)
colnames(c4_internal)[2] <- "measure"
colnames(c4_internal)[4] <- "normalization"

c5_outdegree <- outdegree[,c(1,4)]
c5_outdegree$role <- "outdegree"
c5_outdegree$outdegree2 <- min.max.norm(c5_outdegree$outdegree)
colnames(c5_outdegree)[2] <- "measure"
colnames(c5_outdegree)[4] <- "normalization"


c6_indegree <- indegree[,c(1,4)]
c6_indegree$role <- "indegree"
c6_indegree$indegree2 <- min.max.norm(c6_indegree$indegree)
colnames(c6_indegree)[2] <- "measure"
colnames(c6_indegree)[4] <- "normalization"

# namelist

stat <- merge(c1_bonding, c2_connecting,by="Label", all=T)
stat <- merge(stat, c3_external,by="Label", all=T)
stat <- merge(stat, c4_internal,by="Label", all=T)
stat <- merge(stat, c5_outdegree,by="Label", all=T)
stat <- merge(stat, c6_indegree,by="Label", all=T)

stat[is.na(stat)] <- 0

# ---- correlation for numbers ----

stat2 <- stat[,c(1,4,7,10,13,16,19)] 
colnames(stat2)[2] <- "bonding"
colnames(stat2)[3] <- "connecting"
colnames(stat2)[4] <- "external"
colnames(stat2)[5] <- "internal"
colnames(stat2)[6] <- "outdegree"
colnames(stat2)[7] <- "indegree"

stat3 <- stat2[,-1]

metrix2 <- rcorr(as.matrix(stat3), type = c("pearson"))
pro2 <- metrix2$P
correlation2 <- metrix2$r
# res_cor <- cor(stat3)
# corrplot(corr=res_cor) 
  
# corrplot(corr = res_cor,order = "AOE",type="upper",tl.pos = "d")
# corrplot(corr = res_cor,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")


# correlation for dummy
stat4 <- stat[, c(3,6,9,12,15,18)]
colnames(stat4)[1] <- "bonding"
colnames(stat4)[2] <- "connecting"
colnames(stat4)[3] <- "external"
colnames(stat4)[4] <- "internal"
colnames(stat4)[5] <- "outdegree"
colnames(stat4)[6] <- "indegree"  

obs <- length(stat4$bonding)
for (i in 1:obs){
  if(stat4[i,1]!=0){
    stat4[i,1]<-1
  }
}
for (i in 1:obs){
  if(stat4[i,2]!=0){
    stat4[i,2]<-1
  }
}
for (i in 1:obs){
  if(stat4[i,3]!=0){
    stat4[i,3]<-1
  }
}
for (i in 1:obs){
  if(stat4[i,4]!=0){
    stat4[i,4]<-1
  }
}
for (i in 1:obs){
  if(stat4[i,5]!=0){
    stat4[i,5]<-1
  }
}
for (i in 1:obs){
  if(stat4[i,6]!=0){
    stat4[i,6]<-1
  }
}

stat4$bonding <- as.numeric(stat4$bonding)
stat4$connecting <- as.numeric(stat4$connecting)
stat4$external <- as.numeric(stat4$external)
stat4$internal <- as.numeric(stat4$internal)
stat4$outdegree <- as.numeric(stat4$outdegree)
stat4$indegree <- as.numeric(stat4$indegree)

metrix1 <- rcorr(as.matrix(stat4), type = c("spearman"))
correlation <- metrix1$r
pro <- metrix1$P

# res_cor <- cor(stat4)
# corrplot(corr=res_cor) 

#corrplot(corr = res_cor,order = "AOE",type="upper",tl.pos = "d")
#corrplot(corr = res_cor,add=TRUE, type="lower", method="number",order="AOE",diag=FALSE,tl.pos="n", cl.pos="n")

#chart.Correlation(stat4, method = "pearson", histogram=F, pch=19)

write.csv(pro,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/p-value(dummy).csv")
write.csv(correlation,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/correlation(dummy).csv")
write.csv(pro2,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/p-value(standard).csv")
write.csv(correlation2,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/roles/correlation(standard).csv")




