library(dplyr)
library(plyr)
library(stringr)

# ---- prepare ----
rm(list = ls())
dev.off()

link <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_link.csv",header = T,as.is = T)
node <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_node.csv",header = T,as.is = T)

if_cross11 <- data.frame(Label=link$Source,if_cross = link$If_across,weight =link$Weight, backup=link$If_across)
if_cross22 <- data.frame(Label=link$Target,if_cross = link$If_across,weight =link$Weight, backup=link$If_across)

# ---- GET DATA ----
obs <- length(if_cross11$Label)
for (i in 1:obs){
  if (if_cross11[i,"backup"]==1){
    if_cross11[i,"if_cross"]<- "external relation"
  } else {if_cross11[i,"if_cross"]<- "internal relation"}
}

for (i in 1:obs){
  if (if_cross22[i,"backup"]==1){
    if_cross22[i,"if_cross"]<- "external relation"
  } else {if_cross22[i,"if_cross"]<- "internal relation"}
}

coop <- bind_rows(if_cross11, if_cross22)
coop <- coop[,-4]
coop <- coop[order(coop$Label, coop$if_cross),]

obs0 <- length(coop$Label)
for (i in 2:obs0){
  if (coop[i,"Label"]==coop[i-1,"Label"]& coop[i,"if_cross"]==coop[i-1,"if_cross"]){
    coop[i,"weight"]<- coop[i,"weight"]+coop[i-1,"weight"]
    coop[i-1,"weight"]<-NA
  }
}
coop <- coop[complete.cases(coop$weight),]

# coop2 <- count(coop,c("Label","if_cross"))

coop$internal_relation <- 0
coop$external_relation <- 0

obs2 <- length(coop$Label)
coop <- coop[order(coop$Label,coop$if_cross),]
coop[1,"external_relation"]<- coop[1,"weight"]
for (i in 2:obs){
  if(coop[i,"if_cross"]=="external relation"){
    coop[i,"external_relation"] <- coop[i,"weight"]
  } else {
    coop[i-1,"internal_relation"] <- coop[i,"weight"]
    coop[i,"Label"]<- NA
  }
}

coop2 <- coop[complete.cases(coop$Label),] 
coop2 <- coop2[,-(2:3)]

#for (i in 1:(obs2-1)) {
#  if (coop2[i,"Label"]==coop2[i+1,"Label"]){
#    coop2[i,"internal_relation"]<-coop2[i+1,"internal_relation"]
#    coop2[i+1, "if_cross"] <-NA
#  }
#}

#coop2 <- coop2
#coop2 <- coop2[complete.cases(coop2[,2]),]
#row.names(coop2) <- NULL
#coop2 <- coop2[,-(2:3)]

coop3 <- merge(coop2, node, by="Label")
coop3 <- coop3[,-4]




coop_pre <- data.frame(measures=c(NA),count=c(NA))

coop_pre[1,1] <- "number of links"
coop_pre[1,2] <- length(link$Source)

coop_pre[2,1] <- "external relations(include weight)"
coop_pre[2,2] <- sum(coop2$external_relation) # greater than No. links since links *2 for both sides

coop_pre[3,1] <- "internal relations(include weight)"
coop_pre[3,2] <- sum(coop2$internal_relation) 

coop_pre[4,1] <- "average external per person"
coop_pre[4,2] <- mean(coop2$external_relation)

coop_pre[5,1] <- "average internal per person"
coop_pre[5,2] <- mean(coop2$internal_relation)

coop3$ratio_internal_to_external <- coop3$internal_relation / coop3$external_relation
coop_pre[6,1] <- "average ratio_ItoE"
coop_pre[6,2] <- mean(coop3$ratio_internal_to_external)

coop_pre[7,1] <- "largest external relations"
coop_pre[7,2] <- max(coop3$external_relation)

coop_pre[8,1] <- "largest internal relations"
coop_pre[8,2] <- max(coop3$internal_relation)

coop_pre[9,1] <- "largest ratio"
coop_pre[9,2] <- max(coop3$ratio_internal_to_external)

write.csv(coop_pre,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_freq.csv")
write.csv(coop3,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/coop_crossing_list.csv")


