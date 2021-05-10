


rm(list = ls())
dev.off()

mobi_node <- read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_node.csv",header = T,as.is = T)
mobi_link <- read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_link.csv",header = T,as.is = T)
mobi_node <- mobi_node[,-1]
mobi_link <- mobi_link[,-1]

# ---- outdegree ----
outdegree <- data.frame(Label = mobi_link$Source, weight = mobi_link$Weight, if_cross = mobi_link$If_across)

outdegree$internal <- 0
outdegree$external <- 0

obs <- length(outdegree$Label)
outdegree <- outdegree[order(outdegree$Label, outdegree$if_cross),]
row.names(outdegree)<- NULL

for (i in 2:obs){
  if (outdegree[i,"Label"]==outdegree[i-1,"Label"]& outdegree[i,"if_cross"]==outdegree[i-1,"if_cross"]){
    outdegree[i,"weight"]<- outdegree[i,"weight"]+outdegree[i-1, "weight"]
    outdegree[i-1, "weight"]<-NA
  }
}
outdegree <- outdegree[complete.cases(outdegree$weight),]
row.names(outdegree)<- NULL

obs <- length(outdegree$Label)
outdegree <- outdegree[order(outdegree$Label, outdegree$if_cross),]
row.names(outdegree)<- NULL

for (i in 1:obs){
  if (outdegree[i,"if_cross"]==1){
    outdegree[i,"external"]<- outdegree[i,"weight"]
  } else if (outdegree[i,"if_cross"]==0){
    outdegree[i,"internal"]<- outdegree[i,"weight"]
  }
}
for (i in 2:obs){
  if (outdegree[i,"Label"]==outdegree[i-1,"Label"]){
    outdegree[i-1, "external"]<- outdegree[i, "external"]
    outdegree[i, "weight"]<- NA
  }
}
outdegree<- outdegree[complete.cases(outdegree$weight),]
row.names(outdegree)<- NULL

outdegree$outdegree <- outdegree$internal+outdegree$external
outdegree <- outdegree[,-(2:3)]

outdegree <- merge(outdegree,mobi_node,by="Label")

# ---- indegree ----

indegree <- data.frame(Label = mobi_link$Target, weight = mobi_link$Weight, if_cross = mobi_link$If_across)

indegree$internal <- 0
indegree$external <- 0

obs2 <- length(indegree$Label)
indegree <- indegree[order(indegree$Label,indegree$if_cross),]
# indegree <- indegree[-(1:3),]
row.names(indegree)<- NULL


for (i in 2:obs2){
  if (indegree[i,"Label"]==indegree[i-1,"Label"]& indegree[i,"if_cross"]==indegree[i-1,"if_cross"]){
    indegree[i,"weight"]<- indegree[i,"weight"]+indegree[i-1, "weight"]
    indegree[i-1, "weight"]<-NA
  }
}
indegree <- indegree[complete.cases(indegree$weight),]
row.names(indegree)<- NULL

obs3 <- length(indegree$Label)
indegree <- indegree[order(indegree$Label, indegree$if_cross),]
row.names(indegree)<- NULL

for (i in 1:obs3){
  if (indegree[i,"if_cross"]==1){
    indegree[i,"external"]<- indegree[i,"weight"]
  } else if (indegree[i,"if_cross"]==0){
    indegree[i,"internal"]<- indegree[i,"weight"]
  }
}
for (i in 2:obs3){
  if (indegree[i,"Label"]==indegree[i-1,"Label"]){
    indegree[i-1, "external"]<- indegree[i, "external"]
    indegree[i, "weight"]<- NA
  }
}
indegree<- indegree[complete.cases(indegree$weight),]
row.names(indegree)<- NULL

indegree$indegree <- indegree$internal+indegree$external
indegree <- indegree[,-(2:3)]

indegree <- merge(indegree,mobi_node,by="Label")


# ---- stat ----

mobi_pre <- data.frame(measures=c(NA),count=c(NA))

mobi_pre[1,1] <- "number of links"
mobi_pre[1,2] <- length(mobi_link$Source)

mobi_pre[2,1] <- "outdegree"
mobi_pre[2,2] <- sum(outdegree$outdegree) # greater than No. links since links *2 for both sides

mobi_pre[3,1] <- "indegree"
mobi_pre[3,2] <- sum(indegree$indegree) 

mobi_pre[4,1] <- "average outdegree per person"
mobi_pre[4,2] <- mean(outdegree$outdegree)

mobi_pre[5,1] <- "average indegree per person"
mobi_pre[5,2] <- mean(indegree$indegree)

# ---- output ----

write.csv(indegree,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_indegree.csv")
write.csv(outdegree,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_outdegree.csv")
write.csv(mobi_pre,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_pre.csv")

