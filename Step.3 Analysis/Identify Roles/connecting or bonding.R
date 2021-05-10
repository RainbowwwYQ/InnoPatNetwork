
# ---- prepare ----
rm(list = ls())
dev.off()

node <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_node.csv",header = T,as.is = T)
link <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_link.csv",header = T,as.is = T)
node <- node[,-1]
link <- link[,-1]

#node <- node[order(node$eigenvector, decreasing = T),]
#row.names(node)<-NULL
#split_eigen <- node[29,"eigenvector"]

node <- node[order(node$pagerank, decreasing = T),]
row.names(node)<-NULL
split_pagerank <- node[38,"pagerank"]

node <- node[order(node$degree, decreasing = T),]
row.names(node)<-NULL
split_degree <- node[38,"degree"]

node <- node[order(node$betweenness, decreasing = T),]
row.names(node)<-NULL
split_between <- node[38,"betweenness"]

node$role_bonding <- NA
node$role_connecting <- NA
obs <- length(node$Label)
for (i in 1:obs){
  if (node[i,"degree"]>= split_degree & node[i,"pagerank"]>=split_pagerank){
    node[i,"role_bonding"]<- "bonding"
  }
}

for (i in 1:obs){
  if (node[i,"betweenness"]>= split_between & node[i,"pagerank"]>=split_pagerank){
    node[i,"role_connecting"]<- "connecting"
  }
}

connecting <- node[complete.cases(node$role_connecting),]
bonding <- node[complete.cases(node$role_bonding),]

# ---- output ----

write.csv(connecting,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_connecting.csv")
write.csv(bonding,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/all_bonding.csv")

