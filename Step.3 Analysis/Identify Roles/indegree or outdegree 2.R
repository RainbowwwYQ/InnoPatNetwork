

# install.packages("ggrepel")
library(dplyr)
library(plyr)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggrepel)
# ---- prepare ----
rm(list = ls())
dev.off()

outdegree <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_outdegree.csv",header = T,as.is = T)
outdegree_top <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_outdegree (top10).csv",header = T,as.is = T)
indegree<-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_indegree.csv",header = T,as.is = T)
indegree_top <-read.csv(file = "/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_indegree (top10).csv",header = T,as.is = T)

via <- bind_rows(indegree,outdegree)
via <- via[!duplicated(via$Label),]
via <- via[,-(2:4)]
via <- via[,-14]

indegree <- indegree[,-(2:3)]
indegree <- indegree[,-(3:14)]
outdegree <- outdegree[,-(2:3)]
outdegree <- outdegree[,-(3:14)]

gather <- merge(outdegree, indegree, by="Label", all = T)
gather[is.na(gather)]=0

gather <- merge(gather, via, by="Label")
gather$Label_backup <- gather$Label
obs <- length(gather$Label)
for (i in 1:obs){
  if (gather[i,"degree"] <12){
    gather[i, "Label_backup"]<- NA
  }
}

colnames(gather)[2]<- "out_degree"
colnames(gather)[3] <- "in_degree"

gather[155,3]<-27
gather <- gather[-156,]
gather[302,3]<-40
gather <- gather[-303,]

gather[155,7] <- "public institute"

row.names(gather)<-NULL

# ---- graphs ----
sp <- ggplot(data=gather,aes(x=in_degree, y=out_degree, 
                             colour = regions, shape=classification))+
  geom_point(size=gather$degree/2, alpha=0.5)+
  scale_color_wsj()+
  scale_shape_manual(values=c(2,0,1,5))

#change background color + baseline

sp <- sp + theme_bw() + theme(panel.border = element_blank(),panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
sp <- sp + geom_hline(aes(yintercept=0),colour="#BB0000",linetype="dashed")
sp <- sp + geom_vline(aes(xintercept=0),colour="#BB0000",linetype="dashed")
sp <- sp + geom_text_repel(aes(y = out_degree + .2,label=Label_backup))+theme_classic(base_size = 10)
sp

# ---- measures ----

stat <- data.frame(mobility = c(NA), measures = c(NA), count = c(NA),percentage=c(NA))

# ---- external ----
stat[1,"mobility"] <- "outdegree"
stat[1,"measures"]<- "number of top_outdegree"
stat[1,"count"]<- length(outdegree_top$Label)

stat[2,"mobility"] <- "outdegree"
stat[2,"measures"]<- "number of university"
stat[2,"count"]<- length(which(outdegree_top$classification=="university"))
stat[2,"percentage"]<- length(which(outdegree_top$classification=="university"))/length(outdegree_top$Label)

stat[3,"mobility"] <- "outdegree"
stat[3,"measures"]<- "number of company"
stat[3,"count"]<- length(which(outdegree_top$classification=="business"))
stat[3,"percentage"]<- length(which(outdegree_top$classification=="business"))/length(outdegree_top$Label)

stat[4,"mobility"] <- "outdegree"
stat[4,"measures"]<- "number of public institutes"
stat[4,"count"]<- length(which(outdegree_top$classification=="public institute"))
stat[4,"percentage"]<- length(which(outdegree_top$classification=="public institute"))/length(outdegree_top$Label)

stat[5,"mobility"] <- "outdegree"
stat[5,"measures"]<- "number of individual"
stat[5,"count"]<- length(which(outdegree_top$classification=="individual"))
stat[5,"percentage"]<- length(which(outdegree_top$classification=="individual"))/length(outdegree_top$Label)

stat[6,"mobility"] <- "outdegree"
stat[6,"measures"]<- "number of internal actor"
stat[6,"count"]<- length(which(outdegree_top$regions=="internal"))
stat[6,"percentage"]<- length(which(outdegree_top$regions=="internal"))/length(outdegree_top$Label)

stat[7,"mobility"] <- "outdegree"
stat[7,"measures"]<- "number of external actor"
stat[7,"count"]<- length(which(outdegree_top$regions=="external"))
stat[7,"percentage"]<- length(which(outdegree_top$regions=="external"))/length(outdegree_top$Label)

# ---- internal ----

stat[8,"mobility"] <- "indegree"
stat[8,"measures"]<- "number of top_indegree"
stat[8,"count"]<- length(indegree_top$Label)

stat[9,"mobility"] <- "indegree"
stat[9,"measures"]<- "number of university"
stat[9,"count"]<- length(which(indegree_top$classification=="university"))
stat[9,"percentage"]<- length(which(indegree_top$classification=="university"))/length(indegree_top$Label)

stat[10,"mobility"] <- "indegree"
stat[10,"measures"]<- "number of company"
stat[10,"count"]<- length(which(indegree_top$classification=="business"))
stat[10,"percentage"]<- length(which(indegree_top$classification=="business"))/length(indegree_top$Label)

stat[11,"mobility"] <- "indegree"
stat[11,"measures"]<- "number of public institutes"
stat[11,"count"]<- length(which(indegree_top$classification=="public institute"))
stat[11,"percentage"]<- length(which(indegree_top$classification=="public institute"))/length(indegree_top$Label)

stat[12,"mobility"] <- "indegree"
stat[12,"measures"]<- "number of individual"
stat[12,"count"]<- length(which(indegree_top$classification=="individual"))
stat[12,"percentage"]<- length(which(indegree_top$classification=="individual"))/length(indegree_top$Label)

stat[13,"mobility"] <- "indegree"
stat[13,"measures"]<- "number of internal actor"
stat[13,"count"]<- length(which(indegree_top$regions=="internal"))
stat[13,"percentage"]<- length(which(indegree_top$regions=="internal"))/length(indegree_top$Label)

stat[14,"mobility"] <- "indegree"
stat[14,"measures"]<- "number of external actor"
stat[14,"count"]<- length(which(indegree_top$regions=="external"))
stat[14,"percentage"]<- length(which(indegree_top$regions=="external"))/length(indegree_top$Label)

# ---- both ----

outdegree_top2 <- outdegree_top[,c(1,4)]
indegree_top2 <- indegree_top[,c(1,4)]
outdegree_top3 <- left_join(outdegree_top2, indegree_top2, by="Label")
outdegree_top3[is.na(outdegree_top3)]=0
indegree_top3 <- left_join(indegree_top2, outdegree_top2, by="Label")
indegree_top3[is.na(indegree_top3)]=0
outdegree_top3$oriented <- outdegree_top3$outdegree - outdegree_top3$indegree
indegree_top3$oriented <- indegree_top3$outdegree - indegree_top3$indegree

stat[15,"mobility"]<- "outdegree"
stat[15,"measures"]<-"number of outdegree_oriented"
stat[15,"count"]<- length(which(outdegree_top3$oriented>0))
stat[15,"percentage"]<- length(which(outdegree_top3$oriented>0)) / length(outdegree_top3$Label)


stat[16,"mobility"]<- "indegree"
stat[16,"measures"]<-"number of outdegree_oriented"
stat[16,"count"]<- length(which(indegree_top3$oriented>0))
stat[16,"percentage"]<- length(which(indegree_top3$oriented>0))/length(indegree_top3$Label)

# ---- output ---- 
write.csv(stat,file="/Users/mayhe/Desktop/thesis preparation/thesis drafts/thesis/data process/original analysis/Montreal/all_years/mobility&cooperation/mobi_in&out_stat.csv")
