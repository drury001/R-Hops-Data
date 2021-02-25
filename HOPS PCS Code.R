### Load Packages ###

library(gdata)
library(tidyverse)
library(factoextra)
library(psych)
library(cluster)
library(reshape2)
library(kableExtra)
library(gplots)


### 

HOD=read.csv("Hops oil and acid -every.csv")        ### Load Hops Oil and Acid Data
Hops_CVHOP=read.csv("Hops_CVHOP.csv")   ### Load CV Hops LLC proprietary indicators
Hops_IP=read.csv("Hops_IP.csv")   ### Load public domain vs patented indicators
#Hops_price=read.csv("Hops price.csv")


rownames(HOD) <- HOD$Variety ### Make Hops Variety the row names
HOD=HOD[-1]   ### Remove "Variety" from the matrix 
HOD.no_other=HOD[-8]   ### Remove generic "other" from the matrix

summary(HOD)  ### Print distributional summary of each oil and acid

df <- scale(HOD.no_other)  ### Normalize all variables to the same scale 

pairs(df)  ### Created a matrix of scatterplots for each variable by each variable 

res.pca=prcomp(df)
fviz_eig(res.pca)
fviz_pca_ind(res.pca,label="ind",repel = TRUE)


fviz_pca_ind(res.pca,label="ind",repel = TRUE,col.ind=(Hops_CVHOP$Proprietary))

#fviz_pca_biplot(res.pca, repel = TRUE )




#di <- dist(HOD.no_other, method="euclidean")
#tree <- hclust(di, )
#plot(tree, xlab="")
#fviz_pca_biplot(res.pca, repel = TRUE )




di <- dist(HOD.no_other, method="euclidean")
tree <- hclust(di, )
plot(tree, xlab="")


### K-means

df <- scale(HOD.no_other)

#fviz_nbclust(df, kmeans,
#             method = "gap_stat")

fviz_nbclust(df, pam, method="silhouette")+theme_classic()


pm <- eclust(df,FUNcluster="pam", k=2,hc_metric = "euclidean")

#pm.sil<-silhouette(pm$cluster, dist(df))
#fviz_silhouette(pm.sil)




df<-cbind(df, as.factor (pm$cluster))
df=as.data.frame(df)
colnames(df)[10]<-c("Group")

df.m <- melt(df, id = c("Group"))
df.m$Group <- as.character(df.m$Group)

p <- ggplot(data = df.m, aes(x=variable, y=value)) +
  geom_boxplot(aes(fill = Group),outlier.size = 1) +
 # facet_wrap( ~ variable, scales="free", ncol = 9) +
  xlab(label = NULL) + ylab(label = NULL) + ggtitle("Boxplots for 3 Groups of Clients") +
  guides(fill=guide_legend(title="Groups"))

p 


HOD.no_other.groups=cbind(HOD.no_other, as.factor (pm$cluster))
colnames(HOD.no_other.groups)[10]<-c("Group")

GroupsSummary <- describeBy(HOD.no_other.groups, HOD.no_other.groups[,'Group'])

kable(GroupsSummary[[1]], format = "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

kable(GroupsSummary[[2]], format = "html", digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))



m <- as.matrix(dist(HOD.no_other, method="euclidean"))


#m.m_Citra=filter(m.m, Var1=="CITRA® HBC 394 CV.")

m[upper.tri(m)] <- NA

m.m=melt(m)

#m.m_filter=filter(m.m,!is.na(value))

#m.m_rank=arrange(m.m_filter,value)

#m.m_rank_row=rownames_to_column(m.m_rank)

#filter(m.m_rank_row, Var1=="CV12" & Var2=="CVC" | Var1=="CVC" & Var2=="CV12")

m_filtered  <- m.m %>% filter(!is.na(value))%>%
               filter(value>0) %>%
               arrange(value)%>%
               rownames_to_column()

filter(m_filtered, Var1=="CV12" & Var2=="CVC" | Var1=="CVC" & Var2=="CV12")




d <- dist(HOD.no_other, method="euclidean")


# Hierarchical clustering using Ward's method
res.hc <- hclust(d,  "average")

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 2)

# Visualize
plot(res.hc, cex = 0.6) # plot tree
#rect.hclust(res.hc, k = 2, border = 2:5) # add rectangle

library("Hmisc")
res2 <- rcorr(as.matrix(df),type=c("spearman"))
res2 

library("PerformanceAnalytics")
chart.Correlation(as.matrix(df), histogram=TRUE, pch=19)

df_price=cbind(df,scale(Hops_price$nomralized_price))

chart.Correlation(as.matrix(df_price), histogram=TRUE, pch=19)




df_price_IP=cbind(df_price,Hops_IP$Proprietary)

chart.Correlation(as.matrix(df_price_IP), histogram=TRUE, pch=19)



library(wNNSel)
library(reshape2)


H_price=read.csv("Hops web prices.csv")

head(H_price)

price_matrix=dcast(H_price, X.1~Company , value.var = "Price.per.oz")


rownames(price_matrix) <- price_matrix$X.1
price_matrix_1=price_matrix[-1]
price_matrix_1=as.matrix(price_matrix_1)


HOD_Price_Merge=merge(df,price_matrix_1,by="row.names")

rownames(HOD_Price_Merge) <- HOD_Price_Merge$Row.names
HODPM=as.matrix(HOD_Price_Merge[-1])


HODPM_impute=wNNSel(as.matrix(HODPM))

HODPM_impute_Done=HODPM_impute$x.impute


HODPM_impute_Done=as.data.frame(HODPM_impute_Done)


chart.Correlation(as.matrix(HODPM_impute_Done), histogram=TRUE, pch=19)

