
install.packages("gplots")
library(tidyverse)
library(factoextra)
library(psych)
library(cluster)
library(reshape2)
library(kableExtra)
library(gplots)

HOD=as.data.frame(Hops_oil_and_acid_every)
rownames(HOD) <- HOD$Variety

HOD=HOD[-1]

HOD.no_other=HOD[-8]

res.pca=prcomp(HOD.no_other, scale. = TRUE)

fviz_eig(res.pca)

fviz_pca_ind(res.pca,label="ind",repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE )



HODHOD=HOD %>% arrange(Beta)


di <- dist(HOD.no_other, method="euclidean")
tree <- hclust(di, method="ward.D")
plot(tree, xlab="")


### K-means

df <- scale(HOD.no_other)

fviz_nbclust(df, kmeans,
             method = "gap_stat")

fviz_nbclust(df, pam, method="silhouette")+theme_classic()


pm <- eclust(df,FUNcluster="pam", k=2,hc_metric = "euclidean")

pm.sil<-silhouette(pm$cluster, dist(df))
fviz_silhouette(pm.sil)




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


m.m=melt(m)



