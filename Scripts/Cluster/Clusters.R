####################################
## Cluster analysis 
## Author: Tainá Rocha 
####################################

#Aplicando boostrap pelo pvclust

briofitas_matrix_pvclus_<- read.csv("./briofitas_pvclus.csv")
briofitas_matrix_pvclus <- briofitas_matrix_pvclus_ [,-1]

cluster.upgma.jac <-pvclust(briofitas_matrix_pvclus, method.hclust ="average", method.dist = "binary", nboot=1000)

plot(cluster.upgma.jac, print.pv=TRUE, print.num=FALSE, float=0.01,
     col.pv=c(2,3,8), cex.pv=0.8, font.pv=NULL, col=NULL, cex=NULL,
     font=NULL, lty=NULL, lwd=NULL, main="", sub=NULL, xlab=NULL)



#conferindo se o cluster representa bem a matriz de distância.
# cálculo do coeficiente de correlação cofenética
# lembre-se sempre de relatar o valor do coeficiente. Na legenda da figura é uma boa opção.

vals.dist.cof.jac<-cophenetic(cluster.upgma.jac) #cálculo da matriz cofenética
cor(jaccard_vegan, vals.dist.cof.jac)    #coeficiente de correlação em si = -0.7893426


