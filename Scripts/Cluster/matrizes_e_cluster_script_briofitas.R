##############################################################
# Matrizes de distância e Cluster                            #
# Tainá Rocha                                                #
##############################################################

# carregando pacotes
library(vegan)
library(betapart)
library(ggplot2)
library(pvclust)

############## Exemplo
#data(ceram.s)
#ceram.core.s<-betapart.core(ceram.s)
#ceram.dist.jac<-beta.pair(ceram.core.s, index.family="jac")
#ceram.dist.sor<-beta.pair(ceram.core.s, index.family="sor")
#ceram.multi.jac<-beta.multi(ceram.core.s, index.family="jac")
#ceram.multi.sor<-beta.multi(ceram.core.s, index.family="sor")

#Carregando a planilha

briofitas_matrix_pvclus <- read.csv("./briofitas_pvclus.csv")

briofitas_clust <- briofitas_matrix_pvclus[ ,-1]

similaridade_names <- read.csv("./briofitas_PA.csv", sep = ";")

similaridade <- similaridade_names[ ,-1]

briofitas_simi <- betapart.core(similaridade)

#write.table(shared_sp, "./briofitas_similiradade.txt")

##### Jaccard 
jaccard.dist <- beta.pair(briofitas_simi, index.family = "jaccard")
jaccard.dist 

jac <- beta.multi(briofitas_simi, index.family = "jaccard")
jac

vals.dist.upgma.b <- hclust(jaccard.dist$beta.jac, method = "average")

plot(vals.dist.upgma.b)

# 4- conferindo se o cluster representa bem a matriz de distância.
# cálculo do coeficiente de correlação cofenética
# lembre-se sempre de relatar o valor do coeficiente. Na legenda da figura é uma boa opção.

vals.dist.cof<-cophenetic(vals.dist.upgma.b) #cálculo da matriz cofenética
cor(jaccard.dist$beta.jac, vals.dist.cof)    #coeficiente de correlação em si = 0.7893426


#####
# 5- Visualizando o agrupamendo na forma de dendrograma
# visualização vertical
plot(vals.dist.upgma.b)
plot(vals.dist.upgma.b, hang=-1, las=1, xlabs=" ", ylab= "Jaccard", main="Cluster dendogram")
labels(vals.dist.upgma.b)

# visualização horizontal
dend.upgma.b<-as.dendrogram(vals.dist.upgma.b)
plot(dend.upgma.b, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.


# visualização vertical
plot(vals.dist.upgma.b)
plot(vals.dist.upgma.b, hang=-1, las=1)


################################################################
#### Jaccard pelo vegan

#jaccard_vegan <- betadiver(similaridade, "j")
#jaccard_vegan

#distancia_vegan <- vegdist(jaccard_vegan, method="jaccard")
#cluster <- hclust(distancia_vegan, method = "average")
##plot(cluster)


#################### Sorense´pelo vegan 

#sorense_vegan <- vegdist(similaridade, binary = T)
#sorense_vegan
#cluster_s <- hclust(sorense_vegan, method = "average")
#plot(cluster_s)

# 3- calculando o cluster
# Não se esqueça de considerar o método de ligação a ser usado
# quase sempre, o método de ligação é o upgma "average",a não ser que outro método seja extremamente justificado. 

#vals.cluster.upgma<-hclust(jaccard.dist$beta.jac, method="average")

vals.cluster.upgma <-pvclust(briofitas_clust, method.hclust ="average", method.dist = "binary", nboot=1000)



plot(vals.cluster.upgma, print.pv=TRUE, print.num=FALSE, float=0.01,
     col.pv=c(2,3,8), cex.pv=0.8, font.pv=NULL, col=NULL, cex=NULL,
     font=NULL, lty=NULL, lwd=NULL, main="", sub=NULL, xlab=NULL)


#####
# 4- conferindo se o cluster representa bem a matriz de distância.
# cálculo do coeficiente de correlação cofenética
# lembre-se sempre de relatar o valor do coeficiente. Na legenda da figura é uma boa opção.

vals.dist.cof<-cophenetic(vals.cluster.upgma) #cálculo da matriz cofenética
cor(jaccard.dist$beta.jac, vals.dist.cof)             # coeficiente de correlação em si

#####
# 5- Visualizando o agrupamendo na forma de dendrograma
# visualização vertical
plot(vals.cluster.upgma)
plot(vals.cluster.upgma, hang=-1, las=1, xlabs=" ", ylab= "Jaccard", main="Cluster dendogram")
labels(vals.cluster.upgma)

# visualização horizontal
vals.dend.upgma<-as.dendrogram(vals.cluster.upgma)
plot(vals.dend.upgma, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.

#### Sorense

son <- beta.multi(similaridade, index.family = "sorensen")
son

son_2 <- beta.pair(similaridade, index.family = "sorensen")
son_2

########## Dend 

vals.cluster.upgma_s<-hclust(son_2$beta.sne, method="average")

vals.dist.cof<-cophenetic(vals.cluster.upgma_s) #cálculo da matriz cofenética
cor(son_2$beta.sne, vals.dist.cof)             # coeficiente de correlação em si

#####
# 5- Visualizando o agrupamendo na forma de dendrograma
# visualização vertical
plot(vals.cluster.upgma_s)
plot(vals.cluster.upgma_s, hang=-1, las=1)



# visualização horizontal
vals.dend.upgma_s<-as.dendrogram(vals.cluster.upgma_s)
plot(vals.dend.upgma_s, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.


#####
## ajustes que podem ser úteis
# tamanho do gráfico
# opção 1: trabalhar com as opções do menu "export". mexa nas dimensões em width e height, no teste mesmo.

# opção 2: script. e depende do sistema operacional. no meu caso, função windows:
windows(width = 6, height = 7, rescale = "fixed")
plot(vals.dend.upgma, horiz=T, hang=-1, las=1)

## MExendo nos nomes
vals.cod<-ifelse(dados$Ambiente=="Mata Primaria", "MP", "MS")
nomes<-paste(amb.cod, "-", dados$UA, sep="")

## ajustando os nomes. 
# 1) eles podem ser incorporados nos dados originais 
# 2) usar os nomes como labels no cluster
amb.cluster.upgma$labels<-nomes

# ajustando a margem direita do grafico (ficou curta)
par(mar=c(4, 2, 2, 5))

amb.dend.upgma<-as.dendrogram(amb.cluster.upgma)
plot(amb.dend.upgma, horiz=T, hang=-1, las=1)

## Algumas comentários
# dados de quantidade (densidade, biomassa, abundância etc), considere usar bray-curtis (method =" bray")
# presença/ ausência: coeficiente de Jaccard (method="jaccard", binary="T")
# para outras distâncias (que ñ estão na lista do vegdist), crie uma métrica com a função designdist

