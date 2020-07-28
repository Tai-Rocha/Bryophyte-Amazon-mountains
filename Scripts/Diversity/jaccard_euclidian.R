##############################################################
# Matrizes de distância e Cluster                            #
# Tainá Rocha                                                #
##############################################################
library(vegan)
library(dendextend)

briofitas_teste <- read.csv('./briofitas_PA.csv')

briofitas_teste<-t(briofitas_teste)

briofitas_teste_dataframe <- as.data.frame(briofitas_teste)

briofitas_teste_dataframe_ <- briofitas_teste_dataframe[, 1:1]

rm(briofitas_teste_dataframe_)

#briofitas_teste <- write.table(briofitas_teste, '/briofitas.txt')

#briofitas <- read.csv('./briofitas_t.csv', sep = ";", dec = ".")

#briofitas_matrix <- briofitas[ ,-1]

briofitas.dist.jac<-vegdist(briofitas_teste_dataframe_2, method="jaccard")

briofitas.cluster.upgma<-hclust(briofitas.dist.jac, method="average")

#####
# 5- Visualizando o agrupamendo na forma de dendrograma

# visualização vertical
plot(briofitas.cluster.upgma)
plot(briofitas.cluster.upgma, hang=-1, las=1)



jaccard_briofitas <- betadiver(briofitas_matrix, 'j')

jaccard_briofitas<- write.table(jaccard_2,'./jaccard_briofitas.txt')

# visualização horizontal
briofitas.cluster.upgma<-as.dendrogram(briofitas.cluster.upgma)
plot(briofitas.cluster.upgma, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.

mean(jaccard_briofitas)

















######### Change the labels, and then plot:

briofitas.cluster.upgma.labels %>% set("labels", c("CUR", "CAB", "SEG", "NEB", "ARA")) %>% plot



briofitas.cluster.upgma.labels<-set(briofitas.cluster.upgma, labels=c("CUR", "CAB", "SEG", "NEB", "ARA"))

plot(briofitas.cluster.upgma.labels)

set(jaccard_briofitas, what, value)

























#####
# 4- conferindo se o cluster representa bem a matriz de distância.
# cálculo do coeficiente de correlação cofenética
# lembre-se sempre de relatar o valor do coeficiente. Na legenda da figura é uma boa opção.
vals.dist.cof_t<-cophenetic(vals.cluster.upgma_t) #cálculo da matriz cofenética
cor(vals.dist.jac_t, vals.dist.cof_t)             # coeficiente de correlação em si


# visualização horizontal
vals.dend.upgma_t<-as.dendrogram(vals.cluster.upgma_t)
plot(vals.dend.upgma_t, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.


#####
## ajustes que podem ser úteis
# tamanho do gráfico
# opção 1: trabalhar com as opções do menu "export". mexa nas dimensões em width e height, no teste mesmo.

# opção 2: script. e depende do sistema operacional. no meu caso, função windows:
windows(width = 6, height = 7, rescale = 'fixed')
plot(vals.dend.upgma, horiz=T, hang=-1, las=1)

plot(vals.dend.upgma,hang=-1, cex= 0.3)

plot(vals.dend.upgma, xlim = c(1, 419), ylim = c(1,20))


vals.dend.upgma$labels<-nomes


