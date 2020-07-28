##############################################################
# Matrizes de distância e Cluster                            #
# Kele Rocha Firmiano                                        #
##############################################################

setwd("C:/curso_r/r_bio_conservacao/modulo_6")

dir()
dados<-read.table("insetos.txt", header=T, sep="\t", dec=".")

# Conferindo os dados
summary(dados)  # há muitos dados, o que não é tão fácil de ver. 
str(dados)      # aqui há um panorama melhor, mas se perde informações, tipo média. 

# carregando pacotes
library(vegan)

# separando os dados
amb<-dados[ ,4:7]

#####
# 1- Padronização de variáveis
# Alguns passos a serem seguidos:
# 1- Há necessidade de padronizar os dados? 
# 2- Em geral, unidades de medidas de magnitudes diferentes pedem padronização (isto é, media =0 e ds = 1)
# caso as variáveis tenham a mesma unidade de medidas, não há necessidade de padronizar. 
amb.pad<-decostand(amb, method="standardize")

#####
# 2- cálculo da matriz de distância 
# a escolha da métrica depende: da natureza dos dados coletados, e da pergunta de pesquisa.
amb.dist.euc<-vegdist(amb.pad, method="euclidean")

#####
# 3- calculando o cluster
# Não se esqueça de considerar o método de ligação a ser usado
# quase sempre, o método de ligação é o upgma "average",a não ser que outro método seja extremamente justificado. 
amb.cluster.upgma<-hclust(amb.dist.euc, method="average")

#####
# 4- conferindo se o cluster representa bem a matriz de distância.
# cálculo do coeficiente de correlação cofenética
# lembre-se sempre de relatar o valor do coeficiente. Na legenda da figura é uma boa opção.
amb.dist.cof<-cophenetic(amb.cluster.upgma) #cálculo da matriz cofenética
cor(amb.dist.euc, amb.dist.cof)             # coeficiente de correlação em si

#####
# 5- Visualizando o agrupamendo na forma de dendrograma
# visualização vertical
plot(amb.cluster.upgma)
plot(amb.cluster.upgma, hang=-1, las=1)

# visualização horizontal
amb.dend.upgma<-as.dendrogram(amb.cluster.upgma)
plot(amb.dend.upgma, horiz=T, hang=-1, las=1) # horiz só funciona se o objeto for do tipo dendrogram, e não clust.

#####
## ajustes que podem ser úteis
# tamanho do gráfico
# opção 1: trabalhar com as opções do menu "export". mexa nas dimensões em width e height, no teste mesmo.

# opção 2: script. e depende do sistema operacional. no meu caso, função windows:
windows(width = 6, height = 7, rescale = "fixed")
plot(amb.dend.upgma, horiz=T, hang=-1, las=1)

## MExendo nos nomes
amb.cod<-ifelse(dados$Ambiente=="Mata Primaria", "MP", "MS")
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

