#############################################################
# β diversity measures                                      #
# Author: Tainá Rocha                                        #
##############################################################

# carregando pacotes
library(vegan)
library(betapart)
library(ggplot2)
library(pvclust)
library(betapart)
#####################

#Carregando a planilha

matrix_similaridade_ <- read.csv("./briofitas_PA.csv", sep = ";")

matrix_similaridade <- matrix_similaridade_[ ,-1]

briofitas_simi <- betapart.core(matrix_similaridade)


#### Jaccard index (VEGAN)

jaccard_vegan <- betadiver(matrix_similaridade, "j")
jaccard_vegan


#### Sorense index (VEGAN)

sorense_vegan <- betadiver(matrix_similaridade, "sor")
sorense_vegan


### CHAO index (VEGAN)

chao_vegan <- betadiver(matrix_similaridade, "co")
chao_vegan


