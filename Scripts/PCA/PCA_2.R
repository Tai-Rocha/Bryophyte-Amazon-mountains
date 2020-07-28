###########################################################################
# Principal component analysis (PCA) 21 variables from wordclim version 2  
# Other graphics
# Tainá Rocha                     
# Last update : 10/03/2020
###########################################################################

### Library

library(factoextra)
library(FactoMineR)
library(ggcorrplot)
library(ggplot2)
library(psych)
library(rgdal)
library(rgeos)
library(vegan)

########################################### 

## Read tables and build PCA

pca_21_var <- read.csv("./data/PCA/stand_test.csv", sep = ",")

    res.pca <- prcomp(pca_21_var[, -22],  scale = TRUE)
    

### Color individuals by groups
    
    fviz_pca_ind(res.pca, label="none", pointsize = 4, habillage=pca_21_var$Montain)  + 
        labs(title ="Individuals factor map- PCA", x = "PC1", y = "PC2")

tiff("ind_factor_map.tiff", width = 984, height = 615, unit = "px", res = 300)
    dev.off()    
### Biplot of individuals of variables
      
fviz_pca_biplot(res.pca, geom = "point", habillage=pca_21_var$Montain, pointsize = 3, 
                select.var = list(contrib = 21), labelsize = 3, label = "none") + 
    labs(title ="Biplot of variables and individuals", x = "PC1", y = "PC2")

tiff("Biplot.tiff", width = 984, height = 615, unit = "px", res = 300)
dev.off()

########################################### End 
    
  