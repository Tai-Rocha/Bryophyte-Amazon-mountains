###################################################################
# Principal component analysis (PCA) 21 variables from wordclim version 2  
# Tainá Rocha                     
# Last update : 10/03/2020
#################################################################

  library(factoextra)
  library(FactoMineR)
  library(ggcorrplot)
  library(ggplot2)
  library(psych)
  library(rgdal)
  library(rgeos)
  library(vegan)

### Read table of variable information in records. Standardize

pca_21_var <- read.table("./data/PCA/pca_21_var.txt")

vals_pca_21_var<-decostand(pca_21_var, method="standardize")

boxplot(vals_pca_21_var)

#PCA FACTOREXTRA

PCA_Graphic <- prcomp(vals_pca_21_var)

fviz_pca_var(PCA_Graphic, axes = c(1, 2), geom = c("arrow", "text"),
             label = "all", invisible = "none", labelsize = 3,
             col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
             col.circle = "grey70", select.var = list(name =NULL, cos2 = NULL, contrib = 21)) + 
              labs(title ="Principal component analysis (PCA)", x = "PC1", y = "PC2")

tiff("PCA_21_vars.tiff", width = 984, height = 615, unit = "px", res = 300)

dev.off()
    
################# END 
