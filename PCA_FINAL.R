############# PCA Briofitas #############
############# Tainá Rocha   #############

library(lattice)
library(ggplot2)
library(psych)
library(raster)
library(rgdal)
library(rgeos)
library(rJava)
library(FactoMineR)
library(vegan)
library(maptools)
library(factoextra)
library(ggcorrplot)

### Read table of variable information in records 

pca_briofitas <- read.table("./vals_pca_standardize.txt")

boxplot(pca_briofitas)


#boxplot(scale(pca_briofitas))

########
# Correlation matrix
correlation <- round(cor(pca_briofitas), 2)
correlation

# Plot

ggcorrplot(correlation, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 1.5, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title=" ", 
           ggtheme=theme_classic,
           legend.title="Correlation"
)


ggsave("correlation.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')


# PCA with cor matriz 

pca.cor <- PCA(pca_briofitas, scale.unit = T, graph = FALSE)

# matriz de covariância

round(cor(pca_briofitas),2)

# autovalores
round(pca.cor$eig,3)

# A proporção de variação retida pelos componentes principais (CP) pode ser extraída da seguinte forma

pca.cor$eig


# A importância dos CP pode ser visualizada usando o scree plot :
fviz_screeplot(pca.cor, ncp=4, choice="variance", addlabels=F, main=NULL, barfill="white", linecolor= "black", geom= c("bar","line"),barcolor = "black")+ theme_minimal()

ggsave("Scree_Plot_.tiff", units="in", width=5, height=4, dpi=100, compression = 'lzw')


# A correlação entre uma variável e um CP é chamada de carga (loadings). 

round(pca.cor$var$cor,4)


##Factorial map

# Quanto mais próxima uma variável for do círculo de correlações, melhor sua representação no mapa fatorial (e mais # Importante é a variável para a interpretação desses componentes)
# As variáveis próximas ao centro do gráfico são menos importantes para os primeiros componentes.
# No gráfico abaixo os componentes são coloridas de acordo com os valores do coseno quadrado:

briofitas.pca <- prcomp(vals_pca,  scale = TRUE)
fviz_pca_ind(briofitas.pca)


PCA <- fviz_pca_var(briofitas.pca, col.var="contrib", title="PCA")+
  scale_color_gradient2(low="white", mid="gray",
                        high="black", midpoint=0.5) + theme_minimal()
plot(PCA)

teste <- fviz_pca_biplot(briofitas.pca, label ="ind")

teste2<-fviz_pca_biplot(briofitas.pca, label ="var", col.ind="cos2", title = "PCA") +
  scale_color_gradient2(low="white", mid="gray",
                        high="black", midpoint=0.5) + theme_minimal()+theme_minimal()


plot(teste2)

ggsave("PCA_2.tiff", units="in", width=5, height=4, dpi=100, compression = 'lzw')

