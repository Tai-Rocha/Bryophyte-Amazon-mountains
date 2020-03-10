###########################################################################
# Principal component analysis (PCA) 21 variables from wordclim version 2  
# Tainá Rocha                     
# Last update : 10/03/2020
###########################################################################

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

fviz_pca_var(PCA_Graphic, select.var = list(contrib = 6))

fviz_pca_biplot(PCA_Graphic)

# Select the top 30 contributing individuals

fviz_pca_biplot(pca_test, label="var", select.var = list(contrib = 6),
                select.ind = list(contrib = 10))

###################

fviz_pca_var(pca_test, alpha.var="contrib") +
  theme_minimal()

fviz_pca_var(pca_test, axes = c(1, 2), geom = c("arrow", "text"),
             label = "all", invisible = "none", labelsize = 3,
             col.var = "black", alpha.var = 1, col.quanti.sup = "blue",
             col.circle = "grey70",
             select.var = list(name =NULL, cos2 = NULL, contrib = 21))


tiff("./PCA_no_names.tiff", bg = "white", unit= "px", res = 300)
dev.off()
# PCA with cor matriz 

PCA_21_variables <- PCA(vals_pca, scale.unit = T, ncp = 5, graph = TRUE, ind.sup = 20:21)


plot(PCA_21_variables, graph.type = "ggplot")

plot(PCA_21_variables, habillage = 10, cex=0.3)



test_ <- PCA(vals_pca, quanti.sup = 7:8, quali.sup = 10)

gr <- plot(PCA_21_variables)
gr + theme(panel.grid.major = element_blank(),
           plot.title=element_text(size=14, color="blue"),
           axis.title = element_text(size=12, color="red"))

plot(PCA_21_variables, choix = "var",
     col.ind="black", col.ind.sup="blue", 
     col.var="black", label = "var", lim.cos2.var = 0, title = PCA, palette=NULL,
     autoLab = "auto", new.plot = FALSE, select = NULL,
     shadowtext = FALSE, graph.type = "ggplot", ggoptions = NULL)

#PCA_21_variables_2 <- PCA(vals_pca, graph = FALSE)
#print(PCA_21_variables_2)


plot(PCA_21_variables, cex=1.5,scale.unit = T, ncp = 21)


par(PCA_21_variables, )

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



