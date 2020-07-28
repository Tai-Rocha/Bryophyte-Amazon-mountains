#################################################################
# Correlation matrix among 21 variables from wordclim version 2  
# Tainá Rocha                     
# Last update : 09/03/2020
#################################################################

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



