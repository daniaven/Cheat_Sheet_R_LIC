# 1) Boxplots ----
# vamos a hacer todos los gráficos con ggplot2
library(ggplot2)

p1 <- ggplot(df_ejemplo.2.pivot, aes(x = tipo_celular, y = Expresion)) +
  geom_boxplot(color = "#5EA7B9") + # para elegir colores recomiendo: https://coolors.co/1a2c64-1d6288-71af73-e0d66a-e1a83d
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + # por ejemplo
  theme_bw()
p1

# si lo queres con relleno (cada tipo celular un color distinto:
p1 <- ggplot(df_ejemplo.2.pivot, 
             aes(x = tipo_celular, y = Expresion, fill = tipo_celular)) + # acá tenemos que agregar el fill
  geom_boxplot(color = "black") + 
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + 
  scale_fill_manual(values=c("#1A2C64", "#1D6288","#47B0AF","#99D490","#6BB046",# para elegir colores recomiendo: https://coolors.co/1a2c64-1d6288-71af73-e0d66a-e1a83d
                             "#F0E958","#EFA51B","#EF8747","#C84F4F","#8B0000"))+ # aca se agregan los colores del fill
  theme_bw()
p1

# Si lo querés con los puntos de cada muestra:
p1 <- ggplot(df_ejemplo.2.pivot, 
             aes(x = tipo_celular, y = Expresion, fill = tipo_celular)) + 
  geom_boxplot(color = "black") + 
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + 
  scale_fill_manual(values=c("#1A2C64", "#1D6288","#47B0AF","#99D490","#6BB046",
                             "#F0E958","#EFA51B","#EF8747","#C84F4F","#8B0000"))+
  geom_dotplot(binaxis='y', stackdir='center',
               position=position_dodge(1))+
  theme_bw()
p1

# Otra forma de agregar puntos que a mi me gusta más
p1 <- ggplot(df_ejemplo.2.pivot, 
             aes(x = tipo_celular, y = Expresion, fill = tipo_celular)) + 
  geom_boxplot(color = "black") + 
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + 
  scale_fill_manual(values=c("#1A2C64", "#1D6288","#47B0AF","#99D490","#6BB046",
                             "#F0E958","#EFA51B","#EF8747","#C84F4F","#8B0000"))+
  geom_jitter(color="black", size=1, alpha=0.9) + # podes modificar el tamaño con size
  theme_bw()
p1

# Cambiar ángulo de eje x
p1 <- ggplot(df_ejemplo.2.pivot, 
             aes(x = tipo_celular, y = Expresion, fill = tipo_celular)) + 
  geom_boxplot(color = "black") + 
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + 
  scale_fill_manual(values=c("#1A2C64", "#1D6288","#47B0AF","#99D490","#6BB046",
                             "#F0E958","#EFA51B","#EF8747","#C84F4F","#8B0000"))+
  geom_jitter(color="black", size=1, alpha=0.9) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + #podes ajustar el ángulo
  theme_bw()
p1

# Graficar más de un solo parámetro
df_ejemplo$Toxicidad_Aguda <- as.factor(df_ejemplo$Toxicidad_Aguda)

p2 <- ggplot(df_ejemplo, 
             aes(x = Toxicidad_Aguda, y = Probabilidad_recaída, fill = Muerte)) + 
  geom_boxplot(color = "black") + 
  labs(x = "Toxicidad Aguda", y = "Probabilidad de Recaída") + 
  scale_fill_manual(values=c("#1D6288","#99D490"))+
  geom_jitter(color="black", size=1, alpha=0.9) +
  theme_bw()
p2

# 2) Violin plot ----
v1 <- ggplot(df_ejemplo.2.pivot, 
             aes(x = tipo_celular, y = Expresion, fill = tipo_celular)) + 
  geom_violin() + # acá se aclara que es un violin
  labs(x = "Tipo celular", y = "Abundancia relativa (TPM)") + 
  scale_fill_manual(values=c("#1A2C64", "#1D6288","#47B0AF","#99D490","#6BB046",
                             "#F0E958","#EFA51B","#EF8747","#C84F4F","#8B0000"))+
  geom_jitter(color="black", size=1, alpha=0.9) +
  scale_x_discrete(guide = guide_axis(angle = 90)) + #podes ajustar el ángulo
  theme_bw()
v1

# 3) Plotear 2 gráficos en uno: ----
library(gridExtra)
p <- grid.arrange(p1, p2, ncol = 2)


# 4) Guardar un gráfico con buena calidad ----

ggsave(p, file="Boxplot_tipo_celular_vs_abund_relat.png", device = "png", dpi = 300,
       height = 5, width = 9) # el ancho (width) y altura (height) podes irla probando
                              # recomiendo dpi = 300, si querés mejor calidad podes aumentarlo 
                              # pero el archivo va a ser más pesado y va a tardar más en guardar

# 5) Heatmaps ----

library(gplots)
myheatcolors <- bluered(20) # Genero los colores que puede tomar el heatmap

m_ejemplo_edit <- t(m_ejemplo)
clustColumns <- hclust(as.dist(1-cor(m_ejemplo_edit, method="spearman")), method="complete") 
# Agrupa columnas por correlación de spearman
# Corta el árbol resultante y crea un vector de colores para los clusters
# Se puede variar la altura del punto de corte (h =) para crear más o menos clusters o 
# simplemente indicar con k= el número de clusters que queres
module.assign <- cutree(clustColumns, h=0.90)

# Ahora asignar un color a cada módulo (para facilitar la identificación y manipulación)
module.color <- rainbow(length(unique(module.assign)), start=0.1, end=0.9)
module.color <- module.color[as.vector(module.assign)]
ColSideColors <- hcl.colors(length(unique(module.assign)), palette = "RdYlBu")
ColSideColors <- ColSideColors[as.vector(module.assign)]

library(heatmaply)
heatmaply(m_ejemplo_edit,
          colors = myheatcolors,
          Rowv=NA, Colv=as.dendrogram(clustColumns),
          k_col = 2,
          col_side_colors = df_ejemplo[,c("Muerte","Toxicidad_Aguda","Grupo_Riesgo")],
          #showticklabels=c(FALSE,FALSE),
          scale='row',
          file = "heatmap.html")

# intentendo guardar el heatmap con buena calidad
library(png)
dpi <- 300
install.packages("webshot2")
library(webshot2)
webshot("heatmap.html", "heatmap_CD20.exp.png", delay = 5, zoom = dpi/96)
webshot("heatmap.html", "heatmap2.png", delay = 0, vwidth = 800, vheight = 700, zoom = dpi / 96)
