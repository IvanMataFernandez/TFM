source("r_scripts/Plots.R")
source("r_scripts/Models.R")
source("r_scripts/Data_importer.R")

# importar dataset

df_empresas <- importar_df_empresas()
view(df_empresas)


# Overview de los datos


colores2 <- c("#1B4965", "#CAE9FF")
colores3 <- c("#1B4965", "#61A5C2", "#CAE9FF")
colores4 <- c("#1B4965", "#2C7DA0", "#61A5C2", "#A9D6E5")
colores5 <- c("#1B4965", "#2C7DA0", "#61A5C2", "#A9D6E5", "#CAE9FF")
colores6 <- c("#1B4965", "#2C7DA0", "#61A5C2", "#89C2D9","#A9D6E5", "#CAE9FF")
colores20 <- c("#264653", "#287271", "#2A9D8F", "#52B69A", "#76C893", "#99D98C", "#B5E48C", "#E9C46A", "#F4A261", "#E76F51", "#D1495B", "#A53860", "#774C60", "#6D597A", "#765D9B", "#8E7DBE", "#577590", "#457B9D", "#1D6996", "#4C9FAD")

colores_arbol <- c("#F08080", "#F77F00", "#2A9D8F", "#277DA1")

  #Año fundacion
  plot_barSimple(df_empresas$Año_Fundacion, colores6, "Año de fundación de start-up", "Año", "...")
  
  table(df_empresas$Año_Fundacion)
 
   #CA
  plot_mapa(df_empresas$CA, "Nº Empresas por comunidad autónoma", "Nº\nEmpresas")
  
  table(df_empresas$CA)

  #Industria
  plot_barSimple(df_empresas$Industria_Secundaria, colores20, "Industria secundaria (Primaria es Software)", "Industria", "Industria")

  table(df_empresas$Industria_Secundaria)
  
  #Empleados
  datos <- data.frame(df_empresas$Empleados)
  plot_2D (datos[[1]], "Cantidad de empleados en empresas", "Nº empleados")
  
  datos <- datos %>%
    filter(df_empresas.Empleados <= 150)
  
  plot_2D (datos[[1]], "Cantidad de empleados en empresas\n(Filtrado Nº empleados < 150)", "Nº empleados")
  estadisticos_cuantitativo(df_empresas$Empleados)
  

  #Madurez_empresarial
  plot_barSimple(df_empresas$Madurez_Empresarial, colores3, "Madurez empresarial", "Madurez empresarial", "Estados de\nMadurez")
  
  table(df_empresas$Madurez_Empresarial)
  
  #Madurez_financiera
  plot_barSimple(df_empresas$Madurez_Financiera, colores4, "Madurez financiera", "Madurez financiera", "Estados de\nMadurez")
  
  table(df_empresas$Madurez_Financiera)
  
  #Es_Acquired
  plot_pie(df_empresas$Fue_Adquirido, "Adquirida", colores2, "Adquisición de empresas") 
  
  table(df_empresas$Fue_Adquirido)
  
  #Es_Spinout
  plot_pie(df_empresas$Es_Spinout, "Spin-out", colores2, "Empresas spin-out") 
  
  table(df_empresas$Es_Spinout)
  
  
   #Valoracion
  datos <- data.frame(df_empresas$Valoracion)
  plot_2D (datos[[1]], "Valoración de empresas", "Valoración (€)")
  
  datos <- datos %>%
    filter(df_empresas.Valoracion <= 50000000)
  
  plot_2D (datos[[1]], "Valoración de empresas\n(Filtrado Valoración < 50M €)", "Valoración (€)")
  
  estadisticos_cuantitativo(df_empresas$Valoracion)
  
  
  
  #Ingresos
  datos <- data.frame(df_empresas$Ingresos)
  plot_2D (datos[[1]], "Ingresos de empresas", "Ingresos (€)")
  
  datos <- datos %>%
    filter(df_empresas.Ingresos <= 8000000)
  
  plot_2D (datos[[1]], "Ingresos de empresas\n(Filtrado Ingresos < 8M €)", "Ingresos (€)")
  
  estadisticos_cuantitativo(df_empresas$Ingresos)
  
  # Financiacion
  datos <- data.frame(df_empresas$Financiacion)
  plot_2D (datos[[1]], "Financiación de empresas en equity", "Financiación total (€)")
  
  datos <- datos %>%
    filter(df_empresas.Financiacion <= 1000000)
  
  plot_2D (datos[[1]], "Financiación de empresas en equity\n(Filtrado Financiación < 1M €)", "Financiación total (€)")
  
  estadisticos_cuantitativo(df_empresas$Financiacion)  
  
  
  
  # Num Rondas
  plot_2D (df_empresas$Rondas_De_Financiacion, "Cantidad de rondas de financiación en empresas", "Nº de rondas")
  
  estadisticos_cuantitativo(df_empresas$Rondas_De_Financiacion)  
  
  # Fundador_En_Serie
  plot_pie(df_empresas$Fundador_En_Serie, "En Serie", colores2, "Al menos un fundador de la empresa fundó anteriormente") 
  
  table(df_empresas$Fundador_En_Serie)
  
  # Fundador_Estudios
  plot_barSimple(df_empresas$Fundador_Estudios, colores4, "Estudios combinados de los fundadores", "Estudios", "Estudios")
  
  table(df_empresas$Fundador_Estudios)
  
  # Fundador edad
  plot_barSimple(df_empresas$Fundador_Edad, colores3, "Rangos de edad del fundador más mayor en empresas", "Rangos de edad", "Rangos\nde edad")
  
  table(df_empresas$Fundador_Edad)
  

# Hipotesis.


# H1: Mayor madurez financiera y empresarial es mas financiacion:

  #Kruskal/ANOVA por cada par
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Madurez_Financiera)
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Madurez_Empresarial)

  #box plot
  plot_box(df_empresas$Madurez_Financiera, df_empresas$Financiacion, 
           "Boxplot: Financiacion vs Madurez Financiera", "Madurez financiera", "Financiación total (€)", colores4, TRUE)
    
  plot_box(df_empresas$Madurez_Empresarial, df_empresas$Financiacion, 
           "Boxplot: Financiacion vs Madurez Empresarial", "Madurez empresarial", "Financiación total (€)", colores3, TRUE)

  
  # MCA
  datos <- data.frame(df_empresas$Madurez_Empresarial, df_empresas$Madurez_Financiera)
  names(datos) <- c("Madurez empresarial", "Madurez financiera")
  realizar_MCA(datos, "Relación entre madurez empresarial y financiera")
  
# H2: Una mayor valoracion economica equivale mas financiacion.

datos <- data.frame(df_empresas$Valoracion, df_empresas$Financiacion) 

  # Pearson
  realizar_correlation_test(df_empresas$Valoracion, df_empresas$Financiacion, "pearson")
  
  names(datos) <- c("Valoración", "Financiación")
  realizar_correlation_matrix(datos, "pearson")

  # KMeans
  cantidad_clusters_Kmeans(datos)
  
  nombres <- as.character(df_empresas$Madurez_Financiera)
  nombres <- paste0(
    nombres,
    ave(nombres, nombres, FUN = seq_along)
  )
  realizar_Kmeans(datos, nombres, 2, "Clústering de empresas según financiación total (€) y valoración (€) | k = 2") 
  
  # Kmeans quitando la primera empresa que sesga la muestra
  
  etiquetas <- as.character(df_empresas$Madurez_Financiera)
  
  
  for (i in 1:3) {
    datos <- datos[-1, ]
    nombres <- nombres[-1]
    etiquetas <- etiquetas[-1]
  }
  


  
  
  set.seed(2) # fijar la seed para que el kmeans dé el mismo resultado y no cambie el mappeo de clusters
  clusters = realizar_Kmeans(datos, nombres, 3, "Clústering de empresas según financiación total (€) y valoración (€) | k = 3\n(Filtrando Serie C y D)")
  
  
  # heatmap
  plot_heatmapNominal(clusters, etiquetas, rev(colores5), "Correlación entre clústers y madurez financiera", "Cluster", "Madurez financiera")
    
  
  tabla <- table(
    Atributo = etiquetas,
    Cluster = clusters
  )
  tabla
  
  
# H3: Existe una relación positiva entre los ingresos y la valoración económica:
  
datos <- data.frame(df_empresas$Valoracion, df_empresas$Ingresos) 

  # Pearson
  realizar_correlation_test(df_empresas$Valoracion, df_empresas$Ingresos, "pearson")
  
  names(datos) <- c("Valoración", "Ingresos")
  realizar_correlation_matrix(datos, "pearson")

  # Scatterplot
  plot_scatterNumerico(datos[[1]], datos[[2]], "Correlación entre valoración e ingresos", "Valoración (€)", "Ingresos (€)", TRUE)

  

  
  
  

# H4: Las empresas con mayor madurez empresarial y/o financiera presentan una mayor valoración económica.
  # (Si la documentacion es muy larga, quitar esta hipotesis pq es la mas redundante)
  
  # Kruskal/ANOVA por cada par
  analisis_comparativo(df_empresas$Valoracion, df_empresas$Madurez_Financiera)
  analisis_comparativo(df_empresas$Valoracion, df_empresas$Madurez_Empresarial)

  #box plot
  plot_box(df_empresas$Madurez_Financiera, df_empresas$Valoracion, 
           "Valoracion económica vs. Madurez financiera", "Madurez financiera", "Valoracion (€)", colores4, TRUE)
  
  plot_box(df_empresas$Madurez_Empresarial, df_empresas$Valoracion, 
           "Valoracion económica vs. Madurez empresarial", "Madurez empresarial", "Valoracion (€)", colores3, TRUE)

  
  
  
# H5: Características empresariales (empleados, spinout, acquired) afecta a la madurez financiera 

datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Empleados, df_empresas$Es_Spinout, df_empresas$Fue_Adquirido) 


  # regresion lineal
  realizar_regresion_lineal(datos, TRUE)
  realizar_regresion_lineal(datos, FALSE)
  
  # scatterplot + heatmap
  plot_scatterNominal(datos[[1]], datos[[2]], colores4, "Correlación entre madurez financiera y número de empleados", "Madurez financiera", "Nº empleados", TRUE)
  plot_heatmapNominal(datos[[1]], datos[[3]], rev(colores5), "Correlación entre madurez financiera y ser spin-out", "Madurez financiera", "Es spin-out") 
  plot_heatmapNominal(datos[[1]], datos[[4]], rev(colores5), "Correlación entre madurez financiera y ser adquirido", "Madurez financiera", "Fue adquirido") # tendencia a ser adquirida sube por tier, excepto para Serie_C_D
  
  
 # plot_barGroup(datos[[3]], datos[[1]], colores4, "Ser spin-out o no agrupado por madurez financiera", "Es spin-out", "Madurez\nfinanciera")
#  plot_barGroup(datos[[4]], datos[[1]], colores4, "Ser adquirido o no agrupado por madurez financiera", "Fue adquirido", "Madurez\nfinanciera")
  


  # decision tree
  names(datos) <- c("Madurez_financiera", "N.empleados", "Spinout", "Adquirida")
  realizar_decision_tree(datos, "Arbol de clasificación de madurez financiera según Nº empleados,\nser spin-out y ser adquirido", as.list(colores_arbol))


  
  
  
  
# H6: Las empresas más antiguas poseen más rondas de financiación y una mayor financiacion

datos <- data.frame(df_empresas$Financiacion, df_empresas$Rondas_De_Financiacion, df_empresas$Año_Fundacion) 

  # regresion lineal
  realizar_regresion_lineal(datos, TRUE)
  realizar_regresion_lineal(datos, FALSE)

  # scatterplot
  plot_scatterNumerico(datos[[1]], datos[[2]], "Correlación entre financiación y número de rondas de financiación", "Financiación total (€)", "Nº rondas", TRUE) # Correlacion
  plot_scatterNumerico(datos[[1]], datos[[3]], "Correlación entre financiación y año de fundación", "Financiación total (€)", "Año de fundación", TRUE) # No correlacion
  

  # Pearson
  realizar_correlation_test(datos[[1]], datos[[2]], "pearson")
  realizar_correlation_test(datos[[1]], datos[[3]], "pearson")
  
  names(datos) <- c("Financiacion", "Nº rondas", "Año fundación")
  realizar_correlation_matrix(datos, "pearson")




  
  
  
# H7: La formación tecnológica, fundador en serie y edad de los fundadores está asociada con la financiacion

#  datos <- data.frame(df_empresas$Financiacion, df_empresas$Fundador_En_Serie, df_empresas$Fundador_Edad, df_empresas$Fundador_Estudios, df_empresas$Fundador_IT_o_Tech) 

# datos
  # regresion lineal
  
#  realizar_regresion_lineal(datos, TRUE)
#  realizar_regresion_lineal(datos, FALSE)

  #Kruskal/ANOVA/t-test.. por cada par
  
#  analisis_comparativo(datos[[1]], datos[[2]])
  #  analisis_comparativo(datos[[1]], datos[[3]])
  #  analisis_comparativo(datos[[1]], datos[[4]]) #
  
  #  analisis_comparativo(datos[[1]], datos[[5]]) #

  # Quitar outliers en plots
  # plot_box(datos[[2]], datos[[1]], 
  #          "Boxplot: Valoracion vs Financiación", "x", "y", colores2, TRUE)
#  plot_box(datos[[3]], datos[[1]], 
  #           "Boxplot: Valoracion vs Financiación", "x", "y", colores3, TRUE)
  #  plot_box(datos[[4]], datos[[1]], 
  #           "Boxplot: Valoracion vs Financiación", "x", "y", colores3, TRUE)
  #  plot_box(datos[[5]], datos[[1]], 
  #          "Boxplot: Valoracion vs Financiación", "x", "y", colores2, TRUE)

  #  datos <- datos %>%
  #    filter(df_empresas.Financiacion <= 200000000)
  
  #  analisis_comparativo(datos[[1]], datos[[2]])
  #analisis_comparativo(datos[[1]], datos[[3]])
  #analisis_comparativo(datos[[1]], datos[[4]]) #
  
  # analisis_comparativo(datos[[1]], datos[[5]]) #
  
  
  
  #plot_box(datos[[2]], datos[[1]], 
  #         "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, FALSE)
  #plot_box(datos[[3]], datos[[1]], 
  #         "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores3, FALSE)
  #plot_box(datos[[4]], datos[[1]], 
  #         "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores3, FALSE)
  #plot_box(datos[[5]], datos[[1]], 
  #         "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, FALSE)
  
  
  # H7: La formación tecnológica, fundador en serie y edad de los fundadores está asociada con la madurez financiacion
  
  
  datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Fundador_En_Serie, df_empresas$Fundador_Edad, df_empresas$Fundador_Estudios) 

  names(datos) <- c("Madurez financiera", "Fundador en serie", "Edad de fundadores", "Estudios de fundadores")
 
  # chi cuadrada
  realizar_chicuadrada(datos[1],datos[2]) #
  realizar_chicuadrada(datos[1],datos[3])
  realizar_chicuadrada(datos[1],datos[4])

  # heatmap
  plot_heatmapNominal(datos[[1]], datos[[2]], rev(colores5), "Correlación entre madurez financiera y fundador en serie", "Madurez financiera", "Fundador en serie") 
  
  
  # MCA
  datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Fundador_En_Serie, df_empresas$Fundador_Estudios, df_empresas$Fundador_Edad) 
  names(datos) <- c("Madurez financiera", "Fundadores en serie", "Estudios de fundadores", "Edad de fundadores")
  
  realizar_MCA(datos, "Relación entre madurez financiera y características de fundadores")
  
  
  datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Fundador_En_Serie) 
  names(datos) <- c("Madurez financiera", "Fundadores en serie")
  
  realizar_MCA(datos, "Relación entre madurez financiera y fundadores en serie")


  
  


  
  
# H8: Las empresas adquiridas presentan diferencias en cantidad de empleados respecto a las no adquiridas


  datos <- data.frame(df_empresas$Empleados, df_empresas$Fue_Adquirido) 
  
  
  # t-test/whitney
  analisis_comparativo(datos[[1]], datos[[2]])

  # box plot
  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Cantidad empleados vs. Fue adquirido", "Fue adquirido", "Nº empleados", colores2, FALSE)
  
  datos <- datos %>%
    filter(df_empresas.Empleados <= 2000)
  
  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Cantidad empleados vs. Fue adquirido\n(Filtrado por Nº empleados < 2000)", "Fue adquirido", "Nº empleados", colores2, TRUE)  
  
  
  
# H9: Ciertas CA estan asociadas con una mayor probabilidad de adquisición.
  

  datos <- data.frame(df_empresas$Fue_Adquirido, df_empresas$CA) 
  names(datos) <- c("Fue adquirido", "Comunidad autónoma")
  
  #chi cuadrada
  realizar_chicuadrada(datos[1],datos[2])
  
  # mapa geografico
  proporciones <- plot_heatmapNominal(df_empresas$CA, df_empresas$Fue_Adquirido, rev(colores5), "Correlación entre madurez empresarial y ser adquirido", "Madurez empresarial", "Fue adquirido") 
  proporciones <- proporciones[10:18, ]

  plot_mapa_proporciones(proporciones$col1, proporciones$porcentaje, "Proporción de start-ups adquiridas por comunidad autónoma", "Porcentaje\nadquiridas")
  plot_pie(df_empresas$Fue_Adquirido, "Adquirida", colores2, "Adquisición de empresas") 
  
  
  
# H10: Los sectores de actividad y localidades están correlacionados

datos <- data.frame(df_empresas$Industria_Secundaria, df_empresas$CA) 
names(datos) <- c("Industria", "Comunidad autónoma")


  #chi cuadrada
  realizar_chicuadrada(datos[1],datos[2])
  
  # heatmap
  plot_heatmapNominal(datos[[1]], datos[[2]], rev(colores5), "Correlación entre subindustria y comunidad autónoma", "Subindustria", "Comunidad autónoma") 
  

  
  
  
  
  

# H11: Existen diferencias en la financiación entre comunidades autónomas o sectores.

datos <- data.frame(df_empresas$Financiacion, df_empresas$CA, df_empresas$Industria_Secundaria) 

  


  # KURKAL / ANOVA
  analisis_comparativo(df_empresas$Financiacion, df_empresas$CA)
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Industria_Secundaria)

  # regresion lineal
  
  # realizar_regresion_lineal(datos, TRUE)
  #  realizar_regresion_lineal(datos, FALSE) 


















num_estudio = 2

switch(num_estudio-1,
       
       {test()}, # 0
       {test()}, # 1
       {test()}, # 2
       {test()}, # 3
       {test()}, # 4
       {test()}, # 5
       {test()}, # 6
       {test()}, # 7
       
       stop("No reconocido")
       
    
)



test <- function() {
  print("HOLA MUBNDI")
}




