source("r_scripts/plots.R")
source("r_scripts/models.R")
source("r_scripts/data_importer.R")

# importar dataset

df_empresas <- importar_df_empresas()
view(df_empresas)



# Overview de los datos

labelX = "label x"
labelY = "label y"

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
 
   #CCAA
  plot_mapa(df_empresas$CCAA, "Empresas por comunidad autónoma", "Nº Empresas")
  
  #Industria
  plot_barSimple(df_empresas$Industria, colores20, "Año fundación", "Año", "Año")
  
  
  #Empleados
  datos <- data.frame(df_empresas$Empleados)
  plot_2D (datos[[1]], "Cantidad de empleados en empresas", "Número de empleados")
  
  datos <- datos %>%
    filter(df_empresas.Empleados <= 150)
  
  plot_2D (datos[[1]], "Cantidad de empleados en empresas", "Número de empleados")
  estadisticos_cuantitativo(df_empresas$Empleados)
  

  #Madurez_empresarial
  plot_barSimple(df_empresas$Madurez_Empresarial, colores3, "Madurez empresarial", labelX, "Estados de\nMadurez")
  
  #Madurez_financiera
  plot_barSimple(df_empresas$Madurez_Financiera, colores4, "Madurez financiera", labelX, "Estados de\nMadurez")
  
  #Es_Acquired
  plot_pie(df_empresas$Es_Acquired, "Adquirida", colores2, "¿Fue adquirida?") 
  
  #Es_Spinout
  plot_pie(df_empresas$Es_Spinout, "Spin-out", colores2, "¿Es un spin-out?") 
  
 
  
   #Valoracion
  datos <- data.frame(df_empresas$Valoracion)
  plot_2D (datos[[1]], "Valoración de empresas", "Valoración en €")
  
  datos <- datos %>%
    filter(df_empresas.Valoracion <= 50000000)
  
  plot_2D (datos[[1]], "Valoración de empresas", "Valoración en €")
  estadisticos_cuantitativo(df_empresas$Valoracion)
  
  
  
  #Ingresos
  datos <- data.frame(df_empresas$Ingresos)
  plot_2D (datos[[1]], "Ingresos de empresas", "Ingresos en €")
  
  datos <- datos %>%
    filter(df_empresas.Ingresos <= 8000000)
  
  plot_2D (datos[[1]], "Ingresos de empresas", "Ingresos en €")
  estadisticos_cuantitativo(df_empresas$Ingresos)
  
  # Financiacion
  datos <- data.frame(df_empresas$Financiacion)
  plot_2D (datos[[1]], "Financiación de empresas", "Financiación en €")
  
  datos <- datos %>%
    filter(df_empresas.Financiacion <= 1000000)
  
  plot_2D (datos[[1]], "Financiación de empresas", "Financiación en €")
  estadisticos_cuantitativo(df_empresas$Financiacion)  
  
  
  
  # Num Rondas
  plot_2D (df_empresas$Rondas_De_Financiacion, "Cantidad de rondas de financiación en empresas", "Nº de rondas")
  
  # Fundador_En_Serie
  plot_pie(df_empresas$Fundador_En_Serie, "En Serie", colores2, "¿Al menos un fundador de la empresa fundó anteriormente?") 
  
  # Fundador_Estudios
  plot_barSimple(df_empresas$Fundador_Estudios, colores4, "Estudios combinados de los fundadores", "Estudios", "Estudios")
  
  # Fundador edad
  plot_barSimple(df_empresas$Fundador_Edad, colores3, "Edad del fundador más mayor", "Rangos de edad", "Rangos\nde edad")
  
  

# Hipotesis.


# H1: Mayor madurez financiera y empresarial es mas financiacion:

  #Kruskal/ANOVA por cada par
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Madurez_Financiera)
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Madurez_Empresarial)

  plot_box(df_empresas$Madurez_Financiera, df_empresas$Financiacion, 
           "Boxplot: Financiacion vs Madurez Financiera", "x", "y", colores4, TRUE)
    
  plot_box(df_empresas$Madurez_Empresarial, df_empresas$Financiacion, 
           "Boxplot: Financiacion vs Madurez Empresarial", "x", "y", colores3, TRUE)

  
  
  
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
  nombres
  realizar_Kmeans(datos, nombres, 2) 
  
  # Kmeans quitando la primera empresa que sesga la muestra
  
  etiquetas <- as.character(df_empresas$Madurez_Financiera)
  
  
  for (i in 1:3) {
    datos <- datos[-1, ]
    nombres <- nombres[-1]
    etiquetas <- etiquetas[-1]
  }
  


  
  

  clusters = realizar_Kmeans(datos, nombres, 3)
  
  plot_heatmapNominal(clusters, etiquetas, rev(colores5), "titulo", "labelX", "labelY")
    
  
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


  plot_scatterNumerico(datos[[1]], datos[[2]], "Correlación entre valoración e ingresos", "Valoración (€)", "Ingresos (€)", TRUE)

  

  
  
  

# H4: Las empresas con mayor madurez empresarial y/o financiera presentan una mayor valoración económica. 
  
  # Kruskal/ANOVA por cada par
  analisis_comparativo(df_empresas$Valoracion, df_empresas$Madurez_Financiera)
  analisis_comparativo(df_empresas$Valoracion, df_empresas$Madurez_Empresarial)

  plot_box(df_empresas$Madurez_Financiera, df_empresas$Valoracion, 
           "Valoracion económica vs. Madurez financiera", "Madurez financiera", "Valoracion (€)", colores4, TRUE)
  
  plot_box(df_empresas$Madurez_Empresarial, df_empresas$Valoracion, 
           "Valoracion económica vs. Madurez empresarial", "Madurez empresarial", "Valoracion (€)", colores3, TRUE)

  
  
  
# H5: Características empresariales (empleados, spinout, acquired) afecta a la madurez financiera 

datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Empleados, df_empresas$Es_Spinout, df_empresas$Es_Acquired) 


  # regresion lineal
  realizar_regresion_lineal(datos, TRUE)
  realizar_regresion_lineal(datos, FALSE)
  
  
  plot_scatterNominal(datos[[1]], datos[[2]], colores4, "Correlación entre madurez financiera y número de empleados", "Madurez financiera", "Nº empleados", TRUE)
  plot_heatmapNominal(datos[[1]], datos[[3]], rev(colores5), "Correlación entre madurez financiera y ser spin-out", "Madurez financiera", "Es spin-out") 
  plot_heatmapNominal(datos[[1]], datos[[4]], rev(colores5), "Correlación entre madurez financiera y ser adquirido", "Madurez financiera", "Fue adquirido") # tendencia a ser adquirida sube por tier, excepto para Serie_C_D
  
  
  plot_barGroup(datos[[3]], datos[[1]], colores4, "Ser spin-out o no agrupado por madurez financiera", "Es spin-out", "Madurez\nfinanciera")
  plot_barGroup(datos[[4]], datos[[1]], colores4, "Ser adquirido o no agrupado por madurez financiera", "Fue adquirido", "Madurez\nfinanciera")
  


  # decision tree
  names(datos) <- c("Madurez_financiera", "N.empleados", "Spinout", "Adquirida")
  realizar_decision_tree(datos, "Arbol de clasificación para madurez financiera", as.list(colores_arbol))


  
  
  
  
# H6: Las empresas más antiguas poseen más rondas de financiación y una mayor financiacion

datos <- data.frame(df_empresas$Financiacion, df_empresas$Rondas_De_Financiacion, df_empresas$Año_Fundacion) 

  # regresion lineal
  realizar_regresion_lineal(datos, TRUE)
  realizar_regresion_lineal(datos, FALSE)

  #plot_scatterNominal(datos[[1]], datos[[4]], colores4, "titulo", "labelX", "labelY", TRUE)
  
  plot_scatterNumerico(datos[[1]], datos[[2]], "titulo", "labelX", "labelY", TRUE) # Correlacion
  plot_scatterNumerico(datos[[1]], datos[[3]], "titulo", "labelX", "labelY", TRUE) # No correlacion
  

  # Pearson
  realizar_correlation_test(datos[[1]], datos[[2]], "pearson")
  realizar_correlation_test(datos[[1]], datos[[3]], "pearson")
  
  names(datos) <- c("Financiacion", "N.rondas", "Año_fundación")
  realizar_correlation_matrix(datos, "pearson")




  
  
  
# H7: La formación tecnológica, fundador en serie y edad de los fundadores está asociada con la financiacion

  datos <- data.frame(df_empresas$Financiacion, df_empresas$Fundador_En_Serie, df_empresas$Fundador_Edad, df_empresas$Fundador_Estudios, df_empresas$Fundador_IT_o_Tech) 
  
  # regresion lineal
#  realizar_regresion_lineal(datos, TRUE)
#  realizar_regresion_lineal(datos, FALSE)

  #Kruskal/ANOVA/t-test.. por cada par
  analisis_comparativo(datos[[1]], datos[[2]])
  analisis_comparativo(datos[[1]], datos[[3]])
  analisis_comparativo(datos[[1]], datos[[4]]) #
  
  analisis_comparativo(datos[[1]], datos[[5]]) #

  # Quitar outliers en plots
  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Valoracion vs Financiación", "x", "y", colores2, TRUE)
  plot_box(datos[[3]], datos[[1]], 
           "Boxplot: Valoracion vs Financiación", "x", "y", colores3, TRUE)
  plot_box(datos[[4]], datos[[1]], 
           "Boxplot: Valoracion vs Financiación", "x", "y", colores3, TRUE)
  plot_box(datos[[5]], datos[[1]], 
           "Boxplot: Valoracion vs Financiación", "x", "y", colores2, TRUE)

  datos <- datos %>%
    filter(df_empresas.Financiacion <= 200000000)
  
  analisis_comparativo(datos[[1]], datos[[2]])
  analisis_comparativo(datos[[1]], datos[[3]])
  analisis_comparativo(datos[[1]], datos[[4]]) #
  
  analisis_comparativo(datos[[1]], datos[[5]]) #
  
  
  
  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, FALSE)
  plot_box(datos[[3]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores3, FALSE)
  plot_box(datos[[4]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores3, FALSE)
  plot_box(datos[[5]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, FALSE)
  
  
  
  
  datos <- data.frame(df_empresas$Madurez_Financiera, df_empresas$Fundador_En_Serie, df_empresas$Fundador_Edad, df_empresas$Fundador_Estudios, df_empresas$Fundador_IT_o_Tech) 
  
  realizar_chicuadrada(datos[[1]],datos[[2]])
  realizar_chicuadrada(datos[[1]],datos[[3]])
  realizar_chicuadrada(datos[[1]],datos[[4]])
  realizar_chicuadrada(datos[[1]],datos[[5]])

  
  # MCA
  datos <- data.frame(df_empresas$Fundador_En_Serie, df_empresas$Fundador_Estudios, df_empresas$Fundador_Edad) 
  realizar_MCA(datos)



  
  
  

# H8: La madurez financiera y/o empresarial está asociada con una mayor probabilidad de adquisición.

  #chi cuadrada
  realizar_chicuadrada(df_empresas$Madurez_Empresarial, df_empresas$Es_Acquired)
  plot_heatmapNominal(df_empresas$Madurez_Empresarial, df_empresas$Es_Acquired, rev(colores5), "titulo", "labelX", "labelY") 
  
  realizar_chicuadrada(df_empresas$Madurez_Financiera, df_empresas$Es_Acquired)
  plot_heatmapNominal(df_empresas$Madurez_Financiera, df_empresas$Es_Acquired, rev(colores5), "titulo", "labelX", "labelY") 
  

  
  
  
  
# H9: Las empresas adquiridas presentan diferencias en cantidad de empleados respecto a las no adquiridas


  datos <- data.frame(df_empresas$Empleados, df_empresas$Es_Acquired) 
  
  
  # t-test/whitney
  analisis_comparativo(datos[[1]], datos[[2]])

  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, TRUE)
  
  datos <- datos %>%
    filter(df_empresas.Empleados <= 2000)
  
  plot_box(datos[[2]], datos[[1]], 
           "Boxplot: Valoracion vs Madurez Financiera", "x", "y", colores2, TRUE)  
  
  
  
# H10: Los sectores de actividad y localidades están correlacionados

datos <- data.frame(df_empresas$Industria, df_empresas$CCAA) 


  #chi cuadrada
  realizar_chicuadrada(df_empresas$Industria, df_empresas$CCAA)


  
  
  
  

# H11: Existen diferencias en la financiación entre comunidades autónomas o sectores.

datos <- data.frame(df_empresas$Financiacion, df_empresas$CCAA, df_empresas$Industria) 

  # regresion lineal


  # KURKAL / ANOVA
  analisis_comparativo(df_empresas$Financiacion, df_empresas$CCAA)
  analisis_comparativo(df_empresas$Financiacion, df_empresas$Industria)

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




