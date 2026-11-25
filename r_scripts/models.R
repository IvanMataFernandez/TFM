library("ggplot2")
library("dplyr")
library("tidyverse")
library("rpart")
library("rattle")
library("rpart.plot")
library("ggcorrplot")
library("factoextra")
library("FSA") 
library("dunn.test") 
#library("mapSpain")
library("FactoMineR")  
library("ggrepel")
library("ordinal")


#source("plots.R")
source("r_scripts/Data_importer.R")

##################################################################################
############################## FUNCTIONS #########################################
##################################################################################


estadisticos_cuantitativo <- function (columna) {
  estadisticos <- c(
    Media = mean(columna),
    Moda = if (length(columna) == 0 || all(is.na(columna))) NA else {
      frecuencias <- table(columna)
      as.numeric(names(frecuencias)[frecuencias == max(frecuencias)])
    },

    Desviacion_estandar = sd(columna),
    Varianza = var(columna),
    Minimo = min(columna),
    Maximo = max(columna),
    Q1 = unname(quantile(columna, 0.25)),
    Mediana = median(columna),
    Q3 = unname(quantile(columna, 0.75)),
    IQR = IQR(columna)
  )
  
  print(estadisticos)
}


realizar_regresion_lineal <- function (matriz, convertir_ordinal_a_numeros) {

  dataset <- matriz 
  
  # Restructurar la formula de la funcion de regresion lineal para fittearla en la funcion creada
  formula_modelo <- reformulate(
    termlabels = names(dataset)[-1],
    response = names(dataset)[1]
  )  
  
  # Convertir variables independientes explicativas
  # ordinales categoricas a numericas para que fitteen en una regresion lineal.
  # No modifica las categoricas nominales
  
  # Este paso solo se realiza si convertir_ordinal_a_numeros = TRUE, idealmente utilizable
  # si se consideran que de una tier ordinal a la siguiente siempre hay el mismo peso para
  # mostrar en un scatterplot despues
  
  if (convertir_ordinal_a_numeros) {
    
    dataset[-1] <- lapply(dataset[-1], function(col) {
      print(is.ordered(col))
      if ((is.character(col) || is.factor(col)) && is.ordered(col)) {
        as.numeric(as.factor(col))
      } else {
        col
      }
    })    
  }
  

  # Verificar si la variable Y es categorica o numerica para usar la funcion de regresion lineal correspondiente
  if (is.numeric(dataset[[1]])) {
    modelo <- lm(formula_modelo, data = dataset)
    
  } else {
    modelo <- clm(formula_modelo, data = dataset)
    
  }
  
  summary(modelo)
}

realizar_decision_tree <- function (matriz, titulo, colores) {

  dataset <- matriz 
  
  # Restructurar la formula de la funcion de decision tree para fittearla en la funcion creada
  formula_modelo <- reformulate(
    termlabels = names(dataset)[-1],
    response = names(dataset)[1]
  )  
   

  if (is.numeric(dataset[[1]])) {
    tree <- rpart(formula_modelo, data = dataset, method = "anova", control = rpart.control(
      minsplit = 2,
      minbucket = 1,
      cp = 0,
      maxdepth = 3,
      xval = 10
    ))
    
    # Representar árbol de regresión
    rpart.plot::rpart.plot(
      tree,
      type = 5,
      extra = 101,
      fallen.leaves = TRUE,
      main = titulo,
      box.palette = colores,
      branch.col = "#555555"
    )    
    
    
  } else {

    tree <- rpart(formula_modelo, data = dataset, method = "class", control = rpart.control(
      minsplit = 2,
      minbucket = 1,
      cp = 0,
      maxdepth = 3,
      xval = 10
    ))
    
    
    rpart.plot::rpart.plot(
      tree,
      type = 5,
      extra = 104,
      fallen.leaves = TRUE,
      main = titulo,
      box.palette = colores,
      branch.col = "#555555"
      
    )
    
  }

  

  
   
}



realizar_correlation_matrix <- function (matriz_numerica, tipo) {
  
  
  correlation <- cor(matriz_numerica, method = tipo) # tipo = "pearson" or "spearman"
  
  # en heatmap
  print(ggcorrplot(correlation, lab = TRUE, lab_size = 8, digits = 5, type = "full", title = paste0("Matriz de correlaciones de ",tools::toTitleCase(tipo))) 
        +
          theme(
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            legend.position = "none",
            correlation  # en texto
          ))
  
}

realizar_correlation_test <- function (col_numerica1, col_numerica2, tipo) {
  

  correlation <- cor.test(col_numerica1, col_numerica2, method = tipo, exact = FALSE)  # tipo = "pearson" or "spearman"
  print(correlation)
  
}




realizar_chicuadrada <- function (col_categorica1, col_categorica2) {
  
  nombre_col1 <- names(col_categorica1)
  nombre_col2 <- names(col_categorica2)
  
  muestra <- table(col_categorica1[[1]], col_categorica2[[1]])
  test <- chisq.test(muestra) # returnea p-value. Si p<0.05, considerar construir un heatmap para ver tendencias
  print(test)
  

  
  datos_heatmap <- as.data.frame(muestra)
  
  datos_heatmap$Residuo <- as.vector(
    test$stdres
  )
  

  
  grafico <- ggplot(datos_heatmap,
         aes(x = Var2,
             y = Var1,
             fill = Residuo)) +
    
    geom_tile(color = "white", linewidth = 1) +
    
    geom_text(
      aes(label = round(Residuo, 2)),
      size = 5
    ) +
    
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0
    ) +
    
    labs(
      title = paste("Asociación entre variables categóricas: \n",nombre_col1,"y", nombre_col2),
      subtitle = paste(
        "Test chi-cuadrado || Estadísticos ->  χ²:",
        round(unname(test$statistic), 3),
        "| p-valor:",
        signif(test$p.value, 3)
      ),
      x = nombre_col2,
      y = nombre_col1,
      fill = "Residuo \nestandarizado \nde Pearson"
    ) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),      
    )
  
  
  print(grafico)
  print(muestra)
}


realizar_shapiro <- function (col_numerica, col_categorica) {
  
  
  cat("Estudio Shapiro: \n \n")
  
  samples <- split(col_numerica, col_categorica)
  p_valor_bajo = FALSE
  
  for (nombre in names(samples)) {
    
    if (length(samples[[nombre]]) > 2) {
      result <- shapiro.test(samples[[nombre]])
      cat("Grupo: ",nombre,"| P-value: ",result$p.value,"   \n")  
      
      if (result$p.value < 0.05) {
        p_valor_bajo = TRUE
      }
      
    } else {
      cat("Grupo: ",nombre,"| P-value: N/A (Menos de tres instancias)   \n")     
      
    }
    

    

  }
  cat("   \n")
  cat("Algun P-value < 0.05: ", p_valor_bajo)
  return (p_valor_bajo)
}


realizar_ANOVA <-function (col_numerica, col_categorica) {
  
  df_data = data.frame(col_numerica, col_categorica)
  
  
  
  anova <- aov(col_numerica ~ col_categorica, data = df_data)
  tukey <- TukeyHSD(anova)
  cat("Anova: \n")
  print(summary(anova))
  cat("\n---------------------------------------------------------------------------------------------\nTukey: \n")
  tukey
  
  
}


realizar_kruskal <- function (col_numerica, col_categorica) {
  
  
  df_data = data.frame(col_numerica, col_categorica)
  
  model <- kruskal.test(col_numerica ~ col_categorica, data = df_data)
  dun_test <- dunnTest(col_numerica, col_categorica, method = "bonferroni")
  cat("Krukal: \n")
  print(model)
  cat("\n---------------------------------------------------------------------------------------------\nDunn-test: \n")
  print(dun_test)
  
  
}

realizar_t_test <- function (col_numerica, col_categorica) {
 
  df_data = data.frame(col_numerica, col_categorica)
  
   
  model <- t.test(col_numerica ~ col_categorica, data = df_data)
  cat("Student's t-test: \n")
  print(model)
  

  
  
}

realizar_wilcox_test <- function (col_numerica, col_categorica) {
  
  muestras <- split(col_numerica, col_categorica)
  
  model <- wilcox.test(muestras[[1]], muestras[[2]], paired = FALSE)
  cat("Mann-Whitney U-test: \n")
  print(model)
  
  
  
  
}


analisis_comparativo <- function (col_numerica, col_categorica) {
  
  cat("Realizando estudio.\n")
  cat("\n-----------------------------------------------------------------------------\n")
  
  
  dist_no_norm <- realizar_shapiro(col_numerica, col_categorica)
  n_muestras <- length(unique(col_categorica))

  if (n_muestras < 2) {
    
    cat("\n No se puede realizar el analisis comparativo con una muestra.\n")
    return()
  
  } else if (n_muestras == 2) {
    
    if (dist_no_norm) {
      cat("\n-----------------------------------------------------------------------------\n")
      cat("\n\n Dos muestras con distribucion no normal -> Realizando Mann-Whitney U test \n")
      cat("-----------------------------------------------------------------------------\n")
      
      realizar_wilcox_test(col_numerica, col_categorica)
      
      
    } else {
      cat("\n-----------------------------------------------------------------------------\n")
      cat("\n\n Dos muestras con distribucion normal -> Realizando Student's t test \n")
      cat("-----------------------------------------------------------------------------\n")
      
      realizar_t_test(col_numerica, col_categorica)
    }
    
  } else {
    
    if (dist_no_norm) {
      cat("\n-----------------------------------------------------------------------------\n")
      cat("\n\n Tres o más muestras con distribucion no normal -> Realizando Kruskal-Wallis test \n")
      cat("-----------------------------------------------------------------------------\n")
      
      realizar_kruskal(col_numerica, col_categorica)      
      
    } else {
      cat("\n-----------------------------------------------------------------------------\n")
      cat("\n\n Tres o más muestras con distribucion normal -> Realizando ANOVA \n")
      cat("-----------------------------------------------------------------------------\n")
      
      realizar_ANOVA(col_numerica, col_categorica)
    } 
  }
  
}


realizar_MCA <- function (df_mca_data, titulo) {

  
  mca_result <- MCA(df_mca_data, graph = FALSE) # quali.sup = c(1,2,3,4,5) <- que variables se quieren mostrar despues tmb pero no son usadas en el calculo
  
  # Extraer coordenadas de las categorías
  categorias <- as.data.frame(mca_result$var$coord)
  # Añadir nombres de las categorías
  categorias$Categorias <- rownames(categorias)

  
  mapping <- do.call(
    rbind,
    lapply(names(df_mca_data), function(col) {
      
      data.frame(
        categoria = unique(as.character(df_mca_data[[col]])),
        atributo = col
      )
      
    })
  )
  
  rownames(mapping) <- NULL
  
  # Identificar el atributo al que pertenece cada categoría
  
  categorias$Atributo <- mapping$atributo[
    match(categorias$Categoria, mapping$categoria)
  ]
  
  categorias
  mapping
  
  # Crear gráfico
  grafico <- ggplot(categorias, aes(
    x = `Dim 1`,
    y = `Dim 2`,
    color = Atributo,
    label = Categorias
  )) +
    geom_point(size = 4) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
    geom_text_repel(size = 5) +
    theme_minimal() +
    labs(
      title = titulo,
      x = "Dimensión 1",
      y = "Dimensión 2",
      color = "Atributo"
    ) +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_blank(), axis.title = element_blank(), legend.text = element_text(size = 14), legend.title = element_text(size = 16))
      
    
  
  print(grafico)
  print(fviz_contrib(mca_result, choice = "var", axes = 1, top = 99, title = "Contribución de atributos en dimensión 1") + labs(y = "Contribución (%)") + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 14), axis.title = element_text(size = 16)) )
  print(fviz_contrib(mca_result, choice = "var", axes = 2, top = 99, title = "Contribución de atributos en dimensión 2") + labs(y = "Contribución (%)") + theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5), axis.text = element_text(size = 14), axis.title = element_text(size = 16)) )
  
  
  cat("Peso (%) de cada dimension. Se plottean las dos primeras con mas peso: \n")
  print(mca_result$eig[,2])

  
  
  
}

cantidad_clusters_Kmeans <- function (df_kmeans_data) {
  
  print(fviz_nbclust(df_kmeans_data, kmeans, method = "wss") + labs(susbtitle = "Elbow method")) # ver cantidad de clusters ideal. Cuando la slope deja de bajar, coger ese valor 
}



realizar_Kmeans <- function (df_kmeans_data, nombres, k, titulo) {
  matriz <- df_kmeans_data
  matriz <- scale(matriz) # mappear la info para que se proprocione a escala
  
  clusterResult <- kmeans(matriz, centers = k, nstart = 100) # hacer k-means con k clusters
  clusters <- clusterResult$cluster # obtener los mapeos
  
  # dar nombre a cada instancia mapeada en los clusters
  rownames(matriz) <- paste(nombres) 
    

  
  
  # PCA
  pca <- prcomp(
    matriz,
    center = FALSE,
    scale. = FALSE
  )  

  # Composición de las dimensiones
  cat("Composición de las dimensiones PCA. Contra más valor absoluto de variable, más contribución a dimensión \n")
  print(pca$rotation[, 1:2])
  
  # Varianza explicada
  varianza <- pca$sdev^2
  porcentaje <- varianza / sum(varianza) * 100

  cat("La representación bidimensional converva el ",porcentaje[1] + porcentaje[2], "% de los datos: \n")
  print(porcentaje[1:2])
    
  
  # Gráfico de composición de variables
  print(factoextra::fviz_pca_var(
    pca,
    col.var = "contrib",
    repel = TRUE
  ))  
  
  # visualizar los clusters creados
  print(fviz_cluster(list(data = matriz, cluster = clusters)) + ggtitle(titulo) +
          scale_x_continuous(labels = NULL) +
          scale_y_continuous(labels = NULL) +
          theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none", 
          axis.text = element_blank(),
          axis.ticks = element_line(color = "black"),
          axis.title = element_text(size = 16)
        )) 
 
  

  
  
  
  return(clusters)


 

}

#### UNUSED ################################

realizar_fisher <- function (col_categorica1, col_categorica2) {
  muestra <- table(col_categorica1, col_categorica2)
  test <- fisher.test(muestra)
  print(test)
  
  # Frecuencias esperadas bajo independencia
  esperadas <- outer(
    rowSums(muestra),
    colSums(muestra)
  ) / sum(muestra)
  
  # Residuos de Pearson
  residuos <- (muestra - esperadas) / sqrt(esperadas)
  
  # Preparar datos para heatmap
  datos_heatmap <- as.data.frame(muestra)
  
  datos_heatmap$Residuo <- as.vector(residuos)
  
  grafico <- ggplot(
    datos_heatmap,
    aes(
      x = col_categorica2,
      y = col_categorica1,
      fill = Residuo
    )
  ) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(
      aes(label = round(Residuo, 2)),
      size = 5
    ) +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0
    ) +
    labs(
      title = "Asociación entre variables categóricas",
      subtitle = paste("Test exacto de Fisher | p-value:", 
                       signif(test$p.value, 3)),
      x = "Variable 2",
      y = "Variable 1",
      fill = "Residuo \nde Pearson"
    ) +
    theme_minimal() + 
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      )
    )
  
  print(grafico)
  print(muestra)
  
  
  
}






##################################################################################
###################################### MAIN ######################################
##################################################################################



if (FALSE) {



##### cargar df
df_excel = NULL
df_excel <- importar_df_prueba()

# Crear columna nueva binaria. Oponiones neutrales y negativas son neutrales. Opiniones positivas son positivas
df_excel <- df_excel %>% 
  mutate(Sentiment_New = case_when(
    Sentiment %in% c("Very Negative", "Negative", "Neutral") ~ "Negative",
    Sentiment %in% c("Positive", "Very Positive") ~ "Positive",
    TRUE ~ NA_character_
  ))






##### Ver info de muestra en formato numerico (estadisticos)

estadisticos_cuantitativo(df_excel$CSAT_Score) # mostrar estadisticos
table(df_excel$CSAT_Score) # mostrar frecuencias (cuantitativos) -> Mejor hacerlo en graficos

table(df_excel$Sentiment) # mostrar frecuencias (cualitativos)

head(df_excel) # ver primeras X entradas (10 por default)
view(df_excel) # abrir ventana para ver detalles DF






##### Models

### Regresion lineal correinte y categorica
# Compatibles con categoricas y numericas. Idealmente solo usar numericas o categorica ordinales en Y.
                                                                   

# Coger solo las columnas interesantes (la primera es la variable dependiente (Y), 
# las demas son explicativas numericas y/o ordinales (X))

matriz <- data.frame(df_excel$CSAT_Score, df_excel$Channel, df_excel$Sentiment) 
convertir_ordinal_a_numero <- FALSE

realizar_regresion_lineal(matriz, convertir_ordinal_a_numero)

# Con Y numerica
# Estimate para numericos             -> Y = X * (estimate)
# Estimate para categoricos ordinales -> (ignorar)

# Estimate para categoricos           -> Si se tiene este valor, es [e^(estimate)] veces mas probable que la tier de la variable
# nominales                           -> categorica tambien aumente / disminuya (depende del signo) cuando comparado con el valor
#                                     -> que tendria con el valor referencia que no aparece en el estudio.






# aggregate(CSAT_Score ~ Sentiment,data = df_excel,mean) # ver la media de cada rango



# con Y categorica
# Estimate para numericos    -> Por cada unidad adicional de la variable, es [e^(estimate)] veces mas probabable que la tier de 
#                               la variable categorica tambien aunemente / disminuya (depende del signo).
# Estimate para categoricos 
# ordinales                  -> (ignorar)

# Estimate para categoricos -> Si se tiene este valor, es [e^(estimate)] veces mas probable que la tier de la variable
# nominales                 -> categorica tambien aumente / disminuya (depende del signo) cuando comparado con el valor
#                           -> que tendria con el valor referencia que no aparece en el estudio.


matriz <- data.frame(df_excel$Sentiment, df_excel$Response_Time, df_excel$Channel) 
convertir_ordinal_a_numero <- FALSE

realizar_regresion_lineal(matriz, convertir_ordinal_a_numero)






### Decision Tree
# Vars pueden ser numericas / nominales. (No usar nominales con muchos buckets pq puede tardar horas en computar)


# Coger solo las columnas interesantes (la primera es la variable a modelar, 
# las demas son las explicativas numericas y/o ordinales (X))

matriz <- data.frame(df_excel$Call_Duration, df_excel$CSAT_Score)

matriz <- data.frame(df_excel$Sentiment, df_excel$CSAT_Score)
names(matriz) <- c("Sentiment", "CSAT_Score")
realizar_decision_tree(matriz)








### Comparar correlaciones entre pares de variables

  ## Correlation Matrix
  # Usar correlation matrix para comparar trends entre pares de variables where al menos una de ella es numerica o categorica ordinal.
  # Usar Pearson con relaciones continuas y lineales. Otherwise, Spearman
  
  matriz <- data.frame(df_excel$CSAT_Score, df_excel$Call_Duration) # Coger solo las columnas interesantes (deben ser numericas)

  realizar_correlation_matrix(matriz, "pearson") # pearson / spearman


  # Calcular el p-value de la correlacion entre un par de variables
  
  realizar_correlation_test(df_excel$CSAT_Score, df_excel$Call_Duration, "pearson") # pearson / spearman
  realizar_correlation_test(df_excel$Sentiment, df_excel$Call_Duration, "spearman") # pearson / spearman
  

  ## Chi cuadrada y Fisher
  # Usar Chi cuadrada o Fisher en vez de correlation matrix para comparar trends entre dos variables categoricas nominales (no ordinales)
  # Usar Fisher con muestras pequeñas o cuando existen atributos poco representandos en la muestra, otherwise Chi cuadrada 

  # Chi cuadrada
  realizar_chicuadrada(df_excel$Call_Center, df_excel$Response_Time) # returnea p-value. Si p<0.05, considerar ver el heatmap 
                                                                     # mostrado (valores +- 1.96 interesantes)

  # fisher
  realizar_fisher(df_excel$Call_Center, df_excel$Response_Time)
  

  
  
### Comparar correlaciones:
analisis_comparativo(df_excel$CSAT_Score, df_excel$Sentiment) # implementa los cuatro estudios principales puestos abajo y elige el apropiado
  
  
  
### Comparando 3 o mas listas de numeros independientes por labels: Shapiro test

# Comprobar si las distribuciones de cada label normales con shapiro test.  p-value < 0.05 -> Distribucion no normal
# Si las distribuciones son normales, runnear ANOVA, otherwise runnear Kruskal-Wallis

# Dividir la columna del atributo numerica en listas independientes por cada atributo, para runnear shapiro en cada una
realizar_shapiro(df_excel$CSAT_Score, df_excel$Sentiment)
colores = c("gray", "green" , "blue", "red", "yellow")

 
  ## ANOVA (Asumiendo que Shapiro test RETURNEA FALSE -> para cada label generalmente p > 0.05)
  # ANOVA verifica si alguna muestra proviene de distinta media (p < 0.05).
  
  # anova <- aov(VarX ~ VarY, data = df)
  # tukey <- TukeyHSD(anova)
  
  realizar_ANOVA(df_excel$CSAT_Score, df_excel$Sentiment)

  plot_box(df_excel$Sentiment, df_excel$CSAT_Score, "Boxplot", "Sentiment", "Score", colores, FALSE)  
  
  ## Kruskal-Wallis test (Asumiendo que Shapiro test RETURNEA TRUE -> para cada label generalmente p < 0.05)
  # Kruskal verifica si alguna muestra proviene de distribuciones distintas (p < 0.05)
  
  # model <- kruskal.test(VarX ~ VarY, data = df)
  # dun_test <- dunnTest(df$varX, df$varY, method = "bonferroni")
  
  realizar_kruskal(df_excel$CSAT_Score, df_excel$Sentiment) # Usar el p-adj en dunn-test
  
  plot_box(df_excel$Sentiment, df_excel$CSAT_Score, "Boxplot", "Sentiment", "Score", colores, FALSE)
  
  

### Comparando 2 o mas listas de numeros independientes por labels: Shapiro test

# Comprobar si las distribuciones de cada label son normales con shapiro test.  p-value < 0.05 -> Distribucion no normal
# Si AMBAS distribuciones son normales, runnear t-test, otherwise runnear Mann-Whitney U

# Dividir la columna del atributo numerica en listas independientes por cada atributo, para runnear shapiro en cada una
realizar_shapiro(df_excel$CSAT_Score, df_excel$Sentiment_New)


  ## Student's t-test (Asumiendo que Shapiro test RETURNEA FALSE -> para cada label generalmente p > 0.05)
  # t-test verifica si las muestras provienen de distinta media (p < 0.05).
  
  # model <- t.test(VarX ~ VarY, data = df)
  
  realizar_t_test(df_excel$CSAT_Score, df_excel$Sentiment_New)

  ## Mann-Whitney U-test test (Asumiendo que Shapiro test RETURNEA TRUE -> para cada label generalmente p < 0.05)
  # Mann-Whitney U-test verifica si las muestras provienen de distintas distribuciones (p < 0.05).
  
  realizar_wilcox_test(df_excel$CSAT_Score, df_excel$Sentiment_New)

  
  
  
### MCA (verificar que variables nominales tienden a aparecer juntas)

mca_data <- data.frame(df_excel$Sentiment, df_excel$Reason, df_excel$Channel, df_excel$Response_Time)
realizar_MCA(mca_data) # muestra las correlaciones entre los valores de cada atributo.







### K-means (verificar patrones entre variables numericas)

matriz <- data.frame(df_excel$CSAT_Score, df_excel$Call_Duration) # Coger solo las columnas interesantes (deben ser numericas)
cantidad_clusters_Kmeans(matriz)
realizar_Kmeans(matriz, df_excel$ID, 3) # añadir el nombre para etiquetar instancias


























########################################### CODIGO DEPRICADO #############################################

matriz <- data.frame(df_excel$CSAT_Score, df_excel$Call_Duration) # Coger solo las columnas interesantes (deben ser numericas)
matriz <- scale(matriz) # mappear la info para que se proprocione a escala


fviz_nbclust(matriz, kmeans, method = "wss") + labs(susbtitle = "Elbow method") # ver cantidad de clusters ideal. Cuando la slope deja de bajar, coger ese valor
clusters <- kmeans(matriz, centers = 2, nstart = 100) # hacer k-means con 2 clusters
clusters <- clusters$cluster # obtener los mapeos

if (TRUE) {
  # dar nombre a cada instancia mapeada en los clusters
  rownames(matriz) <- paste(df_excel$ID) 
  
} else {
  # O hacerlo mediante llamandolo valorAtributo_NdeOcurrencia
  sentimientos <- df_excel$Sentiment_New[seq_len(nrow(matriz))] 
  
  rownames(matriz) <- paste0(
    sentimientos, "_",
    ave(seq_along(sentimientos), sentimientos, FUN = seq_along)
  )
  
}


fviz_cluster(list(data = matriz, cluster = clusters)) # visualizar los clusters creados


table(clusters, df_excel$Sentiment_New) # Se pueden hacer comparativas de como los clusters encajan con otras variables


}




