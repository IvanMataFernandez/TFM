library("ggplot2")
library("dplyr")
library("tidyverse")
library("mapSpain")

source("r_scripts/Data_importer.R")

##################################################################################
############################## FUNCTIONS #########################################
##################################################################################



##### FUNCIONES PARA PLOTTEAR ######


### Plot map 

plot_mapa <- function(columna_ccaa, titulo, leyenda) {
  
  ### Mapa de España
  ccaa <- mapSpain::esp_get_ccaa()   # Obtener geometrías de las CCAA
  ccaa_en_libreria <- unique(ccaa$ine.ccaa.name)
  ccaa_comunes <- c("ANDALUCIA", "ARAGON", "ASTURIAS", "BALEARES", "CANARIAS", "CANTABRIA", "CASTILLA_Y_LEON", "CASTILLA_LA_MANCHA", "CATALUÑA", "VALENCIA", "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "EUSKADI", "RIOJA", "CEUTA", "MELILLA")
  lista <- ccaa_en_libreria[match(columna_ccaa, ccaa_comunes)]
  
  
  ### Calcular frecuencias
  frec <- as.data.frame(table(lista))
  names(frec) <- c("ccaa", "cant")
  
  ### Calcular porcentajes sobre el total
  frec$porcentaje <- frec$cant / sum(frec$cant) * 100
  
  ### Unir con geometrías
  mapa <- ccaa |>
    left_join(frec, by = c("ine.ccaa.name" = "ccaa"))
  

  
  ### Dibujar mapa
  
  # Dibujar el mapa
  ggplot(mapa) +
    geom_sf(aes(fill = porcentaje), color = "white", linewidth = 0.3) +
    geom_sf_text(
      aes(label = ifelse(
        is.na(porcentaje),
        "N/A",
        paste0(cant,"\n( ",round(porcentaje, 1), "% )")
      )),
      size = 4,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_gradient(
      low = "#CAE9FF",
      high = "#1B4965",
      na.value = "grey70",
      name = leyenda,
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    ) +
    labs(title = paste("                         ", titulo)) +
    theme_void() + theme(
      
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = c(0.1, 0.9)
    )
  
}


plot_mapa_proporciones <- function(columna_ccaa, proporciones, titulo, leyenda) {
  
  ### Mapa de España
  ccaa <- mapSpain::esp_get_ccaa() # Obtener geometrías de las CCAA
  ccaa_en_libreria <- unique(ccaa$ine.ccaa.name)
  ccaa_comunes <- c("ANDALUCIA", "ARAGON", "ASTURIAS", "BALEARES", "CANARIAS", "CANTABRIA", "CASTILLA_Y_LEON", "CASTILLA_LA_MANCHA", "CATALUÑA", "VALENCIA", "EXTREMADURA", "GALICIA", "MADRID", "MURCIA", "NAVARRA", "EUSKADI", "RIOJA", "CEUTA", "MELILLA")
  lista <- ccaa_en_libreria[match(columna_ccaa, ccaa_comunes)]
  
  porcentajes <- data.frame(
    comunidad = lista,
    porcentaje = proporciones
  )
  
  
  print(porcentajes)
  
  
  mapa <- ccaa %>%
    left_join(porcentajes, by = c("ine.ccaa.name" = "comunidad"))
  
  
  
  
  # Dibujar el mapa
  ggplot(mapa) +
    geom_sf(aes(fill = porcentaje), color = "white", linewidth = 0.3) +
    geom_sf_text(
      aes(label = ifelse(
        is.na(porcentaje),
        "N/A",
        paste0(round(porcentaje, 1), "%")
      )),
      size = 4,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_gradient(
      low = "#CAE9FF",
      high = "#1B4965",
      na.value = "grey70",
      name = leyenda,
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    ) +
    labs(title = paste("               ", titulo)) +
    theme_void() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = c(0.1, 0.8)
    )

}

### Plot piechart 

plot_pie <- function(columna_nominal, label_leyenda, colores, titulo) {
  
  data <- as.data.frame(table(columna_nominal)) # Obtener la variable categorica del df
  names(data) <- c("group", "value") 

  ggplot(data, aes(x = "", y = value, fill = group)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    theme_void() +
    geom_text(
      aes(label = paste0(
        group, "\n",
        round(value / sum(value) * 100, 1), "% (", value, ")"
      )),
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 6
    ) + scale_fill_manual(values=colores) + labs(fill = label_leyenda, title = titulo) +
      theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
  
}



### Plot heatmap (nominal x nominal)
plot_heatmapNominal <- function (columna_nominal1, columna_nominal2, colores, titulo, labelX, labelY) {
  
  dataset <- as.data.frame(table(
    col1 = columna_nominal1,
    col2 = columna_nominal2
  ))
  
  colnames(dataset) <- c(
    "col1",
    "col2",
    "frecuencia"
  )
  
  dataset <- dataset %>%
    group_by(col1) %>%
    mutate(
      porcentaje = frecuencia / sum(frecuencia) * 100,
      frecuencia_normalizada = frecuencia / max(frecuencia)
    ) %>%
    ungroup()


  plot <- ggplot(dataset,
         aes(x = col1, y = col2,
             fill = porcentaje)) +
    geom_tile(color = "white") +
    geom_text(
      aes(label = paste0(
        frecuencia, "\n",
        round(porcentaje, 1), "%"
      )),
      color = "black",
      size = 7
    ) +
    scale_fill_gradientn(
      colours = colores,
      values = scales::rescale(c(0, 25, 50, 75, 100)),
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      name = "Porcentaje\npor columna"
    ) +
    labs(
      title = titulo,
      x = labelX,
      y = labelY
    ) +
    theme_minimal() + theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 18),
      legend.position = "none"
    )

  if (length(unique(columna_nominal1)) > 10 || length(unique(columna_nominal2)) > 10) {
    plot <- plot + theme(
      text = element_text(size = 9),
      plot.title = element_text(size = 11),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7) 
    )
  }
    

  
  
  
  
    print(plot)
  
    return(dataset)
}
  


### Plot scatterplot (nominal x numerico) 

plot_scatterNominal <- function (columna_nominal, columna_numerica, colores, titulo, labelX, labelY, linea_de_regresion) {
  
  dataset <- data.frame(
    columna_nominal,
    columna_numerica)

  grafico <- ggplot(data = dataset, mapping = aes(x = columna_nominal, y = columna_numerica, color = columna_nominal)) + 
    geom_jitter(size = 3,
                position = position_jitter(width = 0, height = 0.1)) + 
    
    scale_color_manual(values = colores, name = labelX) + labs(
      title = titulo,            
      x = labelX,
      y = labelY,
    ) + theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.position = "none"
    )
  
  
  
  if (linea_de_regresion) {
    grafico <- grafico + geom_smooth(
      aes(group = 1),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      color = "red",
      linewidth = 1
    ) +
      scale_color_manual(
        values = colores,
        name = labelX
      ) 
  }
  
  print(grafico)
  
  
  
}

### Plot scatterplot (numerico x numerico) 

plot_scatterNumerico <- function (columna_numerica1, columna_numerica2, titulo, labelX, labelY, linea_de_regresion) {
  
  dataset <- data.frame(
    columna_numerica1,
    columna_numerica2)
  
  grafico <- ggplot(data = dataset, mapping = aes(x = columna_numerica1, y = columna_numerica2)) + geom_jitter(size = 3) + labs(
    title = titulo,            
    x = labelX,
    y = labelY
  ) + theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
    legend.position = "none"
  )
  
  if (linea_de_regresion) {
    grafico <- grafico   + geom_smooth(method = "lm",
                                       se = FALSE,
                                       color = "red",
                                       linewidth = 1) 
  }
  
  print(grafico)

  
}

### Plot diagrama de barras (nominal / discreto)

plot_barSimple <- function (columna, colores, titulo, labelX, leyenda) {
  dataset <- data.frame(
    columna)
  
  if (is.numeric(dataset[[1]])) {
    plot <- ggplot(data = dataset, mapping = aes(x = factor(columna))) + geom_bar(fill = colores, color = "white", width = 0.65) +
            geom_text(
              stat = "count",
              aes(
                label = paste0(
                  after_stat(count),
                  " (",
                  round(after_stat(count / sum(count) * 100), 1),
                  "%)"
                )
              ),
              vjust = -0.5,
              size = 5
            ) +  
            scale_fill_manual(values=colores) + labs(title = titulo, x = labelX, y = "Cantidad") +
                theme(
                  plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                  axis.text = element_text(size = 14),
                  axis.title = element_text(size = 16),
                  legend.position = "none"
                )
          
  } else {
    plot <- ggplot(data = dataset, mapping = aes(x = columna, fill = columna)) + geom_bar(width = 0.65) + scale_fill_manual(values=colores) +
            geom_text(
              stat = "count",
              aes(
                label = paste0(
                  after_stat(count),
                  " (",
                  round(after_stat(count / sum(count) * 100), 1),
                  "%)"
                )
              ),
              vjust = -0.5,
              size = 5
            ) + labs(title = titulo, x = labelX, y = "Cantidad", fill = leyenda) +
            theme(
              plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              legend.position = "none"
            )  
            
      

    
    if (length(unique(columna)) > 10) {
      
      plot$layers <- plot$layers[-which(
        sapply(plot$layers, function(x) inherits(x$geom, "GeomText"))
      )]
      
      plot <- plot + theme(
      text = element_text(size = 9),
      plot.title = element_text(size = 11),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7)
      ) + geom_text(
        stat = "count",
        aes(
          label = paste0(
            after_stat(count),
            " (",
            round(after_stat(count / sum(count) * 100), 1),
            "%)"
          )
        ),
        vjust = -0.5,
        size = 3.5
      )
      
    }
    
    

  }
  
  print(plot)

  
  
}



### Plot diagrama de barras (nominal grouped by nominal)

plot_barGroup <- function (columna_nominal, columna_grupal, colores, titulo, labelX, leyenda) {
  dataset <- data.frame(
    columna_nominal,
    columna_grupal)
  
  
  dataset <- dataset %>%
    mutate(
      columna_nominal = factor(columna_nominal),
      columna_grupal = factor(columna_grupal)
    ) %>%
    count(columna_nominal, columna_grupal) %>%
    complete(
      columna_nominal,
      columna_grupal,
      fill = list(n = 0)
    )
  
  
  ggplot(
    data = dataset,
    mapping = aes(
      x = columna_nominal,
      y = n,
      fill = columna_grupal
    )
  ) +
    geom_col(position = position_dodge(preserve = "single")) +
    scale_x_discrete(drop = FALSE) +
    scale_fill_manual(
      values = colores,
      drop = FALSE
    ) +
    labs(
      title = titulo,
      x = labelX,
      y = "Cantidad",
      fill = leyenda
    ) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16)
    )
}



### Plot como los valores de una var numerica van aumentando en el tiempo

plot_2D <- function (columna_numerico, titulo, labelY) {
  
  dataset <- data.frame(
    columna_numerico) 
  
  dataset$columna_numerico <- sort(dataset$columna_numerico)
  
  ggplot(data = dataset, mapping = aes(x = 1:length(columna_numerico), y = columna_numerico)) + geom_ribbon(
    aes(ymin = pmin(columna_numerico, 0),
        ymax = pmax(columna_numerico, 0),
        fill = columna_numerico >= 0),
    alpha = 0.35
  ) +
    geom_hline(yintercept = 0, color = "gray40",
               linewidth = 0.6) +
    geom_line(color = "#2878B5", linewidth = 1) +
    geom_point(color = "#E76F51", size = 2) +
    scale_fill_manual(
      values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51"),
      labels = c("TRUE" = "Positiva", "FALSE" = "Negativa"),
      name = "f(x)"
    ) + labs(title = titulo, x = "Instancias", y = labelY) +   theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.position = "none"
    )
  
  
  
  
}

plot_box <- function (columna_nominal, columna_numerico, titulo, labelX, labelY, colores, mostrar_instancias) {
  
  dataset <- data.frame(
    columna_nominal,
    columna_numerico
  ) 
  
  if (mostrar_instancias) {
    # ver los valores de las instancias
    ggplot(data = dataset, mapping = aes(x = columna_nominal, y = columna_numerico, fill = columna_nominal)) + geom_boxplot(outlier.shape = NA) + geom_jitter() + scale_fill_manual(values = colores, name = labelX) + labs(
      title = titulo,            
      x = labelX,
      y = labelY
    ) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
      )
    
    
  } else {
    ggplot(data = dataset, mapping = aes(x = columna_nominal, y = columna_numerico, fill = columna_nominal)) + geom_boxplot() + scale_fill_manual(values = colores, name = labelX) + labs(
      title = titulo,            
      x = labelX,
      y = labelY
    ) +
     theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "none",
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16)
      )
    
  }
  
  
}


plot_funcion_exponencial <- function(base, labX, labY) {
  eje_x <- seq(0, 100, by = 0.1)
  eje_y <- base^eje_x
  
  plot(eje_x, eje_y,
       type = "l",
       col = "blue",
       lwd = 2,
       xlab = labX,
       ylab = labY,
       las = 1,
       cex.axis = 1.5,
       cex.lab = 1.3,
       cex.main = 1.8,
       yaxt = "n")  
  
  
  valores_y <- seq(0, 10, by = 1)
  
  axis(side = 2,
       at = valores_y,
       labels = paste0("x", valores_y),
       las = 1,
       cex.axis = 1.5)  
  
  grid()
  
  
} 

######## UNUSED ###############


### Plot forestchart 

plot_forest <- function (nombre_intervalos, estimacion, estimacion_inferior, estimacion_superior, titulo, labelX, labelY) {
  
  
  resultados <- data.frame(
    Variable = nombre_intervalos,
    Estimacion = estimacion,  
    IC_inferior = estimacion_inferior,
    IC_superior = estimacion_superior
  )
  
  
  ggplot(resultados,
         aes(x = Estimacion, y = Variable)) +
    geom_vline(xintercept = 1,
               linetype = "dashed",
               color = "red") +
    geom_errorbar(
      aes(xmin = IC_inferior,
          xmax = IC_superior),
      width = 0.2
    ) +
    geom_point(size = 3, color = "steelblue") +
    labs(
      title = titulo,              
      x = labelX,
      y = labelY
    ) +
    theme_minimal() + theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none"
    )
}






### Plot heatmap 

plot_heatmap <- function (columna_nominal1, columna_nominal2, columna_numerico, nombre_escala, titulo, subtitulo, labelX, labelY) {
  
  
  dataset <- data.frame(columna_nominal1, columna_nominal2, columna_numerico)
  
  df <- dataset %>% 
    group_by(columna_nominal1, columna_nominal2) %>% 
    summarise(
      numerico_medio = mean(columna_numerico, na.rm = TRUE), 
      N = sum(!is.na(columna_numerico)),
      .groups = "drop"
    )
  
  
  ggplot(df,
         aes(x = columna_nominal2,
             y = columna_nominal1,
             fill = numerico_medio)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(aes(label = round(numerico_medio, 2)), size = 3) +
    scale_fill_gradient(
      low = "red",
      high = "green",
      name = nombre_escala,
      na.value = "grey90"
    ) +
    labs(
      title = titulo,             # Renombrar texto explicativo here
      subtitle = subtitulo,
      x = labelX,
      y = labelY
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )           
  
  
  
  
}

### Plot histograma

plot_hist <- function (columna_numerico, titulo, labelX, labelY, rangos, colores) {
  dataset <- data.frame(
    columna_numerico) 
  
  
  ggplot(data = dataset, mapping = aes(x = columna_numerico)) +  geom_histogram(
    breaks = rangos,
    fill = colores,
    color = "black"
  ) +
    scale_x_continuous(
      breaks = rangos,
      limits = range(rangos)
    ) + labs(title = titulo, x = labelX, y = labelY) +
    theme_minimal(base_size = 13)  +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "none"
    )
  
}

##################################################################################
###################################### MAIN ######################################
##################################################################################

if (FALSE) {


    ##### Cargar df #####
df_excel = NULL
df_excel <- importar_df_prueba()

  

    ##### Llamadas a funciones custom para plottear datos #####


### Mapa de España
lista_ccaa <- c("MADRID", "MADRID", "CATALUÑA")

plot_mapa(lista_ccaa)



### Basic piechart (analizar proporcion de valores respecto a una variable nominal)

columna_nominal <- df_excel$Sentiment

texto_leyenda <- "Sentiment"
colores <- c(
  "Very Negative" = "#FF6B6B",
  "Negative"    = "#51CF66",
  "Neutral" = "#FFD43B",
  "Positive"     = "#845EF7",
  "Very Positive"    = "#17C365"
)

plot_pie(columna_nominal, texto_leyenda, colores)



### heatmap (analizar un valor numerico respecto a combinaciones de 2 labels)
dataset <- df_excel
dataset$nominal1 <- df_excel$Reason
dataset$nominal2 <- df_excel$State
dataset$numerico <- df_excel$CSAT_Score
colores2 <- c("#1B4965", "#61A5C2")

titulo <- "Heatmap de satisfacción del cliente"
subtitulo <- "Puntuación media de CSAT por motivo y estado"
labelX <- "Estado (State)"
labelY <-"Motivo (Reason)"
nombre_escala = "CSAT medio"

plot_heatmap(dataset$nominal1, dataset$nominal2, dataset$numerico, nombre_escala, titulo, subtitulo, labelX, labelY )

plot_heatmapNominal(dataset$nominal1, dataset$nominal2, colores2, titulo, subtitulo, labelX, labelY)
  
### Forestplot (para mostrar intervalos de confiaza visualmente en analisis estadisticos)

variable = c("Reason A", "Reason B", "Reason C",    # Listar los nombres de los intervalos analizados here
             "Reason D", "Reason E")
estimacion = c(0.75, 1.20, 0.95, 1.50, 0.60)        # Añadir por cada intervalo, su estimacion, 95% conf min y 95% conf max
iC_inferior = c(0.55, 0.90, 0.70, 1.10, 0.35)
iC_superior = c(1.00, 1.60, 1.30, 2.00, 0.95)

titulo <- "Forest plot de estimaciones"
labelX <- "Estimación e intervalo de confianza del 95 %"
labelY <-"Variable"

plot_forest(variable, estimacion, iC_inferior, iC_superior, titulo, labelX, labelY)



### Scatterplot (mostrar tendencias de valores respecto a una variable numerica, sorteada por una nominal)

columna_nominal = df_excel$Sentiment
columna_numerica1 = df_excel$CSAT_Score

titulo <- "Scatterplot"
labelX <- "Var1"
labelY <-"Var2"

colores = c( "Very Negative" = "gray", "Negative" = "green" , "Neutral" = "blue", "Positive" = "red", "Very Positive" = "yellow")

linea_de_regresion_lineal <- TRUE

plot_scatterNominal(columna_nominal, columna_numerica1, colores, titulo, labelX, labelY, linea_de_regresion_lineal)



### Scatterplot (mostrar tendencias de instancias respecto a dos variables numericas en un plano 2D)

columna_numerica1 = df_excel$CSAT_Score
columna_numerica2 = df_excel$Call_Duration

titulo <- "Scatterplot"
labelX <- "Var1"
labelY <-"Var2"

plot_scatterNumerico(columna_numerica1, columna_numerica2, titulo, labelX, labelY, linea_de_regresion_lineal)



### Barplots (ver las frecuencias de una variable nominal)

dataset <- df_excel
dataset$nominal <- dataset$Sentiment
colores <- c("green", "red", "yellow", "blue", "purple")
titulo <- "Diagrama de barras"
labelX <- "Valoracion"
leyenda <- "Valoraciones"

plot_barSimple(dataset$nominal, colores, titulo, labelX, leyenda)



# Barplots (ver las frecuencias de una variable nominal agrupadas por otra nominal)

dataset <- df_excel
dataset$nominal <- dataset$Sentiment
dataset$groupby <- dataset$Reason
colores <- c("green", "red", "yellow")
titulo <- "Diagrama de barras"
labelX <- "Valoracion"
leyenda <- "Tipos de pago"

plot_barGroup(dataset$nominal, dataset$groupby, colores, titulo, labelX, leyenda)



### Histograms (ver concretraciones de variable continua agrupada en rangos)

dataset$numerico <- df_excel$CSAT_Score
titulo <- "Diagrama acumulativo"
labelX <- "LabX"
labelY <- "Valoracion"
rangos <- c(0,3,5,10)  #seq(1, 10, by = 1)
colores <- c("green", "red", "yellow")

plot_hist(dataset$numerico , titulo, labelX, labelY, rangos, colores)



### Plots plano 2D (mostrar como va aumentando el valor de una variable continua a lo largo de la muestra)
dataset <- df_excel
dataset$numerico <- dataset$CSAT_Score
titulo <- "Diagrama acumulativo"
labelY <- "Valoracion"

plot_2D(dataset$numerico, titulo, labelY)



### Boxplot
col1 <- df_excel$Sentiment
col2 <- df_excel$CSAT_Score
colores = c("gray", "green" , "blue", "red", "yellow")
mostrar_instancias = TRUE

titulo <- "Boxplot"
labelX <- "Sentiment"
labelY <-"Score"

plot_box(col1, col2, titulo, labelX, labelY, colores, mostrar_instancias)


















########################################### CODIGO DEPRICADO #############################################


##### Plots.  ggplot(data = <DATA>, mapping = aes (<MAPPINGS>)) + <GEOM_FUNCTION>() + <GGANIMATE_FUNCTION>()


### Mapa de España


lista <- c("Madrid, Comunidad de", "Cataluña", "Madrid, Comunidad de") # Añadir aqui la columna de CCAA, con los nombres mapeados 
frec <- as.data.frame(table(lista))                                    # para que encajen con lo que la libreria entiende
names(frec) <- c("ccaa", "cant")


# 3. Comprobar los nombres disponibles
unique(frec$ccaa)


# Obtener geometrías de las CCAA
ccaa <- mapSpain::esp_get_ccaa()
unique(ccaa$ine.ccaa.name)

mapa <- ccaa |>
  left_join(frec, by = c("ine.ccaa.name" = "ccaa"))


# Dibujar el mapa
ggplot(mapa) +
  geom_sf(aes(fill = cant), color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low = "green",
    high = "red",
    na.value = "grey90",
    name = "Nº Empresas"   
  ) +
  labs(title = "                                         Empresas por comunidad autónoma") +
  theme_void()





### Basic piechart (analizar proporcion de valores respecto a una variable nominal)

data <- as.data.frame(table(df_excel$Sentiment)) # Obtener la variable categorica del df
names(data) <- c("group", "value") # Renombrar "group" al tipo de categoria

ggplot(data, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(
    aes(label = paste0(
      group, "\n",
      round(value / sum(value) * 100, 1), "% (", value, ")"
    )),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  scale_fill_manual(values = c( # Cambiar el color de los posibles valores del label here
    "Very Negative" = "#FF6B6B",
    "Negative"    = "#51CF66",
    "Neutral" = "#FFD43B",
    "Positive"     = "#845EF7",
    "Very Positive"    = "#17C365"
    
  ))


### heatmap (analizar un valor numerico respecto a combinaciones de 2 labels)

df_excel$nominal1 <- dataset$Reason
df_excel$nominal2 <- dataset$State
df_excel$numerico <- dataset$CSAT_Score

titulo <- "Heatmap de satisfacción del cliente"
subtitulo <- "Puntuación media de CSAT por motivo y estado"
labelX <- "Estado (State)"
labelY <-"Motivo (Reason)"
nombre_escala = "CSAT medio"



dataset <- dataset %>% # Recopilar de df_excel 
  group_by(nominal1, nominal2) %>% # splitteado en grupos de "Reason" y "State"
  summarise(
    numerico_medio = mean(numerico, na.rm = TRUE), # Calcular la media del CSAT_Score por cada grupo 
    N = sum(!is.na(numerico)),
    .groups = "drop"
  )


ggplot(dataset,
       aes(x = nominal2,
           y = nominal1,
           fill = numerico_medio)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = round(numerico_medio, 2)), size = 3) +
  scale_fill_gradient(
    low = colores[1],
    high = colores[2],
    name = nombre_escala,
    na.value = "grey90"
  ) +
  labs(
    title = titulo,             # Renombrar texto explicativo here
    subtitle = subtitulo,
    x = labelX,
    y = labelY
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


### Forestplot (para mostrar intervalos de confiaza en analisis estadisticos)

resultados <- data.frame(
  Variable = c("Reason A", "Reason B", "Reason C", # Listar los tipos de intervalos analizados here
               "Reason D", "Reason E"),
  Estimacion = c(0.75, 1.20, 0.95, 1.50, 0.60),  
  IC_inferior = c(0.55, 0.90, 0.70, 1.10, 0.35),
  IC_superior = c(1.00, 1.60, 1.30, 2.00, 0.95)
)

titulo <- "Forest plot de estimaciones"
labelX <- "Estimación e intervalo de confianza del 95 %"
labelY <-"Variable"

ggplot(resultados,
       aes(x = Estimacion, y = Variable)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "red") +
  geom_errorbar(
    aes(xmin = IC_inferior,
        xmax = IC_superior),
    width = 0.2
  ) +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = titulo,              
    x = labelX,
    y = labelY
  ) +
  theme_minimal()


### Scatterplot (mostrar tendencias de valores respecto a una variable numerica, sorteada por una nominal)
dataset = df_excel
valX = df_excel$Sentiment
valY = df_excel$CSAT_Score
colores = c( "Very Negative" = "gray", "Negative" = "green" , "Neutral" = "blue", "Positive" = "red", "Very Positive" = "yellow")

ggplot(data = dataset, mapping = aes(x = valX, y = valY, color = valX)) + geom_jitter() + scale_color_manual(values = colores)



### Barplots (ver las frecuencias de una variable nominal)

dataset <- df_excel
dataset$nominal <- dataset$Sentiment
colores <- c("green", "red", "yellow", "blue", "purple")
titulo <- "Diagrama de barras"
labelX <- "Valoracion"
leyenda <- "Valoraciones"

ggplot(data = dataset, mapping = aes(x = nominal, fill = nominal)) + geom_bar() + scale_fill_manual(values=colores) + labs(title = titulo, x = labelX, y = "Cantidad", fill = leyenda)

# Barplots (ver las frecuencias de una variable nominal agrupadas por otra nominal)

dataset <- df_excel
dataset$nominal <- dataset$Sentiment
dataset$groupby <- dataset$Reason
colores <- c("green", "red", "yellow")
titulo <- "Diagrama de barras"
labelX <- "Valoracion"
leyenda <- "Tipos de pago"

ggplot(data = dataset, mapping = aes(x = factor(nominal), fill = factor(groupby))) + geom_bar(position = "dodge", stat = "count")  + scale_fill_manual(values=colores) + labs(title = titulo, x = labelX, y = "Cantidad", fill = leyenda)


#p + geom_bar(position = "fill", stat = "count")
#p + geom_bar(position = "stack", stat = "count")
#barplot(table(df_excel$Sentiment), horiz = FALSE, col = "green", border = "red", main = "grafico", sub = "subtitulo", xlab = "cilindros", ylab = "label en Y")


### Histograms

dataset <- df_excel
dataset$numerico <- dataset$CSAT_Score
titulo <- "Diagrama acumulativo"
labelX <- "LabX"
labelY <- "Valoracion"
rangos <- c(0,3,5,10)  #seq(1, 10, by = 1)
colores <- c("green", "red", "yellow")

ggplot(data = dataset, mapping = aes(x = dataset$numerico)) +  geom_histogram(
  breaks = rangos,
  fill = colores,
  color = "black"
) +
  scale_x_continuous(
    breaks = rangos,
    limits = range(rangos)
  ) + labs(title = titulo, x = labelX, y = labelY) +
  theme_minimal(base_size = 13)  
#hist(df_excel$CSAT_Score, breaks = seq(0, 10, 2), col = "gray", border = "gray10", main = "titulo", xlab = "var", ylab = "conteo") 




### Plots plano 2D (mostrar la cantidad de un valor numerico a lo largo de la muestra)
dataset <- df_excel
dataset$numerico <- dataset$CSAT_Score
titulo <- "Diagrama acumulativo"
labelY <- "Valoracion"

dataset$numerico <- sort(dataset$numerico)
ggplot(data = dataset, mapping = aes(x = 1:length(dataset$numerico), y = dataset$numerico)) + geom_ribbon(
  aes(ymin = pmin(numerico, 0),
      ymax = pmax(numerico, 0),
      fill = numerico >= 0),
  alpha = 0.35
) +
  geom_hline(yintercept = 0, color = "gray40",
             linewidth = 0.6) +
  geom_line(color = "#2878B5", linewidth = 1) +
  geom_point(color = "#E76F51", size = 2) +
  scale_fill_manual(
    values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51"),
    labels = c("TRUE" = "Positiva", "FALSE" = "Negativa"),
    name = "f(x)"
  ) + labs(title = titulo, x = "Instancias", y = labelY) +   theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


#ggplot(data = df_excel, mapping = aes(x = CSAT_Score, y = Call_Duration, color = Sentiment)) + geom_point() + geom_smooth(method = "lm")
#ggplot(data = df_excel, mapping = aes(x = CSAT_Score, y = Call_Duration, color = Sentiment)) + geom_point() + geom_abline(intercept = 0, slope = 1, col = "blue")


}



