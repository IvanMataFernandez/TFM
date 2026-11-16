library("readxl")
library("dplyr")



importar_df_empresas <- function () {
  # Obtener el DF from excel
  ruta_excel <- "C:\\Users\\ivanm\\Desktop\\RStudioWS\\main\\muestras\\MuestraEmpresas.xlsx"
  xlsx <- read_excel(ruta_excel)
  df <- data.frame(xlsx)
  
  
  df <- df %>%
    select(-CIUDAD, -MODELO, -SUB_INDUSTRIA, -AUTONOMIA_FINANCIERA)   # Elminar estas 4 cols, no las veo necesarias por ahora
  
  
  # Combinar columnas Fundador_En_IT & Fundador_En_Tech
  df <- df %>%
    mutate(FUNDADOR_ESTUDIOS = case_when(
      FUNDADOR_EN_IT == "SI" & FUNDADOR_EN_TECH == "SI" ~ "NO_RELACIONADOS",
   #   FUNDADOR_EN_IT == "SI" & FUNDADOR_EN_TECH == "NO" ~ "INFORMÁTICA",   <- ninguna instancia cumple con esto, redundant
      FUNDADOR_EN_IT == "NO" & FUNDADOR_EN_TECH == "SI" ~ "TECNOLOGÍA",
      FUNDADOR_EN_IT == "NO" & FUNDADOR_EN_TECH == "NO" ~ "IT_&_TECH"
    ))
  
  df <- df %>%
    select(-FUNDADOR_EN_IT, -FUNDADOR_EN_TECH)   
  
  # Combinar columnas FUNDADOR_35 & FUNDADOR_35_45 & FUNDADOR_45
  
  df <- df %>%
    mutate(FUNDADOR_EDAD = case_when(
      FUNDADOR_35 == "NO" ~ "<35",
      FUNDADOR_35 == "SI" & FUNDADOR_45 == "NO" ~ "35-45",
      FUNDADOR_45 == "SI" ~ ">45",
    ))
  
  df <- df %>%
    select(-FUNDADOR_35, -FUNDADOR_35_45, -FUNDADOR_45)   
  
  
  
  
  label_list <- c("EARLY_STAGE", "BREAKOUT_STAGE", "LATE_STAGE")
  df$MADUREZ_EMPRESARIAL <- factor(df$MADUREZ_EMPRESARIAL, ordered = TRUE, levels = label_list, labels = label_list)
  
  label_list <- c("SEED", "SERIE_A", "SERIE_B", "SERIE_C_D" )
  df$MADUREZ_FINANCIERA <- factor(df$MADUREZ_FINANCIERA, ordered = TRUE, levels = label_list, labels = label_list)
  
  label_list <- c("NO", "SI")
  df$ES_ACQUIRED <- factor(df$ES_ACQUIRED, ordered = FALSE, levels = label_list, labels = label_list)
  df$ES_SPINOUT <- factor(df$ES_SPINOUT, ordered = FALSE, levels = label_list, labels = label_list)
  df$IT_OR_TECH <- factor(df$IT_OR_TECH, ordered = FALSE, levels = label_list, labels = label_list)
  df$SERIAL_FOUNDER <- factor(df$SERIAL_FOUNDER, ordered = FALSE, levels = label_list, labels = label_list)
  
  label_list <- c("NO_RELACIONADOS", "TECNOLOGÍA", "IT_&_TECH")
  df$FUNDADOR_ESTUDIOS <- factor(df$FUNDADOR_ESTUDIOS, ordered = FALSE, levels = label_list, labels = label_list)
  
  label_list <- c("<35", "35-45", ">45")
  df$FUNDADOR_EDAD <- factor(df$FUNDADOR_EDAD, ordered = TRUE, levels = label_list, labels = label_list)
  
  df$AÑO_FUNDACION <- as.integer(df$AÑO_FUNDACION)
  df$N_EMPLEADOS <- as.integer(df$N_EMPLEADOS)
  df$VALORACION <- as.integer(df$VALORACION)
  df$INGRESOS <- as.integer(df$INGRESOS)
  df$PN <- as.integer(df$PN)
  df$RONDAS_DE_FINANCIACION <- as.integer(df$RONDAS_DE_FINANCIACION)
  
  
  
  names(df) <- c("Nombre", "Año_Fundacion", "CCAA", "Industria", "Empleados","Madurez_Empresarial" ,"Madurez_Financiera", "Es_Acquired", "Es_Spinout", "Valoracion", "Ingresos", "Financiacion", "Rondas_De_Financiacion", "Fundador_IT_o_Tech", "Fundador_En_Serie", "Fundador_Estudios", "Fundador_Edad") 
  
  return(df)
  
  
  
}



importar_df_prueba <- function () {
  
  # Obtener el DF from excel
  ruta_excel <- "C:\\Users\\ivanm\\Desktop\\RStudioWS\\main\\muestras\\testsample.xlsx"
  xlsx <- read_excel(ruta_excel)
  df <- data.frame(xlsx)
  
  # Renombrar columnas
  names(df) <- c("ID", "Customer_Name", "Sentiment", "CSAT_Score", "Call_Timestamp", "Reason", "City", "State", "Channel", "Response_Time", "Call_Duration", "Call_Center") 
  
  
  # Reorganizar categoricos (para nominales no usar el parametro "levels")
  label_list <- c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive")
  df$Sentiment <- factor(df$Sentiment, ordered = TRUE, levels = label_list, labels = label_list)
  
  label_list <- c("Below SLA", "Within SLA", "Above SLA")
  df$Response_Time <- factor(df$Response_Time, ordered = TRUE, levels = label_list, labels = label_list)
  
  
  return (df)
  
}











#df <- df %>%
#  separate_rows(MODELO, SUB_INDUSTRIA, sep = ",")











