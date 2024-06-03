library(shiny)
library(caret)
library(dplyr)
library(xgboost)
library(SHAPforxgboost)
library(fastDummies)

# Modelo: 
modelo_xgb<- readRDS("datos/modeloXGB")

modelo_xgb1<- readRDS("datos/modelointerpretacion")

#Datos: 
datos<- readxl::read_excel("datos/datosmodelo.xlsx")
datos<- datos %>% mutate(fraud_reported = as.factor(fraud_reported))
variables_cat <- c("incident_severity", "collision_type", "incident_type", "authorities_contacted")
datos_int <- dummy_cols(datos, select_columns = variables_cat, remove_selected_columns = TRUE  )


## "vehicle_claim"         "property_claim"        "injury_claim"          "incident_severity"     "collision_type"       
## "incident_type"         "authorities_contacted" "fraud_reported" 

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico")
  ),
  titlePanel(
    div(
      img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSap6F0SL1cLIpZv6IJbC2D2DYXgBOdzrTDJx2H4m7XWg&s", height = 100, width = 300),
      "Predicción de Reclamaciones de Seguro",
      style = "display: flex; align-items: center;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #73EDFF; color: white;",
      numericInput("vehicle_claim", "Monto de Reclamo por Vehículo:", value = NULL, min = min(datos$vehicle_claim), max = max(datos$vehicle_claim)),
      numericInput("property_claim", "Monto de Reclamo por Propiedad:", value = NULL,  min =  min(datos$property_claim), max = max(datos$property_claim)),
      numericInput("injury_claim", "Monto de Reclamo por Lesiones:", value = NULL,  min =  min(datos$injury_claim), max = max(datos$injury_claim)),
      selectInput("incident_severity", "Gravedad del Incidente",
                  choices = unique(datos$incident_severity) ),
      selectInput("collision_type", "Tipo de Colisión:",
                  choices = unique(datos$collision_type)),
      selectInput("incident_type", "Tipo de Incidente:",
                  choices = unique(datos$incident_type)),
      selectInput("authorities_contacted", "¿Se contactaron autoridades?",
                  choices = unique(datos$authorities_contacted))
    ),
    
    mainPanel(
      actionButton("predict_button", "Realizar Predicción"),
      textOutput("prediction_output"),
      textOutput("probability_output"), 
      h1("Interpretabilidad"),
      plotOutput("model_plot1"),
      plotOutput("model_plot2")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$predict_button, {
    tryCatch({
      # Verificar si algún parámetro requerido está ausente
      if ( length(input$vehicle_claim) == 0 || is.null(input$property_claim) || is.null(input$injury_claim)) {
        # Mostrar un mensaje de error si falta algún parámetro
        stop("Faltan parámetros. Por favor completa todos los campos.")
      }
      
      new_data <<- data.frame(
        vehicle_claim = input$vehicle_claim,
        property_claim = input$property_claim,
        injury_claim = input$injury_claim,
        incident_severity = input$incident_severity,
        collision_type = input$collision_type,
        incident_type = input$incident_type,
        authorities_contacted = input$authorities_contacted
      )
      
      
      
      
      
      predicted_class <- tryCatch({
         predict(modelo_xgb, new_data)
      }, error = function(e) {
        stop("Error al realizar la predicción. Faltan parámetros.")
      })
      

      output$prediction_output <- renderText({
        paste("Clase predicha:", predicted_class)
      })
      
      
      probability <- tryCatch({
          predict(modelo_xgb, newdata = new_data, type = "prob")
      }, error = function(e) {
        stop("Error al realizar la predicción. Faltan Parámetros.")
      })
      
      output$probability_output <- renderText({
        paste("Probabilidad de fraude:", round(probability[,2],4), ifelse(predicted_class == 1, " - Potencial Fraude.", "- Riesgo de fraude bajo.")  )
      })
      
      
      ### Intepretabilidad: 
      
      output$model_plot1 <- renderPlot({
        
        variables_cat <- c("incident_severity", "collision_type", "incident_type", "authorities_contacted")
        datosmodelo <- dummy_cols(new_data, select_columns = variables_cat, remove_selected_columns = TRUE  )
        
        
        nombres_faltantes <- setdiff(names(datos_int), names(datosmodelo))
        df <- data.frame(matrix(0, 1, ncol = length(nombres_faltantes)))
        names(df) <- nombres_faltantes
        
        datosmodelo<- cbind(datosmodelo, df)
        
        datosmodelo<<- datosmodelo %>% select(-fraud_reported)
        
        datosmodelo<- datosmodelo %>% select( modelo_xgb1$feature_names)
        
        
        X1<<- as.matrix(datosmodelo)
        
        shap_values <- shap.values(xgb_model = modelo_xgb1, X_train = X1)
        
        shap_values$mean_shap_score
        
        
        shap_values_iris <- shap_values$shap_score
        
        shap_long_iris <- shap.prep(xgb_model = modelo_xgb1, X_train = X1)
        # is the same as: using given shap_contrib
        shap_long_iris <- shap.prep(shap_contrib = shap_values_iris, X_train = X1)
        
        p1<- shap.plot.summary(shap_long_iris)
        
        print(p1)
      })
   

 
      
    }, error = function(e) {
      # Capturar y mostrar el mensaje de error
      output$prediction_output <- renderText({
        paste("Error:", e$message)
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
