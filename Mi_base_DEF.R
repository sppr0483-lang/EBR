setwd("C:/Users/agarciadeleon/WPy64-38123/scripts/EBR/Riesgo_Producto/Insumos")
library(shiny)
#library(RiesgoProducto)
source("Agrupacion.R")
source("Preparar_datos.R")
library(DT)

ui <- fluidPage(

  titlePanel("Riesgo Producto/Sub Riesgo Ocupación"),

  sidebarLayout(

    sidebarPanel(

      fileInput(
        "calibracion",
        "Factor_Representantes.csv"
      ),

      actionButton(
        "ejecutar",
        "Generar Modelo"
      )
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Score",
          verbatimTextOutput("score")
        ),

        tabPanel(
          "Clasificación",
          fileInput(
            "productos",
            "Productos CSV"
          ),

          actionButton(
            "clasificar",
            "Clasificar"
          ),
          
          downloadButton(
            "descargar",
            "Descargar CSV"
          ),

          DTOutput("resultado")
        )
      )
    )
  )
)


server <- function(input, output, session){

  modelo <- reactiveValues()

  observeEvent(input$ejecutar,{

    modelo$C1 <- Preparar_datos(
      input$calibracion$datapath
    )
#C = modelo$C1
#C = as.matrix(C)
    n= ncol(modelo$C1)
    modelo$X <- Generador_X(n)

    modelo$B <- Construir_Grupos_interiores(
      modelo$C1,
      modelo$X
    )

    modelo$Score <- Fisher_A(
      modelo$C1,
      modelo$X
    )
  })

  output$score <- renderPrint({
    modelo$Score
  })

  observeEvent(input$clasificar,{

    productos <- read.csv(
      input$productos$datapath
    )

    vars <- productos

    productos$Mediana <- apply(
      vars,
      1,
      function(x)
        Valor_distancia2(
          modelo$C1,
          modelo$X,
          as.numeric(x)
        )
    )


    productos$Distancia <- apply(
      vars,
      1,
      function(x)
        Valor_distancia(
          modelo$C1,
          modelo$X,
          as.numeric(x)
        )
    )

    productos$Grupo <- apply(
      vars,
      1,
      function(x){
        Grupo(
          modelo$C1,
          modelo$X,
          as.numeric(x)
        )
      }
    )

    modelo$resultado <- productos

  })

  output$resultado <- renderDT({
    modelo$resultado
  })
  
  output$descargar <- downloadHandler(
    
    filename = function() {
      paste0(
        "Clasificacion_",
        format(Sys.Date(), "%Y%m%d"),
        ".csv"
      )
    },
    
    content = function(file) {
      
      req(modelo$resultado)
      
      write.csv(
        modelo$resultado,
        file,
        row.names = FALSE,
        fileEncoding = "UTF-8"
      )
    }
    
  )
}

shinyApp(ui = ui, server = server)
