library(shiny)  # Carrega o pacote Shiny

# Define a interface do usuário
ui <- fluidPage(
  # Título da aplicação
  titlePanel("Probabilidade de Falha em Equipamentos"),
  
  # Primeira linha de entrada com opções e campo de entrada numérica
  fluidRow(
    column(6, 
           # Botões de rádio para selecionar o tipo de cálculo
           radioButtons("opcao", 
                        label = "Selecione o cálculo", 
                        choices = list("Probabilidade exata" = 1, 
                                       "Menos que" = 2, 
                                       "Mais que" = 3), 
                        selected = 1)
    ),
    column(6, 
           # Entrada numérica para a ocorrência atual
           numericInput("ocorrencia", 
                        "Ocorrência atual:", 
                        value = 1, 
                        min = 1, 
                        max = 99), 
           # Botão para processar os dados
           actionButton("processar", "Processar")
    )
  ),
  
  # Segunda linha para o gráfico de saída
  fluidRow(
    column(12, 
           # Saída do gráfico
           plotOutput("Graf")
    )
  ),
  
  # Adiciona um estilo CSS customizado para melhorar a aparência
  tags$style(HTML("
    body {
      background-color: #f5f5f5;
    }
    .shiny-input-container {
      margin-bottom: 20px;
    }
    .btn {
      background-color: #4CAF50;
      color: white;
      border-radius: 8px;
    }
    .btn:hover {
      background-color: #45a049;
    }
    h2 {
      color: #333333;
    }
  "))
)

# Define a lógica do servidor
server <- function(input, output) {
  # Observa o evento do botão processar
  observeEvent(input$processar, {
    # Define a taxa de ocorrência (lambda) a partir da entrada do usuário
    lamb <- input$ocorrencia
    
    # Define o tipo de cálculo a partir da seleção do usuário
    tipo <- input$opcao
    
    # Define o intervalo de valores a serem calculados
    inic <- lamb - 2
    fim <- lamb + 2
    
    # Calcula as probabilidades com base no tipo selecionado
    if (tipo == 1) {
      x <- dpois(inic:fim, lambda = lamb)
      tit <- "Probabilidade de Ocorrência"
    }
    
    if (tipo == 2) {
      x <- ppois(inic:fim, lambda = lamb)
      tit <- "Probabilidade de Ocorrência Menor Que"
    }
    
    if (tipo == 3) {
      x <- ppois(inic:fim, lambda = lamb, lower.tail = FALSE)
      tit <- "Probabilidade de Ocorrência Maior Que"
    }
    
    # Prepara os rótulos para o gráfico
    z <- as.character(round(x, 4))
    y <- as.character(inic:fim)
    lab <- paste(y, "prob:", z) 
    
    # Define as cores para as barras
    bar_colors <- colorRampPalette(c("#FF9999", "#FF6666", "#FF3333", "#FF0000", "#CC0000"))(length(x))
    
    # Gera o gráfico de barras com as probabilidades e cores
    output$Graf <- renderPlot({
      barplot(x, names.arg = lab, col = bar_colors, main = tit, border = "white")
    })
  })
}

# Executa a aplicação Shiny
shinyApp(ui = ui, server = server)
