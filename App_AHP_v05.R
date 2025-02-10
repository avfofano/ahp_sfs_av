library(shiny)
library(dplyr)
library(DT)  # Pacote DT para tabelas interativas
library(plotly)  # Pacote plotly para gráficos interativos
library(purrr)  # Para usar map_dfr
library(tidyr)
library(markdown)
# library(shinythemes)
# install.packages("shinythemes")

# I. Dados iniciais
nomes_dos_locais <- df_original %>%
  distinct(nome_local) %>%
  pull(nome_local)

# Extração dos nomes dos elementos
nomes_elementos <- df_original %>%
  distinct(nome_elemento) %>%
  pull(nome_elemento)

# II. Funções
######
### FUNÇÃO mostra_elementos_locais()
### Retorna uma coluna com elementos e as colunas com os nomes dos locais; as células são os rankings (graus de adequação dos elementos)
######
mostra_elementos_locais <- function(){
  # Dados iniciais
  nomes_dos_locais <- df_original %>%
    distinct(nome_local) %>%
    pull(nome_local)

  # Função para processar cada local e incluir o nome do local
  processar_local <- function(nome_local) {
    resultado <- Instalacao$new(nome_local, df_original, pesos_ahp)$ranking_elementos
    resultado %>% mutate(nome_local = nome_local)  # Adiciona a coluna nome_local
  }

  # Aplicar a função a todos os locais e combinar os resultados em um único data frame
  inst_locais_elementos <- map(nomes_dos_locais, processar_local) |> list_rbind()

  # Criar o dataframe wide com elementos nas linhas e locais nas colunas
  df_wide <- inst_locais_elementos %>%
    select(elementos, nome_sistema, nome_local, ranking_elementos) %>%
    pivot_wider(names_from = nome_local, values_from = ranking_elementos, values_fill = list(ranking_elementos = NA))

  media_elementos <- as.data.frame(rowMeans(df_wide[, 3:ncol(df_wide)], na.rm = TRUE))
  
  colnames(media_elementos) <- "Media"

  df_wide <- cbind(df_wide, media_elementos)
  
  # colnames(df_wide)[c(1,2)] <- c("Elementos", "Sistemas")
  # colnames(df_wide)[1] <- "Elementos"
  # colnames(df_wide)[2] <- "Sistemas"
  # Exibir o resultado
  return(df_wide)
}

######
### FUNÇÃO mostra_graus_adequacao_locais()
### Retorna os locais e graus de adequação geral
######
mostra_graus_adequacao_locais <- function() {
  # Dados iniciais
  nomes_dos_locais <- df_original %>%
    distinct(nome_local) %>%
    pull(nome_local)

  # Função para processar cada local e retornar um dataframe com nome_local e grau_adequacao
  processar_graus_adequacao <- function(nome_local) {
    # Calcula o grau de adequação para o local
    grau_adequacao <- Instalacao$new(nome_local, df_original, pesos_ahp)$ranking_local
    
    # Cria um dataframe com o nome do local e o grau de adequação
    data.frame(
      nome_local = nome_local,
      grau_adequacao = grau_adequacao,
      stringsAsFactors = FALSE
    )
  }

  # Aplica a função a todos os locais e combina os resultados em um único dataframe
  graus_adequacao_locais <- map_dfr(nomes_dos_locais, processar_graus_adequacao)

  # Retorna o dataframe final
  return(graus_adequacao_locais)
}

### III. UI: User Interface
ui <- navbarPage("PriRef",
  tabPanel("Graus de Adequação",     
    fluidPage(
      h3("Graus de Adequação: Locais, Sistemas e Elementos"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("escolha", "Escolha um Local:", choices = nomes_dos_locais),
          checkboxGroupInput(
            "sistemas", 
            "Selecione os Sistemas:",
            choices = c("Mecânico" = "M", "Civil" = "C", "Elétrico" = "E"),
            selected = c("M", "C", "E")
          ),
          sliderInput(
            "intervalo_ranking", 
            "Filtrar por intervalo de Graus de Adequação:", 
            min = 0, 
            max = 1, 
            value = c(0, 1),
            step = 0.01
          )
        ),
        
        mainPanel(
          fluidRow(
            column(
              width = 8,  
              h4("Graus de Adequação dos Elementos"),
              DTOutput("ranking_elementos")
            ),
            column(
              width = 4,  
              h4("Graus de Adequação"),
              verbatimTextOutput("ranking_local"),
              # h4("GA por Sistemas"),
              # verbatimTextOutput("ranking_civil"),
              # verbatimTextOutput("ranking_mecanico"),
              # verbatimTextOutput("ranking_eletrico"),
              
              br(),
              h4("GA por Sistemas"),
              plotlyOutput("grafico_barras_sistemas")
            )
          )
        )
      )
    )
  ),
  
  tabPanel("Elementos Críticos",    
    fluidPage(
      fluidRow(  
      #fluidRow(
      #column(width = 3,
         sidebarPanel(
          selectInput(
            "elementos_selecionados",
            "Selecione os Elementos:",
            choices = c("Todos os elementos", nomes_elementos),  # Inclui a opção "Todos os elementos" primeiro
            multiple = TRUE  # Permite múltiplas seleções
          )
          #)
        ,
        
      #column(width = 3,
        #sidebarPanel(
          checkboxGroupInput(
            "sistemas_criticos", 
            "Selecione os Sistemas:",
            choices = c("Mecânico" = "M", "Civil" = "C", "Elétrico" = "E"),
            selected = c("M", "C", "E")
          )
        #)
      #)
        ),
        
      column(width = 8,
          
          plotlyOutput("grafico_barras")  # Gráfico de barras
          
          )

    ),

      fluidRow(
        mainPanel(
          DTOutput("elementos_criticos")  # Exibe a tabela de elementos críticos
        )
      )
    )
  ),

  tabPanel("Sobre", 
    fluidPage(
      h3("Informações sobre o sistema"),
      includeMarkdown("README.md")
      
    )
  )
)

# IV. Server
server <- function(input, output, session) {
  #####
  ## PARTE DA tabPanel "Graus de Adequação"
  #####
  
  # Reativo para criar a instância da classe R6 com base no local escolhido
  instalacao_reactive <- reactive({
    nome_local_escolhido <- input$escolha
    Instalacao$new(nome_local_escolhido, df_original, pesos_ahp)
  })
  
  # Reativos para os rankings
  ranking_local <- reactive({
    round(instalacao_reactive()$ranking_local, 3)
  })
  
  ranking_elementos <- reactive({
    instalacao_reactive()$ranking_elementos
  })
  
  ranking_civil <- reactive({
    round(instalacao_reactive()$ranking_civil, 3)
  })
  
  ranking_mecanico <- reactive({
    round(instalacao_reactive()$ranking_mecanico, 3)
  })
  
  ranking_eletrico <- reactive({
    round(instalacao_reactive()$ranking_eletrico, 3)
  })
  
  # Filtro reativo para a tabela ranking_elementos com base nos sistemas selecionados e no intervalo de ranking
  ranking_elementos_filtrado <- reactive({
    sistemas_selecionados <- input$sistemas
    intervalo <- input$intervalo_ranking  
    
    dados <- ranking_elementos()
    
    # Verifica se a coluna "nome_sistema" existe no dataframe
    if ("nome_sistema" %in% colnames(dados)) {
      dados <- dados %>% filter(nome_sistema %in% sistemas_selecionados)
    }
    
    # Verifica se a coluna "ranking_elementos" existe no dataframe
    if ("ranking_elementos" %in% colnames(dados)) {
      dados <- dados %>% filter(ranking_elementos >= intervalo[1] & ranking_elementos <= intervalo[2])
    }
    
    # Formata as colunas numéricas para 3 casas decimais
    dados <- dados %>%
      mutate(across(where(is.numeric), ~ round(., 3)))
    
    dados
  })
  
  # Atualiza as opções do selectInput (se necessário)
  observe({
    updateSelectInput(session, "escolha", choices = nomes_dos_locais)
  })
  
  # Outputs
  output$ranking_local <- renderText({
    paste("GA Local:", ranking_local())
  })
  
  output$ranking_elementos <- renderDT({
    dados <- ranking_elementos_filtrado()
    if (nrow(dados) == 0) {
      showNotification("Nenhum dado encontrado para os sistemas e intervalo selecionados.", type = "warning")
    }
    datatable(dados, options = list(pageLength = 12), filter = "none")
  })
  
  output$ranking_civil <- renderText({
    paste("Civil:", ranking_civil())
  })
  
  output$ranking_mecanico <- renderText({
    paste("Mecânico:", ranking_mecanico())
  })
  
  output$ranking_eletrico <- renderText({
    paste("Elétrico:", ranking_eletrico())
  })
  
  
  output$grafico_barras_sistemas <- renderPlotly({
    # Cria um dataframe com os valores dos rankings
    dados_barras <- data.frame(
      Sistema = c("Civil", "Mecânico", "Elétrico"),
      Valor = c(ranking_civil(), ranking_mecanico(), ranking_eletrico())
    )
    
    # Ordena os valores para atribuir as cores corretamente
    dados_barras <- dados_barras %>%
      arrange(Valor)
    
    # Define as cores com base na ordem dos valores
    cores <- c("red", "yellow", "green")  # Vermelho para o menor, amarelo para o intermediário, verde para o maior
    
    # Cria o gráfico de barras
    plot_ly(dados_barras, x = ~Sistema, y = ~Valor, type = 'bar', 
            marker = list(color = cores)) %>%
      layout(
        #title = "Ranking por Sistema",
        xaxis = list(title = "Sistema"),
        yaxis = list(title = "Valor")
      )
})

  
  #####
  ## PARTE DA tabPanel "Elementos Críticos"
  #####
  
  # Função para mostrar os elementos críticos com 3 casas decimais
  elementos_criticos <- reactive({
    elementos_criticos <- mostra_elementos_locais()  # Chama a função para obter os dados

    # Arredonda os valores numéricos para 3 casas decimais
    elementos_criticos <- elementos_criticos %>%
      mutate(across(where(is.numeric), ~ round(., 3)))
    
    # Filtra os elementos com base na seleção do usuário
    if ("Todos os elementos" %in% input$elementos_selecionados) {
      # Se "Todos os elementos" estiver selecionado, não aplica filtro
      elementos_criticos <- elementos_criticos
    } else if (length(input$elementos_selecionados) > 0) {
      # Filtra com base nos elementos selecionados
      elementos_criticos <- elementos_criticos %>%
        filter(elementos %in% input$elementos_selecionados)
    }
    
    # Filtra os elementos críticos com base nos sistemas selecionados
    if (length(input$sistemas_criticos) > 0) {
      elementos_criticos <- elementos_criticos %>%
        filter(nome_sistema %in% input$sistemas_criticos)
    }
    
    elementos_criticos
  })
  
  output$elementos_criticos <- renderDT({
    elementos_criticos() %>%
      datatable(options = list(pageLength = 12), rownames = FALSE)
  })
  
  #####
  ## PARTE DA NOVA tabPanel "Graus de Adequação por Local"
  #####
  
  # Gráfico de barras
  output$grafico_barras <- renderPlotly({
    dados <- mostra_graus_adequacao_locais()  # Obtém os dados
    
    # Cria o gráfico de barras
    plot_ly(
      dados,
      x = ~nome_local,
      y = ~grau_adequacao,
      type = "bar",
      marker = list(color = "blue")  # Cor das barras
    ) %>%
      layout(
        title = "Graus de Adequação por Local",
        xaxis = list(title = "Local"),
        yaxis = list(title = "Grau de Adequação")
      )
  })
}

# Run App
shinyApp(ui, server)
