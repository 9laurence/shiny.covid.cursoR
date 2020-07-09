library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(leaflet)


ui <- dashboardPage(
  dashboardHeader(title = "Análise COVID"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proposta", tabName = "proposta"),
      menuItem("Análise exploratória", tabName = "exploratoria"),
      menuItem("Análise inferencial", tabName = "analises"),
      menuItem("Conclusão", tabName = "conclusao")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("proposta",
              h1("Proposta para o presente Shiny"),
              tags$p("Os dados de COVID-19 tem sido um tópico de grande discussão. Por um lado são necessários dados para se entender melhor a pandemia, por outro, dados de má qualidade podem trazer evidências falsas e subestimar ou superestimar o impacto da pandemia."),
              tags$p("Dessa forma, faz-se válido que exista uma análise da qualidade dos dados. Um caminho para isso é a análise do número de casos pelos dias da semana.
              Tendo em vista que o número de contágio não muda em função do dia da semana, diferenças nesses dados podem indicar lentidão no processamento dos dados e, portanto, defasagem nos dados com a realidade."),
              tags$p("Com isso exposto, o objetivo desse shiny será analisar os dados de COVID-19 do Brasil em função dos dias da semana. Primeiro iremos avaliar os dados, fazendo uma análise exploratória, e depois faremos uma análise inferencial, para tentar entender a qualidade desses dados."),
              tags$p("Na aba de análise exploratória, você encontrará uma tabela descrevendo os dados para cada dia da semana, bem como um boxplot dos dados para os dias da semana, um histograma das medidas em geral e uma série temporal dos dados de COVID-19.
                     Também é possível encontrar um mapa interativo com as medidas de COVID-19 nos estados do Brasil"),
              tags$p("Já na aba de análises inferenciais, o presente leitor encontrará uma Análise de Variância (ANOVA) comparando os dados encontrados para cada dia da semana..")
              ),
      tabItem("exploratoria",
              h1("Análises exploratórias para os dados de COVID-19 do Brasil"),
              box(width = 12,
                  reactableOutput("tabela_descritivos")),
              br(),
              box(
                width = 12,
                selectInput(
                  inputId = "caso_obito",
                  label = "Selecione uma medida para o histograma e para o boxplot",
                  choices = c(Casos = "casosNovos", Obitos = "obitosNovos"),
                  width = "50%")),
              box(
                width = 6,
                title = "Boxplot para cada dia da semana",
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput("boxplot_descritivo")),
              box(
                width = 6,
                title = "Histograma da variavel",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("histograma_descritivo")),
              box(
                width = 3,
                radioButtons(
                  inputId = "caso_obito_ts",
                  label = "Selecione uma medida para a série temporal",
                  choices = c(Casos = "casosNovos", Obitos = "obitosNovos"),
                  width = "50%")),
              box(
                width = 9,
                title = "Serie temporal da variavel",
                solidHeader = TRUE,
                status = "primary",
                plotly::plotlyOutput("serie_temporal")),
              box(
                width = 3,
                radioButtons(
                  inputId = "caso_obito_mapa",
                  label = "Selecione uma medida para o mapa",
                  choices = c(Casos = "casosAcumulado", Obitos = "obitosAcumulado"),
                  width = "100%")
              ),
              box(
                width = 9,
                title = "Mapa com a quantidade de casos e óbitos",
                solidHeader = TRUE,
                status = "primary",
                leafletOutput("mapa")
                  )
              ),
      tabItem("analises",
              h1 = "Análises inferenciais para os dados de COVID-19 do Brasil",
              box(
                width = 12,
                radioButtons(
                  inputId = "caso_obito_inferencia",
                  label = "Selecione uma medida",
                  choices = c(Casos = "casosNovos", Obitos = "obitosNovos"),
                  width = "25%")),
              tags$p("A análise é lenta. Aguarde um pouco..."),
              box(
                width = 12,
                title = "ANOVA entre os dias da semana",
                solidHeader = TRUE,
                status = "primary",
                plotOutput("anova"))
              ),
      tabItem("conclusao",
              h1 = "Conclusão",
              tags$p("Com os dados apresentados no presente shiny é possível concluir que há diferença entre os dias. Essa diferença pode ser um indício de dados confusos, embora outros fatores possam afetar as medidas por dia da semana."),
              tags$p("Um exemplo disso é a diferença de contigente para as análises e o dia que as pessoas optam por procurar o hospital. Porém, de qualquer forma, é possível perceber que há uma diferença nos dados de COVID-19 do Brasil por dia da semana."))
    )
  )
)

server <- function(input, output, session) {

  # Dados
  data <- read_rds("covid.rds") %>%
    filter(regiao %in% "Brasil") %>%
    select(data, semanaEpi, casosAcumulado, casosNovos, obitosAcumulado, obitosNovos) %>%
    mutate(dia_semana = lubridate::wday(data, label = TRUE))

  data_mapa <- read_rds("covid.rds") %>%
    filter(regiao != "Brasil" & is.na(municipio)) %>%
    select(estado, regiao, data, semanaEpi, casosAcumulado, obitosAcumulado, lat, lon) %>%
    group_by(estado) %>%
    summarise(casosAcumulado = max(casosAcumulado),
              obitosAcumulado = max(obitosAcumulado),
              data = max(data),
              semana = max(semanaEpi)) %>%
    left_join(read_rds("capitaislatlon.rds"))


  # análises exploratórias
  # Tabela descritiva
  output$tabela_descritivos <- renderReactable(data %>% select(dia_semana, casosNovos, obitosNovos) %>%
                                                 group_by(dia_semana) %>%
                                                 summarise(Media_Casos_Novos = mean(casosNovos, na.rm = TRUE),
                                                           DP_Casos_Novos = sd(casosNovos, na.rm = TRUE),
                                                           Max_Casos_Novos = max(casosNovos, na.rm = TRUE),
                                                           Media_Obitos = mean(obitosNovos, na.rm = TRUE),
                                                           DP_Obitos = mean(obitosNovos, na.rm = TRUE),
                                                           Max_Obitos = max(obitosNovos, na.rm = TRUE)) %>% reactable())



  # Boxplot descritivo
  output$boxplot_descritivo <- plotly::renderPlotly({
    req(input$caso_obito)

    p_p_bp <- data %>% select(dia_semana, valor = input$caso_obito) %>%
      ggplot(aes(x = dia_semana,
                 y = valor,
                 color = dia_semana,
                 fill = dia_semana,
                 text = paste("Dia da semana:", dia_semana,
                              "<br>Valor:", valor))) +
      geom_boxplot(alpha = 0.5) +
      geom_jitter() +
      labs(x = "Dia da Semana", y = if_else(input$caso_obito == "casosNovos",
                                            "Número de Casos",
                                            "Número de Óbitos")) +
      theme_classic() +
      theme(legend.position = "none")
    plotly::ggplotly(p_p_bp, tooltip = c("text"))
  })

  # Histograma descritivo
  output$histograma_descritivo <- renderPlot({
    req(input$caso_obito)

    data %>% select(dia_semana, valor = input$caso_obito) %>%
      ggplot(aes(x = valor)) +
      geom_histogram(color = "red", fill = "red", alpha = .5) +
      labs(x =  if_else(input$caso_obito == "casosNovos",
                        "Número de Casos",
                        "Número de Óbitos"),
           y = "Frequencia") +
      theme_classic()
  })


  # Serie temporal
  output$serie_temporal <- plotly::renderPlotly({
    req(input$caso_obito_ts)

    p_p_ts <- data %>% select(data, valor = input$caso_obito_ts, dia_semana) %>%
      pivot_longer(-c(data, dia_semana)) %>% rename(tipo_dado = name,
                                                    valor = value) %>%
      ggplot(aes(x = data, y = valor, color = tipo_dado)) +
      geom_line(aes(text = paste0("Data: ", lubridate::day(data), "/", lubridate::month(data), "/", lubridate::year(data),
                                 "<br>Dia da semana: ", dia_semana,
                                 "<br>Valor: ", valor),
                    group = 1),
                alpha = 0.5) +
      scale_color_manual(values = if_else(input$caso_obito_ts == "casosNovos",
                                          "blue",
                                          "red")) +
      labs(x = "Data",
           y = if_else(input$caso_obito_ts == "casosNovos",
                       "Número de Casos",
                       "Número de Óbitos")) +
      theme_classic() +
      theme(legend.position = "none")

    plotly::ggplotly(p_p_ts, tooltip = c("text"))
  })

  # Mapa
  output$mapa <- renderLeaflet({
    req(input$caso_obito_mapa)

    data_mapa_r <- reactive({
      data_mapa %>%
        select(estado, valor = input$caso_obito_mapa, latitude, longitude)
    })

      leaflet(data_mapa_r()) %>% addTiles() %>%
        addCircles(lng = ~longitude, lat = ~latitude, weight = 1,
                   radius = ~valor*as.numeric(if_else(input$caso_obito_mapa == "casosAcumulado",
                                                       1,
                                                       10)),
                   popup = ~paste0("Estado: ", estado,
                                   if_else(input$caso_obito_mapa == "casosAcumulado",
                                           "<br> Casos: ",
                                           "<br> Obitos: "),
                                   valor),
                   color = if_else(input$caso_obito_mapa == "casosAcumulado",
                                   "blue", "red"))



  })



  # Análise
  # ANOVA
  output$anova <- renderPlot({
    req(input$caso_obito_inferencia)

    data_anova <- reactive({
      data %>% select(dia_semana, valor = input$caso_obito_inferencia)
      })

    ggstatsplot::ggwithinstats(
      data = data_anova(),
      x = dia_semana,
      y = valor,
      pairwise.comparisons = TRUE, # show pairwise comparison test results
      title = "Comparação da média de casos/óbitos de COVID-19 por dia da semana",
      ggtheme = theme_classic(),
      ggstatsplot.layer = FALSE,
      messages = FALSE,
      bf.message = FALSE,
      ylab = "Casos/Óbitos novos de Covid",
      xlab = "Dias da Semana")

  })


}

shinyApp(ui, server)
