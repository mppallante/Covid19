#############################################################################
# Nome: COVID-19 (Brasil). Competição da Escola Livre de Ai                 #
# Desenvolvido por: Equipe JMR20 Contra-Vírus
# Matheus Pina Pallante, Julio Cesar Esteves e Romulo Siqueira Santos       #
# Direitos: Código Livre para Contribuição.         #
#############################################################################

# O Projeto tem como Objetivo:
#   Apresentar um evolutivo analítico sobre os casos de Covid-19 no Brasil.
#   Em base a resultados de exames laboratoriais coletados para casos suspeitos 
# prever Positivo ou Negativo.
#   
# Serão Aplicados Modelos como:
#   Forecast
#   Agrupamento Knn
#   Regressão Logistica


# Pacotes -----------------------------------------------------------------

library(shiny)
library(bs4Dash)
library(readr)
library(data.table)
library(tidyr)
library(dplyr)
library(plotly)
library(echarts4r)
library(leaflet)
library(prophet)
library(shinyWidgets)
library(formattable)
library(dygraphs)
library(rJava)
library(readxl)
library(caret)
library(caTools)
library(e1071)

# User Interface ----------------------------------------------------------


ui = bs4DashPage(
  old_school = FALSE,
  sidebar_collapsed = FALSE,
  controlbar_collapsed = TRUE,
  enable_preloader = TRUE,
  loading_duration = 5,
  loading_background = "#1C1C1C",
  # Nome do Dashboard
  title = "COVID-19",
  # Menu Superior
  navbar = bs4DashNavbar(
    skin = 'light'
  ),
  # Menu Lateral Esquerdo
  sidebar = bs4DashSidebar(
    skin = "light",
    status = "primary",
    title = "COVID-19",
    src = 'https://www.monmouth.edu/covid-19/wp-content/uploads/sites/770/2020/03/cdc-w9KEokhajKw-unsplash.jpg',
    brandColor = "gray-light",
    bs4SidebarMenu(
      # Páginas do Dashboard
      bs4SidebarHeader("Análise Descritiva"),
      bs4SidebarMenuItem(
        startExpanded = T,
        tabName = "descritive",
        icon = "chart-pie",
        text = "Coronavírus no Brasil"
      ),
      bs4SidebarHeader("Análise Estatística"),
      bs4SidebarMenuItem(
        tabName = "statistic",
        icon = "chart-line",
        text = "Projeção de Contágios"
      ),
      bs4SidebarMenuItem(
        tabName = "knn",
        icon = "project-diagram",
        text = "Riscos de Disseminação"
      ),
      bs4SidebarMenuItem(
        tabName = "logistica",
        icon = "file-medical-alt",
        text = "Predição de Diagnósticos"
      ),
      #Detalhes sobre o Dashboard
      # Sobre o COVID19
      bs4SidebarHeader("Informações"),
      bs4SidebarMenuItem(
        tabName = "Projeto",
        icon = "file-alt",
        text = "Detalhes sobre o Projeto"
      ),
      bs4SidebarMenuItem(
        tabName = "about",
        icon = "info",
        text = "Aplicação"
      )
    )
  ),
  # Menu de Controles Lateral Direito
  # controlbar = bs4DashControlbar(
  #   width = 300,
  #   skin = 'light',
  #   disable = TRUE
  # ),
  # Footer
  footer = bs4DashFooter(
    copyrights = a(
      href = "https://github.com/romsiq/Projetos", 
      target = "_blank", "Código Livre para Contribuição."
    ),
    right_text = lubridate::year(Sys.time())
  ), 
  # Corpo do Dahboard
  body = bs4DashBody(
    bs4TabItems(
      # Página Inicial
      bs4TabItem(
        tabName = 'descritive',
        fluidPage(
          # Caixa de Informações Brasil (Confirmados e Mortes)
          bs4Card(title = 'Indicadores de Casos Confirmados, Recuperados e Mortes',
                  status = 'primary', width = NULL, closable = F, maximizable = F, collapsible = F,
                  fluidRow(
                    bs4InfoBoxOutput('ConfirmedBR', width = 6),
                    bs4InfoBoxOutput('DeathBR', width = 6),
                    bs4InfoBoxOutput('indBR', width = 6),
                    bs4InfoBoxOutput('recupBR', width = 6)
                  )),
          # Mapa COVID-19 (Volumetria por Cidades)
          bs4Card(title = 'Contágio nos Estados',
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F, height = 600,
                  leafletOutput('GeoBrasil', width = '100%', height = '100%')),
          # Volumetria por Estado (Confirmados e Mortes)
          bs4Card(title = 'Casos Confirmados versus Mortes por Estado', height = 500,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  plotlyOutput('NumEstado', width = '100%', height = "100%")),
          # Volumetria por Estado (Confirmados e Mortes)
          bs4Card(title = 'Evolutivo de Contágio por Estado', height = 500,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  echarts4rOutput('TimeEstado', width = '100%', height = "100%"))
        )
      ),
      # Análises Estatísticas
      bs4TabItem(
        tabName = 'statistic',
        fluidRow(
          column(width = 11,
                 # Previsão e Comportamento do COVID-19 no Brasil para os proximos 15 dias (Casos)
                 bs4Card(title = 'Projeção de Contágios para os próximos 15 dias', height = 350,
                         status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                         dygraphOutput('ConfirmedE_BR', width = '100%', height = '100%')),
                 # Previsão e Comportamento do COVID-19 no Brasil para os proximos 15 dias (Mortes)
                 bs4Card(title = 'Projeção de Mortes para os próximos 15 dias', height = 350,
                         status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                         dygraphOutput('DeathsE_BR', width = '100%', height = '100%'))
          ),
          column(width = 1,
                 prettyRadioButtons(
                   inputId = "Estados",
                   label = "Localiddade:", 
                   choices = c("BRASIL","SP","RJ","AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RN","RO","RR","RS","SC","SE","TO"),
                   selected = "BRASIL",
                   status = "primary",
                   animation = "smooth"
                 ))
        )
      ),
      # Análises de Agrupamento
      bs4TabItem(
        tabName = 'knn',
        fluidPage(
          # Mapa Clusterizado (Agrupamento) de locais confirmados para o COVID-19
          bs4Card(title = 'Concentração de Casos Confirmados (Agrupamentos) ', height = 450, 
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  leafletOutput('ClusterBrasil', width = '100%', height = '100%')),
          # Tabela de Estatisticas e Cluster
          bs4Card(title = 'Informações sobre os Agrupamentos', height = 220,
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  formattableOutput('ClusterTable', width = '100%', height = '100%'))
        )
      ),
      # Predição Exames Laboratoriais
      bs4TabItem(
        tabName = 'logistica',
        fluidPage(
          # Regressão Logistica - Casos confirmados em base a exames laboratoriais
          bs4Card(title = 'Predição de Resultados (Positivos ou Negativos) em base a Exames Laboratoriais', height = 450, 
                  status = 'primary', width = NULL, closable = F, maximizable = T, collapsible = F,
                  formattableOutput('predicaoexames', width = '100%', height = '100%')),
        )
      ),
      # Documentos sobre o Dashboard
      bs4TabItem(
        tabName = 'Projeto',
        fluidPage(
          bs4Card(width = NULL, closable = F, maximizable = T, status = "primary",
                  title = "Detalhes sobre o Projeto",
                  tags$iframe(style='height:500px; width:100%; scrolling=yes',
                              src='https://github.com/romsiq/Covid19/blob/master/Projeto_Coronavirus_EscolaLivreIA.pdf')
                  )
        )
      ),
      
      # Sobre
      bs4TabItem(
        tabName = 'about',
        fluidPage(
          bs4Jumbotron(
            title = "COVID-19",
            lead = "Desenvolvido para análise dos casos de COVID-19 no Brasil - 2020",
            status = "primary",
            btn_name = 'COVID-19',
            href = "https://www.google.com/search?q=covid-19&rlz=1C1SQJL_pt-BRBR864BR864&oq=cov&aqs=chrome.0.69i59j69i57j69i60l3.933j0j1&sourceid=chrome&ie=UTF-8"
          )
        )
      )
    )
  ) 
)

# Server Side -------------------------------------------------------------

server <- function(input, output, session) {
  # Base de Dados
  # Série Temporal - Brasil
  citiesTimes <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities-time.csv'))
  # Cidades
  cities <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv'))
  # GPS Cidades
  citiesGPS <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-gps.csv'))
  # Estados
  states <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv'))
  # Total Brasil - Estados
  brasil <- read_csv(url('https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv'))
  # Base de Recuperados Global
  recup_global <- read_csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv'))
  # Base do Hospital Albert Einstein - #Base encontra-se em: https://github.com/romsiq/Covid19
  exames <- read_excel('/Users/romsiq/Downloads/dataset_hospital.xlsx') 
  #Seleção de variáveis que serão utilizadas no experimento de predição
  exames <- exames %>% select('Influenza B','Respiratory Syncytial Virus',
                       'CoronavirusNL63','Coronavirus HKU1','Rhinovirus/Enterovirus','Chlamydophila pneumoniae','Adenovirus',
                       'Coronavirus229E','CoronavirusOC43','Inf A H1N1 2009','Metapneumovirus','Influenza B, rapid test',
                       'Influenza A, rapid test','Strepto A','Hemoglobin','Red blood Cells','Hematocrit','Platelets',
                       'Patient age quantile','Basophils','Lymphocytes','Leukocytes','Hematocrit','SARS-Cov-2 exam result')
  
  #Tratamento dos Dados
  exames$`Influenza B` <- replace_na(as.character(exames$`Influenza B`), 'Nulo') 
  exames$`Respiratory Syncytial Virus` <- replace_na(as.character(exames$`Respiratory Syncytial Virus`), 'Nulo') 
  exames$`CoronavirusNL63` <- replace_na(as.character(exames$`CoronavirusNL63`), 'Nulo') 
  exames$`Coronavirus HKU1` <- replace_na(as.character(exames$`Coronavirus HKU1`), 'Nulo') 
  exames$`Rhinovirus/Enterovirus` <- replace_na(as.character(exames$`Rhinovirus/Enterovirus`), 'Nulo') 
  exames$`Chlamydophila pneumoniae` <- replace_na(as.character(exames$`Chlamydophila pneumoniae`), 'Nulo') 
  exames$`Adenovirus` <- replace_na(as.character(exames$`Adenovirus`), 'Nulo') 
  exames$`Coronavirus229E` <- replace_na(as.character(exames$`Coronavirus229E`), 'Nulo') 
  exames$`CoronavirusOC43` <- replace_na(as.character(exames$`CoronavirusOC43`), 'Nulo') 
  exames$`Inf A H1N1 2009` <- replace_na(as.character(exames$`Inf A H1N1 2009`), 'Nulo') 
  exames$`Metapneumovirus` <- replace_na(as.character(exames$`Metapneumovirus`), 'Nulo') 
  exames$`Influenza B, rapid test` <- replace_na(as.character(exames$`Influenza B, rapid test`), 'Nulo') 
  exames$`Influenza A, rapid test` <- replace_na(as.character(exames$`Influenza A, rapid test`), 'Nulo') 
  exames$`Strepto A` <- replace_na(as.character(exames$`Strepto A`), 'Nulo') 
  #Replace NA para 0 sobre as variáveis numéricas
  exames[is.na(exames)] <- 0
  #Trasnformação das variáveis (Auxílio Dummies)
  exames$`Influenza B` = replace(exames$`Influenza B`, exames$`Influenza B` == 'Nulo', 3)
  exames$`Influenza B` = replace(exames$`Influenza B`, exames$`Influenza B` == 'not_detected', 0)
  exames$`Influenza B` = replace(exames$`Influenza B`, exames$`Influenza B` == 'detected', 1)
  exames$`Respiratory Syncytial Virus` = replace(exames$`Respiratory Syncytial Virus`, exames$`Respiratory Syncytial Virus` == 'Nulo', 3)
  exames$`Respiratory Syncytial Virus` = replace(exames$`Respiratory Syncytial Virus`, exames$`Respiratory Syncytial Virus` == 'not_detected', 0)
  exames$`Respiratory Syncytial Virus` = replace(exames$`Respiratory Syncytial Virus`, exames$`Respiratory Syncytial Virus` == 'detected', 1)
  exames$`CoronavirusNL63` = replace(exames$`CoronavirusNL63`, exames$`CoronavirusNL63` == 'Nulo', 3)
  exames$`CoronavirusNL63` = replace(exames$`CoronavirusNL63`, exames$`CoronavirusNL63` == 'not_detected', 0)
  exames$`CoronavirusNL63` = replace(exames$`CoronavirusNL63`, exames$`CoronavirusNL63` == 'detected', 1)
  exames$`Coronavirus HKU1` = replace(exames$`Coronavirus HKU1`, exames$`Coronavirus HKU1` == 'Nulo', 3)
  exames$`Coronavirus HKU1` = replace(exames$`Coronavirus HKU1`, exames$`Coronavirus HKU1` == 'not_detected', 0)
  exames$`Coronavirus HKU1` = replace(exames$`Coronavirus HKU1`, exames$`Coronavirus HKU1` == 'detected', 1)
  exames$`Rhinovirus/Enterovirus` = replace(exames$`Rhinovirus/Enterovirus`, exames$`Rhinovirus/Enterovirus` == 'Nulo', 3)
  exames$`Rhinovirus/Enterovirus` = replace(exames$`Rhinovirus/Enterovirus`, exames$`Rhinovirus/Enterovirus` == 'not_detected', 0)
  exames$`Rhinovirus/Enterovirus` = replace(exames$`Rhinovirus/Enterovirus`, exames$`Rhinovirus/Enterovirus` == 'detected', 1)
  exames$`Chlamydophila pneumoniae` = replace(exames$`Chlamydophila pneumoniae`, exames$`Chlamydophila pneumoniae` == 'Nulo', 3)
  exames$`Chlamydophila pneumoniae` = replace(exames$`Chlamydophila pneumoniae`, exames$`Chlamydophila pneumoniae` == 'not_detected', 0)
  exames$`Chlamydophila pneumoniae` = replace(exames$`Chlamydophila pneumoniae`, exames$`Chlamydophila pneumoniae` == 'detected', 1)
  exames$`Adenovirus` = replace(exames$`Adenovirus`, exames$`Adenovirus` == 'Nulo', 3)
  exames$`Adenovirus` = replace(exames$`Adenovirus`, exames$`Adenovirus` == 'not_detected', 0)
  exames$`Adenovirus` = replace(exames$`Adenovirus`, exames$`Adenovirus` == 'detected', 1)
  exames$`Coronavirus229E` = replace(exames$`Coronavirus229E`, exames$`Coronavirus229E` == 'Nulo', 3)
  exames$`Coronavirus229E` = replace(exames$`Coronavirus229E`, exames$`Coronavirus229E` == 'not_detected', 0)
  exames$`Coronavirus229E` = replace(exames$`Coronavirus229E`, exames$`Coronavirus229E` == 'detected', 1)
  exames$`CoronavirusOC43` = replace(exames$`CoronavirusOC43`, exames$`CoronavirusOC43` == 'Nulo', 3)
  exames$`CoronavirusOC43` = replace(exames$`CoronavirusOC43`, exames$`CoronavirusOC43` == 'not_detected', 0)
  exames$`CoronavirusOC43` = replace(exames$`CoronavirusOC43`, exames$`CoronavirusOC43` == 'detected', 1)
  exames$`Inf A H1N1 2009` = replace(exames$`Inf A H1N1 2009`, exames$`Inf A H1N1 2009` == 'Nulo', 3)
  exames$`Inf A H1N1 2009` = replace(exames$`Inf A H1N1 2009`, exames$`Inf A H1N1 2009` == 'not_detected', 0)
  exames$`Inf A H1N1 2009` = replace(exames$`Inf A H1N1 2009`, exames$`Inf A H1N1 2009` == 'detected', 1)
  exames$`Metapneumovirus` = replace(exames$`Metapneumovirus`, exames$`Metapneumovirus` == 'Nulo', 3)
  exames$`Metapneumovirus` = replace(exames$`Metapneumovirus`, exames$`Metapneumovirus` == 'not_detected', 0)
  exames$`Metapneumovirus` = replace(exames$`Metapneumovirus`, exames$`Metapneumovirus` == 'detected', 1)
  exames$`Influenza B, rapid test` = replace(exames$`Influenza B, rapid test`, exames$`Influenza B, rapid test` == 'Nulo', 3)
  exames$`Influenza B, rapid test` = replace(exames$`Influenza B, rapid test`, exames$`Influenza B, rapid test` == 'negative', 0)
  exames$`Influenza B, rapid test` = replace(exames$`Influenza B, rapid test`, exames$`Influenza B, rapid test` == 'positive', 1)
  exames$`Influenza A, rapid test` = replace(exames$`Influenza A, rapid test`, exames$`Influenza A, rapid test` == 'Nulo', 3)
  exames$`Influenza A, rapid test` = replace(exames$`Influenza A, rapid test`, exames$`Influenza A, rapid test` == 'negative', 0)
  exames$`Influenza A, rapid test` = replace(exames$`Influenza A, rapid test`, exames$`Influenza A, rapid test` == 'positive', 1)
  exames$`Strepto A` = replace(exames$`Strepto A`, exames$`Strepto A` == 'Nulo', 3)
  exames$`Strepto A` = replace(exames$`Strepto A`, exames$`Strepto A` == 'negative', 0)
  exames$`Strepto A` = replace(exames$`Strepto A`, exames$`Strepto A` == 'positive', 1)
  exames$`SARS-Cov-2 exam result` = replace(exames$`SARS-Cov-2 exam result`, exames$`SARS-Cov-2 exam result` == 'negative', 0)
  exames$`SARS-Cov-2 exam result` = replace(exames$`SARS-Cov-2 exam result`, exames$`SARS-Cov-2 exam result` == 'positive', 1)
  #Transformação das variáveis em Numéricas
  exames$`Influenza B` = as.numeric(exames$`Influenza B`)
  exames$`Respiratory Syncytial Virus` = as.numeric(exames$`Respiratory Syncytial Virus`)
  exames$CoronavirusNL63 = as.numeric(exames$CoronavirusNL63)
  exames$`Coronavirus HKU1` = as.numeric(exames$`Coronavirus HKU1`)
  exames$`Rhinovirus/Enterovirus` = as.numeric(exames$`Rhinovirus/Enterovirus`)
  exames$`Chlamydophila pneumoniae` = as.numeric(exames$`Chlamydophila pneumoniae`)
  exames$Adenovirus = as.numeric(exames$Adenovirus)
  exames$Coronavirus229E = as.numeric(exames$Coronavirus229E)
  exames$CoronavirusOC43 = as.numeric(exames$CoronavirusOC43)
  exames$`Inf A H1N1 2009` = as.numeric(exames$`Inf A H1N1 2009`)
  exames$Metapneumovirus = as.numeric(exames$Metapneumovirus)
  exames$`Influenza B, rapid test` = as.numeric(exames$`Influenza B, rapid test`)
  exames$`Influenza A, rapid test` = as.numeric(exames$`Influenza A, rapid test`)
  exames$`Strepto A` = as.numeric(exames$`Strepto A`)
  exames$`SARS-Cov-2 exam result` = as.numeric(exames$`SARS-Cov-2 exam result`)
    
 # Caixa de Informações - Brasil (Confirmados e Mortes)
  output$ConfirmedBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Confirmados',
      value = subset(brasil$totalCasesMS, brasil$state == 'TOTAL'),
      icon = 'user-check',
      status = 'warning'
    )
  })
  output$DeathBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Mortes',
      value = subset(brasil$deaths, brasil$state == 'TOTAL'),
      icon = 'chart-line',
      status = 'danger'
    )
  })
  output$indBR <- renderbs4InfoBox({
    bs4InfoBox(
      title = 'Índice Mortalidade',
      value = subset(round(brasil$deaths/brasil$totalCasesMS * 100, digits = 2), brasil$state == 'TOTAL'),
      icon = 'check',
      status = 'danger'
    )
  })
  output$recupBR <- renderbs4InfoBox({
    recuperados <- subset(recup_global, recup_global$'Country/Region' == 'Brazil')
    bs4InfoBox(
      title = 'Recuperados',
      value = recuperados[1,length(recuperados)],
      icon = 'check',
      status = 'success'
    )
  })

  # Mapa - Distribuição do COVID-19 no Brasil
  output$GeoBrasil <- renderLeaflet(
    citiesGPS %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 weight = ~sqrt(total),
                 color = 'FireBrick',
                 opacity = .8,
                 popup = ~paste0('<b>Localidade: </b>', name,
                                 '<br>',
                                 '<b>Volumetria: </b>', total)
      )
  )
  # Volumetria por Estados (COVID-19)
  output$NumEstado <- renderPlotly({
    brasil <- subset(brasil, brasil$state != 'TOTAL')
    brasil <- arrange(brasil, desc(totalCasesMS))
    names(brasil)[2] <- 'Estados'
    plot_ly(brasil, x = ~Estados, y = ~totalCasesMS, type = 'bar', name = 'Confirmados',
            text = ~totalCasesMS, textposition = 'auto', marker = list(color = 'SteelBlue')) %>%
      add_trace(y = ~deaths, name = 'Mortes', text = ~deaths, textposition = 'auto', marker = list(color = 'FireBrick')) %>%
      layout(yaxis = list(title = 'Volumetria'), barmode = 'group')
  })
  # Evolutivo por Tempo - Estados
  output$TimeEstado <- renderEcharts4r({
    states$date <- lubridate::ymd(states$date)
    states$state <- replace(states$state, states$state == 'TOTAL', 'BRASIL')
    states$state <- factor(states$state)
    states <- arrange(states, desc(totalCases))
    states %>%
      group_by(state) %>%
      e_charts(date) %>%
      e_line(totalCases) %>%
      e_labels() %>%
      e_tooltip(trigger = "item")
  })
  # Previsão do COVID-19 no Brasil (Casos e Mortes) - Projeção utilizando Facebook Phophet
  output$ConfirmedE_BR <- renderDygraph({
    time <- states
    time$state <- replace(time$state, time$state == 'TOTAL', 'BRASIL')
    time <- subset(time, time$state == input$Estados)
    time <- data.frame(
      ds = lubridate::ymd(time$date),
      y = time$totalCases
    )
    m <- prophet(time)
    future <- make_future_dataframe(m, periods = 15)
    forecast <- predict(m, future)
    dyplot.prophet(m, forecast)
  })
  output$DeathsE_BR <- renderDygraph({
    time <- states
    time$state <- replace(time$state, time$state == 'TOTAL', 'BRASIL')
    time <- subset(time, time$state == input$Estados)
    time <- data.frame(
      ds = lubridate::ymd(time$date),
      y = time$deaths
    )
    m <- prophet(time)
    future <- make_future_dataframe(m, periods = 15)
    forecast <- predict(m, future)
    dyplot.prophet(m, forecast)
  })
  # PREPARAÇÃO PARA O AGRUPAMENTO - Clusters
  nei <- citiesGPS
  nei <- subset(nei, nei$type == '1')
  km <- kmeans(nei[5], centers = 3)
  nei$Clusters <- km$cluster
  centros <- data.frame(km$centers)
  centros$Clusters <- c(1,2,3)
  centros <- arrange(centros, total)
  centros$Risco <- c('Concentração Baixa', 'Concentração Média', 'Concentração Alta')
  pal <- colorFactor(c('Gold','DarkOrange','FireBrick'), levels = centros$Clusters)
  # Mapa - Cluster do Brasil - Riscos
  output$ClusterBrasil <- renderLeaflet({
    nei %>%
      leaflet() %>%
      addTiles() %>%
      addCircles(lng = ~lon,
                 lat = ~lat,
                 weight = ~sqrt(total),
                 color = ~pal(nei$Clusters),
                 opacity = 1,
                 label = ~name
      ) 
  })
  # Tabela - Dados do Agrupamento
  output$ClusterTable <- renderFormattable({
    names(centros)[1] <- 'Média de Casos Confirmados'
    formattable(centros,
                align = c("l", rep("r", NCOL(centros) - 1)),
                list(
                  `total` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                  `Clusters` = formatter("span", style = ~ style(color = c('Gold','DarkOrange','FireBrick'), font.weight = "bold")),
                  `Risco` = formatter("span", style = ~ style(color = c('Gold','DarkOrange','FireBrick'), font.weight = "bold"))
                ))
  })
  
  # Previsão em Base aos Exames Labortoriais - Positivo ou Negativo - Regressão Logistica
  output$predicaoexames <- renderFormattable({
    #Adequando a base com 50% de dados positivos e 50% com dados negativos
    base_negativo <-subset(exames, exames$`SARS-Cov-2 exam result` == 0)
    base_positivo <-subset(exames, exames$`SARS-Cov-2 exam result` == 1)
    base_negativo <- base_negativo[1:558,]  
    exames <- rbind(base_negativo, base_positivo)
    #Modelo de Predição Linear Binomial
    modelo = glm(`SARS-Cov-2 exam result` ~ `Influenza B` + `Respiratory Syncytial Virus`+ CoronavirusNL63 + 
                   `Coronavirus HKU1` + `Rhinovirus/Enterovirus` + `Chlamydophila pneumoniae` + Adenovirus + 
                   Coronavirus229E + CoronavirusOC43 + `Inf A H1N1 2009` + Metapneumovirus + `Influenza B, rapid test` + 
                   `Influenza A, rapid test` + `Strepto A` + Hemoglobin + `Red blood Cells` + Hematocrit + Platelets + 
                   `Patient age quantile` + Basophils + Lymphocytes + Leukocytes + Hematocrit, data=exames, family =binomial) 
    #Base de Treino e Teste - Split 
    trainIndex <- sample.split(exames$`SARS-Cov-2 exam result`,SplitRatio = 0.75)
    examesTrain <- subset(exames,trainIndex==T)
    examesTest <- subset(exames,trainIndex==F)
    #Executando a Predição
    predicao <- predict(modelo, examesTest, type = 'response')
    predicao <- ifelse(predicao > 0.5, 1,0) # Um valor ajustado é a previsão de um modelo estatístico do valor médio da resposta quando você insere os valores dos preditores.
    predicao <- as.data.frame(predicao)
    #Matriz de Confusão a qual será disponibilizada no Dashboard
    cm <- confusionMatrix(factor(predicao$predicao), factor(examesTest$`SARS-Cov-2 exam result`)) #Matriz de Confusão
    #Plotando Matriz de Confusão em formto tabela.
    matriz <- cm$table
    matriz <- as.data.frame(matriz)
    formattable(matriz)
    
  })  
}

# Run Application Options/Settings ----------------------------------------

shinyApp(ui = ui, server = server)
