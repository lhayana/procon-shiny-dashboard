library('data.table')
library('dplyr')
library('ggplot2')
library('plotly')
library('shiny')
library('shinyWidgets')
library('shinydashboard') #Personaliza dashboard

dados = read.csv("C:/Users/Lhayana/Downloads/dashboard_com_r-dados/dashboard_com_r-dados/dados_limpos.csv")

media_chamados_ano = dados %>%
                      group_by(anocalendario) %>%
                      summarise(qtd_chamados = n()) %>%
                      summarise(medias_chamado_ano = mean(qtd_chamados)) %>%
                      as.integer()

cabecalho = dashboardHeader(title = "Dashboard Procon")
barra_lateral = dashboardSidebar(width = '230px',
                                 sidebarMenu(
                                   menuItem('Dashboard', tabName = 'dashboard',
                                            icon=icon('dashboard')),
                                   menuItem('Informações', tabName = 'infos',
                                            icon=icon('info-circle'))
                                 ))
painel_principal = dashboardBody(
  
  tabItems(
    tabItem(tabName = 'infos',
            h1("Informações"),
            infoBox(title= 'Contato', icon=icon('envelope-square'),
                    subtitle = 'Para mais informações, entre em contato em 123@mail.com')),
    tabItem(tabName = 'dashboard',
                      fluidRow(
              valueBox(subtitle = 'Registros', value = nrow(dados), 
                       icon=icon('database')),
              valueBox(subtitle = 'Reclamações por Ano',
                      value=media_chamados_ano,
                      icon=icon("list")),
              valueBoxOutput(outputId = 'qtdUf')
            ),
            
            fluidRow(
              column(width=12,
                     box(title="Filtros", width="100%",
                      column(width=12, box(width = '100%',
                         awesomeCheckboxGroup(inputId = 'select_UF', label = 'Estados:',
                                            choices = c('Todos',unique(dados$UF)), 
                                            selected = 'Todos', inline=T))
                        ),
                      column(width = 6, box(width = '100%',
                             dateRangeInput(inputId = 'data_abertura',
                                            label = 'Data Abertura:',
                                            format = 'dd-mm-yyyy',
                                            start = min(as.Date(dados$DataAbertura)),
                                            end = max(as.Date(dados$DataAbertura))))
                        ),
                      column(width = 6, box(width = '100%',
                                             selectizeInput(inputId = 'assunto',
                                                            label = 'Descrição Assunto:',
                                                            choices = c('Todos', unique(dados$DescricaoAssunto)),
                                                            multiple = TRUE, options = list(maxItems = 5),
                                                            selected = 'Todos'))
                      )
                     ) ##final box
              )
            ), ##final linha
            fluidRow(
              column(width = 12,
                     box(width = '100%',
                     plotlyOutput(outputId = 'data', width='100%'),
                     textOutput(outputId = 'descUf')))
            ),
          
            fluidRow(
              column(width = 6,
                     box(width = '100%',
                         plotlyOutput(outputId = 'uf'))),
              column(width = 6,
                     box(width = '100%',
                         plotlyOutput(outputId = 'atendida')))
            ),
            
            fluidRow(
              column(width = 12,
                     box(width = '100%',
                         plotlyOutput(outputId = 'atendidaAno')))
            )
            )
  ),
  
  
  
)

ui = dashboardPage(header = cabecalho,
                   sidebar = barra_lateral,
                   body = painel_principal)

### Old UI without dashboard and widgets libraries

# ui2 <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar
#     sidebarLayout(
#         sidebarPanel(
#             checkboxGroupInput(inputId = 'select_UF', label = 'Estados:',
#                                choices = c('Todos',unique(dados$UF)), 
#                                selected = 'Todos'),
#             dateRangeInput(inputId = 'data_abertura',
#                            label = 'Data Abertura:',
#                            format = 'dd-mm-yyyy',
#                            start = min(as.Date(dados$DataAbertura)),
#                            end = max(as.Date(dados$DataAbertura))),
#             selectizeInput(inputId = 'assunto',
#                         label = 'Descrição Assunto:',
#                         choices = c('Todos', unique(dados$DescricaoAssunto)),
#                         multiple = TRUE, options = list(maxItems = 5),
#                         selected = 'Todos')
#         ),
# 
#         #Painel principal - Output dos gráficos
#         mainPanel(
#            textOutput(outputId = 'descData'),
#            plotlyOutput(outputId = 'data'),
#            textOutput(outputId = 'descUf'),
#            plotlyOutput(outputId = 'uf'),
#            plotlyOutput(outputId = 'atendida'),
#            plotlyOutput(outputId = 'atendidaAno')
#         )
#     )
# )

# Server que irá rodar.
server <- function(input, output) {

  # Põe filtros para funcionar
  dados_selecionados = reactive({
    if(!'Todos' %in% input$select_UF){
      dados = dados%>%
              filter(UF %in% input$select_UF)
    }
    
    if(!'Todos' %in% input$assunto){
        dados = dados%>%
          filter(DescricaoAssunto %in% input$assunto)
      }
    
    print(dados$DataAbertura)
    dados = dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] &
                      as.Date(DataAbertura) <= input$data_abertura[2])
    dados
  })
  
  output$listaUF = renderPrint({
    unique(dados_selecionados()$UF)
  })
  
  # Inputs dos gráficos
  output$data = renderPlotly({
    ggplotly(
      data.frame(table(as.Date(dados_selecionados()$DataArquivamento))) %>%
      rename(Data = Var1, Qtd = Freq) %>%
      ggplot(aes(as.Date(Data), Qtd)) +
      geom_line(group=1) +
      theme_bw() +
      ggtitle('Quantidade de reclamações por ano-mês') +
      scale_x_date(date_labels = '%b-%Y', breaks='1 years')
    )
  })
  
  output$uf = renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$UF)) %>%
        rename(UF = Var1, Qtd=Freq) %>%
        ggplot(aes(x=reorder(UF,Qtd), y=Qtd, 
                   text=paste('UF:', UF, "<br>", "QTD:", Qtd)))+
        geom_bar(fill='blue', stat='identity')+
        coord_flip()+
        xlab('UF')+
        theme_bw()+
        ggtitle('Quantidade de reclamações por UF')
    )
  })
  
  output$atendida = renderPlotly({
    ggplotly(
      ggplot(dados_selecionados()) +
        geom_bar(aes(Atendida), fill = c('red', 'green'), stat = 'count') +
        ylab('Quantidade') +
        theme_bw() +
        ggtitle("Quantidade de chamados atendidos")
    )
  })
  
  output$atendidaAno = renderPlotly({
    ggplotly(
      data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>% 
        rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
        ggplot() + 
        geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), stat = 'identity', position = position_dodge2( )) +
        theme_bw() +
        ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')
    )
  })
  
  output$descData = renderText({
    paste("Gráfico com a quantidade de reclamações feitos entre: ",
          as.Date(min(dados_selecionados()$DataAbertura)), "e",
          as.Date(max(dados_selecionados()$DataAbertura)))
  })
  
  output$descUf = renderText({
    estados = paste(unique(dados_selecionados()$UF), collapse = ',')
    paste("Gráfico com a quantidade de reclamações feitos pelas UF's: ", estados)
  })
  
  output$qtdUf = renderValueBox({
    valueBox(value=length(unique(dados_selecionados()$UF)),
             subtitle = 'UFs selecionadas', icon=icon('map-marker') )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
