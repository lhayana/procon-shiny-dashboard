library('data.table')
library('dplyr')
library('ggplot2')
library('plotly')
library('shiny')
library('shinyWidgets')
library('shinydashboard')

dados = read.csv("C:/Users/Lhayana/Downloads/dashboard_com_r-dados/dashboard_com_r-dados/dados_limpos.csv")

graph_atendidos = ggplot(dados) +
                  geom_bar(aes(Atendida), fill = c('red', 'green'), stat = 'count') +
                  ylab('Quantidade') +
                  theme_bw() +
                  ggtitle("Quantidade de chamados atendidos")

graph_atendidos = ggplotly(graph_atendidos)
graph_atendidos

grafico_uf = data.frame(table(dados$UF)) %>%
              rename(UF = Var1, Qtd=Freq) %>%
              ggplot(aes(x=reorder(UF,Qtd), y=Qtd, 
                         text=paste('UF:', UF, "<br>", "QTD:", Qtd)))+
              geom_bar(fill='blue', stat='identity')+
              coord_flip()+
              xlab('UF')+
              theme_bw()+
              ggtitle('Quantidade de reclamações por UF')

grafico_uf = ggplotly(grafico_uf, tooltip = 'text')
grafico_uf

#Ano-mês-dia

grafico_data = data.frame(table(as.Date(dados$DataArquivamento))) %>%
                rename(Data = Var1, Qtd = Freq) %>%
                ggplot(aes(as.Date(Data), Qtd)) +
                geom_line(group=1) +
                theme_bw() +
                ggtitle('Quantidade de reclamações por ano-mês')
                scale_x_date(date_labels = '%b-%Y', breaks='1 years')
  
grafico_data=ggplotly(grafico_data)
grafico_data

grafico_atendida_ano = data.frame(table(dados$anocalendario, dados$Atendida)) %>% 
    rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
    ggplot() + 
    geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), stat = 'identity', position = position_dodge2( )) +
    theme_bw() +
    ggtitle('Quantidade de Reclamações Atendidas(não) por Ano')

grafico_atendida_ano=ggplotly(grafico_atendida_ano)
grafico_atendida_ano
