#
# Calculadora epidêmica
#
#

library(shiny)
library(plotly)  
library(tidyverse)
library(magrittr)
library(lubridate)
library(tibble)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(rvest)
library(gt)
library(deSolve)
library(EpiEstim)
library(incidence)
library(distcrete)
library(epitrix)
library(projections)
library(scales)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
tabsetPanel(
  tabPanel("Início", 
  

  # Application title
  tags$h1("Calculadora Epidêmica"),
  
  fluidRow(
    
    column(12,
  # Application title
  #titlePanel("Calculadora Epidêmica"),
  ## Texto ##
  tags$p(
    "Esta calculadora implementa o modelo compartimental clássico para doenças 
    infecciosas, chamado modelo", tags$strong("SEIR:")
  ), 
  
  tags$p(
    tags$strong("     Suscetível → Exposto → Infectado → Removido     ") 
  ),
  
  tags$p(
    "Trata-se de um modelo idealizado de espalhamento de uma epidemia, que segue sendo utilizado em
    pesquisas científicas (alguns exemplos podem ser encontrados em:",
    tags$a(href = "https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30260-9/fulltext", 
           "[Wu, et. al]"),
    tags$a(href = " https://cmmid.github.io/topics/covid19/current-patterns-transmission/wuhan-early-dynamics.html", 
           "[Kucharski et. al]"),
    tags$a(href = "https://www.medrxiv.org/content/10.1101/2020.03.21.20040022v1", 
           "[Arenas et. al]"),
    "."
  ),
  
  
  tags$p(
    "A dinâmica do modelo é caracterizada por um sistema de quatro equações equações
      diferenciais ordinárias que correspondem aos estágios de progressão da doença."
  ),
  
  tags$p("A calculadora foi criada utilizando como base o layout e o modelo
           disponível em:",
         tags$a(href = "http://gabgoh.github.io/COVID/index.html", 
                "http://gabgoh.github.io/COVID/index.html"),".",
         "Os parâmetros utilizados na simulação foram retirados da literatura, 
         utilizando dados de localidades onde a pandemia de COVID-19 se iniciou, como
         Wuhan, Hubei e Princess Diamond."
  )
  
  )), # Final fluid row
  #================================================================================#
  
  tags$hr(),
  
  #================================================================================#
  
  fluidRow(
    
    ## Coluna 1
    column(2,
           tags$h2(withMathJax(sprintf("Parâmetros populacionais e $R_0$"))),
           sliderInput(inputId = "N", label = "Tamanho da população", value = 1000, min = 1000, max = 1500000000, step = 2000),
           sliderInput(inputId = "I0", label = "Número inicial de infectados", value = 1, min = 1, max = 1000),
           sliderInput(inputId = "Rt", label = withMathJax(sprintf("Número Reprodutivo básico $R_0$")), value = 2.2, min = 0.01, max = 10, step = 0.01)
           
    ),
    
    ## Coluna 2
    column(2, offset = 0.75,
           tags$h2("Dinâmica da transmissão"),
           sliderInput(inputId = "time_values", label = "Tempo de simulação", value = 20, min = 2, max = 200),
           sliderInput(inputId = "theta", label ="Redução percentual na transmissão", value = 0, min = 0, max = 1, step = 0.01),
           sliderInput(inputId = "Tinf", label ="Tempo (dias) em que o paciente é infeccioso, Tinf", value = 2.9, min = 0, max = 24, step = 0.2),
           sliderInput(inputId = "Tinc", label ="Tempo (dias) de incubação, Tinc", value = 5.20, min = 0.15, max = 24, step = 0.2)
    ),
    
    
    ## ## Coluna 3
    column(7, offset = 1,
           plotlyOutput("seir_plot")
    )
    
  ), # Final da fluidRow
  
  tags$hr(),
  #================================================================================#
  
  # Dinâmica Clínica
  tags$h1("Dinâmica Clínica"),
  fluidRow(
    ## Coluna 1
    column(2,
           tags$h3("Estatísticas de Mortalidade"),
           sliderInput(inputId = "delta", label = "Taxa de letalidade (%)", value = 2, min = 0, max = 100, step = 0.01),
           sliderInput(inputId = "Tm", label = "Tempo do final do período de incubação até a morte (em dias)", value = 5.8, min = 5.8, max = 100, step = 0.01)
           
           ),
    
    ## Coluna 2
    column(2, offset = 0.75,
           tags$h3("Tempos de Recuperação"),
           sliderInput(inputId = "Tint", label = "Tempo de internamento hospitalar (em dias)", value = 0.1, min = 0.1, max = 100, step = 0.1),
           sliderInput(inputId = "Trec", label = "Tempo de recuperação para casos leves (em dias)", value = 0.1, min = 0.1, max = 100, step = 0.1)
    ),
    
    ## Coluna 3
    column(2, offset = 0.75,
           tags$h3("Estatísticas Assitenciais"),
           sliderInput(inputId = "tau", label = "Taxa de hospitalização (%)", value = 1, min = 0, max = 100, step = 0.01),
           sliderInput(inputId = "Thosp", label = "Tempo decorrido até a hospitalização (em dias)", value = 0.1, min = 0.1, max = 100, step = 0.1)
           
    ),
    
    ## ## Coluna 4
    column(6, offset = 0.5,
           plotlyOutput("fig")
    )
    
  ), # fim da fluidRow
  
  
  #================================================================================#
  tags$hr(),
  
  #================================================================================#
  tags$p(
    "Este é um modelo inicial para uso didático e está em constante atualização. 
    Mais detalhes serão adicionados em breve.",
    tags$a(href = "https://www.irrd.org/covid-19/",
           "Instituto para Redução de Risco  e Desastre de Pernambuco (IRRD/PE).")
  )
  ), # fim da tabPanel1 
  
  # Segunda aba
  tabPanel("Sobre o modelo", 
           
           fluidRow(
             
             column(11, offset = 0.75,
                    tags$h1("Modelo Compartimental SEIR"),
                    
                    tags$p("A dinâmica do modelo é caracterizada pelo conjunto de equações 
                  diferenciais ordinárias abaixo:")
                    ),
      
             #=============================================#
             column(4, align = "center", offset = 4,

             
             withMathJax(sprintf("$$\\frac{dS}{dt} = - \\frac{\\mathcal{R}_t}{T_{inf}} IS/N$$")),
             
             withMathJax(sprintf("$$\\frac{dE}{dt} =  \\frac{\\mathcal{R}_t}{T_{inf}} IS/N - T_{inc}^{-1} E$$")),
             
             withMathJax(sprintf("$$\\frac{dI}{dt} = T_{inc}^{-1} E - T_{inf}^{-1} I$$")),
             
             withMathJax(sprintf("$$\\frac{dR}{dt} = T_{inf}^{-1} I$$"))
             
           ),
           #=============================================#
           column(11, offset = 0.75,
                  tags$p(
                  "A dinâmica clínica neste modelo é uma formulação do modelo SEIR que simula a 
                  progressão da doença numa resolução mais alta a partir da subdivisão do",
                  
                  "compartimento R em pacientes com caso leve da doença (se recuperam sem a necessidade",
                  
                  "de internamento); caso moderado (pacientes que requerem hospitalização",
                  
                  "mas sobrevivem), e fatal (pacientes que requerem hospitalização, mas não sobrevivem).",
                  
                  
                  tags$p("Cada uma dessas variáveis segue a sua própria trajetória até
                  o resultado final, e a soma desses compartimentos resultam nos valores 
                  preditos pelo modelo SEIR. Note que, por simplicidade, assume-se que
                  todos os casos fatais são internados em hospitais imediatamente depois
                  do período infeccioso.")
                  ),
                  
                  tags$p("Daí, os compartimentos auxiliares, podem ser escritos como:")
                  
                  ), # fim da coluna 
           #=============================================#
           column(4, align = "center", offset = 4,
                  withMathJax(sprintf("$$\\frac{dR}{dt} = T_{inf}^{-1} I - T_{rec}^{-1} R - \\tau T_{hosp}^{-1} R$$")),
                  withMathJax(sprintf("$$\\frac{dH}{dt} = \\tau T_{hosp}^{-1} R - \\delta T_{m}^{-1} H - T_{int}^{-1} H$$")),
                  withMathJax(sprintf("$$\\frac{dRec}{dt} = T_{rec}^{-1} R + T_{int}^{-1} H$$")),
                  withMathJax(sprintf("$$\\frac{dM}{dt} = \\delta T_{m}^{-1} H$$"))
           ),
          #=============================================#
          
          column(11, offset = 0.75,
                 tags$h2("Parâmetros"),
                 
          tags$div(
            tags$ul(
              tags$li(withMathJax(sprintf("$\\mathcal{R}_t$: número reprodutivo básico ajustado.
                                          Pode ser escrito como $\\mathcal{R}_t = (1 - \\theta) R_0$,
                                          em que $\\theta$ é um fator de redução percentual na transmissão."))),
              tags$li(withMathJax(sprintf("$T_{inf}$: Tempo em dias em que o paciente é infeccioso"))),
              tags$li(withMathJax(sprintf("$T_{inc}$: Tempo de incubação em dias"))),
              tags$li(withMathJax(sprintf("$\\delta$: taxa de mortalidade"))),
              tags$li(withMathJax(sprintf("$\\tau$: taxa de internamento"))),
              tags$li(withMathJax(sprintf("$T_m$: tempo do final do período de incubação até a morte."))),
              tags$li(withMathJax(sprintf("$T_{int}$: tempo de internamento"))),
              tags$li(withMathJax(sprintf("$T_{rec}$: tempo de recuperação para casos leves"))),
              tags$li(withMathJax(sprintf("$T_{hosp}$: tempo decorrido até a hospitalização."))),
            )
          )
          )
          #=============================================#
          
          ) # fim da fluidRow
        
           ) # fim da tabPanel 2
  
) # Fim da tabsetPanel
) # Final da UI
#================================================================================#   



#================================================================================#   
# Server
#================================================================================#   
server <- function(input, output) {

    seir <- function(time, y, parms) {
        par <- as.list(c(parms,y))
        with(par, {
            dS <- - (((1-theta)*Rt)/Tinf) * I * S/N
            dE <- (((1-theta)*Rt)/Tinf) * I  * S/N - (1/Tinc) * E
            dI <- (1/Tinc) * E - (1/Tinf) * I
            dR <- (1/Tinf) * I
            list(c(dS, dE, dI, dR))
        })
    }
    

    
    # Parâmetros e simulação
    resultado_seir <- reactive({
        req(input$I0, input$N,input$time_values, input$Rt, input$theta, input$Tinf, input$Tinc)
        E0 = 0
        I0 = input$I0
        R0 = 0
        S0 = input$N 
        inicial <- c(S = S0, E = E0, I = I0, R = R0)
        parms <- c(N = input$N, Rt = input$Rt, Tinf = input$Tinf, 
                   Tinc = input$Tinc, theta = input$theta)
        ode(y = inicial, times = seq(1:input$time_values), 
            func = seir, parms = parms)
    }) 


    output$seir_plot <- renderPlotly({
      val <- as.data.frame(resultado_seir())
      seir_plot <-  plot_ly(val, 
                      x = ~ time, 
                      y = ~ R, 
                      type = 'bar', 
                      name = 'Removido')
      seir_plot <- seir_plot %>%  add_trace(y = ~ I, name = 'Infectado')
      seir_plot <- seir_plot %>%  add_trace(y = ~ E, name = 'Exposto')
      seir_plot <- seir_plot %>% layout(xaxis = list(title = "Tempo (em dias)"),
                            yaxis = list(title = 'Número de pessoas'),
                            barmode = 'stack')
      seir_plot
    })
    
# Modelo 2

    #===================================#
    seir2 <- function(time, y, parms) {
      par <- as.list(c(parms,y))
      with(par, {
        dS <- - (Rt/Tinf) * I * S/N
        dE <- (Rt/Tinf) * I * S/N - (1/Tinc) * E
        dI <- (1/Tinc) * E - (1/Tinf) * I
        dR <- (1/Tinf) * I - (1/Trec) * R -  tau/100 * (1/Thosp) * R
        dH <- tau/100 * (1/Thosp) * R - delta/100 * (1/Tm) * H - (1/Tint) * H
        dRec <- (1/Trec) * R + (1/Tint) * H
        dM <- delta/100 * (1/Tm) * H
        list(c(dS, dE, dI, dR, dRec, dH, dM))
      })
    }
    
    resultado_seir2 <- reactive({
      req(input$I0, input$N,input$time_values, input$Rt, input$theta, input$Tinf, 
          input$Tinc, input$delta, input$Tm, input$Tint, input$Trec, input$Trec,
          input$tau, input$Thosp)
      
      # Simulação
      init2 <- c(S = input$N, E = 0, I = input$I0, R = 0, Rec = 0, H = 0, M = 0)
      t2 <-  1:input$time_values
      parms2 <- c(N = 1002, Rt = input$Rt, Tinf = input$Tinf, 
                  Tinc = input$Tinc, Trec = input$Trec, Tint = input$Tint, 
                  delta = input$delta, tau = input$tau, Thosp = input$Thosp, Tm = input$Tm)
      ode(y =init2, times = t2, func = seir2, parms = parms2)
    })
    
    
    #==================================#
    
    
    output$fig <- renderPlotly({
      val2 <- as.data.frame(resultado_seir2())
      fig <-  plot_ly(val2, 
                      x = ~ time, 
                      y = ~ Rec, 
                      type = 'bar', 
                      name = 'Recuperados')
      fig <- fig %>%  add_trace(y = ~ H, name = 'Hospitalizados')
      fig <- fig %>%  add_trace(y = ~ M, name = 'Fatalidades')
      fig <- fig %>% layout(xaxis = list(title = "Tempo (em dias)"),
                            yaxis = list(title = 'Número de pessoas'),
                            barmode = 'stack')
      fig
    })
    
    
} # Fim do server

# Run the application 
shinyApp(ui = ui, server = server)



