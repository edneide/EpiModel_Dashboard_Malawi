library(shiny)
library(shinydashboard)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(shinythemes)
library(tidyverse)
library(scales)
library(shinyWidgets)

##--Loading dataframes for baseline simulations 
masking <- read_csv("masking_with_date.csv")
current <- read_csv("current_with_date.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))
districts_names <- read_csv("districts_names.csv")
ta_names <- read_csv("tas_names.csv")

df_country_dash <- read_csv("df_country_dash_initial.csv")


hosp_time <- 4
crit_time <- 8


##--UI
ui <- fluidPage(theme = shinytheme("united"),
                ##--National
                tabsetPanel(
                  tabPanel("National",
                           fluidRow(
                             titlePanel("Epidemiological model for COVID-19 - Malawi"),
                             tags$p(),
                             tags$p(),
                             column(width = 7,
                                    column(width = 6,
                                           plotlyOutput("fig")
                                           ),
                                    column(width = 6,
                                           plotlyOutput("fig_country2")
                                           ),
                                    column(width = 6,
                                           plotlyOutput("fig_country3")
                                    ),
                                    column(width = 6,
                                           plotlyOutput("fig_country4")
                                    ),
                                    tags$p(),
                                    #tags$hr(),
                             ),
                             column(width = 5,
                                    # Policy Levers
                                    h3(strong("Policy Levers")),
                                    tags$hr(),
                                    column(width = 4.0, strong("% Masking"),
                                           tags$h6("Current %: 15 %"),
                                           numericInput('mask_perc', 
                                                        label = "New %",
                                                        value = 15, 
                                                        min = 15,
                                                        max = 100),
                                           # Length of Intervention # Days
                                           numericInput('time_intervention_mask',
                                                        label = "Length of Intervention\n(# Days)",
                                                        value = 7,
                                                        min = 7,
                                                        max = 180)
                                    ),
                                    column(width = 4.0, strong("% Physical Distancing"),
                                           tags$h6(paste0("Current %: ", 100*current$reduc[which(current$date==lubridate::today())]),"%"),
                                           numericInput('distancing_perc',
                                                        label = "New %",
                                                        value = 8,
                                                        min = 8,
                                                        max = 100),
                                           numericInput('time_intervention_dist',
                                                        label = "Length of Intervention\n(# Days)",
                                                        value = 7,
                                                        min = 7,
                                                        max = 180)
                                    ),
                                    column(width = 4,
                                           #--Time Horizon Projection
                                            numericInput('projection', 'Time Horizon Projection (days)',
                                                         value = as.numeric(difftime(as.Date("2021-03-03"), today(), units = "days")) , 
                                                         min = 1, 
                                                         max = as.numeric(difftime(as.Date("2021-03-03"), today(), units = "days")) ),
                                           
                                           selectInput('level', 'Level of Interest', choices = c("National", "District", "TA")),
                                           
                                           actionButton("runreportButton", "Run Report", width = '250'
                                                        )
                                           )
                             ),
                            
                             column(width = 5,
                                    # Model Priors
                                    h3(strong("Fixed Model Parameters")),
                                    tags$hr(),
                                    column(width = 3.0, 
                                           tags$p(strong("R0:"), "1.9"),
                                           tags$p(strong("Infectious Time (Days):"), "7"),
                                           tags$p(strong("Hospitalized Time (Days):"), "4"),
                                                  tags$p(strong("ICU Time (Days):"), "8")
                                           
                                    ),
                                    column(width = 3.0, 
                                           tags$p(strong("ICU Risk Among Hospitalized:")),
                                           tags$p(em("Pediatrics (<20):"), "5.0%"),
                                           tags$p(em("Adults (20-49):"), "14%"),
                                           tags$p(em("Elderly (50+):"), "28%"),
                                    ),
                                    column(width = 3.0,
                                           (strong("Hospitalized Rate of Infected:")),
                                           em("Pediatrics (<20):", "0.0090%"),
                                           em("Adults (20-49):", "1.2%"),
                                           em("Elderly (50+):", "5.5%")
                                           ),
                                    column(width = 3.0,
                                           strong("Fatality Rate of ICU:"),
                                           em("Pediatrics (<20):", "9.0%"),
                                           em("Adults (20-49):", "20%"),
                                           em("Elderly (50+):", "59%")
                                           ),
                                    ),
                             # Reductions
                             tags$p(),
                             column(width = 5,
                                    h3(strong("Reductions")),
                                    tags$hr(),
                                    h5(strong("Absolute reduction due to implemented measures:")),
                                    tableOutput('table_reductions_country_abs'),
                                    h5(strong("Percentual reduction due to implemented measures:")),
                                    tableOutput('table_reductions_country')
                                    ),
                            column(width = 12,
                                   paste("Generated results:", today()),
                                   DT::DTOutput('table_national')
                                   )
                           )##--end of FluidRow
      
                           ##---TABLE--##
                           # tags$hr(),
                           # fluidRow(
                           #   paste("Generated results:", today()),
                           #   DT::DTOutput('table_national')
                           # )
                  ), ##--end National
                  
                  ##--District
                  tabPanel("District",
                           fluidRow(
                             titlePanel(strong("District Level")),
                             column(width = 8,
                                           column(width = 6,
                                                  plotlyOutput("fig_district")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_district2")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_district3")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_district4")
                                           ),
    
                             ),
                             column(width = 4, 
                                    selectInput('district', 'District', 
                                                choices = districts_names$districts),
                                    tags$hr(),
                                    h3(strong("Reductions")),
                                    tags$hr(),
                                    h5(strong("Absolute reduction due to implemented measures:")),
                                    tags$p(),
                                    tableOutput('table_reductions_district_abs'),
                                    tags$p(),
                                    h5(strong("Percentual reduction due to implemented measures:")),
                                    tags$p(),
                                    tableOutput('table_reductions_districts')
                             )
                           ),
                           tags$p(
                             paste("Generated results:", today()),
                             DT::DTOutput('table_district')
                           )
                  ), ##--end Panel district
                  
                  ##--TA
                  tabPanel("TA",
                           fluidRow(
                             titlePanel(strong("TA Level")),
                             column(width = 8,
                                           column(width = 6,
                                                  plotlyOutput("fig_ta")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_ta2")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_ta3")
                                           ),
                                           column(width = 6,
                                                  plotlyOutput("fig_ta4")

                             )),
                             column(width = 4, 
                                    selectInput('ta', 'TA', 
                                                choices = ta_names$ta_name),
                                    tags$hr(),
                                    tags$h3(strong("Reductions")),
                                    tags$hr(),
                                    h5(strong("Absolute reduction due to implemented measures:")),
                                    tags$p(),
                                    tableOutput('table_reductions_ta_abs'),
                                    tags$p(),
                                    h5(strong("Percentual reduction due to implemented measures:")),
                                    tags$p(),
                                    tableOutput('table_reductions_ta_perc')
                             )
                           ),
                           tags$p(
                             paste("Generated results:", today()),
                             DT::DTOutput('table_ta')
                           )
                  ), ##--end panel TA

                  ##--More Information
                  tabPanel("More Information",
                           fluidRow(titlePanel(strong("More Information")),
                                    column(11, offset = 0.75,
                                           tags$p("The user can set four parameters: the masking percentage, the length of masking intervention,
                                       the physical distancing percentage, and length of the distancing intervention."),
                                           tags$p("It is also possible to set the time horizon for the projections."),
                                           
                                           tags$p("Some parameters are fixed:")
                                    ),
                                    
                                    column(11, offset = 0.75,
                                           
                                           tags$div(
                                             tags$ul(
                                               tags$li(strong("R0:"), "is the basic reproduction number"),
                                               tags$li("The Infectious Time in Days"),
                                               tags$li("The Hospitalized Time in Days"),
                                               tags$li("The ICU Time in Days"),
                                               tags$li("The ICU Risk Among Hospitalized: this parameter is different 
                                                  for each age class considered (Pediatrics, Adults, and Elderly)"),
                                               tags$li("Hospitalized Rate of Infection: this parameter is different 
                                                  for each age class considered (Pediatrics, Adults, and Elderly)"),
                                               tags$li("The Fatality Rate of ICU: this parameter is different 
                                                  for each age class considered (Pediatrics, Adults, and Elderly)"),
                                               
                                             )
                                           )
                                    )
                                    
                           ),##--end of fluidRow
                           fluidRow(
                             column(11, offset = 0.75,
                                    tags$p("Once the parameters are set, the user can click on the Run Report button to
                         execute the simulation.
                         The first result is presented as four plots showing the disease dynamic for the baseline status quo simulation, i. e.,
                         when there is no control meausres applied,
                         and the intervention simulation. 
                         
                         Both simulations are run for 365 days, going from April 1st, 2020 until March 3rd, 2021. 
                        
                        The first graph shows the cumulative cases, the second shows the cumulative severe cases 
                         (the sum of hospitalizations, ICU and death), the third plot shows the cumulative hospitalizations and ICU,
                         and the fourth graph shows the cumulative deaths. By moving the mouse over the graphs it is possible to see
                         the corresponding numbers.
                         Below the graph, two tables are showing the potential reductions achieved due to the implemented measures. 
                         The first table Absolute reduction to implemented measures show the potential reductions in the number of cases, hospitalizations, ICU, and deaths. The second table Percentual reduction due to implemented measures show the same quantities but in percentage. Those comparisons are made using the values from the simulation projection and the status quo projection.
                         At the bottom of the dashboard, the user can find a table showing 
                                the current and future situations. The first four columns refer to the 
                                current situation and are named as Cases (to date), Hosp. (to date), 
                                ICU (to date), Death (to date). The other eight columns refer to projections. 
                                There is the status quo projections, which means that no control measure was 
                                taken, named as Cases (status quo), Hosp. (status quo), ICU (status quo), 
                                Death (status quo), and the simulation projection, that take into account 
                                the control measures set by the user, named as Cases (simulation projection), 
                                Hosp. (simulation projection), ICU (simulation projection), 
                                Death (simulation projection).The user can navigate by the District and TA 
                                tabs to see the same information described before, for different territorial levels. The districts and TAs can be selected from a list in the right panel, and all the information is updated on the screen.")
                             )
                           )##--end of fluidRow
                  ) ##--end of More Information 
))##--end UI

##---------------------------------##
##--Server-------------------------##
##---------------------------------##
server <- function(input, output, session) {
  
  
  #--Creating the inputs .csv's based on user's selection 
  
  maskdatasetInput <- observe({
    
    if(input$runreportButton == 0) return()
    
    masking_new <- masking %>% 
      mutate(
        masking_compliance2 = ifelse(date < today(), masking_compliance,
                                     ifelse(between(date, today(), today() + days(input$time_intervention_mask-1)),
                                            input$mask_perc/100, masking_compliance))
      )
    
    write.csv(data.frame(masking_compliance = masking_new$masking_compliance2),
              "inputs/masking/masking_compliance_new.csv", row.names = FALSE)
    
  })
  
  ##--##
  distancingInput <- observe({
    
    if(input$runreportButton == 0) return()
    
    current <- current %>% 
      mutate(reduc_new = ifelse(date < today(), reduc, 
                                ifelse(between(date, today(), today() +
                                                 days(input$time_intervention_dist - 1)),
                                       input$distancing_perc/100, reduc)))
    write.csv(data.frame(reduc = current$reduc_new), 
              "inputs/reductionScenarios/current_new.csv", 
              row.names = FALSE)
  })
  ##-----------------------------------------------------##
  
  ##-----------------------##
  ##--Simulation Function--##
  ##-----------------------##
  
  simulation_function <- reactive({
    
    if(input$runreportButton == 0) return()
    
    AGE_CHILD <- 1
    AGE_ADULT <- 2
    AGE_ELDER <- 3
    
    ages = c(AGE_CHILD, AGE_ADULT, AGE_ELDER)
    age_names = c('Pediatric', 'Adult', 'Elderly')
    
    df_distancing <- read_csv('inputs/reductionScenarios/current_new.csv')
    df_masking <- read_csv('inputs/masking/masking_compliance_new.csv')
    df_locations <- read_csv('inputs/MW COVID Inputs.csv')
    df_params <- read_csv('inputs/params_inits_template.csv')
    df_seed <- read_csv('inputs/simulation-seeddates-ta-20200910.csv')
    
    # Setup model parameters
    
    age_infection_rates <- rbind(
      c(df_params$ped2ped, df_params$ped2ad, df_params$ped2eld),
      c(df_params$ad2ped,  df_params$ad2ad,  df_params$ad2eld),
      c(df_params$eld2ped, df_params$eld2ad, df_params$eld2eld)
    )
    
    susceptibility <- c(df_params$susceptibility_p, df_params$susceptibility_a, df_params$susceptibility_e)
    
    r0 <- df_params$R0
    exposed_time <- 1 / df_params$kappa
    infected_time <- 1 / df_params$kappa2
    hosp_time <- 1 / df_params$tau
    crit_time <- 1 / df_params$tau2
    mask_effectiveness <- df_params$efficacy
    seed_threshold <- df_params$seed_date_threshold
    
    excluded_locations <- c(20407,10106,20251,20118,20102,20511,21071,30303,21070,10110,10314)
    df_locations <- df_locations %>% 
      filter(!(TA_Code %in% excluded_locations))
    
    df_locations$age_code = 0
    
    df_locations$age_code[df_locations$Age == 'Pediatrics'] <- AGE_CHILD
    df_locations$age_code[df_locations$Age == 'Adults'] <- AGE_ADULT
    df_locations$age_code[df_locations$Age == 'Elderly'] <- AGE_ELDER
    
    seed_column <- paste('day_n', seed_threshold, sep='')
    df_seed_dates <- df_seed[c('adm_id', seed_column)] %>% rename(start_day=seed_column)
    df_locations <- left_join(df_locations, df_seed_dates, by=c('TA_Code'='adm_id'))
    
    behaviour_mod <- (1 - df_distancing$reduc) * (1 - df_masking$masking_compliance * mask_effectiveness)
    
    base_infection_rate <- r0 / (exposed_time + infected_time)
    
    dfs_ages <- list(
      df_locations[df_locations$age_code == AGE_CHILD,],
      df_locations[df_locations$age_code == AGE_ADULT,],
      df_locations[df_locations$age_code == AGE_ELDER,]
    )
    
    s <- list()
    n <- list()
    e <- list()
    i <- list()
    h <- list()
    c <- list()
    r <- list()
    d <- list()
    
    for (age in ages) {
      dfs_ages[[age]]$pop_infection_rate <- base_infection_rate * susceptibility[[age]] / dfs_ages[[age]]$Population
      dfs_ages[[age]]$empty_state = 0
      
      s[[age]] <- matrix(dfs_ages[[age]]$Population)
      n[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      e[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      i[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      h[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      c[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      r[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      d[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    }
    
    n_days = 365
    
    for (day in 2:n_days) {
      yday <- day - 1
      
      for (age in ages) {
        s_ <- s[[age]]
        n_ <- n[[age]]
        e_ <- e[[age]]
        i_ <- i[[age]]
        h_ <- h[[age]]
        c_ <- c[[age]]
        r_ <- r[[age]]
        d_ <- d[[age]]
        
        # Calculate new infections from each source age
        new_by_age <- sapply(ages, function(src_age) {
          dfs_ages[[src_age]]$pop_infection_rate * age_infection_rates[[src_age, age]] * (e[[src_age]][,yday] + i[[src_age]][,yday])
        })
        new_infections <- rowSums(new_by_age) * behaviour_mod[[day]] * s_[,yday]
        
        s[[age]] <- cbind(s_, s_[,yday] - new_infections)
        n[[age]] <- cbind(n_, 1 * new_infections)
        e[[age]] <- cbind(e_, e_[,yday] + new_infections - e_[,yday] / exposed_time)
        i[[age]] <- cbind(i_, i_[,yday] + e_[,yday] / exposed_time - i_[,yday] / infected_time)
        h[[age]] <- cbind(h_, h_[,yday] + i_[,yday] * dfs_ages[[age]]$Hospitalization / infected_time - h_[,yday] / hosp_time)
        c[[age]] <- cbind(c_, c_[,yday] + h_[,yday] * dfs_ages[[age]]$Crit_of_Hosp / hosp_time - c_[,yday] / crit_time)
        r[[age]] <- cbind(r_, r_[,yday] + 
                            i_[,yday] * (1 - dfs_ages[[age]]$Hospitalization) / infected_time +
                            h_[,yday] * (1 - dfs_ages[[age]]$Crit_of_Hosp) / hosp_time +
                            c_[,yday] * (1 - dfs_ages[[age]]$FR_of_Crit) / crit_time)
        d[[age]] <- cbind(d_, d_[,yday] + c_[,yday] * dfs_ages[[age]]$FR_of_Crit / crit_time)
      }
      
      # Add one infected adult to all locations with this start day
      e[[AGE_ADULT]][,day] <- e[[AGE_ADULT]][,day] + (dfs_ages[[AGE_ADULT]]$start_day == day)
    }
    
    df_loc_info <- dfs_ages[[age]][c('TA_Code','Lvl3','Lvl4')]
    df_loc_info
    
    df_pandemic <- bind_rows(lapply(ages, function(age) {
      rbind(
        cbind(df_loc_info, Age=age_names[[age]], State='Susceptible', s[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='New Infections', n[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Exposed', e[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Infected', i[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Hospitalized', h[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Critical', c[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Recovered', r[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Dead', d[[age]])
      )
    }))
    
    df_ta <- df_pandemic %>% tidyr::pivot_longer(
      !matches("[A-Za-z]"), names_to='Day', values_to='People', 
      names_transform=list(Day=as.integer))
    
    df_district <- df_ta %>% 
      group_by(Lvl3,Day,State) %>%
      summarise(People=sum(People))
    
    df_country <- df_district %>% 
      group_by(Day,State) %>%
      summarise(People=sum(People))
    
    ##--Adding dates
    ##--Country
    df_country_spread <- spread(df_country, key = State, value = People) 
    df_country_spread$date <- seq(from = as.Date("2020-04-01"),
                                  by = "day", length.out = 365)
    
    ##--District
    df_district_spread <- spread(df_district, key = State, value = People) 
    districts <- df_district_spread$Lvl3 %>% unique()
    df_district_spread$date <- rep(seq(from = as.Date("2020-04-01"),
                                       by = "day", length.out = 365), length(districts))
    
    ##--TA
    df_ta2 <- df_ta 
    df_ta2$date <- rep(seq(from = as.Date("2020-04-01"), 
                           by = "day", length.out = 365), 10152)  
    
    ##--Organizing the outputs in a list
    list_output_sim <- list(country = df_country_spread, district = df_district_spread, ta = df_ta2)
    return(list_output_sim)
  }) # end of simulation function
  
  ##--Creating country table from simulation 
  country_projection_sim <- reactive({
    df_country_spread <- simulation_function()[[1]] %>%
      dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead %>% round()
    Severe_cases <- ICU_cum + Hospitalized_cum + Death
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:365,
                              Cases_sim = Cases_cum,
                              Hospitalizations_sim = round(Hospitalized_cum),
                              ICU_sim = round(ICU_cum), 
                              Death_sim = Death,
                              Severe_sim = Severe_cases)
    
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection))
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(date, Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("date", "Cases (simulation projection)",
                                            "Hosp. (simulation projection)",
                                            "ICU (simulation projection)",
                                            "Death (simulation projection)")
    list_sim <- list(df_country_dash, df_country_dash_projection2)
    return(list_sim)
  })
  
  ##--Creating district table from simulation
  district_sim <- reactive({
    district_df <- simulation_function()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time 
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time 
    Death_dist <- district_df$Dead %>% round()
    Severe_cases <- ICU_cum_dist + Hospitalized_cum_dist + Death_dist
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:365), 
                           date = district_df$date,
                           Cases = Cases_cum_dist, 
                           Hospitalizations = round(Hospitalized_cum_dist),
                           ICU = round(ICU_cum_dist), 
                           Death = Death_dist,
                           Severe = Severe_cases) 
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection)) %>% 
      dplyr::select(Cases, Hospitalizations, ICU, Death)
    names(district_projection_sim) <- c("Cases (simulation projection)",
                                        "Hosp. (simulation projection)",
                                        "ICU (simulation projection)",
                                        "Death (simulation projection)")
    ##--district_projection_sim -- OK
    result <- list(district_df2, district_projection_sim)
    return(result)
  })
  
  ##--Creating TA table from simulation
  ta_simulation <- reactive({
    ta_test2 <- simulation_function()[[3]] %>% 
      filter(Lvl4 == input$ta) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State) %>% 
      summarize(new_inf = sum(People)) %>% 
      mutate(TA = input$ta)
    
    ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
    Hosp_cum_ta = cumsum(ta1_spread$Hospitalized)/hosp_time 
    Critical_cum_ta = cumsum(ta1_spread$Critical)/crit_time 
    Dead_ta =  ta1_spread$Dead %>% round()
    Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
    
    simulation_ta <- tibble(date = ta1_spread$date,
                            Cases = Cases_cum_ta,
                            Hospitalizations = round(Hosp_cum_ta),
                            ICU = round(Critical_cum_ta),
                            Death = Dead_ta,
                            Severe = Severe_ta)
    ##--Total
    #simulation_ta
    ##-------------------------##
    ##--Projection--##
    projection_ta <- simulation_ta %>% 
      filter(date == lubridate::today() + lubridate::days(input$projection))
    names(projection_ta) <- c("date", "Cases (simulation projection)",
                              "Hosp. (simulation projection)",
                              "ICU (simulation projection)",
                              "Death (simulation projection)")
    #projection_ta 
    
    result <- list(simulation_ta, projection_ta)
    return(result)
  })
  
  ##-----------------------##
  ##--Simulation Baseline--##
  ##-----------------------##
  simulation_baseline <- reactive({
    
    if(input$runreportButton == 0) return()
    
    AGE_CHILD <- 1
    AGE_ADULT <- 2
    AGE_ELDER <- 3
    
    ages = c(AGE_CHILD, AGE_ADULT, AGE_ELDER)
    age_names = c('Pediatric', 'Adult', 'Elderly')
    
    df_distancing <- read_csv('inputs/reductionScenarios/current.csv')
    df_masking <- read_csv('inputs/masking/masking_compliance.csv')
    df_locations <- read_csv('inputs/MW COVID Inputs.csv')
    df_params <- read_csv('inputs/params_inits_template.csv')
    df_seed <- read_csv('inputs/simulation-seeddates-ta-20200910.csv')
    
    # Setup model parameters
    
    age_infection_rates <- rbind(
      c(df_params$ped2ped, df_params$ped2ad, df_params$ped2eld),
      c(df_params$ad2ped,  df_params$ad2ad,  df_params$ad2eld),
      c(df_params$eld2ped, df_params$eld2ad, df_params$eld2eld)
    )
    
    susceptibility <- c(df_params$susceptibility_p, df_params$susceptibility_a, df_params$susceptibility_e)
    
    r0 <- df_params$R0
    exposed_time <- 1 / df_params$kappa
    infected_time <- 1 / df_params$kappa2
    hosp_time <- 1 / df_params$tau
    crit_time <- 1 / df_params$tau2
    mask_effectiveness <- df_params$efficacy
    seed_threshold <- df_params$seed_date_threshold
    
    excluded_locations <- c(20407,10106,20251,20118,20102,20511,21071,30303,21070,10110,10314)
    df_locations <- df_locations %>% 
      filter(!(TA_Code %in% excluded_locations))
    
    df_locations$age_code = 0
    
    df_locations$age_code[df_locations$Age == 'Pediatrics'] <- AGE_CHILD
    df_locations$age_code[df_locations$Age == 'Adults'] <- AGE_ADULT
    df_locations$age_code[df_locations$Age == 'Elderly'] <- AGE_ELDER
    
    seed_column <- paste('day_n', seed_threshold, sep='')
    df_seed_dates <- df_seed[c('adm_id', seed_column)] %>% rename(start_day=seed_column)
    df_locations <- left_join(df_locations, df_seed_dates, by=c('TA_Code'='adm_id'))
    
    behaviour_mod <- (1 - df_distancing$reduc) * (1 - df_masking$masking_compliance * mask_effectiveness)
    
    base_infection_rate <- r0 / (exposed_time + infected_time)
    
    dfs_ages <- list(
      df_locations[df_locations$age_code == AGE_CHILD,],
      df_locations[df_locations$age_code == AGE_ADULT,],
      df_locations[df_locations$age_code == AGE_ELDER,]
    )
    
    s <- list()
    n <- list()
    e <- list()
    i <- list()
    h <- list()
    c <- list()
    r <- list()
    d <- list()
    
    for (age in ages) {
      dfs_ages[[age]]$pop_infection_rate <- base_infection_rate * susceptibility[[age]] / dfs_ages[[age]]$Population
      dfs_ages[[age]]$empty_state = 0
      
      s[[age]] <- matrix(dfs_ages[[age]]$Population)
      n[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      e[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      i[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      h[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      c[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      r[[age]] <- matrix(dfs_ages[[age]]$empty_state)
      d[[age]] <- matrix(dfs_ages[[age]]$empty_state)
    }
    
    n_days = 365
    
    for (day in 2:n_days) {
      yday <- day - 1
      
      for (age in ages) {
        s_ <- s[[age]]
        n_ <- n[[age]]
        e_ <- e[[age]]
        i_ <- i[[age]]
        h_ <- h[[age]]
        c_ <- c[[age]]
        r_ <- r[[age]]
        d_ <- d[[age]]
        
        # Calculate new infections from each source age
        new_by_age <- sapply(ages, function(src_age) {
          dfs_ages[[src_age]]$pop_infection_rate * age_infection_rates[[src_age, age]] * (e[[src_age]][,yday] + i[[src_age]][,yday])
        })
        new_infections <- rowSums(new_by_age) * behaviour_mod[[day]] * s_[,yday]
        
        s[[age]] <- cbind(s_, s_[,yday] - new_infections)
        n[[age]] <- cbind(n_, 1 * new_infections)
        e[[age]] <- cbind(e_, e_[,yday] + new_infections - e_[,yday] / exposed_time)
        i[[age]] <- cbind(i_, i_[,yday] + e_[,yday] / exposed_time - i_[,yday] / infected_time)
        h[[age]] <- cbind(h_, h_[,yday] + i_[,yday] * dfs_ages[[age]]$Hospitalization / infected_time - h_[,yday] / hosp_time)
        c[[age]] <- cbind(c_, c_[,yday] + h_[,yday] * dfs_ages[[age]]$Crit_of_Hosp / hosp_time - c_[,yday] / crit_time)
        r[[age]] <- cbind(r_, r_[,yday] + 
                            i_[,yday] * (1 - dfs_ages[[age]]$Hospitalization) / infected_time +
                            h_[,yday] * (1 - dfs_ages[[age]]$Crit_of_Hosp) / hosp_time +
                            c_[,yday] * (1 - dfs_ages[[age]]$FR_of_Crit) / crit_time)
        d[[age]] <- cbind(d_, d_[,yday] + c_[,yday] * dfs_ages[[age]]$FR_of_Crit / crit_time)
      }
      
      # Add one infected adult to all locations with this start day
      e[[AGE_ADULT]][,day] <- e[[AGE_ADULT]][,day] + (dfs_ages[[AGE_ADULT]]$start_day == day)
    }
    
    df_loc_info <- dfs_ages[[age]][c('TA_Code','Lvl3','Lvl4')]
    df_loc_info
    
    df_pandemic <- bind_rows(lapply(ages, function(age) {
      rbind(
        cbind(df_loc_info, Age=age_names[[age]], State='Susceptible', s[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='New Infections', n[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Exposed', e[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Infected', i[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Hospitalized', h[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Critical', c[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Recovered', r[[age]]),
        cbind(df_loc_info, Age=age_names[[age]], State='Dead', d[[age]])
      )
    }))
    
    df_ta <- df_pandemic %>% tidyr::pivot_longer(
      !matches("[A-Za-z]"), names_to='Day', values_to='People', 
      names_transform=list(Day=as.integer))
    
    df_district <- df_ta %>% 
      group_by(Lvl3,Day,State) %>%
      summarise(People=sum(People))
    
    df_country <- df_district %>% 
      group_by(Day,State) %>%
      summarise(People=sum(People))
    
    ##--Adding dates
    ##--Country
    df_country_spread <- spread(df_country, key = State, value = People) 
    df_country_spread$date <- seq(from = as.Date("2020-04-01"),
                                  by = "day", length.out = 365)
    
    ##--District
    df_district_spread <- spread(df_district, key = State, value = People) 
    districts <- df_district_spread$Lvl3 %>% unique()
    df_district_spread$date <- rep(seq(from = as.Date("2020-04-01"),
                                       by = "day", length.out = 365), length(districts))
    
    ##--TA
    df_ta2 <- df_ta 
    df_ta2$date <- rep(seq(from = as.Date("2020-04-01"), 
                           by = "day", length.out = 365), 10152)  
    
    ##--Organizing the outputs in a list
    list_output_sim <- list(country = df_country_spread, district = df_district_spread, ta = df_ta2)
    return(list_output_sim)
  }) # end of simulation baseline
  
  ##--Creating country table simulation for status quo
  country_projection_status_quo <- reactive({
    df_country_spread <- simulation_baseline()[[1]] %>%
      dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead %>% round()
    Severe_cases <- round(Death + Hospitalized_cum + ICU_cum)
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:365,
                              Cases_sq = Cases_cum,
                              Hospitalizations_sq = round(Hospitalized_cum),
                              ICU_sq = round(ICU_cum), 
                              Death_sq = Death,
                              Severe_sq = Severe_cases)
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection))
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(df_country_dash_projection2) <- c("Cases (status quo)",
                                            "Hosp. (status quo)",
                                            "ICU (status quo)",
                                            "Death (status quo)")
    list_status_quo <- list(df_country_dash, df_country_dash_projection2)
    return(list_status_quo)
  })
  
  ##--Creating country table simulation for to date
  country_projection_to_date <- reactive({
    df_country_spread <- simulation_baseline()[[1]] %>%
      dplyr::mutate(Cases = `New Infections`)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized)/hosp_time 
    ICU_cum <- cumsum(df_country_spread$Critical)/crit_time 
    Death <- df_country_spread$Dead %>% round()
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              Cases_sim = Cases_cum,
                              Hospitalizations_sim = round(Hospitalized_cum),
                              ICU_sim = round(ICU_cum), 
                              Death_sim = Death)
    
    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today())
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("Cases (to date)",
                                            "Hosp. (to date)",
                                            "ICU (to date)",
                                            "Death (to date)")
    return(df_country_dash_projection2)
  })
  
  ##--Creating district table simulation for status quo 
  district_projection_status_quo <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time 
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time 
    Death_dist <- district_df$Dead %>% round()
    Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:365), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = round(Hospitalized_cum_dist),
                           ICU_sq = round(ICU_cum_dist), 
                           Death_sq = Death_dist,
                           Severe_sq = Severe_dist
                           ) 
    #district_df2 -- OK
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection)) %>% 
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(district_projection_sim) <- c("Cases (status quo)",
                                        "Hosp. (status quo)",
                                        "ICU (status quo)",
                                        "Death (status quo)")
    ##--district_projection_sim -- OK
    result <- list(district_df2, district_projection_sim)
    return(result)
  })
  
  ##--Creating district table simulation for to date
  district_projection_to_date <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      filter(Lvl3 == input$district) %>% 
      mutate(Cases = `New Infections`)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized)/hosp_time
    ICU_cum_dist <- cumsum(district_df$Critical)/crit_time
    Death_dist <- district_df$Dead %>% round()
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:365), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = round(Hospitalized_cum_dist),
                           ICU_sq = round(ICU_cum_dist), 
                           Death_sq = Death_dist) 
    #district_df2 -- OK
    ##------------------------------------------------------------##
    ##--Projection--##
    district_projection_sim <- district_df2 %>% 
      dplyr::filter(date == lubridate::today()) %>% 
      dplyr::select(Cases_sq, Hospitalizations_sq, ICU_sq, Death_sq)
    names(district_projection_sim) <- c("Cases (to date)",
                                        "Hosp. (to date)",
                                        "ICU (to date)",
                                        "Death (to date)")
    
    district_projection_sim
  })
  
  ##--Creating TA table simulation status quo
  ta_simulation_status_quo <- reactive({
    ta_test2 <- simulation_baseline()[[3]] %>% 
      filter(Lvl4 == input$ta) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State) %>% 
      summarize(new_inf = sum(People)) %>% 
      mutate(TA = input$ta)
    
    ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
    Hosp_cum_ta = cumsum(ta1_spread$Hospitalized)/hosp_time
    Critical_cum_ta = cumsum(ta1_spread$Critical)/crit_time
    Dead_ta =  ta1_spread$Dead %>% round()
    Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
    
    simulation_ta_sq <- tibble(date = ta1_spread$date,
                               time = seq(1:365),
                               Cases_sq = Cases_cum_ta,
                               Hospitalizations_sq = round(Hosp_cum_ta),
                               ICU_sq = round(Critical_cum_ta),
                               Death_sq = Dead_ta,
                               Severe_sq = Severe_ta)
    ##-------------------------##
    projection_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today() + lubridate::days(input$projection)) %>% 
      select(date, Cases_sq:Death_sq)
    names(projection_ta_sq) <- c("date", "Cases (status quo projection)",
                                 "Hosp. (status quo projection)",
                                 "Critical (status quo projection)",
                                 "Death (status quo projection)")
    
    ##-------------------------##
    to_date_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today()) %>% 
      select(date, Cases_sq:Death_sq)
    names(to_date_ta_sq) <- c("date", "Cases (to date)",
                              "Hosp. (to date)",
                              "Critical (to date)",
                              "Death (to date)")
    
    result <- list(simulation_ta_sq, projection_ta_sq, to_date_ta_sq)
    return(result)
  })
  
  
  ##-----------------##
  ##--Plot National--##
  ##-----------------##
  #--Cases
  output$fig <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash %>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% layout(
        title = "<b>Cases</b>"
      )
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Intervention', line = list(color = 'pink'))
      return(fig)
    }else{
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Cases_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'red'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% layout(
        title = "<b>Cases</b>"
      )
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Intervention', line = list(color = 'pink'))
      return(fig)
    } 
  })
  
  ##--Aggregated critical care, hospitalizations, deaths
  output$fig_country2 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Severe_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Severe cases',
                      line = list(color = 'orange'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Severe_sim, name = 'Severe cases (sim)', line = list(color = 'rgb(255, 223, 153)'))
      fig <- fig %>% layout(
        title = "<b>Severe Cases</b>"
      )
      return(fig)
    }else{
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Severe_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'orange'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Severe_sim, name = 'Intervention', line = list(color = 'rgb(255, 223, 153)'))
      fig <- fig %>% layout(
        title = "<b>Severe Cases</b>"
      )
      return(fig)
    } 
  })
  
  ##--Hospitalization and Critical Care
  output$fig_country3 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Hosp. (Status Quo)',
                      line = list(color = 'rgb(0, 102, 0)'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, name = 'Hosp. (Intervention)', line = list(color = 'rgb(204,255,204)'))
      fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU (Status Quo)', line = list(color = 'blue'))
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'ICU (Intervention)', line = list(color = 'lightblue'))
      fig <- fig %>% layout(
        title = "<b>Hospitalizations and ICU</b>"
      )
      fig
      return(fig)
    }else{
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Hospitalizations_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Hosp. (Status Quo)',
                      line = list(color = 'rgb(0, 102, 0)'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, name = 'Hosp. (Intervention)', line = list(color = 'rgb(204,255,204)'))
      fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU (Status Quo)', line = list(color = 'blue'))
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'ICU (Intervention)', line = list(color = 'lightblue'))
      #fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
      fig <- fig %>% layout(
        title = "<b>Hospitalizations and ICU</b>"
      )
      return(fig)
    }
    
  })
  
  ##--Deaths
  output$fig_country4 <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_dash%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death_sim, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      return(fig)
    } else{
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])%>% 
        filter(date >= today() - days(7))
      fig <-  plot_ly(data_final_plot,
                      x = ~ date,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Status Quo',
                      line = list(color = 'black'),
                      width = 500,
                      height = 250)
      fig <- fig %>% layout(xaxis = list(title = "Date"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death_sim, name = 'Intervention', line = list(color = 'grey'))
      fig <- fig %>% layout(
        title = "<b>Deaths</b>"
      )
      return(fig)
    }
  })
  
  ##-----------------##
  ##--Plot District--##
  ##-----------------##
  
  ##--Cases
  output$fig_district <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    #data_final_plot <- district_projection_status_quo()[[1]]
    data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)]) %>% 
      filter(date >= today() - days(7))
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Cases_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'red'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% layout(
      title = "<b>Cases</b>"
    )
    fig <- fig %>% add_trace(y = ~ Cases, name = 'Intervention', line = list(color = 'pink'))
    fig
  })
  
  ##--Severe Cases
  output$fig_district2 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    #data_final_plot <- district_projection_status_quo()[[1]]
    data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
      filter(date >= today() - days(7))
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Severe_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'orange'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Severe, name = 'Intervention', line = list(color = 'rgb(255, 223, 153)'))
    fig <- fig %>% layout(
      title = "<b>Severe Cases</b>"
    )
    fig
  })
  
  ##--Hospitalizations and critical care
  output$fig_district3 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    #data_final_plot <- district_projection_status_quo()[[1]]
    data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
      filter(date >= today() - days(7))
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Hospitalizations_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Hosp. (Status Quo)',
                    line = list(color = 'rgb(0, 102, 0)'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hosp. (Intervention)', line = list(color = 'rgb(204,255,204)'))
    fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU (Status Quo)', line = list(color = 'blue'))
    fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (Intervention)', line = list(color = 'lightblue'))
    fig <- fig %>% layout(
      title = "<b>Hospitalizations and ICU</b>"
    )
    fig
  })
  
  ##--Deaths
  output$fig_district4 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    #data_final_plot <- district_projection_status_quo()[[1]]
    data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])%>% 
      filter(date >= today() - days(7))
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Death_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'black'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Death, name = 'Intervention', line = list(color = 'grey'))
    fig <- fig %>% layout(
      title = "<b>Deaths</b>"
    )
    fig
  })
  
  ##-----------##
  ##--Plot TA--##
  ##-----------##
  
  ##--Cases
  output$fig_ta <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    
    data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
      filter(date >= today() - days(7))
    #data_final_plot <- ta_simulation_status_quo()[[1]]
    
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Cases_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'red'),
                    width = 500,
                    height = 250
                    )
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    #fig <- fig %>% add_trace(y = ~ Death, name = 'Death (simulation)', line = list(color = 'grey'))
    #fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU', line = list(color = 'blue'))
    #fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (simulation)', line = list(color = 'lightblue'))
    #fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
    #fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hospitalizations (simulation)', line = list(color = 'rgb(204,255,204)'))
    #fig <- fig %>% add_trace(y = ~ Cases_sq, name = 'Cases', line = list(color = 'red'))
    fig <- fig %>% add_trace(y = ~ Cases, name = 'Intervention', line = list(color = 'pink'))
    fig <- fig %>% layout(
      title = "<b>Cases</b>"
    )
    fig
  })
  
  ##--Severe Cases
  output$fig_ta2 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    
    data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
      filter(date >= today() - days(7))
    #data_final_plot <- ta_simulation_status_quo()[[1]]
    
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Severe_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'orange'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Severe, name = 'Intervention', line = list(color = 'rgb(255, 223, 153)'))
    
    fig <- fig %>% layout(
      title = "<b>Severe Cases</b>"
    )
    fig
  })
  
  ##--Hospitalizations and ICU
  output$fig_ta3 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    
    data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
      filter(date >= today() - days(7))
    #data_final_plot <- ta_simulation_status_quo()[[1]]
    
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Hospitalizations_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Hos. (Status Quo)',
                    line = list(color = 'rgb(0, 102, 0)'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hosp. (Intervention)', line = list(color = 'rgb(204,255,204)'))
    fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU (Status Quo)', line = list(color = 'blue'))
    fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (Intervention)', line = list(color = 'lightblue'))
    #fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
    fig <- fig %>% layout(
      title = "<b>Hospitalizations and ICU</b>"
    )
    fig
  })
  
  ##--Death
  output$fig_ta4 <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    
    data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])%>% 
      filter(date >= today() - days(7))
    #data_final_plot <- ta_simulation_status_quo()[[1]]
    
    fig <-  plot_ly(data_final_plot,
                    x = ~ date,
                    y = ~ Death_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Status Quo',
                    line = list(color = 'black'),
                    width = 500,
                    height = 250)
    fig <- fig %>% layout(xaxis = list(title = "Date"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Death, name = 'Intervention', line = list(color = 'grey'))
    fig <- fig %>% layout(
      title = "<b>Deaths</b>"
    )
    fig
  })
  
  
  ##---------------------------------##  
  ##--Table for Reductions National--##
  ##---------------------------------##

  
  output$table_reductions_country_abs <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- country_projection_status_quo()[[2]][1,1] - country_projection_sim()[[2]][1,2]
    reduc_cases2 <- paste(format(reduc_cases, big.mark = ","), "", sep = " ")
    reduc_hosp <- country_projection_status_quo()[[2]][1,2]  - country_projection_sim()[[2]][1,3] 
    reduc_hosp2 <- paste(format(reduc_hosp, big.mark = ","), "", sep = " ")
    reduc_icu <- country_projection_status_quo()[[2]][1,3]  - country_projection_sim()[[2]][1,4] 
    reduc_icu2 <- paste(format(reduc_icu, big.mark = ","), "", sep = " ")
    reduc_death <-  country_projection_status_quo()[[2]][1,4]  - country_projection_sim()[[2]][1,5]
    reduc_death2 <- paste(format(reduc_death, big.mark = ","), "", sep = " ")
    table <- tibble("Reduction in Cases" = reduc_cases2,
                    "Reduction in Hosp." = reduc_hosp2,
                    "Reduction in ICU" = reduc_icu2,
                    "Reduction in Death" = reduc_death2
                    )
    return(table)
  })
  
  output$table_reductions_country <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(country_projection_sim()[[2]][1,2]/country_projection_status_quo()[[2]][1,1] - 1))
    reduc_hosp <- abs(100*(country_projection_sim()[[2]][1,3]/country_projection_status_quo()[[2]][1,2] - 1))
    reduc_icu <- abs(100*(country_projection_sim()[[2]][1,4]/country_projection_status_quo()[[2]][1,3] - 1))
    reduc_death <- abs(100*(country_projection_sim()[[2]][1,5]/country_projection_status_quo()[[2]][1,4] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  
  ##---------------------------------##  
  ##--Table for Reductions District--##
  ##---------------------------------##
  output$table_reductions_district_abs <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- district_projection_status_quo()[[2]][1,1] - district_sim()[[2]][1,1] 
    reduc_cases2 <- paste(format(reduc_cases, big.mark = ","), "", sep = " ")
    reduc_hosp <- district_projection_status_quo()[[2]][1,2]  - district_sim()[[2]][1,2] 
    reduc_hosp2 <- paste(format(reduc_hosp, big.mark = ","), "", sep = " ")
    reduc_icu <- district_projection_status_quo()[[2]][1,3]  - district_sim()[[2]][1,3] 
    reduc_icu2 <- paste(format(reduc_icu, big.mark = ","), "", sep = " ")
    reduc_death <-  district_projection_status_quo()[[2]][1,4]  - district_sim()[[2]][1,4]
    reduc_death2 <- paste(format(reduc_death, big.mark = ","), "", sep = " ")
    table <- tibble("Reduction in Cases" = reduc_cases2,
                    "Reduction in Hosp." = reduc_hosp2,
                    "Reduction in ICU" = reduc_icu2,
                    "Reduction in Death" = reduc_death2
    )
    return(table)
  })
  
  output$table_reductions_districts <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(district_sim()[[2]][1,1]/district_projection_status_quo()[[2]][1,1] - 1))
    reduc_hosp <- abs(100*(district_sim()[[2]][1,2]/district_projection_status_quo()[[2]][1,2] - 1))
    reduc_icu <- abs(100*(district_sim()[[2]][1,3]/district_projection_status_quo()[[2]][1,3] - 1))
    reduc_death <- abs(100*(district_sim()[[2]][1,4]/district_projection_status_quo()[[2]][1,4] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  ##----------------------------##  
  ##--Table for Reductions TAs--##
  ##----------------------------##
  output$table_reductions_ta_abs <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(ta_simulation_status_quo()[[2]][1,2] - ta_simulation()[[2]][1,2]) 
    reduc_hosp <- abs(ta_simulation_status_quo()[[2]][1,3] - ta_simulation()[[2]][1,3]) 
    reduc_icu <- abs(ta_simulation_status_quo()[[2]][1,4] - ta_simulation()[[2]][1,4]) 
    reduc_death <-  abs(ta_simulation_status_quo()[[2]][1,5] - ta_simulation()[[2]][1,5]) 
    table <- tibble("Reduction in Cases" = reduc_cases,
                    "Reduction in Hosp." = reduc_hosp,
                    "Reduction in ICU" = reduc_icu,
                    "Reduction in Death" = reduc_death
    )
    return(table)
  })
  
  output$table_reductions_ta_perc <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- abs(100*(ta_simulation()[[2]][1,2] /ta_simulation_status_quo()[[2]][1,2] - 1))
    reduc_hosp <- abs(100*(ta_simulation()[[2]][1,3] /ta_simulation_status_quo()[[2]][1,3] - 1))
    reduc_icu <- abs(100*(ta_simulation()[[2]][1,4] /ta_simulation_status_quo()[[2]][1,4] - 1))
    reduc_death <- abs(100*(ta_simulation()[[2]][1,5] /ta_simulation_status_quo()[[2]][1,5] - 1))
    table <- tibble("% Reduction in Cases" = reduc_cases,
                    "% Reduction in Hosp." = reduc_hosp,
                    "% Reduction in ICU" = reduc_icu,
                    "% Reduction in Death" = reduc_death)
    return(table)
  })
  
  ##------------------------------##  
  ##--Table National -- NEW!!!!!--##
  ##------------------------------##  
  
  #--Modified Version for table at the bottom
  #--All the districts together
  
  #--Baseline
  table_all_districts_baseline <- reactive({
    district_df <- simulation_baseline()[[2]] %>% 
      mutate(Cases = `New Infections`)
    
    list_district_baseline <- list()
    districts <- districts_names$districts
    
    ##--Loop--#####
    for(i in 1:length(districts)){
      df_district2 <- district_df %>% 
        filter(Lvl3 == districts_names$districts[i])
      
      Cases_cum_dist <- cumsum(df_district2$Cases) %>% round()
      Hospitalized_cum_dist <- cumsum(df_district2$Hospitalized)/hosp_time 
      ICU_cum_dist <- cumsum(df_district2$Critical)/crit_time
      Death_dist <- df_district2$Dead %>% round()
      Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
      
      df_district <- tibble(time = seq(1:365), 
                            date = df_district2$date,
                            District = districts[i],
                            Cases = Cases_cum_dist, 
                            Hospitalizations = round(Hospitalized_cum_dist),
                            ICU = round(ICU_cum_dist), 
                            Death = Death_dist,
                            Severe = Severe_dist)
      
      list_district_baseline[[i]] <- df_district
    }#--end of loop
    ##--Projection baseline
    table_all <- do.call(rbind, list_district_baseline)
    df_simulation_baseline <- table_all %>% 
      filter(date == today() + days(input$projection)) %>% 
      select(Cases:Death)
    names(df_simulation_baseline) <- c("Cases (status quo projection)",
                                         "Hosp. (status quo projection)", 
                                         "ICU (status quo projection)",
                                         "Death (status quo projection)")
    
    ##--To date
    df_baseline_to_date <- table_all %>% 
      filter(date == today()) %>% 
      select(District:Death)
    names(df_baseline_to_date) <- c("District",
                                       "Cases (to date)",
                                       "Hosp. (to date)", 
                                       "ICU (to date)",
                                       "Death (to date)")
    
    ##--Result 
    result <- list(df_baseline_to_date, df_simulation_baseline)
    return(result)
  }) #--end table_all_districts_baseline

  #--Simulation Projection
  table_all_districts_simulation <- reactive({
    district_df <- simulation_function()[[2]] %>% 
      mutate(Cases = `New Infections`)
    
    list_district_simulation <- list()
    districts <- districts_names$districts
    
    ##--Loop--#####
    for(i in 1:length(districts)){
      df_district2 <- district_df %>% 
        filter(Lvl3 == districts_names$districts[i])
      
      Cases_cum_dist <- cumsum(df_district2$Cases) %>% round()
      Hospitalized_cum_dist <- cumsum(df_district2$Hospitalized)/hosp_time 
      ICU_cum_dist <- cumsum(df_district2$Critical)/crit_time
      Death_dist <- df_district2$Dead %>% round()
      Severe_dist <- round(Hospitalized_cum_dist + ICU_cum_dist + Death_dist)
      
      df_district <- tibble(time = seq(1:365), 
                            date = df_district2$date,
                            District = districts[i],
                            Cases = Cases_cum_dist, 
                            Hospitalizations = round(Hospitalized_cum_dist),
                            ICU = round(ICU_cum_dist), 
                            Death = Death_dist,
                            Severe = Severe_dist)
      
      list_district_simulation[[i]] <- df_district
    }#--end of loop
    table_all <- do.call(rbind, list_district_simulation)
    ##
    df_simulation_projection <- table_all %>% 
      filter(date == today() + days(input$projection)) %>% 
      select(Cases:Death)
    names(df_simulation_projection) <- c("Cases (simulation projection)",
                                         "Hosp. (simulation projection)", 
                                         "ICU (simulation projection)",
                                         "Death (simulation projection)")
    df_simulation_projection
  }) #--end table_all_districts_simulation
  ##----------------------------------------##
  
  
  output$table_national <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
         #--Simulation projection
         df_all_districts <- cbind(table_all_districts_baseline()[[1]],
                                   table_all_districts_baseline()[[2]],
                                   table_all_districts_simulation())

         
       #--Country table
         df_country <- cbind(country_projection_to_date(),
                             country_projection_status_quo()[[2]],
                             country_projection_sim()[[2]][,-1])
         df_country$District <- "Total"
         var1 <- names(df_country)[1]
         var2 <- names(df_country)[length(names(df_country))-1]
         df_country <- df_country %>%
           select(District, var1:var2)
         names(df_country) <- names(df_all_districts)

         df_final <- rbind(df_country, df_all_districts)
         df_final
         
         #--comma separators

          point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
          df2 <- tibble(df_final[,1], point(df_final[,2]), point(df_final[,3]), point(df_final[,4]), 
                        point(df_final[,5]), point(df_final[,6]), point(df_final[,7]), 
                        point(df_final[,8]), point(df_final[,9]), point(df_final[,10]), 
                        point(df_final[,11]), point(df_final[,12]), point(df_final[,13]))
         names(df2) <- names(df_final)
         df2
        

        },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip',
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All")
                           )
        )
      )
  )
  
  ##------------------##  
  ##--Table District--##
  ##------------------##  
  
  ##------------------------------##  
  ##--Table District -- NEW!!!!!--##
  ##------------------------------##
  
  ##--Simulation
  table_tas_district_sim <- reactive({
    df_ta_all <- simulation_function()[[3]] %>% 
      filter(Lvl3 == input$district) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
  
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    
    ##--Creating Loop
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    
    ta_all_final <- ta_all %>% 
      filter(date == today() + days(input$projection)) %>% 
      select(TA:Death)
    
    names(ta_all_final) <- c("TA", "Cases (simulation projection)", "Hosp. (simulation projection)",
                             "ICU (simulation projection)", "Death (simulation projection)")
    ta_all_final
    })
  
  ##--Status quo
  table_tas_district_sq <- reactive({
    df_ta_all <- simulation_baseline()[[3]] %>% 
      filter(Lvl3 == input$district) %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    
    ##--Creating Loop
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    
    ta_all_final <- ta_all %>% 
      filter(date == today() + days(input$projection)) %>% 
      select(TA:Death) 
    
    names(ta_all_final) <- c("TA", "Cases (status quo projection)", "Hosp. (status quo projection)",
                             "ICU (status quo projection)", "Death (status quo projection)")
    
    ##--To date
    ta_all_to_date <- ta_all %>% 
      filter(date == today()) %>% 
      select(TA:Death)
    
    names(ta_all_to_date) <- c("TA", "Cases (to date)", "Hosp. (to date)",
                             "ICU (to date)", "Death (to date)")
    result <- list(ta_all_to_date, ta_all_final)
    return(result)
    })
  ##--------------------------------###
  output$table_district <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        df <- cbind(district_projection_to_date(),
                     district_projection_status_quo()[[2]],
                     district_sim()[[2]]) %>%
          mutate(TA = "Total") %>%
          select(TA, `Cases (to date)` : `Death (simulation projection)`)

        names(df)[6:9] <- c("Cases (status quo projection)", "Hosp. (status quo projection)",
                            "ICU (status quo projection)", "Death (status quo projection)")
        # df
        
         df_all <- cbind(table_tas_district_sq()[[1]],
                         table_tas_district_sq()[[2]][,-1], 
                         table_tas_district_sim()[,-1])
         #df_all

        df_final <- rbind(df, df_all)


         point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
         df2 <- tibble(df_final[,1], point(df_final[,2]), point(df_final[,3]), point(df_final[,4]),
                       point(df_final[,5]), point(df_final[,6]), point(df_final[,7]), point(df_final[,8]),
                       point(df_final[,9]), point(df_final[,10]), point(df_final[,11]),
         point(df_final[,12]), point(df_final[,13]))
         names(df2) <- names(df_final)
        df2
        
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))
        )
        )
  )
  
  ##------------##  
  ##--Table TA--##
  ##------------##  
  
  ##------------------------------##  
  ##--Table TAs   --    NEW!!!!!--##
  ##------------------------------##
  ##--Status quo
  new_ta_table_status_quo <-  reactive({
    df_ta_all <- simulation_baseline()[[3]] %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    #--Organizing compartments
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    
    #--to date
    status_quo_ta_all_to_date <- ta_all %>% 
      filter(date == today())
    names(status_quo_ta_all_to_date) <- c("date", "TA", "Cases (to date)", "Hosp. (to date)",
                                          "ICU (to date)", "Death (to date)", "Severe (to date)")
    status_quo_ta_all_to_date <- status_quo_ta_all_to_date %>% 
      filter(TA != input$ta)
    
    #--projection status quo
    status_quo_ta_all_projection <- ta_all %>% 
      filter(date == today() + days(input_projection))
    names(status_quo_ta_all_projection) <- c("date", "TA", "Cases (status quo projection)", "Hosp. (status quo projection)",
                                             "ICU (status quo projection)", "Death (status quo projection)", "Severe (status quo projection)")
    status_quo_ta_all_projection <- status_quo_ta_all_projection %>% 
      filter(TA != input$ta)
    
    #--Binding to date with status quo projection 
    cbind(status_quo_ta_all_to_date[,-c(1,7)], status_quo_ta_all_projection[,-c(1,2,7)])
  })
  
  ##--Simulation
  new_ta_table_simulation <- reactive({
    df_ta_all <- simulation_function()[[3]] %>% 
      select(Lvl4, State, People, date) %>% 
      group_by(date, State, Lvl4) %>% 
      summarize(new_inf = sum(People))
    df_ta_spread <- spread(df_ta_all, key = State, value = new_inf) %>% 
      mutate(Cases = `New Infections`)
    
    #--Organizing compartments
    ta_names <- df_ta_spread$Lvl4 %>% unique()
    list_ta <- list()
    for(i in 1:length(ta_names)){
      filter_ta <- df_ta_spread %>% 
        filter(Lvl4 == ta_names[i])
      
      Cases_cum_ta = cumsum(filter_ta$Cases) %>% round()
      Hosp_cum_ta = cumsum(filter_ta$Hospitalized)/hosp_time 
      Critical_cum_ta = cumsum(filter_ta$Critical)/crit_time 
      Dead_ta =  filter_ta$Dead %>% round()
      Severe_ta = round(Critical_cum_ta + Hosp_cum_ta + Dead_ta)
      
      simulation_ta <- tibble(date = filter_ta$date,
                              TA = ta_names[i],
                              Cases = Cases_cum_ta,
                              Hospitalizations = round(Hosp_cum_ta),
                              ICU = round(Critical_cum_ta),
                              Death = Dead_ta,
                              Severe = Severe_ta)
      list_ta[[i]] <- simulation_ta
    }
    ta_all <- do.call(rbind, list_ta)
    #--projection simulation
    status_quo_ta_all_projection <- ta_all %>% 
      filter(date == today() + days(input_projection))
    names(status_quo_ta_all_projection) <- c("date", "TA", "Cases (simulation projection)", "Hosp. (simulation projection)",
                                             "ICU (simulation projection)", "Death (simulation projection)", "Severe (simulation projection)")
    status_quo_ta_all_projection <- status_quo_ta_all_projection %>% 
      filter(TA != input$ta)
  })
  ##---------------------------------------##
  
  
  output$table_ta <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        #cbind(ta_simulation_status_quo()[[3]][,-1], ta_simulation_status_quo()[[2]][,-1], ta_simulation()[[2]][,-1])
        df <- cbind(ta_simulation_status_quo()[[3]][,-1],
                     ta_simulation_status_quo()[[2]][,-1],
                     ta_simulation()[[2]][,-c(1,6)])

        df$TA <- input$ta
        df <- df %>% 
          select(TA, `Cases (to date)`:`Death (simulation projection)`)
        
        names(df)[4] <- "ICU (to date)"
        names(df)[8] <- "ICU (status quo projection)"
        
        #df
        ##New tests
        all_tas <- cbind(new_ta_table_status_quo(), new_ta_table_simulation()[,-c(1,2,7)])
        
        all_tas
        final_ta <- rbind(df, all_tas)

        #final_ta

        point <- format_format(big.mark = ",", decimal.mark = ".", scientific = FALSE)
        df2 <- tibble(final_ta[,1], point(final_ta[,2]), point(final_ta[,3]), point(final_ta[,4]),
                        point(final_ta[,5]), point(final_ta[,6]), point(final_ta[,7]), point(final_ta[,8]),
                        point(final_ta[,9]), point(final_ta[,10]), point(final_ta[,11]), point(final_ta[,12]),
                       point(final_ta[,13]))
         names(df2) <- names(final_ta)
         df2
        
        
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      #filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        #dom = 'Bt',
        buttons = c('csv', 'excel'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))
        )
        )
  )
}##--end server

##--App
shinyApp(ui, server)