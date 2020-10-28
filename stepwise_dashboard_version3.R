library(shiny)
library(readr)
library(plotly)
library(DT)
library(lubridate)
library(shinythemes)
library(tidyverse)

##--Loading dataframes for baseline simulations 
masking <- read_csv("masking_with_date.csv")
current <- read_csv("current_with_date.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))
districts_names <- read_csv("districts_names.csv")
ta_names <- read_csv("tas_names.csv")


##--UI
ui <- fluidPage(theme = shinytheme("united"),
  ##--National
  tabsetPanel(
    tabPanel("National",
             fluidRow(
               titlePanel(h1(strong("Epidemiological model for COVID-19 - Malawi"))),
               tags$p(),
               tags$p(),
               column(width = 6,
                      plotlyOutput("fig"),
                      tags$p(),
                      tags$hr(),
                      h4(strong("Absolute reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_country_abs'),
                      tags$p(),
                      h4(strong("Percentual reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_country')
               ),
               column(width = 6,
                      # Model Priors
                      tags$h1("Model Priors"),
                      tags$hr(),
                      column(width = 4.0, 
                             h4(strong("R0:"), "1.9"),
                             h4(strong("Infectious Time (Days):"), "7"),
                             h4(strong("Hospitalized Time (Days):"), "4"),
                             h4(strong("Hospitalized Rate of Infected")),
                             h5(em("Pediatrics (<20):"), "0.0090%"),
                             h5(em("Adults (20-49):"), "1.2%"),
                             h5(em("Elderly (50+):"), "5.5%")
                      ),
                      column(width = 4.0, 
                             h4(strong("ICU Time (Days):"), "8"),
                             h4(strong("ICU Risk Among Hospitalized:")),
                             h5(em("Pediatrics (<20):"), "5.0%"),
                             h5(em("Adults (20-49):"), "14%"),
                             h5(em("Elderly (50+):"), "28%"),
                             h4(strong("Fatality Rate of ICU:")),
                             h5(em("Pediatrics (<20):"), "9.0%"),
                             h5(em("Adults (20-49):"), "20%"),
                             h5(em("Elderly (50+):"), "59%")
                      )),
               column(width = 6,
                      # Policy Levers
                      tags$h1("Policy Levers"),
                      tags$hr(),
                      column(width = 4.0, tags$h3("% Masking"),
                             tags$h4("Current %: 15 %"),
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
                                          max = 50)
                      ),
                      column(width = 4.0, tags$h3("% Physical Distancing"),
                             tags$h4(paste0("Current %: ", 100*current$reduc[which(current$date==lubridate::today())]),"%"),
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
                      )
               )
             ),##--end of FluidRow
             
             fluidRow(
               column(width = 6, 
                      " "
               ),
               column(width = 6,
                      # column(width = 4, 
                      #        selectInput('level', 'Level of Interest', choices = c("National", "District", "TA"))),
                      column(width = 4,
                             #--Time Horizon Projection
                             numericInput('projection', 'Time Horizon Projection (days)', 
                                          value = 3, min = 1, max = 180)
                      )
               )
               
               
             ),
             
             #--Include actionButton to run the report
             fluidRow(
               column(width = 4, offset = 7, 
                      actionButton("runreportButton", "Run Report", width = '300px'))
             ),
             tags$hr(),
             tags$p(
               paste("Generated results:", today())
             ),
             fluidRow(
               DT::DTOutput('table_national')
             )
             ), ##--end National
    ##--District
    tabPanel("District",
             fluidRow(
               titlePanel(h1(strong("District Level"))),
               column(width = 8,
                      plotlyOutput("fig_district"),
                      tags$hr(),
                      h4(strong("Absolute reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_district_abs'),
                      tags$p(),
                      h4(strong("Percentual reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_districts')
                      ),
               column(width = 4, 
                      selectInput('district', 'District', 
                                  choices = districts_names$districts)
                      )
             ),
             tags$hr(),
             tags$p(
               paste("Generated results:", today())
             ),
             fluidRow(
               DT::DTOutput('table_district')
             )
             ), ##--end Panel district
    
    ##--TA
    tabPanel("TA",
             fluidRow(
               titlePanel(h1(strong("TA Level"))),
               column(width = 8,
                      plotlyOutput("fig_ta"),
                      tags$hr(),
                      h4(strong("Absolute reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_ta_abs'),
                      tags$p(),
                      h4(strong("Percentual reduction due to implemented measures:")),
                      tags$p(),
                      tableOutput('table_reductions_ta_perc')
               ),
               column(width = 4, 
                      selectInput('ta', 'TA', 
                                  choices = ta_names$ta_name)
               )
             ),
             tags$hr(),
             tags$p(
               paste("Generated results:", today())
             ),
              fluidRow(
                DT::DTOutput('table_ta')
              )
             )##--end panel TA
  )##--end tabsetPanel
)##--end UI

##--Server
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
  maskdatasetInput <- observe({
    
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
  ##--##
  
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
      dplyr::mutate(Cases = `New Infections` + Hospitalized + Critical)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized) %>% round()
    ICU_cum <- cumsum(df_country_spread$Critical) %>% round()
    Death <- df_country_spread$Dead %>% round()

    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:365,
                              Cases_sim = Cases_cum,
                              Hospitalizations_sim = Hospitalized_cum,
                              ICU_sim = ICU_cum, Death_sim = Death)
   

    df_country_dash_projection <- df_country_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection))
    df_country_dash_projection2 <- df_country_dash_projection %>%
      dplyr::select(Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("Cases (simulation projection)",
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
    mutate(Cases = `New Infections` + Hospitalized + Critical)
  district_df$Cases_cum <- cumsum(district_df$Cases)
  Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
  Hospitalized_cum_dist <- cumsum(district_df$Hospitalized) %>% round()
  ICU_cum_dist <- cumsum(district_df$Critical) %>% round()
  Death_dist <- district_df$Dead %>% round()
  ##------------------------------------------------------------##
  district_df2 <- tibble(time = seq(1:365), 
                        date = district_df$date,
                        Cases = Cases_cum_dist, 
                        Hospitalizations = Hospitalized_cum_dist,
                        ICU = ICU_cum_dist, Death = Death_dist) 
  #district_df2 -- OK
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
    mutate(Cases = `New Infections` + Hospitalized + Critical)
  
  Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
  Hosp_cum_ta = cumsum(ta1_spread$Hospitalized) %>% round()
  Critical_cum_ta = cumsum(ta1_spread$Critical) %>% round()
  Dead_ta =  ta1_spread$Dead %>% round()
  
  simulation_ta <- tibble(date = ta1_spread$date,
                          Cases = Cases_cum_ta,
                          Hospitalizations = Hosp_cum_ta,
                          ICU = Critical_cum_ta,
                          Death = Dead_ta)
  ##--Total
  #simulation_ta
##-------------------------##
  ##--Projection--##
  projection_ta <- simulation_ta %>% 
    filter(date == lubridate::today() + lubridate::days(input$projection))
  names(projection_ta) <- c("date", "Cases (simulation projection)",
                            "Hosp. (simulation projection)",
                            "Critical (simulation projection)",
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
      dplyr::mutate(Cases = `New Infections` + Hospitalized + Critical)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized) %>% round()
    ICU_cum <- cumsum(df_country_spread$Critical) %>% round()
    Death <- df_country_spread$Dead %>% round()
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              time = 1:365,
                              Cases_sq = Cases_cum,
                              Hospitalizations_sq = Hospitalized_cum,
                              ICU_sq = ICU_cum, Death_sq = Death)
    
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
      dplyr::mutate(Cases = `New Infections` + Hospitalized + Critical)
    Cases_cum <- cumsum(df_country_spread$Cases) %>% round()
    Hospitalized_cum <- cumsum(df_country_spread$Hospitalized) %>% round()
    ICU_cum <- cumsum(df_country_spread$Critical) %>% round()
    Death <- df_country_spread$Dead %>% round()
    
    df_country_dash <- tibble(date = df_country_spread$date,
                              Cases_sim = Cases_cum,
                              Hospitalizations_sim = Hospitalized_cum,
                              ICU_sim = ICU_cum, Death_sim = Death)
    
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
      mutate(Cases = `New Infections` + Hospitalized + Critical)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized) %>% round()
    ICU_cum_dist <- cumsum(district_df$Critical) %>% round()
    Death_dist <- district_df$Dead %>% round()
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:365), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = Hospitalized_cum_dist,
                           ICU_sq = ICU_cum_dist, 
                           Death_sq = Death_dist) 
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
      mutate(Cases = `New Infections` + Hospitalized + Critical)
    district_df$Cases_cum <- cumsum(district_df$Cases)
    Cases_cum_dist <- cumsum(district_df$Cases) %>% round()
    Hospitalized_cum_dist <- cumsum(district_df$Hospitalized) %>% round()
    ICU_cum_dist <- cumsum(district_df$Critical) %>% round()
    Death_dist <- district_df$Dead %>% round()
    ##------------------------------------------------------------##
    district_df2 <- tibble(time = seq(1:365), 
                           date = district_df$date,
                           Cases_sq = Cases_cum_dist, 
                           Hospitalizations_sq = Hospitalized_cum_dist,
                           ICU_sq = ICU_cum_dist, 
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
      mutate(Cases = `New Infections` + Hospitalized + Critical)
    
    Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
    Hosp_cum_ta = cumsum(ta1_spread$Hospitalized) %>% round()
    Critical_cum_ta = cumsum(ta1_spread$Critical) %>% round()
    Dead_ta =  ta1_spread$Dead %>% round()
    
    simulation_ta_sq <- tibble(date = ta1_spread$date,
                               time = seq(1:365),
                            Cases_sq = Cases_cum_ta,
                            Hospitalizations_sq = Hosp_cum_ta,
                            ICU_sq = Critical_cum_ta,
                            Death_sq = Dead_ta)
    ##-------------------------##
    projection_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today() + lubridate::days(input$projection))
    names(projection_ta_sq) <- c("date", "Cases (status quo projection)",
                              "Hosp. (status quo projection)",
                              "Critical (status quo projection)",
                              "Death (status quo projection)")
    
    ##-------------------------##
    to_date_ta_sq <- simulation_ta_sq[,-2] %>% 
      filter(date == lubridate::today())
    names(to_date_ta_sq) <- c("date", "Cases (to date)",
                                 "Hosp. (to date)",
                                 "Critical (date)",
                                 "Death (date)")
    
    result <- list(simulation_ta_sq, projection_ta_sq, to_date_ta_sq)
    return(result)
  })
  
  
  ##-----------------##
  ##--Plot National--##
  ##-----------------##
  output$fig <- renderPlotly({
    
    if(input$runreportButton == 0) return()
      data_final_plot <- cbind(country_projection_status_quo()[[1]], country_projection_sim()[[1]][,-c(1,2)])
      fig <-  plot_ly(data_final_plot,
                      x = ~ time,
                      y = ~ Death_sq,
                      type = 'scatter',
                      mode = 'lines',
                      name = 'Death',
                      line = list(color = 'black'))
      fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                            yaxis = list(title = ''))
      fig <- fig %>% add_trace(y = ~ Death_sim, name = 'Death_sim', line = list(color = 'grey'))
      fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU', line = list(color = 'blue'))
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'ICU_sim', line = list(color = 'lightblue'))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, name = 'Hospitalizations_sim', line = list(color = 'rgb(204,255,204)'))
      fig <- fig %>% add_trace(y = ~ Cases_sq, name = 'Cases', line = list(color = 'red'))
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Cases_sim', line = list(color = 'pink'))
      fig
  })
  
  ##-----------------##
  ##--Plot District--##
  ##-----------------##
  output$fig_district <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    #data_final_plot <- district_projection_status_quo()[[1]]
    data_final_plot <- cbind(district_projection_status_quo()[[1]], district_sim()[[1]][,-c(1,2)])
    fig <-  plot_ly(data_final_plot,
                    x = ~ time,
                    y = ~ Death_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Death',
                    line = list(color = 'black'))
    fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Death, name = 'Death (simulation)', line = list(color = 'grey'))
    fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU', line = list(color = 'blue'))
    fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (simulation)', line = list(color = 'lightblue'))
    fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
    fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hospitalizations (simulation)', line = list(color = 'rgb(204,255,204)'))
    fig <- fig %>% add_trace(y = ~ Cases_sq, name = 'Cases', line = list(color = 'red'))
    fig <- fig %>% add_trace(y = ~ Cases, name = 'Cases (simulation)', line = list(color = 'pink'))
    fig
  })
  
  ##-----------##
  ##--Plot TA--##
  ##-----------##
  output$fig_ta <- renderPlotly({
    
    if(input$runreportButton == 0) return()
    
    data_final_plot <- cbind(ta_simulation_status_quo()[[1]], ta_simulation()[[1]][,-1])
    #data_final_plot <- ta_simulation_status_quo()[[1]]
    
    fig <-  plot_ly(data_final_plot,
                    x = ~ time,
                    y = ~ Death_sq,
                    type = 'scatter',
                    mode = 'lines',
                    name = 'Death',
                    line = list(color = 'black'))
    fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                          yaxis = list(title = ''))
    fig <- fig %>% add_trace(y = ~ Death, name = 'Death (simulation)', line = list(color = 'grey'))
    fig <- fig %>% add_trace(y = ~ ICU_sq, name = 'ICU', line = list(color = 'blue'))
    fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU (simulation)', line = list(color = 'lightblue'))
    fig <- fig %>% add_trace(y = ~ Hospitalizations_sq, name = 'Hospitalizations', line = list(color = 'rgb(0, 102, 0)'))
    fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hospitalizations (simulation)', line = list(color = 'rgb(204,255,204)'))
    fig <- fig %>% add_trace(y = ~ Cases_sq, name = 'Cases', line = list(color = 'red'))
    fig <- fig %>% add_trace(y = ~ Cases, name = 'Cases (simulation)', line = list(color = 'pink'))
    fig
  })
  
  
  ##---------------------------------##  
  ##--Table for Reductions National--##
  ##---------------------------------##
  output$table_reductions_country_abs <- renderTable({
    if(input$runreportButton == 0)return()
    reduc_cases <- country_projection_status_quo()[[2]][1,1] - country_projection_sim()[[2]][1,1] 
    reduc_hosp <- country_projection_status_quo()[[2]][1,2]  - country_projection_sim()[[2]][1,2] 
    reduc_icu <- country_projection_status_quo()[[2]][1,3]  - country_projection_sim()[[2]][1,3] 
    reduc_death <-  country_projection_status_quo()[[2]][1,4]  - country_projection_sim()[[2]][1,4]
    table <- tibble("Reduction in Cases" = reduc_cases,
                    "Reduction in Hosp." = reduc_hosp,
                    "Reduction in ICU" = reduc_icu,
                    "Reduction in Death" = reduc_death
                    )
    return(table)
  })
  
  output$table_reductions_country <- renderTable({
    if(input$runreportButton == 0)return()
      reduc_cases <- abs(100*(country_projection_sim()[[2]][1,1]/country_projection_status_quo()[[2]][1,1] - 1))
      reduc_hosp <- abs(100*(country_projection_sim()[[2]][1,2]/country_projection_status_quo()[[2]][1,2] - 1))
      reduc_icu <- abs(100*(country_projection_sim()[[2]][1,3]/country_projection_status_quo()[[2]][1,3] - 1))
      reduc_death <- abs(100*(country_projection_sim()[[2]][1,4]/country_projection_status_quo()[[2]][1,4] - 1))
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
    reduc_hosp <- district_projection_status_quo()[[2]][1,2]  - district_sim()[[2]][1,2] 
    reduc_icu <- district_projection_status_quo()[[2]][1,3]  - district_sim()[[2]][1,3] 
    reduc_death <-  district_projection_status_quo()[[2]][1,4]  - district_sim()[[2]][1,4]
    table <- tibble("Reduction in Cases" = reduc_cases,
                    "Reduction in Hosp." = reduc_hosp,
                    "Reduction in ICU" = reduc_icu,
                    "Reduction in Death" = reduc_death
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
    reduc_cases <- ta_simulation_status_quo()[[2]][1,2] - ta_simulation()[[2]][1,2] 
    reduc_hosp <- ta_simulation_status_quo()[[2]][1,3] - ta_simulation()[[2]][1,3] 
    reduc_icu <- ta_simulation_status_quo()[[2]][1,4] - ta_simulation()[[2]][1,4] 
    reduc_death <-  ta_simulation_status_quo()[[2]][1,5] - ta_simulation()[[2]][1,5] 
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
  
  ##------------------##  
  ##--Table National--##
  ##------------------##  
  output$table_national <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        cbind(country_projection_to_date(), country_projection_status_quo()[[2]], country_projection_sim()[[2]])
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        buttons = c('csv', 'excel', 'pdf'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))))
  )
  
  ##------------------##  
  ##--Table District--##
  ##------------------##  
  output$table_district <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        cbind(district_projection_to_date(), district_projection_status_quo()[[2]], district_sim()[[2]])
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        buttons = c('csv', 'excel', 'pdf'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))))
  )
  
  ##------------##  
  ##--Table TA--##
  ##------------##  
  output$table_ta <- DT::renderDT(
    DT::datatable(
      {
        if(input$runreportButton == 0) return()
        cbind(ta_simulation_status_quo()[[3]][,-1], ta_simulation_status_quo()[[2]][,-1], ta_simulation()[[2]][,-1])
      },
      extensions = 'Buttons',
      
      callback = JS('table.page("next").draw(false);'),
      filter = 'top',
      options = list(
        deferRender = TRUE,
        pageLength = 10,
        autoWidth = TRUE,
        dom = 'Blfrtip', 
        buttons = c('csv', 'excel', 'pdf'), # buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(c(10 , 25, 50, -1), c(10, 25, 50, "All"))))
  )
}##--end server

##--App
shinyApp(ui, server)