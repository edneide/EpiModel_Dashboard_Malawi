library(shiny)
library(readr)
library(plotly)
library(DT)
#library(lubridate)
library(shinythemes)
library(tidyverse)
#library(dplyr)

#--Set wd
setwd("~/Google Drive/Coronavirus/Malawi-covid_19/Dados_Att_e_Modelos/Malawi-COVID-Epidemiological-Model/Malawi-COVID-Epidemiological-Model-master")

##--Loading masking dataframe for baseline simulations 
masking <- read_csv("baseline_simulation/masking.csv")
current <- read_csv("baseline_simulation/current.csv", 
                    col_types = cols(date = col_date(format = "%Y-%m-%d")))
df_country_baseline_dash <- read_csv("baseline_simulation/df_country_baseline_dash.csv")
df_district_baseline_dash <- read_csv("baseline_simulation/districts_dash_baseline.csv")
df_ta_baseline_dash <- read_csv("baseline_simulation/df_ta_baseline_dash.csv")


##--------##
##---UI---##
##--------##
ui <- fluidPage(theme = shinytheme("united"),
                fluidRow(
                  titlePanel(h1(strong("Epidemiological model for COVID-19 - Malawi"))),
                  column(width = 6,
                         plotlyOutput("fig"),
                         "Graph at national level"
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
                                             max = 180)
                         ),
                         column(width = 4.0, tags$h3("% Physical Distancing"),
                                tags$h4(paste0("Current %: ", 100*current$reduc[which(current$date==lubridate::today())]),"%"),
                                numericInput('distancing_perc',
                                             label = "New %",
                                             value = 35,
                                             min = 0,
                                             max = 100),
                                numericInput('time_intervention_dist',
                                             label = "Length of Intervention\n(# Days)",
                                             value = 7,
                                             min = 7,
                                             max = 180)
                         )
                  )
                ),#end fluid row
                
                #--Include actionButton to prevent write .csv occuring before user finalises selection
                fluidRow(
                  column(width = 4, offset = 7,
                         actionButton("generateButton","Set parameters", width = '300px')
                  )
                ),
                
                
                fluidRow(
                  column(width = 6, 
                         " "
                  ),
                  column(width = 6,
                         column(width = 4, 
                                selectInput('level', 'Level of Interest', choices = c("National", "District", "TA"))),
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
                ##-----------------------------------------------------------------------------------------------##  
                ##-----------------------------------------------------------------------------------------------##  
                tags$hr(),
                tags$p(
                  paste("Generated results:", today())
                ),
                fluidRow(
                  DT::DTOutput('table')
                )
) #--end fluid page--##


##------------##
##---Server---##
##------------##
server <- function(input, output, session) {
  
  ##--------------##
  ##--Simulation--##
  ##--------------##
  
  #--Creating the inputs .csv's based on user's selection 
  
  maskdatasetInput <- observe({
    
    if(input$generateButton == 0) return()
    
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
    
    if(input$generateButton == 0) return()
    
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
  ##--Stepwise simulation--##
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
    ##--End of model simulation 
    
    ##--Organizing the outputs
    
    ##----------------##
    ## National Level ##
    ##----------------##
    ## Simulation Projection 
    df_country_spread <- spread(df_country, key = State, value = People) 
    df_country_spread$date <- seq(from = as.Date("2020-04-01"),
                                  by = "day", length.out = 365)
    df_country_spread <- df_country_spread %>%
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
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection))
    df_country_dash_projection2 <- df_country_dash_projection %>% 
      dplyr::select(Cases_sim, Hospitalizations_sim, ICU_sim, Death_sim)
    names(df_country_dash_projection2) <- c("Cases (simulation projection)",
                                            "Hosp. (simulation projection)",
                                            "ICU (simulation projection)", 
                                            "Death (simulation projection)")
    ## Status quo Projection 
    df_country_status_quo <- df_country_baseline_dash %>%
      dplyr::filter(date == lubridate::today() + lubridate::days(input$projection)) %>%
      dplyr::select(Cases, Hospitalizations, ICU, Death) 
    df_country_status_quo$Death <- round(df_country_status_quo$Death)
    names(df_country_status_quo) <- c("Cases (status quo projection)",
                                      "Hosp. (status quo projection)",
                                      "ICU (status quo projection)",
                                      "Death (status quo projection)")
    
    ## To date
    df_country_to_date <- df_country_baseline_dash %>%
      dplyr::filter(date == today()) %>%
      dplyr::select(Cases:Death) %>% 
      dplyr::mutate(Death = round(Death))
    names(df_country_to_date) <- c("Cases (to date)",
                                   "Hosp. (to date)",
                                   "ICU (to date)", 
                                   "Death (to date)")
    
    ## Total table at National level 
    total_table_national <- cbind(df_country_to_date,
                                  df_country_status_quo, 
                                  df_country_dash_projection2)

    
    #--Choosing the level 
    level_selection <- isolate(input$level)
    
    if(level_selection == "National"){
      list_output <- list(level = total_table_national,
                          national = df_country_dash)
      return(list_output) 
    }else if(level_selection == "District"){
      ##----------------##
      ## District Level ##
      ##----------------##
      df_district_spread <- spread(df_district, key = State, value = People) 
      districts <- df_district_spread$Lvl3 %>% unique()
      df_district_spread$date <- rep(seq(from = as.Date("2020-04-01"),
                                         by = "day", length.out = 365), length(districts))
      df_district_spread <- df_district_spread %>% 
        mutate(Cases = `New Infections` + Hospitalized + Critical)
      
      ##--Filtering by district
      list_district_dash_simulation <- list()
      
      for(i in 1:length(districts)){
        df_district2 <- df_district_spread %>% 
          filter(Lvl3 == districts[i])
        
        Cases_cum_dist <- cumsum(df_district2$Cases) %>% round()
        Hospitalized_cum_dist <- cumsum(df_district2$Hospitalized) %>% round()
        ICU_cum_dist <- cumsum(df_district2$Critical) %>% round()
        Death_dist <- df_district2$Dead %>% round()
        
        df_district <- tibble(time = seq(1:365), 
                              date = df_district2$date,
                              District = districts[i],
                              Cases = Cases_cum_dist, 
                              Hospitalizations = Hospitalized_cum_dist,
                              ICU = ICU_cum_dist, Death = Death_dist)
        
        list_district_dash_simulation[[i]] <- df_district
      }
      
      df_districts_dash_simulation <- do.call(rbind, list_district_dash_simulation) 
      
      district_simulation <- df_districts_dash_simulation %>% 
        dplyr::filter(date == lubridate::today() + lubridate::days(input$projection)) %>%
        dplyr::select(Cases:Death)
      names(district_simulation) <- c("Cases (simulation projection)", "Hosp. (simulation projection)",
                                      "ICU (simulation projection)", "Death (simulation projection)")
      
      ## To date
      district_todate <- df_district_baseline_dash %>% 
        dplyr::filter(date == today()) %>%
        dplyr::select(District:Death)
      names(district_todate) <- c("District", "Cases (to date)", "Hosp. (to date)",
                                  "ICU (to date)", "Death (to date)")
      
      ## Status quo projection
      district_statusquo <- df_district_baseline_dash %>% 
        dplyr::filter(date == lubridate::today() + lubridate::days(input$projection)) %>% 
        dplyr::select(Cases:Death)
      names(district_statusquo) <- c("Cases (status quo projection)", "Hosp. (status quo projection)",
                                     "ICU (status quo projection)", "Death (status quo projection)")
      ## Total table
      total_table_district <- cbind(district_todate,
                                    district_statusquo, 
                                    district_simulation)
      
      list_output <- list(level = total_table_district,
                          national = df_country_dash)
      return(list_output) 
    }else if(level_selection == "TA"){
      ##-------------------##
      ##--TA Level---------##
      ##-------------------##
      ta_simulation <- reactive({
        tas_names <- df_ta$Lvl4 %>% unique()
        
        ##--Compartiments of interest 
        state_table <- c("New Infections", "Hospitalized", "Critical", "Dead")
        
        ##--Inserting date--##
        df_ta2 <- df_ta 
        df_ta2$date <- rep(seq(from = as.Date("2020-04-01"), 
                               by = "day", length.out = 365), 10152)  
        
        
        ##--Filtering for projection
        ta_test <- df_ta2 %>% 
          filter(date <= today() + days(input$projection))
        
        ta_test <- ta_test[,-c(1,2)] %>% 
          filter(State %in% state_table)
        
        ##--Simulation TA
        ##--Creating a list to store TAs information
        list_TAs_simulation <- list()
        for(i in 1:length(tas_names)){
          ta_test2 <- ta_test %>% 
            filter(Lvl4 == tas_names[i]) %>% 
            select(Lvl4, State, People, date) %>% 
            group_by(date, State) %>% 
            summarize(new_inf = sum(People)) %>% 
            mutate(TA = tas_names[i])
          
          ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>% 
            mutate(Cases = `New Infections` + Hospitalized + Critical)
          
          Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
          Hosp_cum_ta = cumsum(ta1_spread$Hospitalized) %>% round()
          Critical_cum_ta = cumsum(ta1_spread$Critical) %>% round()
          Dead_ta =  ta1_spread$Dead %>% round()
          
          todate_ta <- tibble(TA = tas_names[i],
                              Cases = Cases_cum_ta %>% tail(1),
                              Hosp = Hosp_cum_ta %>% tail(1),
                              Critical = Critical_cum_ta %>% tail(1),
                              Death = Dead_ta %>% tail(1))
          
          list_TAs_simulation[[i]] <- todate_ta
        }
        
        TAs_simulation <- do.call(rbind, list_TAs_simulation) 
        names(TAs_simulation) <- c("TA", "Cases (simulation projection)", "Hosp. (simulation projection)",
                                   "ICU (simulation projection)", "Death (simulation projection)")
        TAs_simulation
      })
      
      ##------------------------##
      ##--To date TA--##
      ##------------------------##
      ta_to_date <- reactive({
        tas_names <- df_ta$Lvl4 %>% unique()

        ##--Compartiments of interest
        state_table <- c("New Infections", "Hospitalized", "Critical", "Dead")

        ##--Inserting date--##
        df_ta2 <- df_ta_baseline_dash

        ##--Filtering for projection
        ta_test <- df_ta2 %>%
          filter(date <= today())

        ta_test <- ta_test[,-c(1,2)] %>%
          filter(State %in% state_table)

        ##--Creating a list to store TAs information
        list_TAs_todate <- list()
        for(i in 1:length(tas_names)){
          ta_test2 <- ta_test %>%
            filter(Lvl4 == tas_names[i]) %>%
            select(Lvl4, State, People, date) %>%
            group_by(date, State) %>%
            summarize(new_inf = sum(People)) %>%
            mutate(TA = tas_names[i])

          ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>%
            mutate(Cases = `New Infections` + Hospitalized + Critical)

          Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
          Hosp_cum_ta = cumsum(ta1_spread$Hospitalized) %>% round()
          Critical_cum_ta = cumsum(ta1_spread$Critical) %>% round()
          Dead_ta =  ta1_spread$Dead %>% round()

          todate_ta <- tibble(TA = tas_names[i],
                              Cases = Cases_cum_ta %>% tail(1),
                              Hosp = Hosp_cum_ta %>% tail(1),
                              Critical = Critical_cum_ta %>% tail(1),
                              Death = Dead_ta %>% tail(1))

          list_TAs_todate[[i]] <- todate_ta
        }

        TAs_todate <- do.call(rbind, list_TAs_todate)
        names(TAs_todate) <- c("TA", "Cases (to date)", "Hosp. (to date)",
                                   "ICU (to date)", "Death (to date)")
        TAs_todate
      })

##------------------------##
##--Status quo TA---------##
##------------------------##
ta_status_quo <- reactive({
        tas_names <- df_ta$Lvl4 %>% unique()
        
##--Compartiments of interest
state_table <- c("New Infections", "Hospitalized", "Critical", "Dead")
        
##--Inserting date--##
df_ta2 <- df_ta_baseline_dash
        
##--Filtering for projection
ta_test <- df_ta2 %>%
  filter(date <= today() + days(input$projection))
        
ta_test <- ta_test[,-c(1,2)] %>%
  filter(State %in% state_table)
        
##--Creating a list to store TAs information
list_TAs_statusquo <- list()
for(i in 1:length(tas_names)){
ta_test2 <- ta_test %>%
  filter(Lvl4 == tas_names[i]) %>%
  select(Lvl4, State, People, date) %>%
  group_by(date, State) %>%
  summarize(new_inf = sum(People)) %>%
  mutate(TA = tas_names[i])
          
ta1_spread <- spread(ta_test2, key = State, value = new_inf) %>%
  mutate(Cases = `New Infections` + Hospitalized + Critical)
          
Cases_cum_ta = cumsum(ta1_spread$Cases) %>% round()
Hosp_cum_ta = cumsum(ta1_spread$Hospitalized) %>% round()
Critical_cum_ta = cumsum(ta1_spread$Critical) %>% round()
Dead_ta =  ta1_spread$Dead %>% round()
          
status_quo_ta <- tibble(TA = tas_names[i],
                        Cases = Cases_cum_ta %>% tail(1),
                        Hosp = Hosp_cum_ta %>% tail(1),
                        Critical = Critical_cum_ta %>% tail(1),
                        Death = Dead_ta %>% tail(1))
          
          list_TAs_statusquo[[i]] <- status_quo_ta
        }
        
        TAs_statusquo <- do.call(rbind, list_TAs_statusquo)
        names(TAs_statusquo) <- c("TA", "Cases (status quo projection)", "Hosp. (status quo projection)",
                               "ICU (status quo projection)", "Death (status quo projection)")
        TAs_statusquo
      })
      
##------------------------##
list_output <- list(level = cbind(ta_to_date(), ta_status_quo()[,-1], ta_simulation()[,-1]),
                    national = df_country_dash)
return(list_output) 
    }
  })
  
  
##---------##  
##--Plot--##
##---------##
output$fig <- renderPlotly({
    
    if(input$runreportButton == 0){
      data_final_plot <- df_country_baseline_dash
      fig <-  plot_ly(data_final_plot,
                      x = ~ time,
                      y = ~ Death,
                      type = 'bar',
                      name = 'Death')
      fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                            yaxis = list(title = ''),
                            barmode = 'stack')
      fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU')
      fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hospitalizations')
      fig <- fig %>% add_trace(y = ~ Cases, name = 'Cases')
      fig
    } else {
      data_final_plot <- reactive({
        cbind(df_country_baseline_dash, simulation_function()[[2]][,-1])
      })
      fig <-  plot_ly(data_final_plot(),
                      x = ~ time,
                      y = ~ Death_sim,
                      type = 'bar',
                      name = 'Death (simulation)')
      fig <- fig %>% layout(xaxis = list(title = "Time (days)"),
                            yaxis = list(title = ''),
                            barmode = 'stack')
      fig <- fig %>% add_trace(y = ~ Death, name = 'Death')
      fig <- fig %>% add_trace(y = ~ ICU_sim, name = 'ICU (simulation)')
      fig <- fig %>% add_trace(y = ~ ICU, name = 'ICU')
      fig <- fig %>% add_trace(y = ~ Hospitalizations_sim, name = 'Hosp (simulation)')
      fig <- fig %>% add_trace(y = ~ Hospitalizations, name = 'Hospitalizations')
      fig <- fig %>% add_trace(y = ~ Cases_sim, name = 'Cases (simulation)')
      fig <- fig %>% add_trace(y = ~ Cases, name = 'Cases')
      fig
    }
  })
  
  ##---------##  
  ##--Table--##
  ##---------##  
  output$table <- DT::renderDT(
    DT::datatable(
      {
        simulation_function()[[1]]
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
}#--end of server

##---------------##
##--Run the App--##
##---------------##
shinyApp(ui, server)