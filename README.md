# EpiModel_Dashboard_Malawi

This is a **Shiny application** for a COVID-19 epidemiological model for Malawi.

This application was build in R (version 3.6.3) with the RStudio IDE (version Version 1.4.869). Besides the **shiny** library, other additional libraries were used to organize the model outputs and present the visualizations on the final app, such **readr**, **plotly**, **DT**, **lubridate**, **shinythemes** and **tidyverse**.

## Policy Levers

In this section of the dashboard, the user can set a new percentage for masking and the length of intervention (in days) of this control measure. The same is applied to social distancing. It is also possible to choose the time horizon projection for the parameters set. In this way, the user can have an overview of future scenarios if the masking and social distancing are applied for a specific amount of days. After all parameters setting are done, the user can click on **Run Report** button, to generate the results. 

## Model Priors

This section shows the parameters used in the epidemiological model simulation, and are derived from scientific literature and data from Malawi. These parameters refer to the basic reproduction number $R_0$, Infectious Time (Days), Hospitalized Time (Days), and ICU Time (Days). The Hospitalized Rate of Infected, ICU Risk Among Hospitalized, and Fatality Rate of ICU are presented for different age groups: Pediatrics, Adults, and Elderly.

## Outputs

The first result is presented as a line plot (using the **plotly** package) showing the disease dynamic for the baseline simulation and the new scenario simulation. Both simulations are run for 365 days, going from April 1st, 2020 until March 3rd, 2021. It is possible to see the Deaths, ICU, Hospitalizations, and Cases cumulative graphs for both, baseline and new scenario simulation. By moving the mouse over the plot it is possible to see the corresponding numbers. 

Below the graph, two tables are showing the potential reductions achieved due to the implemented measures. The first table **Absolute reduction to implemented measures** show the potential reductions in the number of cases, hospitalizations, ICU, and deaths. The second table **Percentual reduction due to implemented measures** show the same quantities but in percentage. Those comparisons are made using the values from the simulation projection and the status quo projection.

At the bottom of the dashboard, the user can find a table showing the current and future situations. The first four columns refer to the current situation and are named as **Cases (to date)**, **Hosp. (to date)**, **ICU (to date)**, **Death (to date)**.  The other eight columns refer to projections. There is the status quo projections, which means that no control measure was taken, named as **Cases (status quo)**, **Hosp. (status quo)**, **ICU (status quo)**, **Death (status quo)**, and the simulation projection, that take into account the control measures set by the user, named as  **Cases (simulation projection)**, **Hosp. (simulation projection)**, **ICU (simulation projection)**, **Death (simulation projection)**.

The user can navigate by the **District** and **TA** tabs to see the same information described before, for different territorial levels. The districts and TAs can be selected from a list in the right panel, and all the information is updated on the screen. 






