### MAT 465, Day 13: Iterated forecasting

# Clear out your workspace:
rm(list = ls())

# Load up libraries and functions:
library(tidyverse)
library(broom)  # You may need to install this package

# Source this file for helpful functions
source('https://raw.githubusercontent.com/jmzobitz/soilflux4cast/refs/heads/main/scripts/forecast_helper_functions.R')


forecast_variable <- "temperature" ###   # Define the forecast variable you want

### ADDING PARAMETER UNCERTAINTY:
# Pseudocode to write the model:

lm_model <- function(air_temperature ,
  uncertainty = FALSE) {

  # Enter in all parameters:
  p1 <- 241.68694
  p2 <- 0.90317 ## <-- include the value here


  # List each of the parameters, include the uncertainty:
  p1_sd <- 6.16417
  p2_sd <- 0.02088


  if (uncertainty) {
    # List each of parameters, adding on the uncertainty:
    p1 <- p1 + rnorm(1,sd = p1_sd)
    p2 <- p2 + rnorm(1,sd = p2_sd)

  }


  # Write down the formula to compute, using all covariates:
  p1 + p2*air_temperature

}

# Now run the model at each location:
neon_site="BARC"
date_to_forecast <- "2024-01-01"
forecast_variables <- "air_temperature" 

future <- get_forecast_noaa(site = neon_site,
                            forecast_date = date_to_forecast,
                            met_variables = forecast_variables)

glimpse(future)

future_target <- future |>
  group_by(parameter) |> # This is the ensemble value
  nest() |>
  mutate(data = map(.x=data,.f=~mutate(.x,
                                       prediction = lm_model(air_temperature,uncertainty = TRUE) 
  )
  )
  ) |>
  unnest(cols=c(data)) |>
  mutate(variable = forecast_variable) |>
  select(parameter,datetime,family,site_id,reference_datetime,prediction,variable)

forecast_url <- "https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz"

# then run this line to see your targets
forecast_targets <- read_csv(forecast_url, show_col_types = FALSE)

site_year <- 2024
historical_noaa <- noaa_mean_historical(site = neon_site,
                                        year = site_year)

forecast_variable <- "temperature"

noaa_measurement <- "air_temperature"



regression_formula <- temperature ~ 1 + air_temperature

forecast_fit <- fit_lm_model(target_data = forecast_targets,
                             noaa_data = historical_noaa,
                             regression_equation = regression_formula)


summary(forecast_fit)



### ADDING PROCESS UNCERTAINTY

process_sd <- 2.32 ###  # <---- define your process error
  n_obs <- nrow(future_target)  # This counts the rows

# Now add the process error
future_target$prediction <- future_target$prediction + rnorm(n_obs,sd = process_sd)

# Plot the result:
ggplot(data = future_target) +
  geom_line(aes(x=datetime,y=prediction,group=parameter),alpha=0.4)


### ADDING INITIAL CONDITION UNCERTAINTY
# Computed values:
# target         sd
# abundance   0.126
# richness    0.601
# gcc_90      0.00361
# rcc_90      0.00452
# chla        4.50
# oxygen      0.216
# temperature 0.598
# le          3.44
# nee         0.328


# Extend out the forecast:

iterated_forecast <- iterate_forecast(forecast =  future_target ,  # <--- put in these values
                                      target = forecast_targets ,    # <--- put in these values
                                      ic_sd =0.598       # <--- put in these values
                                      )

# Plot the result:
ggplot(data = iterate_forecast) +
  geom_line(aes(x=datetime,y=prediction,group=parameter),alpha=0.4)




