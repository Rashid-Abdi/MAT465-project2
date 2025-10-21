### MAT 465, Day 14: The forecasting cycle

# Clear out your workspace:
rm(list = ls())

# Load up libraries and functions:
library(tidyverse)
library(broom)  # You may need to install this package

# Source this file for helpful functions
source('https://raw.githubusercontent.com/jmzobitz/soilflux4cast/refs/heads/main/scripts/forecast_helper_functions.R')


# Complete the following function (developed in Day 13 that is a model you are using to develop your forecasts)

### Need to specify the covariates

forecast_model <- function(air_temperature,
                           uncertainty = FALSE) {

  # Enter in all parameters:
  p1 <- -241.68694
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


# Now identify key variables in your forecast

target_name <- "temperature"   ### <--- Identify the variable you wish to forecasts
begin_forecast_date <- "2023-01-01"   ### <--- This is when you want forecasts to begin
end_forecast_date <- "2023-02-01"    ### <--- This is when you want forecasts to end
neon_site <- "BARC"    ### <--- What sites are you working at?
noaa_inputs <- c("air_temperature","surface_downwelling_longwave_flux_in_air")  ### <---   List as many NOAA inputs as needed
param_uncertainty <- FALSE  ### <-- Do we compute uncertainty on parameters?
process_sd <- 2.32  ### <-- What is your process uncertainty? (currently set to zero, but should )

# And the whole forecast cycle!
your_forecasts <- forecast_cycle(
  start_date = begin_forecast_date,
  end_date = end_forecast_date,
  forecast_variable = target_name,
  site_name = neon_site,
  model_fn = forecast_model,
  noaa_vars = noaa_inputs,
  parameter_unc = param_uncertainty,
  process_unc = process_sd
)

### Plot your forecast-observation pairs
your_forecasts |>
  ggplot(aes(x=datetime)) +
  geom_line(aes(y=q0.5)) +
  geom_ribbon(aes(ymin=q0.025,ymax=q0.975),alpha=0.3) +
  geom_point(aes(y=observation)) +
  ylab(target_name) +
  ggtitle(neon_site)

