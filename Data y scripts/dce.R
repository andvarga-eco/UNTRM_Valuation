rm(list = ls())
library(apollo)
library(readr)
library(janitor)

apollo_initialise()

apollo_control = list( modelName = "mnl", 
                       modelDescr = "Basic MNL model on the windmills dataset", 
                       indivID = "id_individual" )

url <- "https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/data-windmills.csv"
database<- read_csv(url)|> clean_names()
database<-na.omit(database)

#Set starting values for the parameters
apollo_beta <- c( b_asc_alt1 = 0.00, 
                  b_asc_alt2 = 0.50,
                  b_asc_alt3 = 0.50, 
                  b_medium_farms = 0.25, 
                  b_small_farms = 0.50, 
                  b_medium_height = 0.25, 
                  b_low_height = 0.50, 
                  b_red_kite = -0.05, 
                  b_min_distance = 0.50, 
                  b_cost = -0.50 )

# Specify the vector of parameters to hold fixed at their starting values
apollo_fixed <- c("b_asc_alt1") 
# Group and validate inputs 
apollo_inputs <- apollo_validateInputs()

## Define the model and likelihood function ---- 
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") { 
  ## Attach inputs and detach after function exit 
  apollo_attach(apollo_beta, apollo_inputs) 
  on.exit(apollo_detach(apollo_beta, apollo_inputs)) 
  # Define the list of utility functions 
  V <- list( 
    alt1 = c(b_asc_alt1 + 
               b_medium_farms * alt1_farm2 + 
               b_small_farms * alt1_farm3 + 
               b_medium_height * alt1_height2 + 
               b_low_height * alt1_height3 + 
               b_red_kite * alt1_redkite + 
               b_min_distance * alt1_distance + 
               b_cost * alt1_cost 
             ), 
    alt2 = c(b_asc_alt2 + 
               b_medium_farms * alt2_farm2 + 
               b_small_farms * alt2_farm3 +
               b_medium_height * alt2_height2 +
               b_low_height * alt2_height3 + 
               b_red_kite * alt2_redkite +
               b_min_distance * alt2_distance +
               b_cost * alt2_cost ), 
    alt3 = c( b_asc_alt3 +
                b_medium_farms * alt3_farm2 +
                b_small_farms * alt3_farm3 + 
                b_medium_height * alt3_height2 +
                b_low_height * alt3_height3 + 
                b_red_kite * alt3_redkite +
                b_min_distance * alt3_distance +
                b_cost * alt3_cost 
              ) 
    )
  # Define settings for MNL model component 
  mnl_settings <- list( alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
                        avail = list(alt1 = 1, alt2 = 1, alt3 = 1), 
                        choiceVar = choice, 
                        V = V 
                        )
  # Probabilities
  P<-list(
    model = apollo_mnl(mnl_settings, functionality)
  )
  # Take the product across observations
  P <- apollo_panelProd(P, apollo_inputs, functionality) 
  # Prepare and return the outputs 
  P <- apollo_prepareProb(P, apollo_inputs, functionality) 
  # Return the probabilities 
  return( P ) 
}

## Estimate the model ---- 
model <- apollo_estimate(
  apollo_beta, 
  apollo_fixed, 
  apollo_probabilities, 
  apollo_inputs, 
  estimate_settings = list(
    writeIter = FALSE, 
    silent = TRUE, 
    estimationRoutine = "bgw" 
    )
  )

## Print model output to console ---- apollo_
output<-apollo_modelOutput( 
  model, 
  modelOutput_settings = list(
    printOutliers = 10 
    )
  )

#mDAP
dap_small_farm<--1*model$estimate["b_small_farms"]/model$estimate["b_cost"]
