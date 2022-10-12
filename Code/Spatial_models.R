# Multivariate analysis
library(maptools)
library(tidyverse)
library(knitr)
library(broom)
library(broom.mixed)
library(kableExtra)
library(magrittr)
library(ggtext)
library(sf)
library(spdep)
library(spatialreg)
library(rgdal)
library(rgeos)
library(glue)

# Load shapefile for county
shp <- st_read("Processed_data/Shapefiles/SHP_county.shp")

# Load regression analysis data consisting of all factors joined to one data frame 
df <- read_csv("Processed_data/regression_analysis.csv")

# Join data frame with shapefile while keeping only those counties that are present in shapefile geometry data
df %<>% 
  right_join(shp %>% 
               mutate(teryt = as.integer(paste0(as.integer(teryt), "000"))) %>% 
               as.data.frame, by = c("Kod" = "teryt", "WOJ.x" = "code"))

# Retrieve coordinates in matrix form
coordinates = st_coordinates(shp)

# Construct neighbours list from polygon list
queen_neighbour = poly2nb (shp , queen = T)

# Create spatial weights for neighbours lists
listw1 = nb2listw (queen_neighbour , style = "W" , zero.policy = TRUE)


# Two-variate regression analysis for only significant factors
formula_univariate <- as.formula(Fully_vaccinated_population ~
                         Votes_on_PiS_commission)

reg1 <- lm(formula_univariate, data = df)
summary(reg1)

tidy(reg1) %>% 
  arrange(desc(estimate)) 
# %>% 
  # kbl(digits = 2, format = "pipe") %>% 
  # kable_material(c("striped", "hover"))

glance(reg1) 
# %>% 
  # kbl(digits = 2, format = "pipe") %>% 
  # kable_material(c("striped", "hover"))

# Check if a spatial dependency is present
moran.test(df$Fully_vaccinated_population, listw1)

# Lagrange Multiplier diagnostics
lm.LMtests(reg1, listw1, test= "all")

# According to LM tests Spatially Error Model should be the best model 


# SLX
reg2 = lmSLX(formula_univariate, data = df, listw1) 

summary(reg2)

impacts(reg2, listw = listw1)

summary(impacts(reg2, listw=listw1, R=500), zstats = TRUE)


# SAR
reg3 <- lagsarlm(formula_univariate, data = df, listw1) 

summary(reg3)

impacts(reg3, listw = listw1)

summary(impacts(reg3, listw=listw1, R=500), zstats = TRUE)


# SEM
reg4 <- errorsarlm(formula_univariate, data=df, listw1) 

tidy(summary(reg4)) %>% arrange(estimate)

glance(summary(reg4))

# With this model only Hausman test can be performed
Hausman.test(reg4) 


# SDEM
reg5 <- errorsarlm(formula_univariate, data = df, listw1, etype = "emixed")

summary(reg5)

impacts(reg5,listw=listw1)

summary(impacts(reg5, listw=listw1, R=500),zstats=TRUE)


# SDM
reg6 <- lagsarlm(formula_univariate, data = df, listw1, type="mixed") 

summary(reg6)

impacts(reg6,listw=listw1)

summary(impacts(reg6,listw=listw1,R=500),zstats=TRUE)


# MANSKI
reg7 <- sacsarlm(formula_univariate, data = df, listw1, type="sacmixed") 

summary(reg7)

impacts(reg7,listw=listw1)

summary(impacts(reg7,listw=listw1,R=500),zstats=TRUE)


# SARAR
reg8 <- sacsarlm(formula_univariate, data = df, listw1, type="sac") 

summary(reg8)

impacts(reg8,listw=listw1)

summary(impacts(reg8,listw=listw1,R=500),zstats=TRUE)

# Ultimate test for reduction of complexity
LR.Sarlm(reg5, reg4)

# Final decision on spatial regression analysis with all factors
# The Spatial Error Model has the best fit out of all the models. Therefore SDEM is used.
summary(reg5) 

glance(summary(reg5)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

summary(impacts(reg5,listw=listw1,R=500),zstats=TRUE)

# Pseudo r square calculations for each models
1-(reg2$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg3$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg4$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg4$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg5$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg6$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg7$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg8$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

# We would choose the 5th model- SDEM based on it's Akaike Information Criterium and good fit

# Heteroskedastity Breusch-Pagan test
bptest.Sarlm(reg5, studentize = TRUE)

# We can assume that residuals are homoscedastic


# Spatial dependency on the residuals from lm model
df_residuals_OLS <- read_csv("Processed_data/linear_regression_prediction_OLS.csv")

# Join data frame with shp. Delete the counties not present in geospatial data
df_residuals_OLS %<>%
  right_join(shp, by = c("county" = "county", "WOJ.x" = "code"))

# Run moran test for residuals
moran.test(df_residuals_OLS$residuals, listw1)

# Run Monte-Carlo simulation of Moran I
moran.mc(df_residuals_OLS$residuals, listw1, nsim=599)



# Run the spatial regression analysis for all the variables
formula <- as.formula(Fully_vaccinated_population ~
                        Feminization_index +
                        Votes_on_PiS_commission +
                        Working_age_population_share +
                        Doctors_and_staff_per_10_thousand_people_in_2020 +
                        Population_per_1_square_km_in_2021 +
                        Higher_education_population_share_in_2011 +
                        Health_expenditure_per_1_person)

reg1 <- lm(formula, data = df)
summary(reg1)

# Lagrange Multiplier diagnostics
lm.LMtests(reg1, listw1, test= "all")

# According to LM tests Spatially Error Model should be the best model 


# SLX
reg2 = lmSLX(formula, data = df, listw1) 

summary(reg2)

impacts(reg2, listw = listw1)

summary(impacts(reg2, listw=listw1, R=500), zstats = TRUE)


# SAR
reg3 <- lagsarlm(formula, data = df, listw1) 

summary(reg3)

impacts(reg3, listw = listw1)

summary(impacts(reg3, listw=listw1, R=500), zstats = TRUE)


# SEM
reg4 <- errorsarlm(formula, data=df, listw1) 

summary(reg4)

# With this model Hausman test can be performed
Hausman.test(reg4) 


# SDEM
reg5 <- errorsarlm(formula, data = df, listw1, etype = "emixed")

summary(reg5)

impacts(reg5,listw=listw1)

summary(impacts(reg5, listw=listw1, R=500),zstats=TRUE)


# SDM
reg6 <- lagsarlm(formula, data = df, listw1, type="mixed") 

summary(reg6)

impacts(reg6,listw=listw1)

summary(impacts(reg6,listw=listw1,R=500),zstats=TRUE)


# MANSKI
reg7 <- sacsarlm(formula, data = df, listw1, type="sacmixed") 

summary(reg7)

impacts(reg7,listw=listw1)

summary(impacts(reg7,listw=listw1,R=500),zstats=TRUE)


# SARAR
reg8 <- sacsarlm(formula, data = df, listw1, type="sac") 

summary(reg8)

impacts(reg8,listw=listw1)

summary(impacts(reg8,listw=listw1,R=500),zstats=TRUE)


# Ultimate test for reduction of complexity
LR.Sarlm(reg4, reg2)

# Final decision on spatial regression analysis with all factors
# From SARAR one I should reduce to the SDEM model and then from it I should switch to SEM
summary(reg4)

# Predict values
distribution_prediction <- df %>%
  mutate(
    predicted_values = predict(reg4, data = .),
    residuals = Fully_vaccinated_population - predicted_values
  )

# Extreme residuals for SEM
distribution_prediction$residuals %>% summary()

# Check the residual distribution spatial dependency
moran.test(distribution_prediction$residuals, listw1)



# Multi-variate regression analysis for only significant factors
formula3 <- as.formula(Fully_vaccinated_population ~
                         Feminization_index +
                         Votes_on_PiS_commission +
                         Working_age_population_share +
                         Higher_education_population_share_in_2011)

reg1 <- lm(formula3, data = df)

# Extreme residuals for OLS model
augment(reg1)$.resid %>% summary()


tidy(reg1) %>% 
  arrange(estimate) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

glance(reg1) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Lagrange Multiplier diagnostics
lm.LMtests(reg1, listw1, test= "all")

# According to LM tests Spatially Error Model should be the best model 


# SLX
reg2 = lmSLX(formula3, data = df, listw1) 

summary(reg2)

impacts(reg2, listw = listw1)

summary(impacts(reg2, listw=listw1, R=500), zstats = TRUE)


# SAR
reg3 <- lagsarlm(formula3, data = df, listw1) 

summary(reg3)

impacts(reg3, listw = listw1)

summary(impacts(reg3, listw=listw1, R=500), zstats = TRUE)


# SEM
reg4 <- errorsarlm(formula3, data=df, listw1) 

summary(reg4)

# With this model Hausman test can be performed
Hausman.test(reg4) 


# SDEM
reg5 <- errorsarlm(formula3, data = df, listw1, etype = "emixed")

summary(reg5)

impacts(reg5,listw=listw1)

summary(impacts(reg5, listw=listw1, R=500),zstats=TRUE)


# SDM
reg6 <- lagsarlm(formula3, data = df, listw1, type="mixed") 

summary(reg6)

impacts(reg6,listw=listw1)

summary(impacts(reg6,listw=listw1,R=500),zstats=TRUE)


# MANSKI
reg7 <- sacsarlm(formula3, data = df, listw1, type="sacmixed") 

summary(reg7)

impacts(reg7,listw=listw1)

summary(impacts(reg7,listw=listw1,R=500),zstats=TRUE)


# SARAR
reg8 <- sacsarlm(formula3, data = df, listw1, type="sac") 

summary(reg8)

impacts(reg8,listw=listw1)

summary(impacts(reg8,listw=listw1,R=500),zstats=TRUE)


# Ultimate test for reduction of complexity
LR.Sarlm(reg5, reg4)

# Final decision on spatial multi-variate regression analysis for only significant factors
# From SEM one I should reduce to the SEM model and then from it I should switch to SEM
summary(reg4)

tidy(summary(reg4)) %>% 
  arrange(desc(estimate)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

glance(summary(reg4)) %>% 
  kbl(digits = 2, format = "pipe") %>% 
  kable_material(c("striped", "hover"))

# Pseudo r square calculations for each models
1-(reg2$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg3$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg4$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg4$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg5$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg6$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg7$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

1-(reg8$SSE/(var(df$Fully_vaccinated_population)*(length(df$Fully_vaccinated_population)-1)))

# Predict values
distribution_prediction <- df %>%
  mutate(
    predicted_values_SEM = predict(reg4, data = .),
    residuals = Fully_vaccinated_population - predicted_values_SEM
    )

# Extreme residuals for SEM
distribution_prediction$residuals %>% summary()

# Check the residual distribution spatial dependency
moran.test(distribution_prediction$residuals, listw1)

# Write csv with predicted values and residuals for SEM
write_csv(distribution_prediction %>% select(1:4, Fully_vaccinated_population, predicted_values_SEM, residuals), "Processed_data/spatial_ditribution_prediction_SEM.csv")  
