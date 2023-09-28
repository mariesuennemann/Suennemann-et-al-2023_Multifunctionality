#### Info session ####
## R.version
##                _                           
## platform       x86_64-w64-mingw32          
## arch           x86_64                      
## os             mingw32                     
## system         x86_64, mingw32             
## status                                     
## major          4                           
## minor          0.2                         
## year           2020                        
## month          06                          
## day            22                          
## svn rev        78730                       
## language       R                           
## version.string R version 4.0.2 (2020-06-22)
## nickname       Taking Off Again

#### Packages ####
packs = c(
  'tidyverse',
  'factoextra', 'piecewiseSEM','performance',
  'glmmTMB', 'magrittr', 'ggfortify'
)

invisible(lapply(packs, library, character.only = T))

#### LUCAS analyses ####
#### > 1. data handling ####
# Clear Workspace
rm(list=ls()) 

# Set working directory and read data
setwd("...")

# Read already z-transformed data
df <- read.csv("LUCAS_data.csv", stringsAsFactors = F)

# Number of samples per Land-use type
count(df, LC_4)  

#### >> 1.1. PCA Soil Texture ####
# Fit PCA 
z.pca.texture = prcomp(df %>% 
                         select(z.clay, z.silt, z.sand), 
                       center = T, 
                       scale. = T)

# Check PCA fit
autoplot(z.pca.texture, loadings = T)

# Add first axis to the dataframe
df$z.pca.text = get_pca_ind(z.pca.texture)$coord[,1]

#### >> 1.2 Check endogenous variables individual model ####
# Soil pH
'z.pH ~ 
  z.water.content + 
  z.AnnualMeanTemp  + z.AnnualPrecip + 
  z.Elevation + z.Latitude + 
  z.pca.text + 
  LC_5' %>%
  lm(., df) %>%
  plot()

# Soil water content 
'z.water.content ~ 
  z.MonthlyMeanTemp + z.AnnualMeanTemp + 
  z.MonthlyPrecipSum + z.AnnualPrecip  + 
  z.Elevation + z.Latitude  + 
  LC_5 + 
  z.pca.text + z.carbon'  %>%
  lm(., df) %>%
  plot()

# Soil carbon content
'z.carbon ~ 
  z.MonthlyMeanTemp + z.AnnualMeanTemp + 
  z.MonthlyPrecipSum + z.AnnualPrecip  + 
  z.Elevation + z.Latitude + 
  z.pca.text + 
  LC_5 +
  z.pH' %>%
  lm(., df) %>%
  plot()

# Soil multifunctionality
'z.meanFunction ~ z.water.content + z.MonthlyMeanTemp + z.AnnualMeanTemp + 
  z.MonthlyPrecipSum + z.AnnualPrecip   + 
  z.Elevation + z.Latitude  + 
  z.pca.text + 
  z.carbon + 
  z.pH + 
  LC_5' %>%
  lm(., df) %>%
  plot()

#### > 2. Structural equation model ####
#### >> 2.1. Model with interaction ####
lucas.sem.interaction <- psem(
  lm(z.pH ~ z.water.content + z.AnnualTemp * LC_5 + z.AnnualPrecip * LC_5 + z.Elevation + z.Latitude + z.pca.text, data = df),
  lm(z.water.content ~ z.MonthlyMeanTemp + z.AnnualTemp * LC_5 + z.MonthlyPrecipSum + z.AnnualPrecip * LC_5 + z.Elevation + z.Latitude + z.pca.text, data = df),
  lm(z.carbon ~ z.MonthlyMeanTemp + z.AnnualTemp * LC_5 + z.MonthlyPrecipSum + z.AnnualPrecip * LC_5  + z.Elevation + z.Latitude + z.pca.text + z.pH, data = df),
  lm(z.meanFunction ~ z.water.content + z.MonthlyMeanTemp * LC_5 + z.AnnualTemp * LC_5 + z.MonthlyPrecipSum * LC_5 + z.AnnualPrecip * LC_5 + z.Elevation + z.Latitude  + z.pca.text + z.carbon + z.pH, data = df)
)

summary(lucas.sem.interaction)

# Coefficients 
a <- coefs(lucas.sem.interaction )

# Get R-squared
rsquared(lucas.sem.interaction )

# Save results
write.csv(a,"results-LUCAS_piecewise_interaction.csv", row.names = FALSE)

#### >> 2.2. Model without interaction ####
lucas.sem <- psem(
  lm(z.pH ~ z.water.content + z.AnnualTemp + z.AnnualPrecip + LC_5 + z.Elevation + z.Latitude + z.pca.text, data = df),
  lm(z.water.content ~ z.MonthlyMeanTemp + z.AnnualTemp + LC_5 + z.MonthlyPrecipSum + z.AnnualPrecip + z.Elevation + z.Latitude + z.pca.text, data = df),
  lm(z.carbon ~ z.water.content + z.MonthlyMeanTemp + z.AnnualTemp + z.MonthlyPrecipSum + z.AnnualPrecip + LC_5  + z.Elevation + z.Latitude + z.pca.text + z.pH, data = df),
  lm(z.meanFunction ~ z.water.content + z.MonthlyMeanTemp + LC_5 + z.AnnualTemp + z.MonthlyPrecipSum + z.AnnualPrecip + z.Elevation + z.Latitude  + z.pca.text + z.carbon + z.pH, data = df)
)

summary(lucas.sem)

# Coefficients 
a <- coefs(lucas.sem)

# Get R-squared
rsquared(lucas.sem)

# Save results
write.csv(a,"results-LUCAS_piecewise.csv", row.names = FALSE)

#### GCEF analyses ####
#### > 3. data handling ####
# Read already z-transformed data
df1 <- read.csv("GCEF_data.csv", stringsAsFactors = F)

#### >> 3.1 Check endogenous variables individual model ####
# Soil pH
'z.pH ~ 
  z.water.content + 
  LC_5 + Clim' %>%
  lm(., df) %>%
  plot()

# Soil water content 
'z.water.content ~ 
  z.carbon + LC_5 + Clim'  %>%
  lm(., df) %>%
  plot()

# Soil carbon content
'z.carbon ~ 
  z.pH + LC_5 + z.pH' %>%
  lm(., df) %>%
  plot()

# Soil multifunctionality
'z.meanFunction ~ 
  z.water.content + 
  z.carbon + 
  z.pH + 
  LC_5 + Clim' %>%
  lm(., df) %>%
  plot()

#### > 4. Structural equation model ####
#### >> 4.1. Model with interaction ####
gcef.sem.interaction.2 <- psem(
  glmmTMB(z.pH ~ z.water.content + Clim * LC_5 + (1|Mainplot), data = df),
  glmmTMB(z.water.content ~ z.carbon + Clim * LC_5, data = df),
  glmmTMB(z.carbon ~ Clim * LC_5 + (1|Mainplot), data = df),
  glmmTMB(z.meanFunction ~ z.water.content + z.carbon + z.pH + LC_5 * Clim + (1|Mainplot), data = df)
)

# Summary
summary(gcef.sem.interaction.2)

# Coefficients 
a <- coefs(gcef.sem.interaction.2)

# Check R2 values
rsquared(gcef.sem.interaction.2)

# Save results 
write.csv(a,"results-GCEF_piecewise_interaction.csv", row.names = FALSE)

#### >> 4.2. Model without interaction ####
gcef.sem <- psem(
  lmer(z.pH ~ z.water.content + Clim + LC_5 + (1|Mainplot), data = df),
  lmer(z.water.content ~ Clim + LC_5 + (1|Mainplot), data = df),
  lmer(z.carbon ~ z.water.content + Clim + LC_5 + (1|Mainplot), data = df),
  lmer(z.meanFunction ~ z.water.content + z.carbon + z.pH + LC_5 + Clim + (1|Mainplot), data = df)
)

# Summary
summary(gcef.sem)

# Coefficients 
a <- coefs(gcef.sem)

# Check R2 values
rsquared(gcef.sem)

#### > 5. Plot ####
# The plot was made manually using Inkshape
