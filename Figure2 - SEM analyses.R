#### Info session ####


#### Packages ####
packs = c(
  'tidyverse',
  'factoextra', 'lavaan','performance',
  'lavaanPlot', 'magrittr', 'ggfortify'
)

invisible(lapply(packs, library, character.only = T))

#### LUCAS analyses ####
#### > 1. data handling ####
# Clear Workspace
rm(list=ls()) 

# Set working directory and read data
setwd("...")

# Read already z-transformed data
df <- read.csv("df.csv", stringsAsFactors = F)

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
#### >> 2.1 SEM structure ####

mod = '
  z.pH ~ 
        z.water.content + 
        z.AnnualMeanTemp + z.AnnualPrecip + 
        z.Elevation + z.Latitude + 
        z.pca.text + LC_5 
        
  z.water.content ~
        z.MonthlyMeanTemp + z.AnnualMeanTemp + 
        z.MonthlyPrecipSum + z.AnnualPrecip  + 
        z.Elevation + z.Latitude + 
        LC_5 + z.pca.text + z.carbon
        
  z.carbon ~ 
        z.MonthlyMeanTemp + z.AnnualMeanTemp + 
        z.MonthlyPrecipSum + z.AnnualPrecip  + 
        z.Elevation + z.Latitude + 
        z.pca.text + LC_5 + z.pH
        
  z.meanFunction ~ 
        z.water.content + 
        z.MonthlyMeanTemp + z.AnnualMeanTemp + 
        z.MonthlyPrecipSum + z.AnnualPrecip + 
        z.Elevation + z.Latitude + 
        z.pca.text + z.carbon + z.pH + LC_5
'

#### >> 2.2 Model fit ####
sem.m.F = sem(model = mod, data = df)

# Summary 
mod.sum <- summary(sem.m.F, standardized = T)
standardizedsolution(sem.m.F)

# Check R2 values
inspect(sem.m.F, 'R2')

# Check model quality 
fitMeasures(sem.m.F)

# CFA analysis
fit <- cfa(sem.m.F)
f <- parameterEstimates(fit)

#### > 3. Save results ####
write.csv(mod.sum,"results-fig-2-LUCAS.csv", row.names = FALSE)

#### > 4. Plot ####
# The plot was made manually using Inkshape

#### GCEF analyses ####
#### > 1. data handling ####
# Read already z-transformed data
df1 <- read.csv("df-GCEF.csv", stringsAsFactors = F)


#### >> 1.1 Check endogenous variables individual model ####
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

#### > 2. Structural equation model ####
#### >> 2.1 SEM structure ####

mod = '
  z.pH ~ z.water.content + LC_5 + Clim 
  z.water.content ~  z.carbon + LC_5 + Clim 
  z.carbon ~ z.pH + LC_5 + Clim 
  z.meanFunction ~ z.water.content + z.carbon + 
                   z.pH + LC_5 + Clim 
'

#### >> 2.2 Model fit ####
sem.m.F = sem(model = mod, data = df1)

# Summary 
mod.sum <- summary(sem.m.F, standardized = T)
standardizedsolution(sem.m.F)

# Check R2 values
inspect(sem.m.F, 'R2')

# Check model quality 
fitMeasures(sem.m.F)

# CFA analysis
fit <- cfa(sem.m.F)
f <- parameterEstimates(fit)

#### > 3. Save results ####
write.csv(mod.sum,"results-fig-2-GCEF.csv", row.names = FALSE)

#### > 4. Plot ####
# The plot was made manually using Inkshape