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
libs <- c(
  'dplyr', 
  'sf', 'raster', 
  "rnaturalearth", "rnaturalearthdata",
  'ggplot2', "Hmisc", "corrplot"
)

invisible(lapply(libs, library, character.only = T))

#### 1. Map Europe ####
#### > 1.1. data #####
df <- read.csv("LUCAS.csv", stringsAsFactors = F,  sep=";")
world <- ne_countries(scale = "medium", returnclass = "sf")

#### > 1.2. plot #####
ggplot(data = world) + 
  geom_sf(data = world,
          fill = '#EEEEEE',
          color = '#D0D1D0',
          size = 0.2) +
  geom_point(data = df, 
             aes(x = Longitude, 
                 y = Latitude, 
                 color = factor(LC_5)), 
             size = 0.6)  +
  scale_color_manual(values=c("#57563A", "#B5AE1D")) +
  annotate(geom = 'point', 
           x = 4451860.43, 
           y = 3144000.17,
           color = 'black',
           size = 2.5) + 
  lims(y = c(1495000, 4703000), x = c(2657000, 6485000))+
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
  theme_void() +
  theme(legend.position = 'none')

#### 2. Correlation matrix LUCAS ####
# Read data
df <- read.csv("LUCAS_GetMeanFunction.csv", 
               stringsAsFactors = F,  
               sep=";")

# Variables distributions
boxplot(df$MUB.Xyl)
boxplot(df$MUB.Cel)
boxplot(df$MUB.Pho)
boxplot(df$MUB.Nag)
boxplot(df$Basal)
boxplot(df$Cmic)
boxplot(df$WSA)

# Create dataframe with the variables of interest 
variables <- c("MUB.Xyl", "MUB.Cel", 
               "MUB.Nag", "MUB.Pho", 
               "Basal_respiration", "Cmic", 
               "WSA")
df1 <- df[variables] |> 
  na.omit()

#### > 2.1. Correlation Matrix ####
# Correlate single variables
correlation <- cor(df2, 
                   method = c("pearson"))

## Flattens Correlation Matrix to table with columns and rows
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

# Correlation matrix
correlation2 <- 
  df2[,1:7] |>
  as.matrix() |>
  rcorr()

flattenCorrMatrix(correlation2$r, # correlation coefficients
                  correlation2$P  # p-values
)
results <- flattenCorrMatrix(
  correlation2$r, 
  correlation2$P
)

#### > 2.2. Save results ####
write.csv(results,
          "results-fig-1-LUCAS.csv", 
          row.names = FALSE)

#### > 2.3. Plot ####
# Format
png(height=1800, width=1800, 
    file="LUCAS_correlation-matrix-single-functions.png", 
    type = "cairo")

# Column and row names
colnames(correlation2$r) <- 
  c("Xyl", "Cel", "Nag", "Pho", "Basal", "Cmic", "WSA")

rownames(correlation2$r) <-
  c("Xyl", "Cel", "Nag", "Pho", "Basal", "Cmic", "WSA")

# Plot
corrplot(correlation2$r, method="color", type="upper", order="original", 
         p.mat = correlation2$P, 
         sig.level = c(.001, .01, .05),
         tl.col = "black",
         tl.srt = 45, tl.cex = 4.8,
         pch.col = "white", outline = TRUE, cl.cex= 4.8,
         insig = "label_sig", pch.cex = 4.8) +
  mtext("", at=7.5, line=-8, cex=5)

dev.off()

#### 3. Correlation matrix GCEF ####
# Read data
df <- read.csv("GCEF_GetMeanFunction&FuncMaxed.csv", 
               stringsAsFactors = F,  
               sep=";")

# Variables distribution
boxplot(df$MUB.Xyl)
boxplot(df$MUB.Cel)
boxplot(df$MUB.Pho)
boxplot(df$MUB.Nag)
boxplot(df$Basal)
boxplot(df$Cmic)
boxplot(df$WSA)

# Create dataframe with the variables of interest 
variables <- 
  c("MUB.Xyl", "MUB.Cel", "MUB.Nag", 
    "MUB.Pho", "Basal", "Cmic", "WSA")

df1 <- df[variables]

#### > 3.1. Correlation Matrix ####
# Correlate single variables
correlation <- 
  cor(df1, 
      method = c("pearson"))

# Correlation matrix
correlation2 <- rcorr(as.matrix(df1[,1:7]))
flattenCorrMatrix(correlation2$r, 
                  correlation2$P)
results <- flattenCorrMatrix(correlation2$r, 
                             correlation2$P)

#### > 3.2. Save results ####
write.csv(results,"results-fig-1-GCEF.csv", 
          row.names = FALSE)

#### > 3.3. Plot ####
# Format
png(height=1800, width=1800, 
    file="GCEF_correlation-matrix-single-functions.png", 
    type = "cairo")

# Column and row names
colnames(correlation2$r) <- 
  c("Xyl", "Cel", "Nag", "Pho", 
    "Basal", "Cmic", "WSA")
rownames(correlation2$r) <-
  c("Xyl", "Cel", "Nag", "Pho", 
    "Basal", "Cmic", "WSA")

# Plot
corrplot(correlation2$r, method="color", type="upper", order="original", 
         p.mat = correlation2$P, 
         sig.level = c(.001, .01, .05),
         tl.col = "black",
         tl.srt = 45, tl.cex = 4.8,
         pch.col = "white", outline = TRUE, cl.cex= 4.8,
         insig = "label_sig", pch.cex = 4.8) +
  mtext("", at=7.5, line=-8, cex=5)

dev.off()

#### 4. Final figure ####
# The original plot was made manually using Inkscape