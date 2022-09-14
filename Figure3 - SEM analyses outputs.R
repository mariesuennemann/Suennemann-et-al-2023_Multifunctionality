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
  'dplyr', 'plyr',
  'lavaan', 'factoextra',
  'ggplot2', 'ggfortify', 
  'performance', 'magrittr',
  'lavaanPlot'
)

invisible(lapply(libs, library, character.only = T))

#### Data ####
# Read in transformed dataframe
df.lucas <- read.csv("Effectsize-LUCAS.csv", 
                     sep=";", stringsAsFactors = F)

df.gcef <- read.csv("Effectsize-GCEF.csv", 
                    sep=";", stringsAsFactors = F)

#### Plot ####
gen.lucas <- ggplot(data = df.lucas,
                    aes(x = Response, 
                        y = Effect)) +
  geom_bar(data = df.lucas,
           aes(x = Response, 
               y = Effect), 
           stat = 'identity', 
           position="dodge", 
           width=0.7, alpha=0.3)

gen.gcef <- ggplot(data = df.gcef,
                   aes(x = Response, 
                       y = Effect)) +
  geom_bar(data = df.gcef,
           aes(x = Response, 
               y = Effect), 
           stat = 'identity', 
           position="dodge", 
           width=0.7, alpha=0.3)
