
library(tidyverse)
library(ggfortify)
library(plotly)
library(dynlm)
library(lmtest)

load("06-data-forecasting.RData")

# Creando DF de entrenamiento ----

DF.training.diapers <- DF.diapers %>% 
  filter(periodo <= "2015-12-01")



# Creando formato TS (time series)

TS.diapers <- ts(data = DF.training.diapers$sales,
                 start = c(2006,1),
                 end = c(2015,12),
                 frequency = 12)



# Descomposición

plot(TS.diapers)

ggplotly(
  
  TS.diapers %>% 
    decompose(type = "additive") %>% 
    autoplot()
  
)






