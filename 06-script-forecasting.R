
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



# Descomposición ----

plot(TS.diapers)

ggplotly(
  
  TS.diapers %>% 
    decompose(type = "additive") %>% 
    autoplot()
  
)



# Definición del modelo ----

diapers.reg <- dynlm(data = TS.diapers,
                     formula = TS.diapers ~
                       trend(TS.diapers))



# Analisis de precisión/asertividad

summary(diapers.reg)



ggplotly(
  
  ggplot(data = DF.training.diapers,
         mapping = aes(x = periodo)) +
    
    geom_point(mapping = aes(y = sales),
               color = "#1d3557") +
    
    geom_line(mapping = aes(y = diapers.reg$fitted.values),
              color = "#e63946") +
    
    theme_minimal() +
    
    theme(axis.title.x = element_blank())
  
)



# Cumplimiento de los supuestos ----

## Normalidad ----

### Histograma

ggplot(mapping = aes(diapers.reg$residuals)) +
  
  geom_histogram(alpha = 0.7,
                 fill = "#457b9d") +
  
  theme_minimal()



### Q-Q plot

plot(diapers.reg, which = 2)



## Test de normalidad

shapiro.test(x = diapers.reg$residuals)
