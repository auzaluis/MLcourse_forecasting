
library(tidyverse)
library(ggfortify)
library(plotly)
library(dynlm)
library(lmtest)

load("06-data-forecasting.RData")

# Creando DF de entrenamiento ----

DF.training.diapers <- DF.diapers %>% 
  filter(periodo <= "2015-12-01") %>% 
  mutate(promo = ifelse(periodo == promo, yes = 1, no = 0),
         post_promo = ifelse(periodo == post_promo, yes = 1, no = 0))


promo <- as_date("2015-07-01")
post_promo <- as_date("2015-08-01")


# Creando formato TS (time series)

TS.diapers <- ts(data = DF.training.diapers %>% 
                   select(sales, promo, post_promo),
                 start = c(2006,1),
                 end = c(2015,12),
                 frequency = 12)



# Descomposición ----

plot(TS.diapers[,"sales"])

ggplotly(
  
  TS.diapers[,"sales"] %>% 
    decompose(type = "additive") %>% 
    autoplot()
  
)



# Definición del modelo ----

diapers.reg <- dynlm(data = TS.diapers,
                     formula = sales ~
                       trend(sales) +
                       season(sales) +
                       L(sales, 1) +
                       promo +
                       post_promo)



# Analisis de precisión/asertividad

summary(diapers.reg)



ggplotly(
  
  
  
)



ggplotly(
  
  data.frame(periodo = DF.training.diapers$periodo,
             sales = DF.training.diapers$sales,
             predicted = c(NA, diapers.reg$fitted.values)) %>% 
  
  ggplot(mapping = aes(x = periodo)) +
    
    geom_point(mapping = aes(y = sales),
               color = "#1d3557") +
    
    geom_line(mapping = aes(y = predicted),
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



### Test de normalidad

shapiro.test(x = diapers.reg$residuals)



## Homocedasticidad ----

### Evaluación gráfica

plot(diapers.reg, which = 1)



### Test de homocedasticidad

bptest(formula = diapers.reg,
       studentize = F)



## Autocorrelación

plot(acf(x = diapers.reg$residuals,
         lag = 12))


Box.test(x = diapers.reg$residuals,
         lag = 12,
         type = "Ljung-Box")



