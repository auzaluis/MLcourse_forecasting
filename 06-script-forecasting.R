
library(tidyverse)
library(ggfortify)
library(plotly)
library(dynlm)
library(lmtest)

load("06-data-forecasting.RData")

# Creando DF de entrenamiento ----

promo <- as_date("2015-07-01")
post_promo <- as_date("2015-08-01")



DF.training.diapers <- DF.diapers %>% 
  filter(periodo <= "2015-12-01") %>% 
  mutate(promo = ifelse(periodo == promo, yes = 1, no = 0),
         post_promo = ifelse(periodo == post_promo, yes = 1, no = 0))



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
  
  ggplot(mapping = aes(x = TS.diapers[-1,"sales"],
                       y = diapers.reg$fitted.values)) +
    
    geom_point(color = "#1d3557") +
    
    theme_minimal() +
    
    labs(title = "Ventas reales VS Predicción",
         x = "Real",
         y = "Predicción") +
    
    theme(plot.title = element_text(hjust = 0.5))
  
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



# Forecasting ----

DF.model.diapers <- tibble(
  
  periodo = DF.training.diapers$periodo[-1],
  diapers.reg$model
  
)


## Creación de vectores ----

periodo <- DF.diapers %>% 
  filter(periodo > max(DF.training.diapers$periodo)) %>% 
  select(periodo)

sales <- DF.diapers %>% 
  filter(periodo > max(DF.training.diapers$periodo)) %>% 
  select(sales)

`trend(sales)` <- seq(from = (nrow(DF.training.diapers) + 1) / 12,
                      to = (nrow(DF.training.diapers) + 5) / 12,
                      by = 1/12)

`season(sales)` <- c("Jan", "Feb", "Mar", "Apr", "May")

`L(sales, 1)` <- DF.diapers %>% 
  filter(periodo >= max(DF.training.diapers$periodo),
         periodo < max(DF.diapers$periodo)) %>% 
  select(sales)

promo      <- c(0, 0, .7, 0, 0)
post_promo <- c(0, 0, 0, .7, 0)



## Creación del DF.testing ----

DF.testing.diapers <- data.frame(
  
  periodo         = periodo,
  sales           = sales,
  `trend(sales)`  = `trend(sales)`,
  `season(sales)` = `season(sales)`,
  `L(sales, 1)`   = `L(sales, 1)`,
  promo           = promo,
  post_promo      = post_promo,
  
  check.names = F
  
)

colnames(DF.testing.diapers)[5] <- "L(sales, 1)"



diapers.reg$coefficients[3:13]



season.ref <- data.frame(
  
  `season(sales)` = c("Jan", "Feb", "Mar",
                      "Apr", "May", "Jun",
                      "Jul", "Aug", "Sep",
                      "Oct", "Nov", "Dec"),
  
  coefficient = c(0, diapers.reg$coefficients[3:13]),
  
  check.names = F,
  row.names = NULL
  
)


## Creación del DF.forecast ----

DF.forecast.diapers <- bind_rows(DF.model.diapers,
                                 DF.testing.diapers)


DF.forecast.diapers2 <- left_join(x = DF.forecast.diapers,
                                  y = season.ref,
                                  by = "season(sales)") %>% 
  
  select(-`season(sales)`) %>% 
  
  rename(`season(sales)` = coefficient) %>% 
  
  mutate(intercept = rep(diapers.reg$coefficients[1],
                         times = nrow(DF.forecast.diapers))) %>% 
  
  relocate(c("intercept", "season(sales)"),
           .after = sales) %>% 
  
  mutate(
    
    prediction = intercept +
      `season(sales)` +
      `trend(sales)` * diapers.reg$coefficients[2] +
      `L(sales, 1)` * diapers.reg$coefficients[14] +
      promo * diapers.reg$coefficients[15] +
      post_promo * diapers.reg$coefficients[16],
    
    forecast = ifelse(test = periodo > max(DF.training.diapers$periodo),
                      yes = "Sí",
                      no = "No")
      
  )
  
  

## Evaluación de la predicción

ggplotly(
  
  DF.forecast.diapers2 %>% 
  
  ggplot(mapping = aes(x = sales,
                       y = prediction,
                       color = forecast)) +
    
    geom_point() +
    
    theme_minimal() +
    
    scale_color_manual(values = c("#1d3557", "#e63946")) +
    
    labs(title = "Ventas reales VS Predicción",
         x = "Real",
         y = "Predicción") +
    
    theme(plot.title = element_text(hjust = 0.5))
  
)



ggplotly(
  
  
  
  DF.forecast.diapers2 %>%
    
    ggplot(mapping = aes(x = periodo,
                         color = forecast)) +
    
    geom_point(mapping = aes(y = sales)) +
    
    geom_line(mapping = aes(y = prediction),
              color = "#787878") +
    
    scale_color_manual(values = c("#1d3557", "#e63946")) +
    
    theme_minimal() +
    
    theme(axis.title.x = element_blank(),
          legend.position = "none")
  
)













