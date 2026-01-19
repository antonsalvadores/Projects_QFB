getwd()
setwd("C:/MÁSTER QFB/2º CURSO/MEDICIÓN DE RIESGOS/JUAN ÁNGEL/TEMA 5/PRÁCTICA")

library(xts)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(gt)
library(fBasics)
library(car)
library(limma)
library(corrplot)
library(quantreg)
library(rugarch)
source('C:/MÁSTER QFB/2º CURSO/MEDICIÓN DE RIESGOS/JUAN ÁNGEL/TEMA 5/PRÁCTICA/GITHUB/caviar.R')

# Leemos la primera hoja del excel, en donde se encuentran las 
# cotizaciones.

data_quotes = read_xlsx("Data_P2.xlsx", sheet=1)

# Había algún problema con los datos así que lo vamos a explicar
# aquí: al ser empresas de distintos países, cotizan en distintos
# mercados y; por tanto, tienen distintos festivos. En algunos
# activos hay dato de cierre para una fecha en concreto y para
# otros no, culpa de esto. La solución que hemos propuesto para esto,
# es la misma que ofrece el software Datastream, i.e, para los 
# días en los que no hay cotización para algún activo, se toma
# como dato de cierre de ese día el dato del día anterior.
# Así obtenemos las 5 series temporales de los 5 distintos activos
# con el mismo número de días (y haciendo referencia a los mismos
# días, por supuesto).

data_quotes <- xts(x=data_quotes[,-1], order.by=data_quotes$Date)
data_quotes <- xts(
  apply(coredata(data_quotes), 2, as.numeric), 
  order.by = index(data_quotes)
)

# Como en el ejercicio se nos solicitan los datos hasta
# el 31/10/2025, borraremos los últimos datos de la serie temporal.

data_quotes <- data_quotes[
  1:(dim(data_quotes)[1] - 19), 
]

# Vamos a crear un data frame con los rendimientos de los 
# activos, para poder tratar con estos (más eficiente).

data_returns <- diff(log(data_quotes))
data_returns <- data_returns[-1,]

# PREGUNTA 1

# PARA HACER EN OVERLEAF

# PREGUNTA 2

# Creemos la cartera equiponderada entre los 4 activos:

w <- rep(0.25, 4)
por <- coredata(data_quotes[, c(1,2,3,4)]) %*% w
por <- xts(por, order.by=index(data_quotes))
colnames(por) <- "Portfolio value"

por_returns <- coredata(data_returns[, c(1,2,3,4)]) %*% w
por_returns <- xts(por_returns, order.by=index(data_returns))
colnames(por_returns) <- "Portfolio returns"

quotes <- merge(data_quotes, por)
returns <- merge(data_returns, por_returns)


# Realicemos ahora un análisis descriptivo de las 6 series temporales
# de rendimientos logarítmicos.


    ################# SERIES TEMPORALES DE PRECIOS #################

names <- colnames(quotes)
colors_plot <- rainbow(length(names))
par(mfrow = c(2, 3))
for (i in 1:6){
  plot(
    as.zoo(quotes[,i]), 
    main = paste(names[i]),
    ylab = "Quotes",
    xlab = "Time",
    col = colors_plot[i],
    lwd = 1.5        
  )
}

# Podemos observar en la serie de precios (lognormales) que 
# todos los activos han tenido una caída muy fuerte en el año
# del COVID. Los menos afectados en esta caída han sido Iberdrola
# y BBVA, con cierta lógica, ya que no pararon sus actividades
# durante la crisis. Salvo esto, todos los activos parece que tienen
# una serie temporal al alza semejante.

    ################# SERIES TEMPORALES DE RENDIMIENTOS #################

par(mfrow = c(2, 3))
for (i in 1:6){
  plot(
    as.zoo(returns[,i]), 
    main = paste(names[i]),
    ylab = "Returns",
    xlab = "Time",
    col = colors_plot[i],
    lwd = 1.5        
  )
}

# Las series de rendimientos más volátiles parecen BBVA e Infineon.
# Además parece existir un cierto clúster de volatilidad en todos
# los activos, lo que implica que las series no se comporten como
# un ruido blanco como deberían, aunque a simple vista lo puedan
# parecer.

    #################      TABLA RESUMEN                  #################
    #################      ESTADÍSTICOS COTIZACIONES      #################

stats_quotes <- basicStats(quotes)
stats_df <- as.data.frame(stats_quotes)
stats_df$Metrica <- rownames(stats_quotes) 
rownames(stats_df) <- NULL                 
stats_df <- stats_df[, c("Metrica", setdiff(names(stats_df), "Metrica"))]

mi_tabla_gt <- stats_df %>%
  gt() %>%
  tab_header(title = "Estadísticos Cotizaciones") %>%
  fmt_number(
    columns = -Metrica,   
    decimals = 4,         
    use_seps = TRUE       
  )
print(mi_tabla_gt)
gtsave(mi_tabla_gt, "Tabla_estadísticos_quotes.png")

# Los estadísticos de las cotizaciones no nos dan una información
# muy relevante en cuanto a las propiedades de los precios, ya que
# la inferencia la debemos realizar sobre los rendimientos (normales)
# y no sobre los precios (lognormales, más difícil de trabajar con
# ellos). Hemos decidido incluirlos por mera curiosidad, para ver
# el precio máximo, mínimo y medio en la serie observada de cada
# activo o demás propiedades. A gusto del lector si es de su interés.

    #################      TABLA RESUMEN                  #################
    #################      ESTADÍSTICOS RENDIMIENTOS      #################

stats_returns <- basicStats(returns)
stats_df <- as.data.frame(stats_returns)
stats_df$Metrica <- rownames(stats_returns) 
rownames(stats_df) <- NULL    

# Cálculo del ratio de Sharpe diario con un tipo diario del 0%:

mu <- colMeans(returns, na.rm = TRUE)
sigma <- apply(returns, 2, sd, na.rm = TRUE)
sharpe_daily <- mu / sigma
sharpe_annual <- sharpe_daily * sqrt(252)

row_sharpe_d <- as.data.frame(t(sharpe_daily))
row_sharpe_d$Metrica <- "Sharpe Ratio (Daily)"
row_sharpe_a <- as.data.frame(t(sharpe_annual))
row_sharpe_a$Metrica <- "Sharpe Ratio (Annualized)"
stats_df <- bind_rows(stats_df, row_sharpe_d, row_sharpe_a)
stats_df <- stats_df[, c("Metrica", setdiff(names(stats_df), "Metrica"))]

mi_tabla_gt <- stats_df %>%
  gt() %>%
  tab_header(title = "Estadísticos Rendimientos") %>%
  fmt_number(
    columns = -Metrica,   
    decimals = 4,         
    use_seps = TRUE       
  )
print(mi_tabla_gt)
gtsave(mi_tabla_gt, "Tabla_estadísticos_returns.png")

# En cuanto a la asimetría y kurtosis (en R se calcula el exceso
# de kurtosis sobre la normal directamente), podemos ver que los
# rendimientos están lejos de ser normales, ya que observamos
# una kurtosis mucho más elevada que para la gaussiana, además
# de asimetrías relevantes. Los activos menos volátiles son AXA
# e Iberdrola, compañías más establecidas en sus sectores
# probablemente. Evidentemente, el índice y la cartera equiponderada,
# efecto de la diversificación de ambos, también son los menos 
# volátiles, sobretodo el índice EuroStoxx600, lógicamente.
# Curiosamente, todos los rendimientos tienen una media diaria positiva,
# algo bueno en cierta medida, aunque en riesgos financieros se consideran
# los rendimientos diarios iguales a 0. Esto tiene relación comparando
# estos resultados con los gráficos de la serie de cotizaciones, 
# donde todos los activos seguían una clara tendencia al alza.
# El mejor ratio de Sharpe lo tiene Iberdrola, curiosamente, seguido
# de la cartera equiponderada (donde minimizamos riesgo, al diversificar)
# y a continuación está Infineon. El índice EuroStoxx600 sale muy
# mal parado por este ratio, ya que al estar tan bien diversificado,
# uno tendería a pensar que tendría un ratio de Sharpe elevado, pero se
# ve que tiene un rendimiento especialmente malo.



    #################      HISTOGRAMAS      #################

names <- colnames(returns)
colors_plot <- rainbow(length(names))
par(mfrow = c(2, 3))
for (i in 1:6){
  h <- hist(returns[,i], breaks = 100, col = "lightgreen", xlab = "Accuracy", 
            main = paste(names[i], "returns vs Gaussian"))
  xfit <- seq(min(returns[,i]), max(returns[,i]), length = 40) 
  yfit <- dnorm(xfit, mean = mean(returns[,i]), sd = sd(returns[,i])) 
  yfit <- yfit * diff(h$mids[1:2]) * length(returns[,i]) 
  lines(xfit, yfit, col = "black", lwd = 2)   
}

# En los histogramas de los activos, donde graficamos también la
# distribución que deberían seguir si los rendimientos fuesen
# normales, se ven claramente las propiedades de asimetría y
# kurtosis que hemos observado en las tablas anteriores. Estos
# reflejan un claro apuntamiento de la distribución (exceso de 
# kurtosis con respecto a la normal) y un desplazamiento de
# la concentración de los puntos hacia la derecha, como indica
# la asimetría negativa comentada. Esto implica además que la cola
# de la distribución a la izquierda es mayor que la cola
# de la distribución a la derecha, i.e, tenemos rendimientos
# mucho mayores (en valor absoluto) negativos que positivos.
# Esto es lógico al observar series financieras ya que normalmente
# las caídas son más grandes que las subidas, que es lo que viene
# a decir el estadístico y como se puede ver en el histograma.


    #################      ACF Y PACF      #################



for (i in 1:6){
  par(mfrow=c(2,3), mar = c(3, 4, 4, 1) + 0.1, oma = c(0, 0, 3, 0))
  plot(
    as.zoo(returns[,i]), 
    main = paste(names[i], "returns"),
    ylab = "Returns",
    xlab = "Time",
    col = colors_plot[i],
    lwd = 1.5        
  )
  acf(returns[, i], lag.max = 10, 
      ylim = c(-0.1, 0.1),
      main = paste("ACF returns"), 
      xlab = "Lag")
  pacf(returns[, i], lag.max = 10, 
       main = paste("PACF returns"), 
       xlab = "Lag")
  plot(
    as.zoo(returns[,i]**2),
    main = paste(names[i], "square returns"),
    ylab = "Square returns",
    xlab = "Time",
    col = colors_plot[i],
    lwd = 1.5        
  )
  acf(returns[, i]**2, lag.max = 10,
      ylim = c(-0.1, 0.1),
      main = paste("ACF square returns"), 
      xlab = "Lag")
  pacf(returns[, i]**2, lag.max = 10, 
       main = paste("PACF square returns"), 
       xlab = "Lag")
  mtext(paste(names[i]), 
        outer = TRUE, cex = 1.5, line = 0, font = 2)
}

# Salvo el primer retardo, cuya autocorrelación cosigo mismo
# es evidentemente 1, tenemos retardos con poca autocorrelación
# entre ellos. Aquellos retardos significativos están por poco
# tocando las bandas de confianza y de manera en cierta medida
# arbitraria para cada activo, por lo que no parece que tenga
# una determinada estructura predictiva rendimientos pasados
# para predecir rendimientos futuros, lo que hace que
# los rendimientos simples se comporten de manera similar
# a un ruido blanco. No parece que tengan algún tipo de 
# estructura ARMA concreta.

# Sin embargo, observando el gráfico de rendimientos
# al cuadrado y sus funciones de autocorrelación,
# se ve claramente correlación entre ellos, lo que implica,
# como ya habíamos comentado, la evidencia de heterocedasticidad
# condicional de los rendimientos. Esto implica que la volatilidad
# es persistente en los rendimientos, lo que da lugar al fenómeno
# del clúster de volatilidad.

# Los comentarios efectuados, como puede comprobar el lector, son
# válidos para todos los activos, ya que presentan unos gráficos
# y estructuras de autocorrelación muy similares.


    #################      QQ-PLOTS      #################

par(mfrow = c(2, 3)) 
for (i in 1:6){
  car::qqPlot(as.numeric(returns[,i]), 
              distribution = "norm", 
              envelope = 0.95,      
              col = "black",        
              col.lines = colors_plot[i], 
              lwd = 2,
              id = FALSE,
              pch = 19, cex = 0.5,
              main = paste(names[i], "Gaussian QQ"),
              xlab = "Theoretical Quantiles", 
              ylab = "Empirical Quantiles")
}
for (i in 1:6){
  car::qqPlot(as.numeric(returns[,i]), 
              distribution = "t", 
              df = 5,
              envelope = 0.95,      
              col = "black",        
              col.lines = colors_plot[i], 
              lwd = 2,
              id = FALSE,
              pch = 19, cex = 0.5,
              main = paste(names[i], "Student-t (df=5) QQ"),
              xlab = "Theoretical Quantiles", 
              ylab = "Empirical Quantiles")
}

# Se observa claramente que las series temporales no siguen una
# distribución normal, pues en las colas de la distribución tienen
# un comportamiento muy diferente al esperado.

# Hicimos un QQ plot con respecto a una distribución t de Student
# con 5 grados de libertad y parece que la distribución empírica
# se asemeja más a la teórica, aunque depende el activo, el comportamiento
# en las colas también está fuera de las bandas de confianza de la
# distribución teórica. El caso más claro de esto es el de AXA.

# No está muy claro si los rendimientos siguen una t-Student o no,
# pero lo que está claro es que no son normales.

    #################      BOXPLOTS      #################

par(mfrow=c(1,1))
boxplot(coredata(returns), 
        col = colors_plot, 
        main = "Distribución de Rendimientos (Comparativa)",
        ylab = "Rendimientos")

# En estos gráficos podemos ver visualmente lo ya comentado
# cuando vimos las tablas estadísticas de las distribuciones.
# El índice EuroStoxx600 y la cartera diversificada son 
# los activos con menor dispersión, como cabía esperar. Tras estos,
# parece que Iberdrola y AXA son las empresas con menor dispersión
# también. Esto lo vemos ahora visualmente, aunque ya lo podíamos
# intuir porque son las empresas con menor volatilidad.


#################      MATRIZ DE CORRELACIÓN      #################

chart.Correlation(returns,  
                  histogram=F,
                  pch = 19,     
                  col = "steelblue",
                  main = "Correlation Matrix"
)          

# Aquí estamos haciendo un análisis multivariante entre los 6
# activos, haciendo un gráfico de dispersión en cada par de ellos
# para ver si hay cierta estructura de correlación, que parece
# claro que si que la hay. El número en el gráfico es la correlación
# de Pearson, que mide la relación lineal entre las variables. Como se 
# puede observar, todos los coeficientes son significativos y 
# estadísticamente distintos de 0. Además, en los gráficos
# se ve claramente que hay un alto grado de correlación entre todos
# ellos.

cor_matrix <- list(cor(returns, method = "pearson"),
         cor(returns, method = "kendall"),
         cor(returns, method = "spearman"))
cor_names <- c("Pearson Correlation", "Kendall Correlation", 
               "Spearman Correlation")

par(mfrow = c(1, 3), oma = c(0,0,0,0), mar = c(0,0,0,0))
for (i in 1:3){
  corrplot(cor_matrix[[i]], 
           method = "circle",       # Cuadrados de color (estilo Heatmap limpio)
           type = "full",          # Matriz completa 6x6
           addCoef.col = "black",  # Añade el número en negro
           tl.col = "black",       # Color de las etiquetas (Nombres)
           tl.cex = 1.1,           # Tamaño del texto de etiquetas
           number.cex = 0.9,       # Tamaño de los números
           cl.pos = "n",           # Quitamos la barra de color lateral (para ahorrar espacio)
           title = paste(cor_names[i]),
           mar = c(0, 0, 1.5, 0)
  )
}

# En este gráfico presentamos tres tipos distintos de correlación.
# Lineal según Pearson, concordante según Kendall (los activos
# tienen la misma dirección) y de rango según Spearman (los mayores
# y peores rendimientos se producen en los mismos días para todos 
# los activos). En efecto, según todas las métricas de correlación,
# parece que los activos son dependientes entre sí.

# A modo de resumen, estas series de rendimientos parecen muy
# dependientes las unas de las otras, no se distribuyen como una
# normal en absoluto y su distribución podría asemejarse a una t
# de Student. Además, se observan fenómenos como el clúster de volatilidad
# y tenemos una varianza cambiante en el tiempo (heterocedasticidad).


# PREGUNTA 3

# Estimemos las regresiones para los cuantiles dados:

taus <- c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)

n_activos <- c(1, 2, 3, 4, 6)
quantile_betas <- as.data.frame(matrix(
  NA, nrow=length(n_activos), ncol=length(taus) + 1
))
rownames(quantile_betas) <- names[n_activos]
colnames(quantile_betas) <- c(paste0("q_", taus[1:4]), "MCO",
                              paste0("q_", taus[5:7]))

# Seleccionamos los activos e inicializamos la tabla donde escribiremos
# las betas para cada activo y cuantil.


quantile_models <- tibble(
  ACTIVO = names[n_activos],
  IDX_COL = n_activos,
  MODEL_RQ = vector("list", length(n_activos)),
  MODEL_LM = vector("list", length(n_activos))
)

# Inicializamos todos los modelos ajustados en un array.

for (i in n_activos){
  fit_rq <- rq(returns[, i] ~ returns[, 5], tau=taus)
  fit_lm <- lm(returns[, i] ~ returns[, 5])
  if (i==6) {
    quantile_models$MODEL_RQ[[i-1]] <- fit_rq
    quantile_models$MODEL_LM[[i-1]] <- fit_lm
  }
  else{
    quantile_models$MODEL_RQ[[i]] <- fit_rq
    quantile_models$MODEL_LM[[i]] <- fit_lm
    }
}

# Guardamos todos los modelos ajustados y extraemos
# las betas para completar la tabla solicitada.

for (i in 1:nrow(quantile_models)){
  model_rq <- quantile_models$MODEL_RQ[[i]]
  model_lm <- quantile_models$MODEL_LM[[i]]
  betas_rq <- coef(model_rq)[2,] 
  beta_mco <- coef(model_lm)[2]
  quantile_betas[i, paste0("q_", taus)] <- betas_rq
  quantile_betas[i, "MCO"] <- beta_mco
}

# Ahora creamos la tabla en modo gt (mejor visualmente).

quantile_betas$Activo <- rownames(quantile_betas)
cols_num <- setdiff(names(quantile_betas), "Activo")
quantile_betas <- quantile_betas[, c("Activo", cols_num)]
colnames(quantile_betas) <- gsub("q_", "", colnames(quantile_betas))
tabla_betas <- quantile_betas %>%
  gt() %>%
  tab_header(title = "Tabla 1: Regresión Cuantílica") %>%
  fmt_number(
    columns = -Activo,
    decimals = 4
  ) %>%
  cols_align(
    align = "center", 
    columns = -Activo
  )
print(tabla_betas)
gtsave(tabla_betas, "Tabla_1_Regresion_Cuantilica.png")

# Como se puede observar en la tabla, tenemos mayores betas
# para cuantiles más extremos como el 0.01 o el 0.99 que 
# para la mediana, lo que implica que la distribución condicional
# de las empresas y de la cartera equiponderada no son simétricas.
# Esto quiere decir, como estamos realizando la regresión frente al
# índice de referencia, que hay más sensibilidad del activo frente
# al mercado en escenarios muy extremos. Esto sucede en todos los 
# activos, donde la beta es mayor en escenarios extremos en comparación
# con la beta MCO (en media) y el cuantil 0.5 (mediana), salvo en el 
# caso de Iberdrola e Infineon, donde tenemos menor sensibilidad
# frente al índice en los extremos muy positivos (gran subida).
# Esto no sucede en los extremos muy negativos (grandes caídas),
# donde en todos los activos tenemos mayor sensibilidad al mercado
# en comparación con la mediana y la media. Esto nos hace reflexionar
# sobre el riesgo sistemático de las empresas, donde si el mercado va
# mal, la empresa va también peor de lo normal (como es lógico) y tiene
# mayor sensibilidad frente al mercado. En el caso de la cartera 
# equiponderada, tenemos unas betas similares en todos los cuantiles,
# lo que es lógico, ya que al diversificar es el efecto que buscamos, 
# incluso no observamos la diferencia entre las betas de las distintas
# colas como se observan en las demás empresas. Por tanto, la regresión
# lineal, en general, subestima el riesgo en las colas, principalmente
# en los momentos de crisis, que son en los que nos importa medir el riesgo.

# Una vez hecha la tabla, haremos los gráficos para luego realizar
# las interpretaciones de las distintas regresiones efectuadas.

par(mfrow = c(1, 1))
colors_taus <- c("plum3", "palevioletred3", "violetred4", 
                  "dodgerblue3",                              
                 "gray30", "gray50", "gray70")

for (j in 1:nrow(quantile_models)) {
  
  # Seleccionamos el activo a graficar
  nombre_activo <- quantile_models$ACTIVO[j]
  idx_col <- quantile_models$IDX_COL[j]
  
  # Seleccionamos los datos a graficar
  Y <- coredata(returns[, idx_col])
  X <- coredata(returns[, 5])
  
  # Recuperamos los modelos cuantílicos y lineal estimados
  mod_rq <- quantile_models$MODEL_RQ[[j]]
  mod_lm <- quantile_models$MODEL_LM[[j]]
  
  par(mar = c(2, 4, 3, 2) + 0.1)
  
  # Realizamos el gráfico
  plot(x = X, y = Y, 
       type = "p",  
       xlab = "Rendimiento EuroStoxx600", 
       ylab = paste("Rendimiento", nombre_activo),
       main = paste("Gráfico regresiones", nombre_activo),
       pch=19,
       cex = 0.1,
       cex.main = 1.2)
  
  # Pintamos las rectas de regresión cuantílica
  coefs_rq <- coef(mod_rq) 
  for (k in 1:ncol(coefs_rq)) {
    # Para que la mediana sea más gruesa que los demás cuantiles
    if (k == 4) { 
      grosor <- 3.5 
      color_linea <- colors_taus[k]
    } else {
      grosor <- 1.5
      color_linea <- colors_taus[k]
    }
    abline(a = coefs_rq[1, k], b = coefs_rq[2, k],
           col = color_linea, lwd = grosor)
  }
  
  # Pintamos la recta de regresión MCO
  abline(mod_lm, lty = 2, col = "red", lwd = 2)
  
  # Añadimos la leyenda
  legend("topleft", 
         legend = c("MCO", paste0("q = ", taus)), 
         col = c("red", colors_taus),          
         lty = c(2, rep(1, length(taus))),      
         lwd = c(2, rep(1.5, length(taus))),
         bty = "n", cex = 0.8, ncol = 2)        
}

# En los gráficos se ve más visualmente que las distintas rectas de
# regresión no son paralelas entre sí, como deberían ser si la 
# distribución condicional de las empresas fuese simétrica,
# con lo que se puede comprobar la relevancia de la regresión cuantílica
# para capturar el riesgo de los activos en los momentos de crisis
# frente a la regresión lineal.


# PREGUNTA 4


# Paso 1

# CaViaR

# Dada la fórmula caviarOptim, debemos seleccionar el modelo
# 2 para optimizar el CaViaR asimétrico, como se solicita.

# Inicializamos el objeto donde guardaremos las betas de los
# distintos activos.
betas_caviar <- matrix(NA, nrow = ncol(returns), ncol = 4)
rownames(betas_caviar) <- colnames(returns)
colnames(betas_caviar) <- c("Beta1", "Beta2", "Beta3", "Beta4")

# Inicializamos el objeto donde guardaremos los VaR CaViaR
var_dinamico_caviar <- xts(matrix(NA, nrow = nrow(returns), 
                                  ncol = ncol(returns)), 
                                  order.by = index(returns))
colnames(var_dinamico_caviar) <- colnames(returns)

# Calculamos el modelo CaViaR para los 6 activos
for (i in 1:ncol(returns)) {
  
  caviarfit <- caviarOptim(returns[,i], 
                           model = 2, 
                           pval = 0.01)
  
  betas_caviar[i, ] <- caviarfit$bestPar
  
  var_dinamico_caviar[, i] <- -caviarfit$VaR
}

# Al realizar la optimización obtenemos los valores de 
# las betas y del VaR dinámico deseados empleando el 
# modelo CaViaR.

# Paramétrico

# Estimaremos ahora un modelo GJR-GARCH(1,1) para la media
# y la volatilidad condicionales de cada activo y así poder
# calcular el VaR dinámico empleando este modelo.

# Como hemos visto en el análisis descriptivo de cada activo
# efectuado anteriormente, parece que asumir una distribución
# t de Student para las innovaciones es más coherente que una
# normal, así que emplearemos esta distribución para ellas.
# Además, en los gráficos de la ACF y la PACF de los activos,
# no parece que haya retardos significativos muy relevantes,
# aunque los hay, pero no asumiremos ningún modelo para la media
# por simplificar el cálculo. Por lo tanto nos tendremos 
# que centrar en obtener la serie de volatilidades condicionales
# de cada activo.

spec_gjr <- ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

# Empleando la librería rugarch, le pasamos el modelo para
# la varianza GJR-GACRH(1,1) y el modelo para la media (ARMA(0,0)),
# además de la distribución de las innovaciones, y la propia 
# librería optimizará el modelo para entregarnos los grados de
# libertad de las innovaciones, la media y la varianza condicionales.
# Vamos a ello.

var_dinamico_garch <- xts(matrix(NA, nrow=nrow(returns), ncol=ncol(returns)), 
                             order.by=index(returns))
colnames(var_dinamico_garch) <- colnames(returns)

# Inicializamos una matriz donde guardaremos el VaR paramétrico.

for (i in 1:ncol(returns)) {
  
  # Ajustamos el modelo especificado
  fit <- ugarchfit(spec = spec_gjr, data = returns[,i], solver = "hybrid")
  
  # Obtenemos la media y volatilidad diarias condicionales
  sigma_t <- sigma(fit)
  mu_t    <- fitted(fit)
  
  # Obtenemos los grados de libertad de las innovaciones t de Student
  coefs <- coef(fit)
  df <- coefs["shape"]
  
  # Obtenemos el valor crítico de la distribución
  q <- qdist(distribution = "std", p = 0.01, shape = df)
  
  # No necesitamos normalizar la varianza porque 
  # qdist te entrega ya el valor crítico normalizado.
  # Obtenemos el VaR paramétrico dinámico
  var_dinamico_garch[, i] <- mu_t + (sigma_t * q)
}

# Efectuaremos ahora los 4 gráficos representando los rendimientos
# y los VaR dinámicos calculados asociados a las 4 empresas.

par(mfrow = c(2, 2)) 

for (i in 1:4) {
  min_y <- min(min(returns[, i]), min(var_dinamico_caviar[, i]),
               min(var_dinamico_garch[, i]))
  max_y <- max(returns[, i])
  
  plot(as.zoo(returns[, i]), 
       main = paste("VaR 1% Dinámico:", colnames(returns)[i]),
       xlab = "Tiempo", 
       ylab = "Rendimientos / VaR",
       col = "gray75",     
       lwd = 1,
       ylim = c(min_y, max_y * 1.1)) 
  
  lines(as.zoo(var_dinamico_caviar[, i]), col = "red", lwd = 1.5)
  
  lines(as.zoo(var_dinamico_garch[, i]), col = "blue", lwd = 1.5, lty = 2)
  
  legend("bottomleft", 
         legend = c("CaViaR", "GARCH"),
         col = c("red", "blue"),
         lty = c(1, 2), 
         lwd = 1.5,
         cex = 0.8,    
         bty = "n")    
}
par(mfrow = c(1, 1))  

# A la vista de los gráficos, ambos modelos VaR son muy semejantes,
# lo que indica que la elección de la distribución t de Student
# para modelizar los rendimientos es acertada, ya que estima
# de manera similar el riesgo que el modelo CaViaR asimétrico,
# donde no se hace ninguna asunción sobre la distribución de estos.
# En el caso de AXA, parece que el VaR paramétrico en momentos de 
# baja volatilidad infraestima el riesgo en comparación con el 
# modelo CaViaR asimétrico, esto probablemente sea debido a la 
# estimación de los parámetros. En cualquier caso, que dos modelos
# tan diferentes se comporten de manera similar significa que son
# modelos robustos y la estimación del riesgo esté bien calibrada.



# Paso 2
  

# En la Pregunta 3 estimamos 5 regresiones cuantílicas con tau=0.01,
# cuya variable dependiente eran los activos y la cartera equiponderada
# y la variable independiente era el índice de referencia. En este caso,
# se nos pide que las variables dependientes sean la cartera equiponderada
# y el índice de referencia, con variables independientes las 4 
# distintas empresas. 

# Bajo la misma manera de proceder que en la Pregunta 3:

# Seleccionamos las empresas
n_activos_4 <- c(1, 2, 3, 4)

quantile_models_4 <- tibble(
  ACTIVO = names[n_activos_4],
  IDX_COL = n_activos_4,
  MODEL_RQ_IDX = vector("list", length(n_activos_4)),
  MODEL_RQ_POR = vector("list", length(n_activos_4))
)

for (i in n_activos_4){
  fit_rq_por <- rq(returns[, 5] ~ returns[, i], tau=taus[1])
  fit_rq_idx <- rq(returns[, 6] ~ returns[, i], tau=taus[1])
  quantile_models_4$MODEL_RQ_POR[[i]] <- fit_rq_por
  quantile_models_4$MODEL_RQ_IDX[[i]] <- fit_rq_idx
}

# Una vez guardados los modelos, extraeremos las betas y sus
# desviaciones típicas, así como el cálculo del pseudo-R^2
# de cada modelo.

rho <- function(u, tau = 0.5) {
  u * (tau - (u < 0))
}

tabla_4_paso_2 <- data.frame()

for (i in 1:nrow(quantile_models_4)) {
  
  # Primero obtenemos los datos para los modelos sobre IDX
  nombre_empresa <- quantile_models_4$ACTIVO[i]
  mod_idx <- quantile_models_4$MODEL_RQ_IDX[[i]]
  
  # Extraemos betas y desviaciones típicas de cada modelo
  summ_idx <- summary(mod_idx, se = "nid") 
  coefs_idx <- summ_idx$coefficients
  
  # Calculamos pseudo-R^2, creando primero modelo sin variables
  y_idx <- mod_idx$y 
  fit_idx <- rq(y_idx ~ 1, tau = taus[1])
  
  RASW_IDX <- sum(rho(mod_idx$residuals, taus[1]))
  TASW_IDX <- sum(rho(fit_idx$residuals, taus[1]))
  
  r2_idx <- 1 - (RASW_IDX / TASW_IDX)
  
  # Lo añadimos a la tabla
  tabla_4_paso_2 <- rbind(tabla_4_paso_2, data.frame(
    Dependiente = "V. Dependiente: IND",
    Empresa = nombre_empresa,
    Beta1 = coefs_idx[1, 1],      
    dt_Beta1 = coefs_idx[1, 2],   
    Beta2 = coefs_idx[2, 1],      
    dt_Beta2 = coefs_idx[2, 2],
    Pseudo_R2 = r2_idx
  ))
  
  # Ahora obtenemos los datos para los modelos POR
  mod_por <- quantile_models_4$MODEL_RQ_POR[[i]]
  
  summ_por <- summary(mod_por, se = "nid")
  coefs_por <- summ_por$coefficients
  
  # Calculamos pseudo-R^2, creando primero modelo sin variables
  y_por <- mod_por$y
  fit_por <- rq(y_por ~ 1, tau = taus[1])
  
  RASW_POR <- sum(rho(mod_por$residuals, taus[1]))
  TASW_POR <- sum(rho(fit_por$residuals, taus[1]))
  
  r2_por <- 1 - (RASW_POR / TASW_POR)
  
  # Lo añadimos a la tabla
  tabla_4_paso_2 <- rbind(tabla_4_paso_2, data.frame(
    Dependiente = "V. Dependiente: POR",
    Empresa = nombre_empresa,
    Beta1 = coefs_por[1, 1],
    dt_Beta1 = coefs_por[1, 2],
    Beta2 = coefs_por[2, 1],
    dt_Beta2 = coefs_por[2, 2],
    Pseudo_R2 = r2_por
  ))
}

# Ponemos la tabla bonita
tabla_4_paso_2 <- tabla_4_paso_2 %>%
  group_by(Dependiente) %>%
  gt() %>%
  tab_header(
    title = "Tabla 2. Estimaciones regresión cuantílica Correlaciones (tau=0.01)"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 4
  ) %>%
  cols_label(
    Beta1 = "β1 (Intercepto)",
    dt_Beta1 = "dt(β1)",
    Beta2 = "β2 (Pendiente)",
    dt_Beta2 = "dt(β2)",
    Pseudo_R2 = "Psdo-R2"
  ) %>%
  cols_align(  
    align = "center",
    columns = where(is.numeric)
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_row_groups()
  )

print(tabla_4_paso_2)
gtsave(tabla_4_paso_2, "Tabla_2_Regresion_Cuantilica.png")

# Como se puede observar, todos los coeficientes tienen una desviación
# típica muy baja, por lo que los podemos considerar a todos los 
# coeficientes de todas las regresiones significativos. Se puede
# comprobar haciendo el contraste t realizando beta/dt(beta) y 
# en efecto todos los contrastes son mayores que el valor crítico.

# En cuanto a beta1, es un coeficiente negativo de aproximadamente
# -0.2 para todas las empresas en relación con el EuroStoxx600
# y la cartera equiponderada. Este es un valor lógico ya que este 
# parámetro representa el cuantil 1% (el VaR) del índice, sea
# la cartera o el EuroStoxx, cuando el rendimiento de la empresa
# es igual a 0. Por tanto, esperamos que sea un número negativo,
# pero tampoco debería serlo en exceso ya que estaríamos
# sobreestimando el riesgo, probablemente.

# Si hablamos de beta2, comenzamos a ver valores más diferentes para
# las distintas empresas. Este parámetro refleja la dependencia
# del índice con respecto a la empresa en pérdidas extremas de esta.
# Se podría interpretar como la dependencia del índice frente a 
# pérdidas extremas de la empresa. En este caso, parece que Iberdrola
# y AXA son las más correlacionadas con el EuroStoxx y con la cartera
# equiponderada, lo cual tiene sentido, al ser una empresa del sector
# energético (Iberdrola) y del sector asegurador, muy grandes en 
# Europa (AXA). Con respecto al EuroStoxx600, el mayor beta2 lo tiene
# con diferencia Iberdrola, pero hay que tener en cuenta que es también
# la empresa con mayor desviación típica, bastante superior a las demás,
# por lo que deberíamos tratar este dato con cautela. En cuanto a la 
# cartera equiponderada, AXA e Iberdrola tienen betas similares, siendo
# igualmente Iberdrola la que mayor tiene. Por tanto parece que estas
# dos empresas son las que mayor influencia tienen en grandes pérdidas
# con respecto al índice. Caso particular es el del BBVA, pues al ser
# un banco uno debería pensar que sus casos extremos también deberían
# influir bastante en el índice, pero cabe recordar que la mayor 
# parte de su negocio está en América Latina, por lo que este debería
# ser el factor que explica que no sea la empresa con mayor beta2.

# Hablando sobre el pseudo-R^2, podríamos identificarlo como la bondad
# de ajuste del modelo, y en este caso AXA es la empresa con 
# mejor ajuste para ambos modelos. Iberdrola es la empresa que peor 
# ajuste tiene en conjunto en ambos modelos, lo que nos hace replantearnos
# lo comentado anteriormente sobre la fiabilidad de su dependencia
# con respecto al índice, como habíamos comentado previamente.


# Paso 3

# Para la realización de este paso, primero necesitamos extraer
# los valores de las betas calculadas en el paso anterior.
# Además, necesitamos el valor del VaR dinámico de las 4 empresas,
# que ya hemos calculado en el paso 1 con dos métodos diferentes.
# Hemos visto que daban resultados muy similares, así que realizaremos
# el cálculo del CoVaR empleando en exclusiva el VaR dinámico CaViaR,
# por sencillez, dado que no suponíamos ninguna distribución
# sobre los rendimientos.


# Inicializamos objetos para guardar CoVaR.
CoVaR_IND_series <- xts(matrix(NA, nrow=nrow(returns), ncol=4),
                        order.by=index(returns))
CoVaR_POR_series <- xts(matrix(NA, nrow=nrow(returns), ncol=4),
                        order.by=index(returns))
colnames(CoVaR_IND_series) <- colnames(returns)[1:4]
colnames(CoVaR_POR_series) <- colnames(returns)[1:4]

# Cálculo CoVaR

for (i in 1:4) {
  
  # Betas de las empresas regresando sobre el índice
  coefs_ind <- coef(quantile_models_4$MODEL_RQ_IDX[[i]]) # Intercepto y Pendiente
  beta1_ind <- coefs_ind[1]
  beta2_ind <- coefs_ind[2]
  
  # Betas de las empresas regresando sobre el portfolio
  coefs_por <- coef(quantile_models_4$MODEL_RQ_POR[[i]]) # Intercepto y Pendiente
  beta1_por <- coefs_por[1]
  beta2_por <- coefs_por[2]
  
  # Variable explicativa
  var <- var_dinamico_caviar[,i]
  CoVaR_IND_series[, i] <- beta1_ind + beta2_ind * var
  CoVaR_POR_series[, i] <- beta1_por + beta2_por * var
}


# Gráfico sobre el EuroStoxx600
par(mfrow=c(1,1))
plot(as.zoo(returns[, 5]), type='l', col="gray90", 
     main="CoVaR del Índice (IND) condicionado a las empresas",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(CoVaR_IND_series), max(returns[, 5])*0.8))

# VaR dinámico
lines(as.zoo(var_dinamico_caviar[, 5]), col="black", lwd=2, lty=1)

# CoVaRs
colores <- c("red", "blue", "green", "purple")
for(i in 1:4){
  lines(as.zoo(CoVaR_IND_series[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento IND", "VaR Incondicional IND",
                colnames(returns)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR del índice

tabla_IND <- merge(var_dinamico_caviar[, 5], CoVaR_IND_series)
colnames(tabla_IND) <- c("VaR Incondicional (IND)",
                        colnames(CoVaR_IND_series))

stats_IND <- data.frame(
  Serie = names(tabla_IND),
  Media = apply(tabla_IND, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_IND, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_IND, 2, sd, na.rm = TRUE)
)
stats_IND$Panel <- "Panel A: EuroStoxx600 (IND)"



# Gráfico sobre cartera equiponderada
plot(as.zoo(returns[, 6]), type='l', col="gray90", 
     main="CoVaR de la Cartera (POR) condicionado a las empresas",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(CoVaR_POR_series), max(returns[, 6])*0.8))

# VaR dinámico
lines(as.zoo(var_dinamico_caviar[, 6]), col="black", lwd=2, lty=1)

# CoVaRs
for(i in 1:4){
  lines(as.zoo(CoVaR_POR_series[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento POR", "VaR Incondicional POR", colnames(returns)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR de POR

tabla_POR <- merge(var_dinamico_caviar[, 6], CoVaR_POR_series)
colnames(tabla_POR) <- c("VaR Incondicional (POR)",
                        colnames(CoVaR_POR_series))

stats_POR <- data.frame(
  Serie = names(tabla_POR),
  Media = apply(tabla_POR, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_POR, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_POR, 2, sd, na.rm = TRUE)
)
stats_POR$Panel <- "Panel B: Cartera (POR)"

tabla_bonita <- rbind(stats_IND, stats_POR)

tabla_bonita <- tabla_bonita %>%
  group_by(Panel) %>% 
  gt() %>%
  tab_header(
    title = md("**Tabla 3. Estadísticos Descriptivos: VaR vs CoVaR**"),
    subtitle = "Comparación de Medias, Medianas y Volatilidad del Riesgo Sistémico"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana, Desv_Tipica),
    decimals = 4
  ) %>%
  cols_label(
    Serie = "Activo / Condición",
    Desv_Tipica = "Desv. Típica"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  data_color(
    columns = Media,
    method = "numeric",
    palette = c("red", "white"),
    domain = NULL 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )

print(tabla_bonita)
gtsave(tabla_bonita, "Tabla_3_CoVaR.png")


# Como podemos observar en el gráfico sobre el índice, así como en
# la tabla descriptiva sobre este, el VaR incondicional del
# EuroStoxx600 está (en valor absoluto) muy por debajo del CoVaR
# relacionado con las demás empresas. Esto hace notar el riesgo
# sistémico de estas empresas, si estas están en malas condiciones,
# tienden a arrastrar consigo al índice. Esto se puede comprobar
# para los valores de la media y la mediana, además con una diferencia
# muy notable entre el VaR incondicional y el CoVaR. En periodos
# malos de mercado, las diferencias entre VaR y CoVaR se reducen, puesto
# que la dependencia entre las variables se magnifica y la diversificación
# pierde su efecto, como se puede observar en el gráfico.

# Comentarios muy similares podemos hacer sobre la cartera equiponderada,
# aunque la diferencia entre VaR y CoVaR en condiciones estables de 
# mercado no sea tan amplia. Esto probablemente sea debido a la menor
# diversificación de POR y, como se puede comprobar, mayor riesgo
# (VaR) de la propia cartera en sí misma, lo que reduce
# la contribución al VaR de cada empresa. Podemos observar la misma
# dependencia, tanto a nivel visual como con los estadísticos descriptivos.
# En condiciones estables de mercado, el CoVaR de las 4 empresas se
# sitúa por debajo del VaR incondicional, señalando que la caída
# de alguna de estas empresas impacta negativamente en la cartera
# POR. Sin embargo, en momentos de crisis sistémicas, las diferencias
# se reducen, como se puede observar claramente en 2020. 

# La desviación típica la podemos interpretar como lo estable
# que es la transmisión del riesgo en estas empresas al mercado.
# BBVA es la empresa con mayor desviación típica, probablemente
# porque su impacto en el mercado dependa mucho de las condiciones
# de este, tiene un comportamiento más cíclico.

# En ambos índices, la empresa con mayor CoVaR es la misma: Iberdrola.
# Al ser una empresa de energía, considerada como segura, podría ser
# el motivo por el que en esta selección de empresas arrastre más al 
# mercado en media.



# Paso 4


# Calcularemos el VaR al 50% usando el modelo paramétrico, ya que
# optimizar el CaViaR al 50% puede dar problemas numéricos.

var_dinamico_garch_4 <- xts(matrix(NA, nrow=nrow(returns), ncol=ncol(returns)), 
                          order.by=index(returns))
colnames(var_dinamico_garch_4) <- colnames(returns)

# Inicializamos una matriz donde guardaremos el VaR paramétrico.

for (i in 1:ncol(returns)) {
  
  # Ajustamos el modelo especificado
  fit <- ugarchfit(spec = spec_gjr, data = returns[,i], solver = "hybrid")
  
  # Obtenemos la media y volatilidad diarias condicionales
  sigma_t <- sigma(fit)
  mu_t    <- fitted(fit)
  
  # Obtenemos los grados de libertad de las innovaciones t de Student
  coefs <- coef(fit)
  df <- coefs["shape"]
  
  # Obtenemos el valor crítico de la distribución
  q <- qdist(distribution = "std", p = 0.5, shape = df)
  
  # No necesitamos normalizar la varianza porque 
  # qdist te entrega ya el valor crítico normalizado.
  # Obtenemos el VaR paramétrico dinámico
  var_dinamico_garch_4[, i] <- mu_t + (sigma_t * q)
}

# Inicializamos matrices para Delta CoVaR
Delta_CoVaR_IND <- xts(matrix(NA, nrow=nrow(returns), ncol=4), order.by=index(returns))
Delta_CoVaR_POR <- xts(matrix(NA, nrow=nrow(returns), ncol=4), order.by=index(returns))
colnames(Delta_CoVaR_IND) <- colnames(returns)[1:4]
colnames(Delta_CoVaR_POR) <- colnames(returns)[1:4]

for (i in 1:4) {
  # IND
  coefs_ind <- coef(quantile_models_4$MODEL_RQ_IDX[[i]])
  beta2_ind <- coefs_ind[2] 
  
  diff_var <- var_dinamico_caviar[,i] - var_dinamico_garch_4[,i]
  Delta_CoVaR_IND[, i] <- beta2_ind * diff_var
  
  # POR
  coefs_por <- coef(quantile_models_4$MODEL_RQ_POR[[i]])
  beta2_por <- coefs_por[2]
  
  diff_var <- var_dinamico_caviar[,i] - var_dinamico_garch_4[,i]
  Delta_CoVaR_POR[, i] <- beta2_por * diff_var
}

# Una vez calculados los Delta CoVaR, procederemos a realizar
# los mismos gráficos y tablas que en el paso anterior.


# Gráfico sobre el EuroStoxx600
par(mfrow=c(1,1))
plot(as.zoo(returns[, 5]), type='l', col="gray90", 
     main="Delta CoVaR del Índice (IND) condicionado a las empresas",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(Delta_CoVaR_IND), max(returns[, 5])*0.8))

# VaR dinámico
lines(as.zoo(var_dinamico_caviar[, 5]), col="black", lwd=2, lty=1)

# CoVaRs
colores <- c("red", "blue", "green", "purple")
for(i in 1:4){
  lines(as.zoo(Delta_CoVaR_IND[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento IND", "VaR Incondicional IND",
                colnames(returns)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR del índice

tabla_IND <- merge(var_dinamico_caviar[, 5], Delta_CoVaR_IND)
colnames(tabla_IND) <- c("VaR Incondicional (IND)",
                         colnames(Delta_CoVaR_IND))

stats_IND <- data.frame(
  Serie = names(tabla_IND),
  Media = apply(tabla_IND, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_IND, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_IND, 2, sd, na.rm = TRUE)
)
stats_IND$Panel <- "Panel A: EuroStoxx600 (IND)"



# Gráfico sobre cartera equiponderada
plot(as.zoo(returns[, 6]), type='l', col="gray90", 
     main="Delta CoVaR de la Cartera (POR) condicionado a las empresas",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(Delta_CoVaR_POR), max(returns[, 6])*0.8))

# VaR dinámico
lines(as.zoo(var_dinamico_caviar[, 6]), col="black", lwd=2, lty=1)

# CoVaRs
for(i in 1:4){
  lines(as.zoo(Delta_CoVaR_POR[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento POR", "VaR Incondicional POR", colnames(returns)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR de POR

tabla_POR <- merge(var_dinamico_caviar[, 6], Delta_CoVaR_POR)
colnames(tabla_POR) <- c("VaR Incondicional (POR)",
                         colnames(Delta_CoVaR_POR))

stats_POR <- data.frame(
  Serie = names(tabla_POR),
  Media = apply(tabla_POR, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_POR, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_POR, 2, sd, na.rm = TRUE)
)
stats_POR$Panel <- "Panel B: Cartera (POR)"

tabla_bonita <- rbind(stats_IND, stats_POR)

tabla_bonita <- tabla_bonita %>%
  group_by(Panel) %>% 
  gt() %>%
  tab_header(
    title = md("**Tabla 4. Estadísticos Descriptivos: VaR vs Delta CoVaR**"),
    subtitle = "Comparación de Medias, Medianas y Volatilidad del Riesgo Sistémico"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana, Desv_Tipica),
    decimals = 4
  ) %>%
  cols_label(
    Serie = "Activo / Condición",
    Desv_Tipica = "Desv. Típica"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  data_color(
    columns = Media,
    method = "numeric",
    palette = c("red", "white"),
    domain = NULL 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )

print(tabla_bonita)
gtsave(tabla_bonita, "Tabla_4_CoVaR.png")


# Habíamos visto que el VaR del índice (IND o POR) al 1%, condicionado
# a que las empresas estuviesen en su cuantil 1%, implicaba que
# Iberdrola era la empresa que más influencia tenía sobre el índice.
# Es decir, condicionado a un nivel de riesgo muy elevado, en condiciones
# de mercado muy malas para la empresa, Iberdrola era la que 
# más influencia tenía sobre el índice para arrastrarlo consigo.
# Sin embargo, al efectuar el Delta CoVaR, donde sustraemos el VaR
# del índice cuando la empresa está en la mediana de sus datos, Iberdrola
# es la empresa con menor Delta CoVaR para el EuroStoxx, y prácticamente
# la peor también para la cartera equiponderada. Esto significa
# que Iberdrola no tiene tanta capacidad de contagio al mercado
# como se podía presuponer observando el CoVaR al 1%, ya que ahora
# al sustraer el CoVaR al 50% de los datos de Iberdrola, el rendimiento
# de la empresa en la mitad de sus datos, se puede comprobar que
# no influye tanto en el mercado. Esto probablemente sea porque Iberdrola
# sea una empresa con poco riesgo en general, como hemos visto
# en métricas anteriores en el trabajo y la diferencia entre
# su VaR al 1% y su VaR al 50% no es muy elevada, por lo que no tiene
# mucho riesgo de contagio al sistema, al ser una empresa muy estable.  
# Al analizar el Delta CovaR, Infineon es la empresa más sistémica 
# para el EuroStoxx y AXA para POR.


# Ahora analicemos el comportamiento de las distintas empresas antes,
# durante y después del año del COVID-19.

inicio_covid <- as.Date("2020-02-15")
fin_covid    <- as.Date("2021-02-15")

# Submuestras para var_dinamico_caviar
var_caviar_pre  <- var_dinamico_caviar[paste0("/", inicio_covid - 1)]
var_caviar_cov  <- var_dinamico_caviar[paste0(inicio_covid, "/", fin_covid)]
var_caviar_post <- var_dinamico_caviar[paste0(fin_covid + 1, "/")]

# Submuestras para Delta_CoVaR_IND
delta_ind_pre  <- Delta_CoVaR_IND[paste0("/", inicio_covid - 1)]
delta_ind_cov  <- Delta_CoVaR_IND[paste0(inicio_covid, "/", fin_covid)]
delta_ind_post <- Delta_CoVaR_IND[paste0(fin_covid + 1, "/")]

# Submuestras para Delta_CoVaR_POR
delta_por_pre  <- Delta_CoVaR_POR[paste0("/", inicio_covid - 1)]
delta_por_cov  <- Delta_CoVaR_POR[paste0(inicio_covid, "/", fin_covid)]
delta_por_post <- Delta_CoVaR_POR[paste0(fin_covid + 1, "/")]

# Submuestras para rendimientos
ret_pre  <- returns[paste0("/", inicio_covid - 1)]
ret_cov  <- returns[paste0(inicio_covid, "/", fin_covid)]
ret_post <- returns[paste0(fin_covid + 1, "/")]

# PERIODO PRE COVID

# Gráfico sobre el EuroStoxx600
par(mfrow=c(1,1))
plot(as.zoo(ret_pre[, 5]), type='l', col="gray90", 
     main="Delta CoVaR del Índice (IND) condicionado a las empresas Pre-COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_ind_pre), max(ret_pre[, 5])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_pre[, 5]), col="black", lwd=2, lty=1)

# CoVaRs
colores <- c("red", "blue", "green", "purple")
for(i in 1:4){
  lines(as.zoo(delta_ind_pre[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento IND", "VaR Incondicional IND",
                colnames(ret_pre)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR del índice

tabla_IND <- merge(var_caviar_pre[, 5], delta_ind_pre)
colnames(tabla_IND) <- c("VaR Incondicional (IND)",
                         colnames(delta_ind_pre))

stats_IND <- data.frame(
  Serie = names(tabla_IND),
  Media = apply(tabla_IND, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_IND, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_IND, 2, sd, na.rm = TRUE)
)
stats_IND$Panel <- "Panel A: EuroStoxx600 (IND)"



# Gráfico sobre cartera equiponderada
plot(as.zoo(ret_pre[, 6]), type='l', col="gray90", 
     main="Delta CoVaR de la Cartera (POR) condicionado a las empresas Pre-COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_por_pre), max(ret_pre[, 6])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_pre[, 6]), col="black", lwd=2, lty=1)

# CoVaRs
for(i in 1:4){
  lines(as.zoo(delta_por_pre[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento POR", "VaR Incondicional POR", colnames(ret_pre)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR de POR

tabla_POR <- merge(var_caviar_pre[, 6], delta_por_pre)
colnames(tabla_POR) <- c("VaR Incondicional (POR)",
                         colnames(delta_por_pre))

stats_POR <- data.frame(
  Serie = names(tabla_POR),
  Media = apply(tabla_POR, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_POR, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_POR, 2, sd, na.rm = TRUE)
)
stats_POR$Panel <- "Panel B: Cartera (POR)"

tabla_bonita <- rbind(stats_IND, stats_POR)

tabla_bonita <- tabla_bonita %>%
  group_by(Panel) %>% 
  gt() %>%
  tab_header(
    title = md("**Tabla 5. Estadísticos Descriptivos: VaR vs Delta CoVaR Pre-COVID**"),
    subtitle = "Comparación de Medias, Medianas y Volatilidad del Riesgo Sistémico"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana, Desv_Tipica),
    decimals = 4
  ) %>%
  cols_label(
    Serie = "Activo / Condición",
    Desv_Tipica = "Desv. Típica"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  data_color(
    columns = Media,
    method = "numeric",
    palette = c("red", "white"),
    domain = NULL 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )

print(tabla_bonita)
gtsave(tabla_bonita, "Tabla_5_CoVaR.png")




# PERIODO COVID

# Gráfico sobre el EuroStoxx600
par(mfrow=c(1,1))
plot(as.zoo(ret_cov[, 5]), type='l', col="gray90", 
     main="Delta CoVaR del Índice (IND) condicionado a las empresas COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_ind_cov), max(ret_cov[, 5])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_cov[, 5]), col="black", lwd=2, lty=1)

# CoVaRs
colores <- c("red", "blue", "green", "purple")
for(i in 1:4){
  lines(as.zoo(delta_ind_cov[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento IND", "VaR Incondicional IND",
                colnames(ret_cov)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR del índice

tabla_IND <- merge(var_caviar_cov[, 5], delta_ind_cov)
colnames(tabla_IND) <- c("VaR Incondicional (IND)",
                         colnames(delta_ind_cov))

stats_IND <- data.frame(
  Serie = names(tabla_IND),
  Media = apply(tabla_IND, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_IND, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_IND, 2, sd, na.rm = TRUE)
)
stats_IND$Panel <- "Panel A: EuroStoxx600 (IND)"



# Gráfico sobre cartera equiponderada
plot(as.zoo(ret_cov[, 6]), type='l', col="gray90", 
     main="Delta CoVaR de la Cartera (POR) condicionado a las empresas COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_por_cov), max(ret_cov[, 6])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_cov[, 6]), col="black", lwd=2, lty=1)

# CoVaRs
for(i in 1:4){
  lines(as.zoo(delta_por_cov[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento POR", "VaR Incondicional POR", colnames(ret_cov)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR de POR

tabla_POR <- merge(var_caviar_cov[, 6], delta_por_cov)
colnames(tabla_POR) <- c("VaR Incondicional (POR)",
                         colnames(delta_por_cov))

stats_POR <- data.frame(
  Serie = names(tabla_POR),
  Media = apply(tabla_POR, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_POR, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_POR, 2, sd, na.rm = TRUE)
)
stats_POR$Panel <- "Panel B: Cartera (POR)"

tabla_bonita <- rbind(stats_IND, stats_POR)

tabla_bonita <- tabla_bonita %>%
  group_by(Panel) %>% 
  gt() %>%
  tab_header(
    title = md("**Tabla 6. Estadísticos Descriptivos: VaR vs Delta CoVaR COVID**"),
    subtitle = "Comparación de Medias, Medianas y Volatilidad del Riesgo Sistémico"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana, Desv_Tipica),
    decimals = 4
  ) %>%
  cols_label(
    Serie = "Activo / Condición",
    Desv_Tipica = "Desv. Típica"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  data_color(
    columns = Media,
    method = "numeric",
    palette = c("red", "white"),
    domain = NULL 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )

print(tabla_bonita)
gtsave(tabla_bonita, "Tabla_6_CoVaR.png")



# PERIODO POST COVID

# Gráfico sobre el EuroStoxx600
par(mfrow=c(1,1))
plot(as.zoo(ret_post[, 5]), type='l', col="gray90", 
     main="Delta CoVaR del Índice (IND) condicionado a las empresas Post-COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_ind_post), max(ret_post[, 5])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_post[, 5]), col="black", lwd=2, lty=1)

# CoVaRs
colores <- c("red", "blue", "green", "purple")
for(i in 1:4){
  lines(as.zoo(delta_ind_post[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento IND", "VaR Incondicional IND",
                colnames(ret_post)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR del índice

tabla_IND <- merge(var_caviar_post[, 5], delta_ind_post)
colnames(tabla_IND) <- c("VaR Incondicional (IND)",
                         colnames(delta_ind_post))

stats_IND <- data.frame(
  Serie = names(tabla_IND),
  Media = apply(tabla_IND, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_IND, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_IND, 2, sd, na.rm = TRUE)
)
stats_IND$Panel <- "Panel A: EuroStoxx600 (IND)"



# Gráfico sobre cartera equiponderada
plot(as.zoo(ret_post[, 6]), type='l', col="gray90", 
     main="Delta CoVaR de la Cartera (POR) condicionado a las empresas Post-COVID",
     ylab="Rendimientos / Riesgo", xlab="Tiempo",
     ylim=c(min(delta_por_post), max(ret_post[, 6])*0.8))

# VaR dinámico
lines(as.zoo(var_caviar_post[, 6]), col="black", lwd=2, lty=1)

# CoVaRs
for(i in 1:4){
  lines(as.zoo(delta_por_post[, i]), col=colores[i], lwd=1)
}

legend("topright", 
       legend=c("Rendimiento POR", "VaR Incondicional POR", colnames(ret_post)[1:4]),
       col=c("gray90", "black", colores), 
       lty=1, lwd=c(1, 2, 1, 1, 1, 1), cex=0.7)

# Tabla estadísticos sobre el VaR y CoVaR de POR

tabla_POR <- merge(var_caviar_post[, 6], delta_por_post)
colnames(tabla_POR) <- c("VaR Incondicional (POR)",
                         colnames(delta_por_post))

stats_POR <- data.frame(
  Serie = names(tabla_POR),
  Media = apply(tabla_POR, 2, mean, na.rm = TRUE),
  Mediana = apply(tabla_POR, 2, median, na.rm = TRUE),
  Desv_Tipica = apply(tabla_POR, 2, sd, na.rm = TRUE)
)
stats_POR$Panel <- "Panel B: Cartera (POR)"

tabla_bonita <- rbind(stats_IND, stats_POR)

tabla_bonita <- tabla_bonita %>%
  group_by(Panel) %>% 
  gt() %>%
  tab_header(
    title = md("**Tabla 7. Estadísticos Descriptivos: VaR vs Delta CoVaR Post-COVID**"),
    subtitle = "Comparación de Medias, Medianas y Volatilidad del Riesgo Sistémico"
  ) %>%
  fmt_number(
    columns = c(Media, Mediana, Desv_Tipica),
    decimals = 4
  ) %>%
  cols_label(
    Serie = "Activo / Condición",
    Desv_Tipica = "Desv. Típica"
  ) %>%
  cols_align(
    align = "center",
    columns = where(is.numeric)
  ) %>%
  data_color(
    columns = Media,
    method = "numeric",
    palette = c("red", "white"),
    domain = NULL 
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )

print(tabla_bonita)
gtsave(tabla_bonita, "Tabla_7_CoVaR.png")


# En las tablas se puede ver que antes y durante la pandemia, las empresas
# más sistémicas son AXA y BBVA, en cierta medida lógico, teniendo
# en cuenta que son empresas bancarias y aseguradoras, formando
# parte del núcleo del sistema financiero europeo.

# Por otro lado tras el COVID-19, podemos comprobar un cambio en
# el riesgo sistémico, siendo Infineon la empresa con mayor Delta CoVaR
# de las cuatro, probablemente debido a la relevancia que está cobrando 
# el sector tecnológico en estos últimos tiempos.







