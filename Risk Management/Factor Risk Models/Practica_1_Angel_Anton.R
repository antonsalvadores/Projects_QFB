getwd()
setwd("C:/MÁSTER QFB/2º CURSO/MEDICIÓN DE RIESGOS/LOLA ROBLES")


library(xts)
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(PerformanceAnalytics)
library(gt)


# Leemos la primera hoja del excel, en donde se encuentran las 
# cotizaciones.

data_quotes = read_xlsx("Data.xlsx", sheet=1, skip=1)

# Vemos que se han modificado los nombres de las columnas
# 57, 138, 259 y 602, probablemente sean datos erróneos.

cols <- c(57, 138, 259, 602)

head(data_quotes[, cols])

# En efecto lo son, así que las borraremos de la base de datos.

data_quotes <- data_quotes[, -cols]
head(data_quotes[, 1])

# Además, la primera columna son las fechas. Por tanto, 
# cambiaremos la estructura del data frame a una serie
# temporal para que cada cotización se asocie al día. 

colnames(data_quotes)[1] <- "Date"

# Renombramos el nombre de la columna de fechas a 'Date'
# para que se vea claramente que es la columna de fechas.

date <- data_quotes$Date

# Almacenamos en date todos los índices temporales.

data_quotes <- xts(x=data_quotes[,-1], order.by=date)
dim(data_quotes)

# Tenemos un data frame con 3655 días cotizados y 597 empresas.

# Leemos la segunda hoja del excel, en donde se encuentran las
# características de cada activo.

data_charact = read_xlsx("Data.xlsx", sheet=2)
colnames(data_charact)

# Vemos que en el excel hay varias columnas repetidas,
# como el nombre, el grupo de la industria y la beta.
# Eliminaremos estas columnas del data frame.

data_charact = data_charact[, -c(4, 6, 14, 18, 19)]
colnames(data_charact)[10] <- "Beta"

dim(data_charact)

# Como podemos ver, tenemos 600 empresas y 15 características
# asociadas a cada una de ellas, de las cuales debemos seleccionar
# 100 empresas. Esto es algo extraño, ya que en el data frame de
# cotizaciones solo tenemos 597 empresas, así que debe haber alguna 
# duplicada o algún fallo, veamos qué es lo que pasa.

# Para empezar, los nombres en data_quotes llevan al final (P),
# indicando precios, por lo que lo borraremos para que tengan el 
# mismo ticker ambos data frames.

colnames(data_quotes) <- gsub("(P)", "", colnames(data_quotes), fixed = TRUE)

# Ahora veamos qué tickers están en data_charact que no están
# en data_quotes.

setdiff(trimws(as.character(data_charact[[1]])), colnames(data_quotes))

# Vemos que las empresas con ticker LXI, BDEV e ICP no están en data_quotes
# y sí que están en data_charact. Probablemente formen parte de las borradas
# en primera instancia de data_quotes ya que eran un error. Las borraremos
# de data_charact también. Veamos a qué filas corresponden estos tickers.

which(trimws(as.character(data_charact[[1]])) %in% c("LXI", "BDEV", "ICP"))

# Son la fila 56, 137 y 258 las que debemos eliminar.

data_charact <- data_charact[-c(56, 137, 258),]


cols <- c(8, 10, 11, 12, 13)
data_charact[, cols] <- lapply(data_charact[, cols], as.numeric)

# Tenemos que cambiar la clase de nuestro data_charact
# de caracter a numérico para poder realizar operaciones
# sobre él. Las columnas de Market Value, Beta, Book to Value, 
# Market Cap y Market Price eran caracter y son números,
# por eso las cambiamos.



# Ahora tenemos ambos data frames bien estructurados para poder
# realizar los ejercicios. Para seleccionar las 100 empresas,
# primero observaremos si hay algunas empresas que no han cotizado
# durante algún día, para así no seleccionarlas y borrarlas de nuestra
# muestra de empresas, o si hay empresas en donde tengan NAs en alguna
# de sus características.

nas_quotes <- which(colSums(is.na(data_quotes)) != 0)
nas_charact <- which(rowSums(is.na(data_charact)) != 0)
assets_nas = union(nas_quotes, nas_charact)

# Aquí obtenemos los índices de los activos en los que hay NAs.
# Si una columna de data_quotes suma distinto de 0, es porque
# hay NAs, ya que al sumar los elementos de is.na(data_quotes)
# (+0 si FALSE (no hay NA), +1 si TRUE (hay NA)), si un activo 
# no tuviera NAs la suma debería dar 0.

data_quotes <- data_quotes[,-assets_nas]
data_charact <- data_charact[-assets_nas,]

# Eliminamos los activos con NAs de nuestra muestra.

dim(data_charact)
dim(data_quotes)

# Ahora solo nos quedan 490 activos, por lo que deberíamos seleccionar
# más o menos 1 de cada 5. Así, seleccionaremos activos de 5 en 5
# (activo 1, activo 6,...) hasta el 490, obteniendo así 98. Añadiremos
# el activo 2 y 3, por ejemplo, para así obtener 100 empresas.

selector <- seq(from=1, to=490, by=5)
length(selector)
selector <- c(selector, c(2,3))

selected_quotes <- data_quotes[, selector]
selected_charact <- data_charact[selector,]

# Ya tenemos nuestra muestra de 100 empresas.

unique(selected_charact$`COUNTRY OF DOMICIL`)

# En nuestra muestra, tenemos activos de Reino Unido, Suiza, Polonia
# Dinamarca, Noruega y Suecia, que cotizan en una divisa distinta
# al euro. Utilizaremos tipos de cambio actuales para actualizar 
# las cotizaciones de estas empresas a euros, ya que el tipo de cambio
# de toda la muestra frente al actual no ha fluctuado mucho
# y puede ser una buena aproximación, necesaria para poder comparar
# los activos. Principalmente necesario para los tipos de cambio
# en los que hay una diferencia mayor, como el esloti y las coronas
# danesa, noruega y sueca.

eur_rate <- c(
  "DK" = 7.47, "UK" = 0.87,"SW" = 0.92, "PO" = 4.23,
  "NW" = 11.6, "SD" = 10.9  
  )

conversion <- eur_rate[selected_charact$"COUNTRY OF DOMICIL"]
conversion[is.na(conversion)] <- 1

# Aquí creamos un vector donde se aplique el tipo de cambio 
# según el país, si el país seleccionado no sale en el vector
# eur_rate, será porque es un país que utiliza el euro. Usando
# esta función el vector creado devuelve un NA y para aquellos
# NAs los obligamos a ser 1 (el tipo de cambio EUR/EUR, obviamente).

selected_quotes_eur <- sweep(
  x = coredata(selected_quotes),
  MARGIN = 2,                   
  STATS = conversion,    
  FUN = "/"                     
)

selected_quotes_eur <- xts(
  apply(coredata(selected_quotes_eur), 2, as.numeric), 
  order.by = index(selected_quotes)
)


# Ahora ya tenemos la cotización de todas las empresas en euros.
# Hagamos un gráfico por ejemplo de los precios en euros
# de las empresas 3, 13, 23, ..., 93.

idx_plot <- seq(from = 3, to = 100, by = 10)

assets_plot <- selected_quotes[, idx_plot]

names <- colnames(assets_plot)

colors_plot <- rainbow(length(idx_plot))

plot(
  assets_plot, 
  main = "Cotizaciones de Empresas Seleccionadas",
  ylab = "Cotizaciones",
  xlab = "Fecha",
  col = colors_plot,
  screens = 1,
  lty = 1       
)
addLegend(
  "topright", 
  legend.names = names,
  col = colors_plot,
  lty = 1
)



selected_returns <- diff(log(selected_quotes_eur))

# Creamos nuestro objeto de rendimientos sobre el que calcularemos
# los ejercicios restantes. Evidentemente, la primera fila de 
# nuestra matriz va a estar llena de NAs, así que la borramos.

selected_returns <- selected_returns[-1,]

# Aquí tenemos la serie de rendimientos de nuestras 100 empresas 
# seleccionadas. Hagamos un gráfico por ejemplo de los rendimientos
# de las empresas 4, ..., 94 de nuevo.

assets_plot <- selected_returns[, idx_plot]

plot(
  assets_plot, 
  main = "Rendimientos de Empresas Seleccionadas",
  ylab = "Rendimiento",
  xlab = "Fecha",
  col = colors_plot,
  screens = 1,  # <-- ¡Importante! Fuerza un solo panel
  lty = 1       # <-- Asegura que todas sean líneas sólidas
)
addLegend(
  "topright", # Posición de la leyenda (puedes cambiarla)
  legend.names = names,
  col = colors_plot,
  lty = 1
)

# Una vez hechos unos gráficos de una muestra de nuestras 100
# empresas seleccionadas (obviamente no podríamos pintar en 
# un gráfico las 100 empresas), caractericemos las componentes 
# principales de nuestras empresas seleccionadas.

# Ejercicio 2


mean_returns <- colMeans(selected_returns)

returns_centered <- selected_returns - mean_returns

T <- dim(returns_centered)[1]

var_returns <- (t(returns_centered)%*%returns_centered) / (T-1)

# Calculamos la matriz de varianzas y covarianzas de los 
# rendimientos para poder calcular las principales componentes.

W <- eigen(var_returns)$vectors
colnames(W) <- colnames(returns_centered)
values <- eigen(var_returns)$values
Lambda <- diag(values)

all.equal(var_returns, W %*% Lambda %*% t(W), check.attributes=FALSE)

# Vemos que son prácticamente iguales así que está bien hecha la
# descomposición espectral de la matriz de varianzas y covarianzas.
# Ahora calcularemos los componentes principales de los rendimientos
# de los activos.

pc_returns <- returns_centered %*% W
pc_returns <- xts(pc_returns, order.by=index(returns_centered))
colnames(pc_returns) <- paste0("PC", 1:ncol(W))

# Ahora debemos ver qué componentes principales explican el 60-70% 
# de las fluctuaciones de los rendimientos (la varianza). Veamos
# que la varianza de cada PCi es el autovalor "i"-ésimo de la 
# matriz de varianzas y covarianzas de los rendimientos (centrados).

# En efecto, Cov(PC) = Cov(R_c * W) = (1 / (T-1)) * (R_c * W)' * (R_c * W).
# Ahora bien, Cov(PC) = W' * (1 / (T-1)) * R_c' * R_c * W, que esto es
# Cov(PC) = W' * Sigma * W, donde Sigma es la matriz de var-covar. Como
# Sigma = W * Lambda * W', se tiene que Cov(PC) = W' * W * Lambda * W' * W,
# como los autovectores son ortonormales (w * W' = W' * W = I), se tiene
# Cov(PC) = Lambda, como se quería demostrar.

# Por tanto, tenemos dos implicaciones de esta prueba. Como Lambda es una 
# matriz diagonal, se tiene que las principales componentes están 
# incorrelacionadas. Además, la varianza de cada PCi es el autovalor
# "i"-ésimo.

# Así, calcularemos el porcentaje de variabilidad de cada PCi como 
# el autovalor "i"-ésimo entre la suma de todos los autovalores.

pc_variability <- values/sum(values)
names(pc_variability) <- paste0("PC", 1:length(pc_variability))

which(cumsum(pc_variability)>=0.6)[1]
which(cumsum(pc_variability)>=0.7)[1]

# Como se puede ver, las 15 primeras principales componentes explican 
# el 60% de la varianza y las 25 primeras el 70%. Como se nos piden
# las componentes principales necesarios para capturar entre el 
# 60 y 70% de las fluctuaciones, tomaremos las K=20 primeras
# principales componentes.


# Para interpretar los tres primeros componentes principales en
# términos de las características de las empresas, nos damos
# cuenta de que las principales componentes son una combinación lineal
# de los rendimientos, donde cada "peso" para cada componente principal
# viene dado por la matriz W de los autovectores. Así, cada columna de
# W son las ponderaciones para cada empresa de esta combinación lineal
# para cada componente principal. Es decir, la primera columna de W
# son las ponderaciones para PC1, la segunda columna de W son las 
# ponderaciones para PC2... Por tanto, realizaremos una regresión 
# lineal de cada columna de W sobre las características deseadas
# de las empresas para ver qué peso tienen en cada componente
# principal las empresas de mayor tamaño o mayor beta, por ejemplo.
# A esto le añadiremos ciertos gráficos para ver cómo se comportan
# las componentes principales con respecto a estas características.


colnames(W) <- paste0("Pesos_PC", 1:length(pc_variability))

df_w_charact <- data.frame(W, selected_charact)

# Ahora tenemos en un data frame los pesos de cada empresa
# para cada PC unido a las características de la empresa.
# Haremos la regresión entre PC_1, PC_2 y PC_3 con respecto al país, 
# la beta, el book to value y el market cap (tamaño).

colnames(selected_charact)

model_pc1 <- lm(Pesos_PC1 ~ COUNTRY.OF.DOMICIL + MARKET.VAL.BY.CO. + 
                  Beta + BOOK.VALUE.PER.SHARE...CURRENT + 
                  MARKET.CAP..PUBLIC....CURRENT, data=df_w_charact)
summary(model_pc1)
model_pc2 <- lm(Pesos_PC2 ~ COUNTRY.OF.DOMICIL + MARKET.VAL.BY.CO. + 
                  Beta + BOOK.VALUE.PER.SHARE...CURRENT + 
                  MARKET.CAP..PUBLIC....CURRENT, data=df_w_charact)
summary(model_pc2)
model_pc3 <- lm(Pesos_PC3 ~ COUNTRY.OF.DOMICIL + MARKET.VAL.BY.CO. + 
                  Beta + BOOK.VALUE.PER.SHARE...CURRENT + 
                  MARKET.CAP..PUBLIC....CURRENT, data=df_w_charact)
summary(model_pc3)

# Al realizar las regresiones, parece que cada componente incluye
# un país en concreto del que está más formado. A mayores, las 
# dos primeras componentes tienen un coeficiente beta muy significativo
# y la tercera componente un coeficiente book to value. Realizaremos
# unos gráficos a ver cómo se componen las componentes principales
# con respecto a estas características.

ggplot(df_w_charact, aes(x = Beta, y = Pesos_PC1)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Beta y Peso en PC1",
    x = "Beta de la Empresa",
    y = "Pesos_PC1"
  ) +
  theme_minimal()

# Como podemos ver, parece claro que a mayor beta, mayor peso en la primera
# componente principal. Es importante destacar que estos pesos son en 
# realidad los elementos de un autovector de la matriz de varianzas y 
# covarianzas, por tanto, es solución del autovector su conjugado también.
# Es decir, la interpretación es la misma si todos los pesos son positivos
# como si todos los pesos son negativos, hemos de mirar el peso en valor
# absoluto. Como se ve en el gráfico, mayor beta pondera mayor peso en la 
# PC1, aunque no es una relación perfecta

ggplot(df_w_charact, aes(x = BOOK.VALUE.PER.SHARE...CURRENT, y = Pesos_PC1)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Book to Value y Peso en PC1",
    x = "Book to Value de la Empresa",
    y = "Pesos_PC1"
  ) +
  theme_minimal()

# En cuanto al book to value, no parece haber una relación muy clara, 
# en parte debido a que la mayoría de empresas tienen un Book to Value
# muy semejante. 

ggplot(df_w_charact, aes(x = MARKET.CAP..PUBLIC....CURRENT, y = Pesos_PC1)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Tamaño y Peso en PC1",
    x = "Tamaño de la Empresa",
    y = "Pesos_PC1"
  ) +
  theme_minimal()

# Lo mismo sucede para la relación entre el tamaño y los pesos en PC1.
# Parece haber un dato atípico que influye bastante en el ajuste y en
# la visualización del gráfico, lo quitaremos para ver mejor la relación.

atipico <- which(df_w_charact[, "MARKET.CAP..PUBLIC....CURRENT"]>1.0e+09)

ggplot(df_w_charact[-atipico, ], aes(x = MARKET.CAP..PUBLIC....CURRENT, 
                         y = Pesos_PC1)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Tamaño y Peso en PC1",
    x = "Tamaño de la Empresa",
    y = "Pesos_PC1"
  ) +
  theme_minimal()

# Sigue sin verse una relación clara, por eso no salió como coeficiente
# significativo. Al menos para los siguientes gráficos con tamaño ya sabemos
# de antemano que hemos de quitar ese dato para el gráfico.

ggplot(df_w_charact, aes(x = Beta, y = Pesos_PC2)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Beta y Peso en PC2",
    x = "Beta de la Empresa",
    y = "Pesos_PC2"
  ) +
  theme_minimal()



# Para la segunda componente principal, tenemos la relación inversa
# que para la primera, ya que la mayoría de componentes son positivos:
# a mayor beta, menor peso en PC2. 

ggplot(df_w_charact, aes(x = BOOK.VALUE.PER.SHARE...CURRENT, y = Pesos_PC2)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Book to Value y Peso en PC2",
    x = "Book to Value de la Empresa",
    y = "Pesos_PC2"
  ) +
  theme_minimal()

# No hay una relación clara como era de esperar, aunque sí que parece que 
# a mayor book to value menor peso en PC2 también, aunque no podemos
# sacar esta conclusión a la ligera.

ggplot(df_w_charact[-atipico, ], aes(x = MARKET.CAP..PUBLIC....CURRENT, 
                                     y = Pesos_PC2)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Tamaño y Peso en PC2",
    x = "Tamaño de la Empresa",
    y = "Pesos_PC2"
  ) +
  theme_minimal()

# Tampoco hay una relación entre los pesos en PC2 y el tamaño clara.

ggplot(df_w_charact, aes(x = Beta, y = Pesos_PC3)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Beta y Peso en PC3",
    x = "Beta de la Empresa",
    y = "Pesos_PC3"
  ) +
  theme_minimal()

# No hay una relación evidente entre la beta y el peso en PC3, aunque
# como sucedía en casos anteriores sí que parece que desciende un poco
# el peso en PC3 a medida que aumenta la beta, como sucedía para PC2.

ggplot(df_w_charact, aes(x = BOOK.VALUE.PER.SHARE...CURRENT, y = Pesos_PC3)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Book to Value y Peso en PC3",
    x = "Book to Value de la Empresa",
    y = "Pesos_PC3"
  ) +
  theme_minimal()

# Lo mismo sucede para el book to value, el cual salía significativo.
# Probablemente se viese significativo por ese dato atípico con mucho
# peso en PC3 y poco book to value,  lo que hace que descienda la
# recta de regresión, pero como vemos en el gráfico no parece haber ninguna
# tendencia lineal aparente.

ggplot(df_w_charact[-atipico, ], aes(x = MARKET.CAP..PUBLIC....CURRENT, 
                                     y = Pesos_PC3)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", col = "blue") +  
  labs(
    title = "Relación entre Tamaño y Peso en PC3",
    x = "Tamaño de la Empresa",
    y = "Pesos_PC3"
  ) +
  theme_minimal()

# De nuevo, lo mismo sucede con el tamaño, no se puede sacar ninguna 
# conclusión clara de lo que se ve en el gráfico.

# En resumen, parece que empresas con mayores betas tienen más peso
# en PC1 y empresas con menores betas tienen mayor peso en PC2, pero
# no podemos extraer ninguna conclusión a mayores de qué
# características componen cada componente principal.


# Ejercicio 3

length(which(W[,1]>0)); length(which(W[,2]>0)); length(which(W[,3]>0))

# Como vemos, nuestra cartera PC1 sería todo en corto y nuestras carteras
# PC2 y PC3 sería una combinación de largo y corto. Como hemos 
# comentado antes, si multiplicamos por -1 cualquier columna, sigue siendo
# autovector de W, i.e, sigue siendo solución. Por tanto, si una cartera
# utilizando los pesos dado por W nos da un resultado negativo, simplemente
# utilizaríamos la cartera inversa (PC1 todo largo, por ejemplo) para ver
# el rendimiento real de cada componente principal. Cada software
# calcula de una manera la matriz de autovectores y parece que R
# entrega el conjugado negativo, algo que es irrelevante a la hora
# de los valores (pesos) que tengamos en nuestra cartera de cada 
# empresa, pero sí es relevante en cuanto a la decisión que tomar
# (largo o corto). Además, las columnas de W no suman 1, por lo que
# tenemos un apalancamiento arbitrario para cada activo, ya que son
# por definición de autovector (ortonormales) los cuadrados de estos
# los que suman 1. Debemos normalizar los pesos de W para poder crear
# una cartera real. Por ejemplo para PC1, donde todos los pesos son
# negativos, si quisiésemos crear una cartera con PC1, la suma
# de todos los pesos sería -1. Sin embargo,
sum(W[,1])
# la suma da -9.548708. 

W_sum <- apply(abs(W), 2, sum)
W_norm <- sweep(W, MARGIN = 2, STATS = W_sum, FUN = "/")
sum(W_norm)

# Ahora ya tenemos los pesos normalizados, (la suma de PC1 da -1)
# así que calcularemos los rendimientos pc multiplicándolos por estos.
# Además, pc_returns lo calculábamos usando los rendimientos centrados,
# ahora hemos de multiplicarlo por los rendimientos reales (selected_returns).

pc_returns_norm <- selected_returns %*% W_norm
pc_returns_norm <- xts(pc_returns_norm, order.by=index(selected_returns))

daily_growth <- pc_returns_norm + 1
cumulative_growth <- apply(daily_growth, 2, cumprod)

# Para calcular la rentabilidad final de cada cartera,
# hemos de sumar uno a los rendimientos diarios y luego realizar
# la función cumprod donde va multiplicando cada día por el 
# rendimiento del día anterior. Así, el último día tendremos 
# la rentabilidad total de cada cartera.

cumulative_growth[T,1]; cumulative_growth[T,2]; cumulative_growth[T,3]

# Como habíamos supuesto, estos resultados son tremenadamente
# bajos, así que lo mejor sería realizar las operaciones inversas
# en nuestros PC, i.e, todo largo para PC1, más largo que corto en PC2
# y cambiar los corto y largos en PC3.

pc_returns_norm <- -1 * pc_returns_norm
daily_growth <- pc_returns_norm + 1
cumulative_growth <- apply(daily_growth, 2, cumprod)
cumulative_growth[T,1]; cumulative_growth[T,2]; cumulative_growth[T,3]


# El resultado, evidentemente, es mucho mejor, ya que utilizando
# la estrategia corto en las tres carteras perdíamos prácticamente
# la totalidad de nuestro dinero y en la estrategia largo ganamos
# en todas. El rendimiento total de la cartera PC1 es de un 216% en 
# 13 años, lo cual está bastante bien. Mejor todavía es el rendimiento
# de la cartera PC2, con un 345% y el peor el de la cartera PC3 con
# un rendimiento del 20% en total, bastante malo teniendo en cuenta
# el intervalo temporal que estamos utilizando.

# Calculemos ahora medidas de rentabilidad y riesgo.


mean_daily_returns <- colMeans(pc_returns_norm)
mean_anual_returns <- (1 + mean_daily_returns)^252 - 1
mean_anual_returns[1]; mean_anual_returns[2]; mean_anual_returns[3]

# La media anualizada de rentabilidad es del 7.110568% para la cartera PC1,
# del 9.569891% para la cartera PC2 y del 1.870839% para la cartera PC3.
# Como habíamos comentado viendo el rendimiento total de la cartera, son
# buenas rentabilidades anuales para las dos primeras y muy mala para la
# última. Para anualizar realizamos el cálculo por 252, que son el número
# de días cotizados en un año.


sd_daily <- apply(pc_returns_norm, 2, sd)
sd_anual <- sd_daily * sqrt(252)
sd_anual[1]; sd_anual[2]; sd_anual[3]

# La volatilidad anualizada de PC1 es del 17.53709%, del 10.81964% para
# PC2 y del 9.951791%. Por tanto, es evidente que la mejor cartera en
# términos de rentabilidad riesgo es la segunda cartera, ya que es
# la menos volátil prácticamente y la que mayor rentabilidad aporta.
# Calculemos sus ratios de Sharpe, tomando como tipo libre de riesgo
# el 0%, como indica en el siguiente ejercicio.

sharpe <- mean_anual_returns / sd_anual
beta_carteras <- -1 * t(W_norm) %*% df_w_charact[, "Beta"] 
treynor <- mean_anual_returns / beta_carteras

# Aquí las betas de las carteras, como se combinan activos a largo
# y a corto, salvo para la primera cartera (todo largo) y la segunda
# más largo que corto, el resto son muy cercanas a 0. Como no tenemos
# los datos de cotización de un mercado de referencia, aproximamos las
# betas de las carteras como la ponderación de las betas de los activos.

mdd <- maxDrawdown(pc_returns_norm)

# En el ejercicio 4 se solicita un método adicional para comparar
# las carteras que presentamos aquí: el maximum drawdown. Es la 
# mayor pérdida de la cartera porcentual desde su punto más alto
# hasta su punto más bajo. Mide la mayor pérdida porcentual
# que soportaría un inversor en el período de estudio, midiendo
# así la resistencia de este a aguantar caídas.

stats <- data.frame(
  Factor = paste0("PC", 1:3),
  Mean_anual = mean_anual_returns[1:3],
  sd_anual   = sd_anual[1:3],
  Mean_daily = mean_daily_returns[1:3],
  sd_daily   = sd_daily[1:3],
  Sharpe_ratio = sharpe[1:3],
  Treynor_ratio = treynor[1:3],
  Maximum_Drawdown = mdd[1:3],
  row.names = NULL
)
stats

# Como era de esperar, según el ratio de Sharpe, la mejor cartera es
# la segunda, tras la primera y la última la de PC3 (a pesar de ser
# la que menor riesgo (volatilidad) tiene). En cuanto al ratio de 
# Treynor, a mayor ratio mejor, por lo que en principio la mejor
# cartera sería la PC3 (incongruente), pero debemos tener en cuenta
# que el ratio de Treynor divide por la beta y en la mayoría de 
# nuestras carteras tienen beta cercana a 0, así que es por esto por
# lo que tiene mayor ratio. Comparando la primera con la segunda, que
# tienen una beta ("normal"), la segunda cartera también es mejor.
# en cuanto al maximum drawdown, la peor pérdida que soportaría un
# inversor si aguantase estas carteras durante toda la serie temporal
# desde 2011 hasta 2025, sería del 42.50936% en el caso de la
# primera cartera, del 37.91225% en caso del PC2 y del 50.28383%
# en la última cartera.

gt_stats <- gt(stats)
print(gt_stats)
gtsave(gt_stats, "Ej_3.png")

# Ejercicio 4

# Creación de cartera equiponderada:

W_equal <- rep(1/100, 100)
returns_equal <- selected_returns %*% W_equal
returns_equal <- xts(returns_equal, order.by=index(selected_returns))

# Las dos carteras de nuestra elección serán una cartera equiponderada
# de activos del Reino Unido y una cartera equiponderada
# de valores cuyas betas sean menores a 0.8 y mayores que 1.2, para
# ver qué tal rinden las empresas británicas y construir una cartera
# de valores muy poco o muy correlacionados con el mercado. Todas
# las posiciones a tomar son a largo.

uk_assets <- which(selected_charact$`COUNTRY OF DOMICIL`=="UK")
W_uk <- rep(0, 100)
W_uk[uk_assets] <- 1 / length(uk_assets)
beta_assets <- which((selected_charact$`Beta` > 1.2) | 
                       (selected_charact$`Beta` < 0.8))
W_betas <- rep(0, 100)
W_betas[beta_assets] <- 1 / length(beta_assets) 
returns_uk <- selected_returns %*% W_uk
returns_uk <- xts(returns_uk, order.by=index(selected_returns))
returns_betas <- selected_returns %*% W_betas
returns_betas <- xts(returns_betas, order.by=index(selected_returns))

mean_daily_equal <- colMeans(returns_equal)
mean_daily_uk <- colMeans(returns_uk)
mean_daily_betas <- colMeans(returns_betas)
mean_anual_equal <- (1 + mean_daily_equal)^252 - 1
mean_anual_uk <- (1 + mean_daily_uk)^252 - 1
mean_anual_betas <- (1 + mean_daily_betas)^252 - 1

mean_anual_equal; mean_anual_uk; mean_anual_betas

# La rentabilidad anualizada de nuestras nuevas carteras arroja unos
# datos muy reveladores: la rentabilidad de la cartera equiponderada
# es del 7.464095%, un rendimiento anual bastante elevado. La cartera
# hecha con empresas del Reino Unido en exclusiva tiene un rendimiento
# bastante malo, del 4.603961% y la cartera con betas altas y bajas
# tiene un buen rendimiento del 7.778983% anual.



sd_daily_equal <- apply(returns_equal, 2, sd)
sd_anual_equal <- sd_daily_equal * sqrt(252)
sd_daily_uk <- apply(returns_uk, 2, sd)
sd_anual_uk <- sd_daily_uk * sqrt(252)
sd_daily_betas <- apply(returns_betas, 2, sd)
sd_anual_betas <- sd_daily_betas * sqrt(252)

sd_anual_equal; sd_anual_uk; sd_anual_betas

# Todas las carteras tienen una desviación típica anualizada
# muy semejante, en torno al 16% las tres, siendo la más baja 
# la cartera equiponderada, 16.05799%, y la más alta la cartera
# británica, 16.72094%. La cartera de betas altas y bajas
# tiene una desviación típica de 16.61297%.

sharpe_equal <- mean_anual_equal / sd_anual_equal
sharpe_uk <- mean_anual_uk / sd_anual_uk
sharpe_betas <- mean_anual_betas / sd_anual_betas
beta_equal <- t(W_equal) %*% df_w_charact[, "Beta"]
beta_uk <- t(W_uk) %*% df_w_charact[, "Beta"]
beta_betas <- t(W_betas) %*% df_w_charact[, "Beta"]
treynor_equal <- mean_anual_equal / beta_equal
treynor_uk <- mean_anual_uk / beta_uk
treynor_betas <- mean_anual_betas / beta_betas
mdd_equal <- maxDrawdown(returns_equal)
mdd_uk <- maxDrawdown(returns_uk)
mdd_betas <- maxDrawdown(returns_betas)

stats <- data.frame(
  Portfolio = c(paste0("PC", 1:3), "Equal", "UK", "Betas"),
  Mean_anual = c(mean_anual_returns[1:3], mean_anual_equal,
                 mean_anual_uk, mean_anual_betas), 
  sd_anual   = c(sd_anual[1:3], sd_anual_equal,
                 sd_anual_uk, sd_anual_betas),
  Sharpe_ratio = c(sharpe[1:3], sharpe_equal, sharpe_uk, sharpe_betas),
  Treynor_ratio = c(treynor[1:3], treynor_equal, treynor_uk, treynor_betas),
  Maximum_Drawdown = c(mdd[1:3], mdd_equal, mdd_uk, mdd_betas),
  row.names = NULL
)
stats

gt_stats <- gt(stats)
print(gt_stats)
gtsave(gt_stats, "Ej_4.png")

# Con respecto a las tres nuevas carteras, la cartera de betas altas y bajas
# se comporta de manera prácticamente igual en todas las métricas que la
# cartera equiponderada de los 100 activos y la cartera de activos
# del Reino Unido es bastante peor que estas. Según todas las métricas,
# el orden de carteras preferidas parece bastante obvio, tanto por
# rentabilidad como por riesgo. La cartera con mejores resultados y
# menor riesgo es la de PC2, clara vencedora. La siguen estas dos
# nuevas carteras, equiponderada y de betas altas y bajas, con
# una rentabilidad 2 puntos porcentuales peor y más riesgo.
# A continuación estaría la cartera PC1, algo lógico, ya que es
# la componente principal que se relacionaba con las betas (el mercado),
# teniendo resultados un poco peores en cuanto a rentabilidad y riesgo.
# Por último, las peores carteras son la del Reino Unido y la de
# PC3, con unos niveles de rentabilidad muy por debajo de la cartera
# equiponderada y mayor riesgo. Los ratios de Sharpe y Treynor nos
# vienen a decir prácticamente lo mismo que la exposición anterior.
# En cuanto a la peor caída soportada de estas nuevas empresas,
# el orden es el mismo que el de las carteras, siendo la máxima
# caída para la cartera equiponderada del 39.06642% y de la cartera
# de betas el 39.99887%. En cuanto a la cartera británica, tenemos
# una caída máxima del 43.86307%.


# Ejercicio 5 y 6

# Planteamiento del modelo:

# Utilizamos los rendimientos de nuestras carteras PC normalizadas,
# las empleadas en el ejercicio 3.


# Vamos a plantear el modelo factorial y luego lo estimaremos
# para ver qué variabilidad de las carteras explican las
# componentes principales.

# El modelo es: 
# R_{p,t} = \alpha_p + \sum_{j=1}^3 \beta_{p,j} * PC_{p,t} + \epsilon_{p,t}

# Debemos suponer para poder calcular el riesgo sistemático e 
# idiosincrático de cada cartera (realizar la descomposición
# de la varianza) que los factores (las tres primeras componentes
# principales) son estacionarios en media y varianza. Así,
# suponiendo que el término de error es un ruido blanco,
# tenemos que el riesgo total es la varianza total de la cartera,
# el riesgo idiosincrático es la varianza del término de error
# (estacionario en varianza por ser ruido blanco iid) y el riesgo
# sistemático es el riesgo explicado por los factores, siendo
# la varianza de estos igual a la forma cuadrática siguiente:
# (\beta_1 \beta_2 \beta_3) %*% \Omega %*% (\beta_1 \beta_2 \beta_3)'
# donde \Omega es la matriz de varianzas y covarianzas de los factores.

# Por tanto, nosotros en R debemos estimar tres modelos de regresión,
# uno para cada cartera, realizar la descomposición de la varianza
# y así poder identificar todos los riesgos.

df_equal_PC <- data.frame(returns_equal, pc_returns_norm)
model_equal_PC <- lm(returns_equal ~ Pesos_PC1 + Pesos_PC2 + Pesos_PC3,
                  data=df_equal_PC)

df_uk_PC <- data.frame(returns_uk, pc_returns_norm)
model_uk_PC <- lm(returns_uk ~ Pesos_PC1 + Pesos_PC2 + Pesos_PC3,
                  data=df_uk_PC)

df_betas_PC <- data.frame(returns_betas, pc_returns_norm)
model_betas_PC <- lm(returns_betas ~ Pesos_PC1 + Pesos_PC2 + Pesos_PC3,
               data=df_betas_PC)

# Aquí guardamos los tres modelos especificados.

betas_equal_PC <- coef(model_equal_PC)
betas_uk_PC <- coef(model_uk_PC)
betas_betas_PC <- coef(model_betas_PC)

# Aquí guardamos los coeficientes (alpha y betas) de los modelos.

var_returns_equal <- (t(returns_equal)%*%returns_equal) / (T-1)
var_returns_uk <- (t(returns_uk)%*%returns_uk) / (T-1)
var_returns_betas <- (t(returns_betas)%*%returns_betas) / (T-1)

# Aquí guardamos la varianza total de los rendimientos de cada modelo.
# Este es el riesgo total de la cartera.

var_pc <- (t(pc_returns_norm[,1:3]) %*% pc_returns_norm[,1:3]) / (T-1)

# Aquí guardamos la matriz de varianzas y covarianzas de los factores
# del modelo (las tres primeras componentes principales).

risk_sist_equal_PC <- betas_equal_PC[2:4] %*% var_pc %*% betas_equal_PC[2:4]
risk_sist_uk_PC <- betas_uk_PC[2:4] %*% var_pc %*% betas_uk_PC[2:4]
risk_sist_betas_PC <- betas_betas_PC[2:4] %*% var_pc %*% betas_betas_PC[2:4]

# Este es el riesgo sistemático de cada cartera.

risk_idiosin_equal_PC <- var_returns_equal - risk_sist_equal_PC
risk_idiosin_uk_PC <- var_returns_uk - risk_sist_uk_PC
risk_idiosin_betas_PC <- var_returns_betas - risk_sist_betas_PC

# Este es el riesgo idiosincrásico de cada cartera.

risk_idiosin_equal_PC / var_returns_equal
risk_idiosin_uk_PC / var_returns_uk
risk_idiosin_betas_PC / var_returns_betas

# Aquí calculamos qué porcentaje del riesgo idiosincrásico corresponde
# a la varianza total de la cartera, algo más explicativo que los 
# valores absolutos. Como se puede comprobar, el riesgo idiosincrático
# de la cartera del Reino Unido es del 14.44158%, lo que quiere decir
# que el modelo explica aproximadamente el 86% de la variabilidad.
# Sin embargo, los resultados para las carteras de betas altas y bajas
# y para la cartera equiponderada los resultados son impresionantes:
# un porcentaje del riesgo idiosincrático del 0.8767318% y 0.3945495%,
# respectivamente. Esto implica que el modelo explica más del
# 99% de la variabilidad de la cartera!! Por lo tanto, las componentes
# principales son un buen modelo para explicar los rendimientos de estas.
# Cabe destacar, que por la construcción de las carteras y de las 
# componentes principales, esto es algo lógico, probablemente
# construyendo carteras diferentes (como la del Reino Unido) saldrían
# resultados diferentes, donde el modelo no explica un porcentaje 
# tan alto de la variabilidad de los modelos.

# Ejercicio 7, 8, 9 y 10:

# Nos piden contruir un modelo BARRA a partir de los rasgos de las empresas
# Procedemos a examinar las características más destacables de nuestra
# selección de 100 empresas.

# Idea: 

# 1) Considerar variables Dummy de pertenencia a un país. Solo considerar
# los países que se repitan más: BD (12), DK(7), FR(16), SD (8), SW (10), UK (27)
# + otros (hay 30). 

# 2) Considerar Dummies por Industry Group. Hay muchos sectores (muchos de ellos
# contienen una sola empresa) pero hay 3 sectores que destacan. Se puede hacer
# una dummy para BANKS (9), PHRMC (5), TELSV (4), que son los que más se repiten.
# Por si captan algún efecto en concreto.

# 3) Considerar Dummy de Beta Alta, Media o Baja. Se pueden hacer 3 categorías
# según los cuantiles de la distribución de betas. Se ordenan los valores de
# la muestra. Se sacan terciles y se hacen 3 categorías diferentes.


# Creamos los conjuntos de países y sectores más repetidos.
paises_top   <- c("BD","DK","FR","SD","SW","UK")
sectores_top <- c("BANKS","PHRMC","TELSV")
terciles <- quantile(selected_charact$Beta, probs=c(0, 1/3, 2/3, 1))

# Tenemos que crear algunas nuevas categorías para definir las variables
# dummy que usaremos en el modelo BARRA. Lo que haremos será crear
# vectores que tomen el valor 1 si la empresa pertenece a esa categoría
# y 0 si no, para añadir estos vectores luego al data frame
# y tener así nuestras variables dummy.

bd <- rep(0,100)
bd[which(selected_charact["COUNTRY OF DOMICIL"] == "BD")] <- 1
dk <- rep(0,100)
dk[which(selected_charact["COUNTRY OF DOMICIL"] == "DK")] <- 1
fr <- rep(0,100)
fr[which(selected_charact["COUNTRY OF DOMICIL"] == "FR")] <- 1
sd <- rep(0,100)
sd[which(selected_charact["COUNTRY OF DOMICIL"] == "SD")] <- 1
sw <- rep(0,100)
sw[which(selected_charact["COUNTRY OF DOMICIL"] == "SW")] <- 1
uk <- rep(0,100)
uk[which(selected_charact["COUNTRY OF DOMICIL"] == "UK")] <- 1
other_ctry <- !(selected_charact[["COUNTRY OF DOMICIL"]] %in% paises_top)
other_ctry <- as.numeric(other_ctry)

# Variables dummy para países

banks <- rep(0,100)
banks[which(selected_charact["IND. GROUP MNEM"] == "BANKS")] <- 1
phrmc <- rep(0,100)
phrmc[which(selected_charact["IND. GROUP MNEM"] == "PHRMC")] <- 1
telsv <- rep(0,100)
telsv[which(selected_charact["IND. GROUP MNEM"] == "TELSV")] <- 1
other_ind <- !(selected_charact[["IND. GROUP MNEM"]] %in% sectores_top)
other_ind <- as.numeric(other_ind)

# Variables dummy para industria

baja <- ifelse(selected_charact$Beta <= terciles[2], 1, 0)
media <- ifelse(selected_charact$Beta > terciles[2] & 
                selected_charact$Beta <= terciles[3], 1, 0)
alta <- ifelse(selected_charact$Beta > terciles[3] &
                 selected_charact$Beta <= terciles[4], 1, 0)

# Variables dummy para betas

# Ahora crearemos un dataframe con cada empresa y sus dummys de pertenencia
# o no a cada categoría.

df_barra_dummy <- data.frame(bd, dk, fr, sd, sw, uk, other_ctry,
                             banks, phrmc, telsv, other_ind, 
                             baja, media, alta)

# Una vez hecho esto, hemos de estimar T modelos, uno para cada día, 
# de los factores asociados a cada categoría para todos los activos.
# Para cada t = 1, ..., T, se tiene el siguiente modelo:

# R_{i,t} = \Beta{i, 1} * f_{1, t} + \Beta{i, 2} * f_{2, t} +
#           \Beta{i, 3} * f_{3, t} + \Beta{i, 4} * f_{4, t} + ... 
#           \epsilon_{i, t}, i = 1, ..., 100

# Donde cada \Beta{i, k} es la variable dummy para el activo i
# que toma el valor 1 si el activo i pertenece a la categoría k
# (por ejemplo, si la categoría k es empresa de UK para el activo i
# esa beta toma valor 1 si es de UK y 0 si no lo es) y cada f_{k, t}
# es el factor no observable a la pertenencia de cada sector que
# debemos estimar.

# Para estimar el modelo, empleando la fórmula dada en los apuntes
# \hat{f}_t = (t(df_barra_dummy) %*% df_barra_dummy)^(-1) %*%
#             t(df_barra_dummy) %*% selected_returns[t,]
# Se estiman los K factores de las categorías de la matriz
# de dummys para cada día. Podemos realizar esta operación
# matricialmente en R y obtener las estimaciones de los K factores
# para cada t:

transpose_returns <- t(selected_returns)

# Necesitamos que la matriz de rendimientos sea la traspuesta para 
# estimar los factores, dada la ecuación previa.

B <- as.matrix(df_barra_dummy)

# No se puede resolver ya que hemos añadido las columnas other_ctry,
# other_ind y alta que se pueden obtener dadas las demás categorías
# de cada sector, así que las eliminaremos.

B <- B[, -c(7, 11, 14)]
BtB_inv <- solve( t(B) %*% B )

# Ahora sí que es una matriz invertible.

B_op <- BtB_inv %*% t(B)
factor_returns <- B_op %*% transpose_returns

# Aquí tenemos las estimaciones de nuestros factores para todos los días.

factor_returns <- t(factor_returns)
factor_returns <- xts(factor_returns, 
                      order.by = index(selected_returns))

# Ya tenemos nuestro modelo BARRA estimado. Ahora calcularemos
# el riesgo explicado e idiosincrásico de cada cartera por este
# modelo. El riesgo total de cada cartera ya lo hemos calculado
# previamente, en ejercicios anteriores.


df_barra <- data.frame(pc_returns_norm[, 1:3], returns_equal,
                       returns_uk, returns_betas, factor_returns)

# Para hacer la descomposición de la varianza, realizaremos
# 6 regresiones lineales, una para cada cartera, donde
# la cartera será la variable endógena y los factores estimados
# del modelo BARRA las variables explicativas. El riesgo total
# cada cartera ya lo hemos calculado.

var_covar_factors <- cov(factor_returns)

# Esta es la matriz de varianzas y covarianzas de los factores del modelo
# BARRA.

model_PC1 <- lm(Pesos_PC1 ~ bd + dk + fr + sd + sw + uk + banks +
                phrmc + telsv + baja + media , data=df_barra)
model_PC2 <- lm(Pesos_PC2 ~ bd + dk + fr + sd + sw + uk + banks +
                  phrmc + telsv + baja + media , data=df_barra)
model_PC3 <- lm(Pesos_PC3 ~ bd + dk + fr + sd + sw + uk + banks +
                  phrmc + telsv + baja + media , data=df_barra)
model_equal <- lm(returns_equal ~ bd + dk + fr + sd + sw + uk + banks +
                  phrmc + telsv + baja + media , data=df_barra)
model_uk <- lm(returns_uk ~ bd + dk + fr + sd + sw + uk + banks +
                  phrmc + telsv + baja + media , data=df_barra)
model_betas <- lm(returns_betas ~ bd + dk + fr + sd + sw + uk + banks +
                  phrmc + telsv + baja + media , data=df_barra)


# Aquí guardamos los seis modelos especificados.

betas_PC1 <- coef(model_PC1)
betas_PC2 <- coef(model_PC2)
betas_PC3 <- coef(model_PC3)
betas_equal <- coef(model_equal)
betas_uk <- coef(model_uk)
betas_betas <- coef(model_betas)

# Aquí guardamos los coeficientes (alpha y betas) de los modelos.

var_pc1 <- (t(pc_returns_norm[,1]) %*% pc_returns_norm[,1]) / (T-1)
var_pc2 <- (t(pc_returns_norm[,2]) %*% pc_returns_norm[,2]) / (T-1)
var_pc3 <- (t(pc_returns_norm[,3]) %*% pc_returns_norm[,3]) / (T-1)

# Aquí guardamos la matriz de varianzas y covarianzas de las carteras de
# las tres primeras componentes principales.

risk_sist_PC1 <- betas_PC1[2:12] %*% var_covar_factors %*% betas_PC1[2:12]
risk_sist_PC2 <- betas_PC2[2:12] %*% var_covar_factors %*% betas_PC2[2:12]
risk_sist_PC3 <- betas_PC3[2:12] %*% var_covar_factors %*% betas_PC3[2:12]
risk_sist_equal <- betas_equal[2:12] %*% var_covar_factors %*% betas_equal[2:12]
risk_sist_uk <- betas_uk[2:12] %*% var_covar_factors %*% betas_uk[2:12]
risk_sist_betas <- betas_betas[2:12] %*% var_covar_factors %*% betas_betas[2:12]

# Este es el riesgo sistemático de cada cartera.

risk_idiosin_PC1 <- var_pc1 - risk_sist_PC1
risk_idiosin_PC2 <- var_pc2 - risk_sist_PC2
risk_idiosin_PC3 <- var_pc3 - risk_sist_PC3
risk_idiosin_equal <- var_returns_equal - risk_sist_equal
risk_idiosin_uk <- var_returns_uk - risk_sist_uk
risk_idiosin_betas <- var_returns_betas - risk_sist_betas

# Este es el riesgo idiosincrásico de cada cartera.

risk_idiosin_PC1 / var_pc1
risk_idiosin_PC2 / var_pc2
risk_idiosin_PC3 / var_pc3
risk_idiosin_equal / var_returns_equal
risk_idiosin_uk / var_returns_uk
risk_idiosin_betas / var_returns_betas

# Aquí calculamos qué porcentaje del riesgo idiosincrásico corresponde
# a la varianza total de la cartera, algo más explicativo que los 
# valores absolutos. Como podemos observar, el porcentaje del riesgo
# idiosincrático no explicado por el modelo para las carteras
# PC2 y PC3 es muy alto, del 38.38099% y 40.25839% respectivamente. 
# La siguen la cartera de betas altas y bajas, donde el modelo ya 
# funciona bien, explicando el 1.020344% y la siguen la cartera de
# PC1, con un 0.5355931% del riesgo no explicado, la cartera de
# activos equiponderados, con un porcentaje no explicado del
# 0.375949% y por último, la cartera de acciones del Reino Unido,
# lógicamente, ya que una categoría era ser empresa del Reino Unido o no,
# es la que el modelo explica mejor, donde solo un 0.02876015% del
# riesgo no está explicado por el modelo, es decir, un porcentaje
# ínfimo. También es lógico, siendo una categoría beta baja, media
# o alta, que nuestras carteras formadas por betas altas y bajas,
# la equiponderada o la PC1 (principalmente explicada por betas
# como ya hemos visto) el modelo explique tan bien la varianza
# de estas carteras.

# En comparación con el modelo PCA, este modelo parece mejor
# que nuestro modelo BARRA; ya que aunque para las carteras
# del Reino Unido y la de activos equiponderados son mejor
# explicadas por este modelo, la diferencia es muy pequeña
# para carteras que se espera estén muy bien explicadas por
# este modelo pero otras carteras, como la PC2 o la PC3,
# están demasiado mal explicadas. Evidentemente las carteras
# formadas por componentes principales, la varianza de la cartera está
# explicada al 100% por el modelo de componentes principales.


stats <- data.frame(
  Portfolio = c(paste0("PC", 1:3), "Equal", "UK", "Betas"),
  Mean_anual = c(mean_anual_returns[1:3], mean_anual_equal,
                 mean_anual_uk, mean_anual_betas), 
  sd_anual   = c(sd_anual[1:3], sd_anual_equal,
                 sd_anual_uk, sd_anual_betas),
  risk_not_explained_ratio_PC_model = c( 0, 0, 0, 
                risk_idiosin_equal_PC / var_returns_equal, 
               1 -  risk_sist_uk_PC / var_returns_uk, 
                risk_idiosin_betas_PC / var_returns_betas 
  ),
  risk_not_explained_ratio_BARRA_model = c( 
                    risk_idiosin_PC1 / var_pc1,
                    risk_idiosin_PC2 / var_pc2,
                    risk_idiosin_PC3 / var_pc3, 
                    risk_idiosin_equal / var_returns_equal, 
                    risk_idiosin_uk / var_returns_uk, 
                    risk_idiosin_betas / var_returns_betas 
  ),
  Sharpe_ratio = c(sharpe[1:3], sharpe_equal, sharpe_uk, sharpe_betas),
  Treynor_ratio = c(treynor[1:3], treynor_equal, treynor_uk, treynor_betas),
  Maximum_Drawdown = c(mdd[1:3], mdd_equal, mdd_uk, mdd_betas),
  row.names = NULL
)
stats

gt_stats <- gt(stats)
print(gt_stats)
gtsave(gt_stats, "Ej_10.png")

