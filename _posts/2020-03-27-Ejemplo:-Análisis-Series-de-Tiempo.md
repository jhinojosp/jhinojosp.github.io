---
layout: post
title: 'Ejemplo: Analisis Serie de Tiempo'
date: 2020-03-27
category: rblogging
---

### Preparación de datos

``` r
library(readxl)

datospib <- read_excel("PIB_TRABAJO_EXAMEN_1_BIE_BIE20200318205927.xls", range = "A10:B170")
datospib.ajuste <- datospib[69:152, ] #Datos de 1997/01 a 2017/04

pib <- ts(datospib.ajuste[,2], start = c(1997,1), frequency = 4) #Objeto de serie de tiempo
```

### Analisis Exploratorio

Gráfica serie de tiempo *pib* del 1997/01 al 2017/04

``` r
plot(pib)
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />
Observamos una clara tendencia deterministica y posible raiz unitaria
aparente y un cambio estructural por la crisis del 2008. Hay sospechs de
estacionaleidad.

Analisamos la serie logarítmica

``` r
logpib <- log(pib)
plot(logpib)
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />
Es muy parecida a la serie original con las mismas teorías sobre la
tendencia, raiz unitaria y estacionaleidad, pero optamos por trabajar
con la serie logarítmica por las propiedade aditivas del logaritmo.

``` r
acf(logpib, lag.max = 40)
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
pacf(logpib, lag.max = 40)
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-4-2.png" style="display: block; margin: auto;" />
Confirmamos la teoría de raiz unitaria.

Descomposición de la serie logarítmica:

``` r
plot(decompose(logpib))
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
plot(decompose(pib))
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-5-2.png" style="display: block; margin: auto;" />
Observamos que el comportamiento estacional es muy fuerte y la tendencia
es muy clara.

Diferencia de orden 1 a la serie *logpib*

``` r
d.logpib <- diff(logpib)
plot(decompose(d.logpib))
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

Nos deshicimos de la tendencia pero aún tenemos comportamientos
estacionales.

#### DUDA: Tenemos que hacer la diferenciación estacional antes de hacer pruebas de raiz unitaria o es indiferente?

### Pruebas de Raiz Unitaria Dickey-Fuller

#### Hipótesis (*logpib*):

Al menos una raíz unitaria

``` r
library(urca)
summary(ur.df(logpib, type = "trend", lags = 4)) #Comenzamos con 4 lags porque tenemos datos cuatrimestrales
```

############################################### 

Augmented Dickey-Fuller Test Unit Root Test
===========================================

############################################### 

Test regression trend

Call: lm(formula = z.diff \~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals: Min 1Q Median 3Q Max -0.055283 -0.005757 0.000864 0.009194
0.032790

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 4.4804614 1.4155752 3.165 0.00227 \*\* z.lag.1 -0.2753454
0.0870727 -3.162 0.00229 \*\* tt 0.0014624 0.0004558 3.209 0.00199 \*\*
z.diff.lag1 0.0365836 0.1137363 0.322 0.74865  
z.diff.lag2 0.0686773 0.0978614 0.702 0.48508  
z.diff.lag3 -0.2383874 0.0953954 -2.499 0.01474 \*  
z.diff.lag4 0.5560564 0.0943173 5.896 1.11e-07 \*\*\* — Signif. codes: 0
‘***’ 0.001 ’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.01472 on 72 degrees of freedom Multiple
R-squared: 0.7958, Adjusted R-squared: 0.7787 F-statistic: 46.76 on 6
and 72 DF, p-value: \< 2.2e-16

Value of test-statistic is: -3.1622 5.7158 5.1482

Critical values for test statistics: 1pct 5pct 10pct tau3 -4.04 -3.45
-3.15 phi2 6.50 4.88 4.16 phi3 8.73 6.49 5.47 \|t\| \> 1.6, pero como
elegimos arbitrariamente *M* tenemos que proponer un *M’* \> *M*.

``` r
summary(ur.df(logpib, type = "trend", lags = 5))
```

############################################### 

Augmented Dickey-Fuller Test Unit Root Test
===========================================

############################################### 

Test regression trend

Call: lm(formula = z.diff \~ z.lag.1 + 1 + tt + z.diff.lag)

Residuals: Min 1Q Median 3Q Max -0.053895 -0.005697 0.000490 0.008984
0.033262

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) 4.5853239 1.5252180 3.006 0.00367 \*\* z.lag.1 -0.2817382
0.0938323 -3.003 0.00371 \*\* tt 0.0014727 0.0004899 3.006 0.00367 \*\*
z.diff.lag1 0.0911677 0.1219270 0.748 0.45713  
z.diff.lag2 0.0804391 0.1137193 0.707 0.48170  
z.diff.lag3 -0.2144196 0.0988211 -2.170 0.03342 \*  
z.diff.lag4 0.5868458 0.1002443 5.854 1.41e-07 \*\*\* z.diff.lag5
-0.0449493 0.1146417 -0.392 0.69619  
— Signif. codes: 0 ‘***’ 0.001 ’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.0147 on 70 degrees of freedom Multiple
R-squared: 0.8018, Adjusted R-squared: 0.782 F-statistic: 40.46 on 7 and
70 DF, p-value: \< 2.2e-16

Value of test-statistic is: -3.0026 5.6917 4.5391

Critical values for test statistics: 1pct 5pct 10pct tau3 -4.04 -3.45
-3.15 phi2 6.50 4.88 4.16 phi3 8.73 6.49 5.47 \|t\| \< 1.6, entonces
podemos empezar a disminuir valores hasta obtener \|t\| \>= 1.6, en este
caso ya vimos que con 4 lags esto se cumple. Por lo tanto tenemos *M*=4

#### DUDA: el *t-value* con 8 lags es 1.599, lo podemos tomar como igual a 1.6? y hacer las pruebas

No se rechazan *H0: a = 0* ni *H0’: beta1 = 0*, entonces la tendencia no
es significativamente distinta de cero y pasamos a evaluar un modelo
*drift*.

``` r
#Empezamos la prueba del modelo drift con los mismos lags del último modelo trend
summary(ur.df(logpib, type = "drift", lags = 4))
```

############################################### 

Augmented Dickey-Fuller Test Unit Root Test
===========================================

############################################### 

Test regression drift

Call: lm(formula = z.diff \~ z.lag.1 + 1 + z.diff.lag)

Residuals: Min 1Q Median 3Q Max -0.059906 -0.006922 0.001868 0.007971
0.032866

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)  
(Intercept) -0.0017875 0.2431133 -0.007 0.994154  
z.lag.1 0.0004631 0.0147443 0.031 0.975032  
z.diff.lag1 -0.1696752 0.0996228 -1.703 0.092789 .  
z.diff.lag2 -0.0704242 0.0931535 -0.756 0.452081  
z.diff.lag3 -0.3647015 0.0922603 -3.953 0.000177 *** z.diff.lag4
0.4945546 0.0980525 5.044 3.22e-06 *** — Signif. codes: 0 ‘***’ 0.001
’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.01563 on 73 degrees of freedom Multiple
R-squared: 0.7666, Adjusted R-squared: 0.7506 F-statistic: 47.94 on 5
and 73 DF, p-value: \< 2.2e-16

Value of test-statistic is: 0.0314 3.0391

Critical values for test statistics: 1pct 5pct 10pct tau2 -3.51 -2.89
-2.58 phi1 6.70 4.71 3.86 No se rechazan ni *H0: a = 0* ni *H0’: beta0 =
0* por lo que el *drift* no es significativamente distinto de cero y
tenemos que evaluar un modelo *no constant*

``` r
summary(ur.df(logpib, type = "none", lags = 4))
```

############################################### 

Augmented Dickey-Fuller Test Unit Root Test
===========================================

############################################### 

Test regression none

Call: lm(formula = z.diff \~ z.lag.1 - 1 + z.diff.lag)

Residuals: Min 1Q Median 3Q Max -0.059903 -0.006906 0.001863 0.007959
0.032852

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)  
z.lag.1 0.0003547 0.0001429 2.482 0.015325 \*  
z.diff.lag1 -0.1696044 0.0984840 -1.722 0.089220 .  
z.diff.lag2 -0.0703846 0.0923669 -0.762 0.448476  
z.diff.lag3 -0.3646768 0.0915743 -3.982 0.000158 *** z.diff.lag4
0.4945567 0.0973874 5.078 2.76e-06 *** — Signif. codes: 0 ‘***’ 0.001
’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.01553 on 74 degrees of freedom Multiple
R-squared: 0.7745, Adjusted R-squared: 0.7593 F-statistic: 50.84 on 5
and 74 DF, p-value: \< 2.2e-16

Value of test-statistic is: 2.4822

Critical values for test statistics: 1pct 5pct 10pct tau1 -2.6 -1.95
-1.61 No se rechazan ni *H0: a = 0* ni *H0’: beta0 = 0*, por lo tanto
hay al menos una raíz unitaria.

#### Conclusión (*logpib*):

Comprobamos la hipótesis de que hay al menos una raiz unitaria en la
serie *logpib*.

Como conlcuimos que *logpib* tiene una raiz unitaria, hacemos pruebas
ADF para la serie diferenciada de orden uno, *d.logpib*. Empezamos
directo en el modelo no constant, siguiendo el proceso de las pruebas de
Dickey-Fuller, con un lag menos que en la útlima prueba realizada.
\#\#\#\# Hipótesis (d.logpib): Con la diferenciación nos deshicimos de
la única raiz unitaria.

``` r
summary(ur.df(d.logpib, type = "none", lags = 3))
```

############################################### 

Augmented Dickey-Fuller Test Unit Root Test
===========================================

############################################### 

Test regression none

Call: lm(formula = z.diff \~ z.lag.1 - 1 + z.diff.lag)

Residuals: Min 1Q Median 3Q Max -0.052962 -0.003922 0.004375 0.010218
0.042150

Coefficients: Estimate Std. Error t value Pr(\>\|t\|)  
z.lag.1 -0.66028 0.20716 -3.187 0.0021 \*\* z.diff.lag1 -0.38990 0.16558
-2.355 0.0212 \*  
z.diff.lag2 -0.35762 0.13626 -2.625 0.0105 \*  
z.diff.lag3 -0.61690 0.08684 -7.104 5.92e-10 \*\*\* — Signif. codes: 0
‘***’ 0.001 ’**’ 0.01 ’*’ 0.05 ‘.’ 0.1 ’ ’ 1

Residual standard error: 0.01605 on 75 degrees of freedom Multiple
R-squared: 0.9255, Adjusted R-squared: 0.9216 F-statistic: 233 on 4 and
75 DF, p-value: \< 2.2e-16

Value of test-statistic is: -3.1872

Critical values for test statistics: 1pct 5pct 10pct tau1 -2.6 -1.95
-1.61

Rechazamos *H0: a = 0* y terminamos la

#### Conclusión (*d.logpib*):

No existe raiz unitaria al rededor de cero para la serie *d.logpib*.

Para concluir las pruebas de raiz unitaria, tenemos que verificar el
orden de differenciación estacional. \#\#\# Hipótesis (d.logpib): Al
menos una diferenciación estacional.

``` r
library(forecast)
nsdiffs(logpib)
```

\[1\] 1 La función nos confirma la hipótesis de que necesitamos una
diferenciación estacional.

### Ajuste de Modelos SARIMA

Proponemos modelos

``` r
sarima.010.010.4 <- arima(logpib, c(0,1,0), list(order = c(0,1,0), 4))
sarima.110.010.4 <- arima(logpib, c(1,1,0), list(order = c(0,1,0), 4))
sarima.110.110.4 <- arima(logpib, c(1,1,0), list(order = c(1,1,0), 4))
sarima.111.110.4 <- arima(logpib, c(1,1,1), list(order = c(1,1,0), 4))
sarima.111.111.4 <- arima(logpib, c(1,1,1), list(order = c(1,1,1), 4))
sarima.210.010.4 <- arima(logpib, c(2,1,0), list(order = c(0,1,0), 4))
sarima.211.010.4 <- arima(logpib, c(2,1,1), list(order = c(0,1,0), 4))
sarima.211.110.4 <- arima(logpib, c(2,1,1), list(order = c(1,1,0), 4))
sarima.211.210.4 <- arima(logpib, c(2,1,1), list(order = c(2,1,0), 4))
sarima.211.211.4 <- arima(logpib, c(2,1,1), list(order = c(2,1,1), 4))
sarima.102.011.4 <- arima(logpib, c(1,0,2), list(order = c(0,1,1), 4))
```

|     | SARIMA Model | AIC               | BIC               | MAE                | MAPE               | RMSE               |
|-----|:-------------|:------------------|:------------------|:-------------------|:-------------------|:-------------------|
| 2   | 010.010.4    | -409.0420 | -406.6726 | 0.01280901 | 0.07781201 | 0.01784303 |
| 3   | 110.010.4    | -408.5506 | -403.8117 | 0.01293482 | 0.07857591 | 0.01767967 |
| 4   | 110.110.4    | -422.5251 | -415.4167 | 0.01168571 | 0.07101696 | 0.01598698 |
| 5   | 111.110.4    | -420.3441 | -410.8663 | 0.01164161 | 0.07074763 | 0.01600019 |
| 6   | 111.111.4    | -425.4588 | -413.6116 | 0.01101015 | 0.06692289 | 0.01523579 |
| 7   | 210.010.4    | -406.9706 | -399.8622 | 0.01283578 | 0.07796715 | 0.01763392 |
| 8   | 211.010.4    | -405.3082 | -395.8304 | 0.01278591 | 0.07765996 | 0.01759720 |
| 9   | 211.110.4    | -420.2142 | -408.3670 | 0.01145349 | 0.06959002 | 0.01582612 |
| 10  | 211.210.4    | -422.2473 | -408.0306 | 0.01107749 | 0.06730046 | 0.01541415 |
| 11  | 211.211.4    | -424.1092 | -407.5231 | 0.01051605 | 0.06389969 | 0.01498379 |
| 12  | 102.011.4    | -431.5399 | -419.6298 | 0.01107740 | 0.06733442 | 0.01505194 |

#### Conclusión:

El mejor podelo por ambos criterios, AIC y BIC, es un
*SARIMA(1,0,2)(0,1,1,4)*.

Comprobamos el resultado de la función autoarima analizando la gráfica
de descomposición de la serie *logpib* diferenciada con lag 4 (un orden
de diferencia estacional)

``` r
sd.logpib <- diff(logpib, lag = 4)
plot(decompose(sd.logpib)) #En principio la diferencia estacional le quita la tendencia también pero sigue teniendo un poco de estacionaleidad, por eso se considera también el ganador "organico" sarima.211.211.4
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

### Validación

Analisis de loa residuales

``` r
checkresiduals(sarima.102.011.4) #Ganador por AIC y BIC. Obtenido con autoarima()
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />
Ljung-Box test

data: Residuals from ARIMA(1,0,2)(0,1,1)\[4\] Q\* = 3.1988, df = 4,
p-value = 0.5251

Model df: 4. Total lags used: 8

``` r
checkresiduals(sarima.211.211.4) #Ganador por AIC
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-16-2.png" style="display: block; margin: auto;" />
Ljung-Box test

data: Residuals from ARIMA(2,1,1)(2,1,1)\[4\] Q\* = 2.6493, df = 3,
p-value = 0.4489

Model df: 6. Total lags used: 9 \#\#\#\# Conclusión: Ambos modelos
muestran ausencia de estructura en los residuales, pdríamos decir que
son un proceso de Ruido Blanco Gaussiano. Por tanto, validamos los
modelos.

### Pronósticos

``` r
plot(forecast(sarima.102.011.4))
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

``` r
plot(forecast(sarima.211.211.4))
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-17-2.png" style="display: block; margin: auto;" />

``` r
plot(ts(datospib[69:160,][,2], start = c(1997,1), frequency = 4), main = "Serie pib, periodo 1997/1 a 2019/4") #serie de tiempo con datos hasta 2019/04
```

<img src="Trabajo_Primer_Parcial_MEF_files/figure-markdown_github/unnamed-chunk-17-3.png" style="display: block; margin: auto;" />
