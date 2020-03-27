---
layout: post
title: "Ejercicio 8 Tarea Modelos ARIMA"
---



Primero hay que cargar todos los paquetes necesarios.


{% highlight r %}
library(quantmod) #Funcion getSymbols para descargar datos de yahoo finance
library(zoo) #Funciones para series de timepo
library(forecast) #Funciones para predicción de modelos 
library(aTSA) #Funciones Augmentes Dickey-Fuller y Phillips Perron
{% endhighlight %}
Así es como NO se grafican funciones de densidad de probabilidad, hay que usar la función density().

{% highlight r %}
plot(rnorm(n=1000))
{% endhighlight %}

<img src="/figure/./_posts/2020-03-26-rmarkdown-test/unnamed-chunk-2-1.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2" style="display: block; margin: auto;" />







