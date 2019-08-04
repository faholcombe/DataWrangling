Clase 1
================
Fausto Holcombe 20170476
July 29, 2019

\#\#Cargar librerias

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.3     v stringr 1.4.0
    ## v tidyr   0.8.3     v forcats 0.4.0

    ## -- Conflicts ---------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tidytext)
library(quanteda)
```

    ## Package version: 1.5.0

    ## Parallel computing: 2 of 8 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

\#\#Usando ReadR

``` r
text_file <- 'data/data/quijote.txt'
readLines(text_file, n = 10, encoding = "UTF-8", skipNul = TRUE)
```

    ##  [1] "EL INGENIOSO HIDALGO DON QUIJOTE DE LA MANCHA"                   
    ##  [2] ""                                                                
    ##  [3] "Miguel de Cervantes Saavedra"                                    
    ##  [4] ""                                                                
    ##  [5] "     Capítulo primero"                                           
    ##  [6] ""                                                                
    ##  [7] "     Que trata de la condición y ejercicio del famoso hidalgo D."
    ##  [8] "Quijote de la "                                                  
    ##  [9] "     Mancha"                                                     
    ## [10] ""

``` r
quijote_lines <- readLines(text_file)
```

\#\#Performance de
funciones

``` r
system.time(readLines(text_file, n = 10, encoding = "UTF-8", skipNul = TRUE))
```

    ##    user  system elapsed 
    ##       0       0       0

``` r
system.time(readLines(text_file))
```

    ##    user  system elapsed 
    ##    0.02    0.00    0.02

\#\#Obtener partes del string

``` r
substr("1234567",1,3)
```

    ## [1] "123"

\#\#Tokenizar

``` r
quijote_frame <- data_frame(txt=quijote_lines)
```

    ## Warning: `data_frame()` is deprecated, use `tibble()`.
    ## This warning is displayed once per session.

``` r
head(quijote_frame)
```

    ## # A tibble: 6 x 1
    ##   txt                                          
    ##   <chr>                                        
    ## 1 EL INGENIOSO HIDALGO DON QUIJOTE DE LA MANCHA
    ## 2 ""                                           
    ## 3 Miguel de Cervantes Saavedra                 
    ## 4 ""                                           
    ## 5 "     CapÃ­tulo primero"                      
    ## 6 ""

``` r
quijote_words <- tidytext::unnest_tokens(quijote_frame, input = txt, output = words, token = "words")
quijote_count <- dplyr::count(quijote_words,words,sort = TRUE)
spanish_stopwords <- data_frame(words = quanteda::stopwords(language = "es"))
quijote_words_clean <- dplyr::anti_join(quijote_words, spanish_stopwords)
```

    ## Joining, by = "words"

``` r
quijote_clean_count <- count(quijote_words_clean, words, sort = TRUE)
quijote_clean_count
```

    ## # A tibble: 7,128 x 2
    ##    words       n
    ##    <chr>   <int>
    ##  1 s         469
    ##  2 n         429
    ##  3 don       370
    ##  4 quijote   362
    ##  5 ã         315
    ##  6 mã        309
    ##  7 sancho    299
    ##  8 dijo      272
    ##  9 l         268
    ## 10 si        264
    ## # ... with 7,118 more rows

\#\#Paquetes para leer excel

``` r
library(readxl)
```

``` r
bancos_activos <- read_excel("data/data/bancos.xlsx", sheet = 2)
head(bancos_activos)
```

    ## # A tibble: 6 x 2
    ##   Bancos     Agencias
    ##   <chr>         <dbl>
    ## 1 G&T              30
    ## 2 OCCIDENTE        20
    ## 3 INDUSTRIAL       18
    ## 4 AGRO             30
    ## 5 CAFE             25
    ## 6 INMOB            20
