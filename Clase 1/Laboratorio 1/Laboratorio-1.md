Laboratorio\#1
================
Fausto Holcombe 20170476
7/31/2019

\#\#Ejercicio 1

``` r
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
library(readxl)
library(readr)
funcion_fecha <- function(x){
  tabla <- read_excel(x)
  columnas <- c("COD_VIAJE", "CLIENTE", "UBICACION", "CANTIDAD", "PILOTO", "Q", "CREDITO", "UNIDAD")
  tabla <- tabla[,columnas]
  mesanio <- substr(x,15,21)
  tabla$fecha <- mesanio
  return(tabla)
}

tabla1 <- funcion_fecha('01-2018.xlsx')
tabla2 <- funcion_fecha('02-2018.xlsx')
tabla3 <- funcion_fecha('03-2018.xlsx')
tabla4 <- funcion_fecha('04-2018.xlsx')
tabla5 <- funcion_fecha('05-2018.xlsx')
tabla6 <- funcion_fecha('06-2018.xlsx')
tabla7 <- funcion_fecha('07-2018.xlsx')
tabla8 <- funcion_fecha('08-2018.xlsx')
```

    ## New names:
    ## * `` -> ...10

``` r
tabla9 <- funcion_fecha('09-2018.xlsx')
tabla10 <- funcion_fecha('10-2018.xlsx')
tabla11 <- funcion_fecha('11-2018.xlsx')

tablamaestra <- rbind(tabla1, tabla2, tabla3, tabla4, tabla5, tabla6, tabla7, tabla8, tabla9, tabla10, tabla11)
```

\#\#Ejercicio 2

``` r
moda <- function(x){
  mayor <- max(x)
  numeros <- 1:mayor
  conteo <- tabulate(x)
  validos <- conteo == max(conteo)
  resultado <- (numeros*validos)
  return(resultado[resultado!=0])
}

lista <- list(c(1,2,2,3,3),c(1,1,1,2,3),c(3,3,3,2,2,1))

lapply(lista,moda)
```

    ## [[1]]
    ## [1] 2 3
    ## 
    ## [[2]]
    ## [1] 1
    ## 
    ## [[3]]
    ## [1] 3

\#\#Ejercicio 3

``` r
library(readr)
archivo <- 'INE_PARQUE_VEHICULAR_080219.txt'
parque <- readr::read_delim(archivo, delim = "|", )
```

    ## Warning: Missing column names filled in: 'X11' [11]

    ## Parsed with column specification:
    ## cols(
    ##   ANIO_ALZA = col_double(),
    ##   MES = col_character(),
    ##   NOMBRE_DEPARTAMENTO = col_character(),
    ##   NOMBRE_MUNICIPIO = col_character(),
    ##   MODELO_VEHICULO = col_character(),
    ##   LINEA_VEHICULO = col_character(),
    ##   TIPO_VEHICULO = col_character(),
    ##   USO_VEHICULO = col_character(),
    ##   MARCA_VEHICULO = col_character(),
    ##   CANTIDAD = col_double(),
    ##   X11 = col_character()
    ## )

    ## Warning: 2362740 parsing failures.
    ## row col   expected     actual                              file
    ##   1  -- 11 columns 10 columns 'INE_PARQUE_VEHICULAR_080219.txt'
    ##   2  -- 11 columns 10 columns 'INE_PARQUE_VEHICULAR_080219.txt'
    ##   3  -- 11 columns 10 columns 'INE_PARQUE_VEHICULAR_080219.txt'
    ##   4  -- 11 columns 10 columns 'INE_PARQUE_VEHICULAR_080219.txt'
    ##   5  -- 11 columns 10 columns 'INE_PARQUE_VEHICULAR_080219.txt'
    ## ... ... .......... .......... .................................
    ## See problems(...) for more details.

``` r
head(parque)
```

    ## # A tibble: 6 x 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAM~ NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>            <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO    HUEHUETENANGO    2007           
    ## 2      2007 05    EL PROGRESO      EL JICARO        2007           
    ## 3      2007 05    SAN MARCOS       OCOS             2007           
    ## 4      2007 05    ESCUINTLA        "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA          MOYUTA           2007           
    ## 6      2007 05    GUATEMALA        FRAIJANES        1997           
    ## # ... with 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, X11 <chr>
