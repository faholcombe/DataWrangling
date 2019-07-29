Repaso
================
Fausto Holcombe 20170476
July 24, 2019

Instalar Librerias
------------------

    ## also installing the dependencies 'pillar', 'tibble'

    ## package 'pillar' successfully unpacked and MD5 sums checked
    ## package 'tibble' successfully unpacked and MD5 sums checked
    ## package 'dplyr' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'RMySQL' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'lubridate' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'openxlsx' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'tidyverse' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'stringr' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'readr' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

    ## package 'DBI' successfully unpacked and MD5 sums checked
    ## 
    ## The downloaded binary packages are in
    ##  C:\Users\faust\AppData\Local\Temp\Rtmp0iFSpX\downloaded_packages

Cargando Librerias
------------------

    ## Loading required package: dplyr

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

Tipos de datos
--------------

``` r
string <- "This is a string"
string
```

    ## [1] "This is a string"

``` r
class(string)
```

    ## [1] "character"

``` r
nchar(string)
```

    ## [1] 16

``` r
length(string)
```

    ## [1] 1

``` r
number <- 234L
class(number)
```

    ## [1] "integer"

``` r
typeof(number)
```

    ## [1] "integer"

``` r
logical <- FALSE
logical
```

    ## [1] FALSE

``` r
logical*1
```

    ## [1] 0

``` r
factor_1 <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Wed", "Thu", "Wed", "Thu")
factor_1 <- factor(factor_1)
factor_1
```

    ##  [1] Mon Tue Wed Thu Fri Sat Sun Mon Tue Wed Thu Fri Sat Sun Wed Thu Wed
    ## [18] Thu
    ## Levels: Fri Mon Sat Sun Thu Tue Wed

``` r
as.numeric(factor_1)
```

    ##  [1] 2 6 7 5 1 3 4 2 6 7 5 1 3 4 7 5 7 5

``` r
factor_2 <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Wed", "Thu", "Wed", "Thu")
factor_2 <- ordered(factor_2, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
factor_2
```

    ##  [1] Mon Tue Wed Thu Fri Sat Sun Mon Tue Wed Thu Fri Sat Sun Wed Thu Wed
    ## [18] Thu
    ## Levels: Mon < Tue < Wed < Thu < Fri < Sat < Sun

Vectors
-------

``` r
sample(x = 1:100, size = 8, replace = FALSE)
```

    ## [1] 93 50  8 12 36 63 77 57

``` r
sample(x = 1:100, size = 8, replace = TRUE)
```

    ## [1] 24 78 11 97 47 87 94 55

``` r
class(sample(x = 1:100, size = 8, replace = TRUE))
```

    ## [1] "integer"

``` r
vector_1 <- c(1:5, "A")
vector_1
```

    ## [1] "1" "2" "3" "4" "5" "A"

``` r
vector_log <- c(1,0,0,1,0,1)
vector_log<- as.logical(vector_log)
vector_log
```

    ## [1]  TRUE FALSE FALSE  TRUE FALSE  TRUE

List
----

``` r
list_1 <- list(c(1:5),as.logical(c(1,0,1)),c("a", "b", "c"))
names(list_1) <- c("first", "second","third")
list_1
```

    ## $first
    ## [1] 1 2 3 4 5
    ## 
    ## $second
    ## [1]  TRUE FALSE  TRUE
    ## 
    ## $third
    ## [1] "a" "b" "c"

``` r
list_1$second[2]
```

    ## [1] FALSE

``` r
list_1[[2]][2]
```

    ## [1] FALSE

Matrices
--------

``` r
matrix(data = 1, nrow = 4, ncol = 5)
```

    ##      [,1] [,2] [,3] [,4] [,5]
    ## [1,]    1    1    1    1    1
    ## [2,]    1    1    1    1    1
    ## [3,]    1    1    1    1    1
    ## [4,]    1    1    1    1    1

Data Frames
-----------

``` r
df <- data.frame(col1 = c("This","is","a","vector","of","strings"), col2 = 1:6, col3 = letters[1:6], stringsAsFactors = FALSE)
nrow(df)
```

    ## [1] 6

``` r
ncol(df)
```

    ## [1] 3

``` r
str(df)
```

    ## 'data.frame':    6 obs. of  3 variables:
    ##  $ col1: chr  "This" "is" "a" "vector" ...
    ##  $ col2: int  1 2 3 4 5 6
    ##  $ col3: chr  "a" "b" "c" "d" ...

Funciones Importantes (BaseR)
-----------------------------

``` r
#df$col4 <- 11:16
new_elements <- c("new_string", 19, "z")
df <- rbind(df, new_elements)
is.na(df)
```

    ##       col1  col2  col3
    ## [1,] FALSE FALSE FALSE
    ## [2,] FALSE FALSE FALSE
    ## [3,] FALSE FALSE FALSE
    ## [4,] FALSE FALSE FALSE
    ## [5,] FALSE FALSE FALSE
    ## [6,] FALSE FALSE FALSE
    ## [7,] FALSE FALSE FALSE
