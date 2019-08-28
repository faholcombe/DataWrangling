Laboratorio 4
================
Fausto Holcombe 20170476
8/28/2019

``` r
library(tidyr)
library(reshape2)
```

    ## 
    ## Attaching package: 'reshape2'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     smiths

``` r
library(plyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:plyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(stringr)
```

``` r
df <- data.frame(row = LETTERS[1:3], a = 1:3, b = 4:6, c = 7:9)
dfm <- melt(df, id = "row")
dfg <- gather(df,key ="variable",value = "value",a:c)
```

``` r
poblacion <- read.csv('raw.csv')
poblacion <- poblacion[-1]
poblacion <- poblacion %>% 
  melt(id = c("country","year"))
poblacion$sexo <- substring(poblacion$variable,0,1)
poblacion$edad <- substring(poblacion$variable,2)
poblacion$variable <- NULL
poblacion$edad <- revalue(poblacion$edad, c("014" = "0-14", "1524" = "15-24", "2534" = "25-34","3544" = "35-44","4554" = "45-54","5564" = "55-64", "65" = ">=65", "u" = "UNKWN"))
poblacion <- poblacion %>% select(country,year,gender = sexo,age_group = edad,frequency = value)
head(poblacion)
```

    ##   country year gender age_group frequency
    ## 1      AD 2000      m      0-14         0
    ## 2      AE 2000      m      0-14         2
    ## 3      AF 2000      m      0-14        52
    ## 4      AG 2000      m      0-14         0
    ## 5      AL 2000      m      0-14         2
    ## 6      AM 2000      m      0-14         2

``` r
load('wide_religion.Rda')
wide_religion %>%
  gather(key = "income_level",value = "frequency",-religion) %>% 
  head()
```

    ##             religion income_level frequency
    ## 1           Agnostic        <$10k        27
    ## 2            Atheist        <$10k        12
    ## 3           Buddhist        <$10k        27
    ## 4           Catholic        <$10k       418
    ## 5 Don’t know/refused        <$10k        15
    ## 6   Evangelical Prot        <$10k       575
