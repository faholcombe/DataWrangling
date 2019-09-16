R Notebook
================

``` r
require(stringr)
```

    ## Loading required package: stringr

``` r
test1 <- c("P243CNJ", "P214HNS", "P345FVJ", "A344SDF", "P2314ASD", "P245ABC") #1
answer1 <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
test2 <- c("Ejemplo1.pdf", "prueba2.PDF", "respuestas_del_examen.jpg", "amor.JPG", "hola.hpq") #2
answer2 <- c(TRUE, TRUE, TRUE, TRUE, FALSE)
test3 <- c("Hola123!", "$H123488", "$H123488Nu", "AERFSdnf", "12345678", "asdfghjk", "ASDFGHJK", "adfd!!1L", "Datawrangling2019!") #3
answer3 <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
test4 <- c("19002324", "31001564", "14011110", "11008921", "20003421") #4
answer4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
test5 <- c("pit", "spot", "spate", "slap two", "respite", "pt", "Pot", "peat", "part") #5
answer5 <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
test6 <- c("+5024562 9999", "5025632-9991", "2614-4501", "4617 8475", "63213445", "30616055", "50124252627", "24-212527", "2324/2526")
answer6 <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
test7 <- c(".@ufm.edu", "faustoholcombe@ufm.edu", "Jorge@ufm.edu", "gjh@gmail.com", "125@ufm.org", "f@ufm.edu.gt")
answer7 <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)

str_detect(string = test1,
           pattern = "^P[0-9]{3}([B-D]|[F-H]|[J-N]|[P-T]|[V-Z]){3}$") == answer1
```

    ## [1] TRUE TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test2,
           pattern = "\\.(PDF|JPG|pdf|jpg)$") == answer2
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test3,
           pattern = "^(?=.*?([:punct:]|\\$))(?=.*?[A-Z])(?=.*?[0-9]).{8,}$?$") == answer3
```

    ## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test4,
           pattern = "^(0[1-9]|[1-2][0-9]|30)0{2}(1{3}[0-9]|11[1-9]{2}|1[2-9]{3}|[2-7][1-9]{3}|8[0-8][0-9]{2}|89[0-6][0-9]|8970)$") == answer4
```

    ## [1] TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test5,
           pattern = "^(p(?=i)|[a-o]|[q-z])") == answer5
```

    ## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test6,
           pattern = "^(\\+502|502)?+[4562][0-9]{3}[-\\s]?[0-9]{4}$") == answer6
```

    ## [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = test7,
           pattern = "^([A-Za-z0-9]|[:punct:])+@(ufm.edu)$") == answer7
```

    ## [1] TRUE TRUE TRUE TRUE TRUE TRUE

``` r
str_detect(string = "aaa55ABC",
           pattern = "^[a-z]{0,3}[0-9]{2,9}[A-Z]{3,}$")
```

    ## [1] TRUE