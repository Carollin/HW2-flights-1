flights hw
================
Chenyi Lin
2/11/2021

CHAPTER 5 data transformation

``` r
library(nycflights13)
library('tidyverse')
```

    ## ─ Attaching packages ──────────────────── tidyverse 1.3.0 ─

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.4     ✓ dplyr   1.0.4
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1

    ## ─ Conflicts ───────────────────── tidyverse_conflicts() ─
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(tibble)
```

``` r
flights
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # … with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
dec25<-filter(flights, month ==1, day == 25)
dec25
```

    ## # A tibble: 922 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1    25       15           1815       360      208           1958
    ##  2  2013     1    25       17           2249        88      119           2357
    ##  3  2013     1    25       26           1850       336      225           2055
    ##  4  2013     1    25      123           2000       323      229           2101
    ##  5  2013     1    25      123           2029       294      215           2140
    ##  6  2013     1    25      456            500        -4      632            648
    ##  7  2013     1    25      519            525        -6      804            820
    ##  8  2013     1    25      527            530        -3      820            829
    ##  9  2013     1    25      535            540        -5      826            850
    ## 10  2013     1    25      539            540        -1     1006           1017
    ## # … with 912 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
sum(is.na(flights$dep_time))
```

    ## [1] 8255

1.  There is 8255 missing data in dep\_time.

<!-- end list -->

``` r
sum(is.na(flights$dep_delay))
```

    ## [1] 8255

``` r
sum(is.na(flights$arr_time))
```

    ## [1] 8713

``` r
sum(is.na(flights$arr_delay))
```

    ## [1] 9430

``` r
sum(is.na(flights$tailnum))
```

    ## [1] 2512

``` r
sum(is.na(flights$air_time))
```

    ## [1] 9430

2.  There are many missing data in arr\_time,
    arr\_delay,dep\_delay,tailnum, air\_time.

<!-- end list -->

``` r
newdep_time = (floor(flights$dep_time/100)*60)+(flights$dep_time %%100)
```

``` r
newflights <- flights[,!(colnames(flights) %in% c("dep_time"))]
newflights
```

    ## # A tibble: 336,776 x 18
    ##     year month   day sched_dep_time dep_delay arr_time sched_arr_time arr_delay
    ##    <int> <int> <int>          <int>     <dbl>    <int>          <int>     <dbl>
    ##  1  2013     1     1            515         2      830            819        11
    ##  2  2013     1     1            529         4      850            830        20
    ##  3  2013     1     1            540         2      923            850        33
    ##  4  2013     1     1            545        -1     1004           1022       -18
    ##  5  2013     1     1            600        -6      812            837       -25
    ##  6  2013     1     1            558        -4      740            728        12
    ##  7  2013     1     1            600        -5      913            854        19
    ##  8  2013     1     1            600        -3      709            723       -14
    ##  9  2013     1     1            600        -3      838            846        -8
    ## 10  2013     1     1            600        -2      753            745         8
    ## # … with 336,766 more rows, and 10 more variables: carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
add_column(newflights, newdep_time = newdep_time,.after = 3)
```

    ## # A tibble: 336,776 x 19
    ##     year month   day newdep_time sched_dep_time dep_delay arr_time
    ##    <int> <int> <int>       <dbl>          <int>     <dbl>    <int>
    ##  1  2013     1     1         317            515         2      830
    ##  2  2013     1     1         333            529         4      850
    ##  3  2013     1     1         342            540         2      923
    ##  4  2013     1     1         344            545        -1     1004
    ##  5  2013     1     1         354            600        -6      812
    ##  6  2013     1     1         354            558        -4      740
    ##  7  2013     1     1         355            600        -5      913
    ##  8  2013     1     1         357            600        -3      709
    ##  9  2013     1     1         357            600        -3      838
    ## 10  2013     1     1         358            600        -2      753
    ## # … with 336,766 more rows, and 12 more variables: sched_arr_time <int>,
    ## #   arr_delay <dbl>, carrier <chr>, flight <int>, tailnum <chr>, origin <chr>,
    ## #   dest <chr>, air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>,
    ## #   time_hour <dttm>

``` r
b <- flights %>% group_by(month,day) %>% summarize(mean(dep_delay,na.rm = T),.groups = "drop")
b
```

    ## # A tibble: 365 x 3
    ##    month   day `mean(dep_delay, na.rm = T)`
    ##  * <int> <int>                        <dbl>
    ##  1     1     1                        11.5 
    ##  2     1     2                        13.9 
    ##  3     1     3                        11.0 
    ##  4     1     4                         8.95
    ##  5     1     5                         5.73
    ##  6     1     6                         7.15
    ##  7     1     7                         5.42
    ##  8     1     8                         2.55
    ##  9     1     9                         2.28
    ## 10     1    10                         2.84
    ## # … with 355 more rows

``` r
a <- flights %>% 
  group_by(month,day) %>% 
  summarise(mean(is.na(dep_time)),.groups = "drop")
c <- add_column(b,avgdep_time = a$`mean(is.na(dep_time))`,.after = 3)

c <- rename(c,avgdelay_time=`mean(dep_delay, na.rm = T)`)
c
```

    ## # A tibble: 365 x 4
    ##    month   day avgdelay_time avgdep_time
    ##    <int> <int>         <dbl>       <dbl>
    ##  1     1     1         11.5      0.00475
    ##  2     1     2         13.9      0.00848
    ##  3     1     3         11.0      0.0109 
    ##  4     1     4          8.95     0.00656
    ##  5     1     5          5.73     0.00417
    ##  6     1     6          7.15     0.00120
    ##  7     1     7          5.42     0.00322
    ##  8     1     8          2.55     0.00445
    ##  9     1     9          2.28     0.00554
    ## 10     1    10          2.84     0.00322
    ## # … with 355 more rows

``` r
ggplot(c,aes(x=avgdelay_time,y=avgdep_time))+
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- --> 3.  
By looking at the plot of average delay time and averge dep time. we can
find that when the delay time is longer, the number of cancelled flights
are larger. That may because the weather or other reasons. However,
there are also many points show that teher is no obvious realtion
between two factors.
