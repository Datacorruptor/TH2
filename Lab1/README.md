``` r
library(stringr)
library(tidymodels)
library(arrow)
library(dplyr)
library(ggplot2)
```

# Задание 1: Надите утечку данных из Вашей сети

Важнейшие документы с результатми нашей исследовательской деятельности в
области создания вакцин скачиваются в виде больших заархивированных
дампов. Один из хостов в нашей сети используется для пересылки этой
информации – он пересылает гораздо больше информации на внешние ресурсы
в Интернете, чем остальные компьютеры нашей сети. Определите его
IP-адрес.

## Импортируем датасет

``` r
df_data <- arrow::read_csv_arrow("/home/archer/Documents/ML/traffic_security.csv")
```

## Дадим имена признакам

``` r
colnames(df_data) <- c('timestamp','src','dst','port','bytes')
head(df_data,3)
```

    ## # A tibble: 3 × 5
    ##       timestamp src           dst           port bytes
    ##         <int64> <chr>         <chr>        <int> <int>
    ## 1 1578326400005 16.79.101.100 12.48.65.39     92 11895
    ## 2 1578326400007 18.43.118.103 14.51.30.86     27   898
    ## 3 1578326400011 15.71.108.118 14.50.119.33    57  7496

## Очистим датасет, оставив в src ip-адреса, только нашего предприятия

``` r
knitr::opts_chunk$set(
  df_data <- df_data[df_data$src > 11 & df_data$src < 15 & df_data$dst < 11 | df_data$dst > 15, ]
)
```

## Найдём ip-адрес и максимальное число передаваемых байтов(ответ кто злоумышленник в организации)

``` r
knitr::opts_chunk$set(
 found_ip1 <- df_data %>%
            group_by(src) %>%
            summarise(bytes = mean(bytes)),
  found_ip1 <- found_ip1[which.max(found_ip1$bytes),],
  print(found_ip1) 
)
```

    ## # A tibble: 1 × 2
    ##   src            bytes
    ##   <chr>          <dbl>
    ## 1 13.37.84.125 192390.

Ответ: 13.37.84.125
