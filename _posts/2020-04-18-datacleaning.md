---
title: "Cleaning Data: Golf Pool Dataset"
date: 2020-04-18
tags: [data cleaning, data science]
header:
  image: "/images/The_Masters.jpg"
excerpt: "Data Cleaning example"
---

## Introduction

In this project, I will tackle a problem of messy, untidy data — cleaning and combining it using R in order for it to be analyzed.

Every year I participate in a professional golf pool, where the participants choose one golfer for each tournament of the golf season. At the end of the year, the player with the highest sum of tournament winnings for the golfers they chose at each event wins the pool. The only rule is that no golfer can be chosen twice.

There are two main keys to doing well in the pool — choosing golfers that will show up at each event, and choosing golfers that traditionally perform well at specific courses. On top of this, some tournaments have much higher purses and are much more valuable to the pool — you want to have your best golfers chosen for the higher-prize tournaments.

## datagolf.org

The folks at [datagolf.org](https://datagolf.org) have a Course History Tool that helps break down the two most pressing issues — where golfers show up, and where they play well. For most of the tournaments in the season (missing a few that are newer/aren't affiliated with the PGA) they have datasets available for each golfer that has ever played the course professionally.

The metrics include number of rounds played, average adjusted strokes-gained, average strokes-gained relative to baseline skill level, and a strokes-gained per round adjustment for the course. The stroked-gained adjustment is a function of the # of rounds a golfer has played and their historical SG relative to baseline.

For example, here is the datagolf Course History for Augusta National GC:

```r
suppressPackageStartupMessages(library(tidyverse))
library(dplyr)
suppressPackageStartupMessages(library(here))
library(stringr)
library(purrr)
library(knitr)
Augusta <- read_csv("augusta_national_gc_CH.csv", col_types = cols())
tibble(Augusta)



```r
## # A tibble: 700 x 6
##    player_name rounds_played historical_true… versus_expected dg_adjustment
##    <chr>               <dbl>            <dbl> <chr>                   <dbl>
##  1 Aaron, Tom…            50           -3.01  0.544889347826…       0.143  
##  2 Huston, Jo…            50            1.57  0.626309599999…       0.164  
##  3 Kuchar, Ma…            50            1.72  0.565279200000…       0.148  
##  4 Adams, John             2           -2.39  -2.69227             -0.0162
##  5 Alexander,…             2           -0.567 N/A                   0      
##  6 Armour III…             2           -2.07  -1.9725              -0.0118
##  7 Atwal, Arj…             2           -2.37  -1.683495            -0.0101
##  8 Baker, Pet…             2           -1.89  -2.14405999999…      -0.0129
##  9 Bakst, Ken              2           -4.55  N/A                   0      
## 10 Bard, Derek             2           -1.73  0.656635000000…       0.00394
## # … with 690 more rows, and 1 more variable: revised_skill <chr>
```


If we could bring all of these course sets together, we could then create models to estimate where a golfer plays the best, the worst, the most, and the least. However, a number of problems exist that prevent this data from initially being analyzed. For one, the course data is all in separate files.

## Combining many datasets

After downloading all the files, we will combine them on top of each other. First, we get a list of all the file paths:

```r
allfiles = list.files(path = here("golffiles"),
                        pattern = ".csv",
                        full.names = TRUE,
                        recursive = TRUE)
```

Then, we write a function, map_fun, that combines all of the individual datasets. Multiple problems arise while writing the function, though:

1. The variables `versus_expected` and `historical_true_sg` have NA values formatted as "N/A", which R reads as a character value. Since not all courses have "N/A" values for `versus_expected` or `historical_true_sg`, some of the course datasets interpret the columns as a characters, while some interpret them as numeric. The datasets won't combine when the formats aren't the same, so to combat this, we replace the "N/A"s with R-friendly NAs and reformat the columns as numeric. Now all the `versus_expected` and `historical_true_sg` columns will combine.

2. When combining the data, the identifying information for each course disappears. To identify which data comes from what course, we first create `splitnames`, which splits the file path for each dataset. By setting course = the eighth element of `splitnames`, we create a variable that identifies each course.

```r
map_fun = function(path) {
     data <- read.csv(path, header = TRUE, stringsAsFactors=FALSE)
     data <- mutate(data, versus_expected = replace(versus_expected, versus_expected == "N/A", NA), historical_true_sg = replace(historical_true_sg, historical_true_sg == "N/A", NA))
     data$versus_expected <- as.numeric(as.character(data$versus_expected))
     data$historical_true_sg <- as.numeric(as.character(data$historical_true_sg))
     splitnames = str_split(path, pattern = "/", simplify = TRUE)
     data <- mutate(data, course = splitnames[8])
     data
}
```

Now that the function is created to read and correctly format each dataset, we use the map_dfr function to combine them. We get rid of the extraneous `revised_skill` variable, and are left with one combined dataset.

```r
combined_course_data = map_dfr(allfiles, map_fun)
combined_course_data <- data_frame(combined_course_data) %>% select(!revised_skill) %>% arrange(course)
combined_course_data
```
```r
## # A tibble: 19,620 x 6
##    player_name rounds_played historical_true… versus_expected dg_adjustment
##    <chr>               <int>            <dbl>           <dbl>         <dbl>
##  1 Aaron, Tom…            50           -3.01            0.545       0.143  
##  2 Huston, Jo…            50            1.57            0.626       0.164  
##  3 Kuchar, Ma…            50            1.72            0.565       0.148  
##  4 Adams, John             2           -2.39           -2.69       -0.0162
##  5 Alexander,…             2           -0.567          NA           0      
##  6 Armour III…             2           -2.07           -1.97       -0.0118
##  7 Atwal, Arj…             2           -2.37           -1.68       -0.0101
##  8 Baker, Pet…             2           -1.89           -2.14       -0.0129
##  9 Bakst, Ken              2           -4.55           NA           0      
## 10 Bard, Derek             2           -1.73            0.657       0.00394
## # … with 19,610 more rows, and 1 more variable: course <chr>
```

## Joining other variables

To complete the dataset, I'd like to add a few more variables and clean things up.

First we'll add the 2020 world golf ranking for each player — only keeping the top 200 players. With only 34 tournaments to choose, I'd like to limit the results to the top 200 golfers.

By copying and pasting into an Excel file, I created a csv file of the top 200 of the WGR, titled `wrldgolfrank2020.csv`.

```r
worldGolf <- read_csv("wrldgolfrank2020.csv", col_types = cols())
tibble(worldGolf)
```

```r
## # A tibble: 200 x 2
##     Rank Name             
##    <dbl> <chr>            
##  1     1 Brooks Koepka    
##  2     2 Rory McIlroy     
##  3     3 Jon Rahm         
##  4     4 Justin Thomas    
##  5     5 Dustin Johnson   
##  6     6 Tiger Woods      
##  7     7 Patrick Cantlay  
##  8     8 Justin Rose      
##  9     9 Xander Schauffele
## 10    10 Tommy Fleetwood  
## # … with 190 more rows
```


### Mutating keys

To join the two tables, we'll need a key that brings the variables together, in this case the players' names. However, the names clearly aren't formatted the same way — in combined_course_data the player_names are formatted as "Last, First". We will mutate the player_name column to make them "First Last".

```r
combined_course_data <- combined_course_data %>% separate(player_name, into = c("last", "first"), sep = ",\\s")
combined_course_data <- unite(combined_course_data, Name, first, last, sep = " ", remove = TRUE)
combined_course_data
```

``` r
## # A tibble: 19,620 x 6
##    Name    rounds_played historical_true… versus_expected dg_adjustment course  
##    <chr>           <int>            <dbl>           <dbl>         <dbl> <chr>   
##  1 Tommy …            50           -3.01            0.545       0.143   augusta…
##  2 John H…            50            1.57            0.626       0.164   augusta…
##  3 Matt K…            50            1.72            0.565       0.148   augusta…
##  4 John A…             2           -2.39           -2.69       -0.0162  augusta…
##  5 Buddy …             2           -0.567          NA           0       augusta…
##  6 Tommy …             2           -2.07           -1.97       -0.0118  augusta…
##  7 Arjun …             2           -2.37           -1.68       -0.0101  augusta…
##  8 Peter …             2           -1.89           -2.14       -0.0129  augusta…
##  9 Ken Ba…             2           -4.55           NA           0       augusta…
## 10 Derek …             2           -1.73            0.657       0.00394 augusta…
## # … with 19,610 more rows
```

### Join and filter

Now we join the worldGolf rank data with combined_course_data, filtering for the top 200 golfers.

```r
combined_course_data <- combined_course_data %>% left_join(worldGolf, by = "Name") %>% filter(Rank < 201)
combined_course_data %>% select(Name, course, Rank)
```

```r
## # A tibble: 2,681 x 3
##    Name              course                      Rank
##    <chr>             <chr>                      <dbl>
##  1 Matt Kuchar       augusta_national_gc_CH.csv    24
##  2 George Coetzee    augusta_national_gc_CH.csv   189
##  3 Nicolas Colsaerts augusta_national_gc_CH.csv   179
##  4 Dylan Frittelli   augusta_national_gc_CH.csv   102
##  5 Shugo Imahira     augusta_national_gc_CH.csv    31
##  6 Matt Jones        augusta_national_gc_CH.csv    99
##  7 Adam Long         augusta_national_gc_CH.csv   116
##  8 Joaquin Niemann   augusta_national_gc_CH.csv    58
##  9 Doc Redman        augusta_national_gc_CH.csv   156
## 10 Brendon Todd      augusta_national_gc_CH.csv    63
## # … with 2,671 more rows
```

## Purse data

It's also important to add in the purse information for each tournament — the bigger the purse, the more money you can potentially win off a golfer who performs well there. The csv file `2020_purse_file.csv` contains the file name for each course, the tournament played there and the attached purse.

```r
pursedata <- read_csv("2020_purse_file.csv")
combined_course_data <- left_join(combined_course_data, pursedata, by = c("course" = "file"))
combined_course_data <-  combined_course_data %>% select(!`DATE-2020`) %>% select(!coursemiss)
combined_course_data
```

```r
## # A tibble: 2,681 x 10
##    Name  rounds_played historical_true… versus_expected dg_adjustment course
##    <chr>         <int>            <dbl>           <dbl>         <dbl> <chr>
##  1 Matt…            50            1.72            0.565       0.148   augus…
##  2 Geor…             2           -0.698          -1.30       -0.00782 augus…
##  3 Nico…             2           -1.70           -1.93       -0.0116  augus…
##  4 Dyla…             2           -1.15           -1.62       -0.00974 augus…
##  5 Shug…             2           -2.28           NA           0       augus…
##  6 Matt…             2           -1.54           -1.70       -0.0102  augus…
##  7 Adam…             2           -1.78           -1.71       -0.0103  augus…
##  8 Joaq…             2           -2.15           -1.54       -0.00927 augus…
##  9 Doc …             2           -2.15           -2.06       -0.0124  augus…
## 10 Bren…             2           -2.39           -2.64       -0.0158  augus…
## # … with 2,671 more rows, and 4 more variables: Rank <dbl>, Week <dbl>,
## #   tournament <chr>, purse <dbl>
```
