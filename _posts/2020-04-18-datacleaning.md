---
title: "Data Cleaning Project: Golf Pool Dataset"
date: 2020-04-18
tags: [data cleaning, data science]
header:
  image: "/images/The_Masters.jpg"
excerpt: "Data Cleaning example"
---

# Cleaning Data

In this project, I will tackle a problem of messy, untidy data — cleaning and combining it using R in order for it to be analyzed.

Every year I participate in a professional golf pool, where the participants choose one golfer for each tournament of the golf season. At the end of the year, the player with the highest sum of tournament winnings for the golfers they chose at each event wins the pool. The only rule is that no golfer can be chosen twice.

There are two main keys to doing well in the pool — choosing golfers that will show up at each event, and choosing golfers that traditionally perform well at specific courses. On top of this, some tournaments have much higher purses and are much more valuable to the pool — you want to have your best golfers chosen for the higher-prize tournaments.

The folks at [link](https://datagolf.org) have a Course History Tool that helps break down the two most pressing issues — where golfers show up, and where they play well. For most of the tournaments in the season (missing a few that are newer/aren't affiliated with the PGA) they have datasets available for each golfer that has ever played the course professionally.

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
```

By bringing all of these course sets together, it becomes easy to see where a golfer plays the best, the worst, the most, and the least. However, a number of problems exist that prevent this data from initially being analyzed. For one, the course data is all in seperate files.

After downloading all the files, we will combine them on top of eachother. First, we get a list of all the file paths:

```r
allfiles = list.files(path = here("golffiles"),
                        pattern = ".csv",
                        full.names = TRUE,
                        recursive = TRUE)
```

Then, we write a function, map_fun, that combines all of the individual datasets. Multiple problems arise while writing the function, though:

1. The variables `versus_expected` and `historical_true_sg` have NA values formatted as "N/A", which R reads as a character value. Since not all courses have "N/A" values for `versus_expected` or `historical_true_sg`, some of the course datasets interpret the columns as a characters, while some interpret them as numeric. The datasets won't combine when the formats aren't the same, so to combat this, we replace the "N/A"s with R-friendly NAs and reformat the columns as numeric. Now all the `versus_expected` and `historical_true_sg` columnms will combine.

2. When combining the data, the identifying information for each course disappears. To identify which data comes from what course, we first create `splitnames`, which splits the file path for each dataset. By setting course = the eighth element of splitnames, we create a variable that identifies each course.

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

```
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

To complete the dataset, I'd like to add a few more variables and clean things up.

First we'll add the 2020 world golf ranking for each player — only keeping the top 200 players. With only 34 tournaments to choose, I'd like to limit the results to the top 200 golfers.

By copying and pasting into an Excel file, I created a csv file of the top 200 of the WGR, titled `wrldgolfrank2020.csv`.

```r
worldGolf <- read_csv("wrldgolfrank2020.csv", col_types = cols())
tibble(worldGolf)
```

The names clearly aren't in the format we want — in combined_course_data the player_names are formatted as "last, first". We will mutate the player_name column to make them "First Last".

```r
combined_course_data <- combined_course_data %>% separate(player_name, into = c("last", "first"), sep = ",\\s")
combined_course_data <- unite(combined_course_data, Name, first, last, sep = " ", remove = TRUE)
combined_course_data
```

Now we join the worldGolf rank data with combined_course_data, filtering for the top 200 golfers.

```r
combined_course_data <- combined_course_data %>% left_join(worldGolf, by = "Name") %>% filter(Rank < 201)
combined_course_data %>% select(Name, course, Rank)
```

It's also important to add in the purse information for each tournament — the bigger the purse, the more money you can potentially win off a golfer who performs well there. The csv file `2020_purse_file.csv` contains the file name for each course, the tournament played there and the attached purse.

```r
pursedata <- read_csv("2020_purse_file.csv")
combined_course_data <- left_join(combined_course_data, pursedata, by = c("course" = "file"))
combined_course_data <-  combined_course_data %>% select(!`DATE-2020`) %>% select(!coursemiss)
combined_course_data
```
