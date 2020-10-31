---
title: "Modeling: Swing discipline adjustment period following MLB promotion?"
date: 2020-10-30
tags: [Logistic Classification, Data Science, Baseball]
header:
  image: "/images/Brewers.png"
excerpt: "Machine Learning, MLB, Data Science"
---

## Background

When promoting a prospect, it is reasonable to assume that some adjustment period exists for the player, given the increased level of competition and the pressure associated with playing for an MLB team.

One aspect of a players' game is his swing discipline — how he reacts to a pitch being thrown. On each pitch a hitter chooses to swing or not — that choice can be modeled with a variety of measurables that go into the decision.

In this report, the effect of recently being promoted will be analyzed to see if it affects a players' swing decisions.  


## Data

Thanks to modern technological innovations, Trackman systems now capture ball position data for every game from A-ball to MLB. With this data, we can see nearly everything about a given pitch, including spin, speed, release point, position as it crosses the plate, etc. The image below shows a glimpse of what this data can provide — a strike zone for a selected left-handed batter in the data who has a tendency to swing at balls low and out of the zone against righties.

```r
library(tidyverse)
df <- df %>% mutate(is_swing = as.factor(as.integer((pitch_call == "FoulBall" | pitch_call == "InPlay" | pitch_call == "StrikeSwinging"))))
df$pitcher_side <- factor(df$pitcher_side, levels = c("Left", "Right"),
                  labels = c("LHP", "RHP"))

strikex <- c(-.95,.95,.95,-.95,-.95)
strikez <- c(1.6,1.6,3.5,3.5,1.6)
zone <- data.frame(strikex,strikez)

ggplot() + geom_path(data = zone, aes(x=strikex, y=strikez)) + coord_equal() + geom_point(data = (df %>% filter(batter_id == "007bc3de")), aes(x = plate_side, y = plate_height, shape = pitch_type, color = is_swing)) + scale_size(range = c(-1.0,2.5)) +  facet_grid(. ~ pitcher_side) + labs(
       color = "Swing",
       title = "Batter 007bc3de Swing Chart ",
       subtitle = "2019-04-01 - 2019-06-30") +
       ylab("Feet from ground") +
       xlab("Feet from center of plate") +   theme(panel.background = element_rect(fill = "white")) + theme(panel.grid.major.y = element_line(color = "#bad0d0", size = .4)) +
  theme(panel.grid.major.x = element_line(color = "#bdd0d0", size = .4))
```

<img src="{{ site.url }}{{ site.baseurl }}/images/strikezone" alt="strike zone tag">

Predicting whether or not a better swung at a pitch is clearly more complicated than just looking at the strike zone and pitch type — a myriad of other variables also go into these decisions.

## Modeling
Using a common logistic classification modeling approach, we take a subset of pitch-level data from 2019 and utilize these variables to predict whether or not the batter swung at the pitch. Then, we can analyze whether or not players' swing decisions were affected by recently being promoted.

Measurements and variables included in the Trackman data includes:

- Pitcher handedness
- Batter handedness
- Outs
- Count
- Pitch speed at release
- Pitch speed when crossing plate
- Vertical break
- Induced vertical break
- Horizontal break
- Height of pitch at plate
- Lateral location of pitch at plate
- Type of pitch
- Pitch height at release
- Pitch lateral location at release
- Pitcher release extension from mound
- Vertical plate approach angle
- Horizontal plate approach angle

Lastly, we include a variable that denotes whether a player was recently promoted — if he has seen less than 100 pitches since moving up a level.

### Data Cleaning and Variable Construction

First we clean the data.

```r
# Create categorical variable for count
df <- df %>% unite(col = count, balls, strikes, sep = "-")

# Check counts
df %>% group_by(count) %>% summarise(ct = n())

# A tibble: 14 x 2
   count     ct
   <chr>  <int>
 1 0-0   281628
 2 0-1   136042
 3 0-2    70559
 4 0-3        1
 5 1-0   113886
 6 1-1   110642
 7 1-2   102311
 8 2-0    40104
 9 2-1    59511
10 2-2    88302
11 3-0    12979
12 3-1    26263
13 3-2    55497
14 4-1        1

# Remove impossible counts
df <-  df %>% filter(count != "0-3", count != "4-1")

# Check outs
df %>% group_by(outs) %>% summarise(ct = n())
# Remove impossible outs

# A tibble: 5 x 2
   outs     ct
  <dbl>  <int>
1     0 383969
2     1 362903
3     2 350684
4     3     38
5    NA    132

# Remove impossible outs
df <-  df %>% filter(outs != 3)

# Check batter handedness
df %>% group_by(batter_side) %>% summarise(ct = n())

# A tibble: 3 x 2
  batter_side     ct
  <chr>        <int>
1 Left        485065
2 Right       612651
3 S               10

# Remove outliers
df <-  df %>% filter(batter_side != "S")

library(cleandata)
library(roll)
library(caret)
df$level[df$level == "MLB"] <- 5
df$level[df$level == "AAA"] <- 4
df$level[df$level == "AA"] <- 3
df$level[df$level == "A+"] <- 2
df$level[df$level == "A"] <- 1
df$level <- as.integer(df$level)

```

Then we construct the recently called-up variable, which is true if promotion occurred in last 100 pitches and the player hasn't been sent back down yet.

```r
df <- df %>% arrange(batter_id, date) %>% mutate(callup = if_else(lag(level, k = 1) < level, if_else(lag(batter_id, k = 1) == batter_id, 1, 0), 0)) %>% mutate(sentdown = if_else(lag(level, k = 1) > level, if_else(lag(batter_id, k = 1) == batter_id, 1, 0), 0)) %>%  mutate(sum_callup= ifelse(lag(batter_id, k = 1) == batter_id, roll::roll_sum(callup, width = 100, min_obs = 1), 998)) %>%  mutate(sum_sentdown= ifelse(lag(batter_id, k = 1) == batter_id, roll::roll_sum(sentdown, width = 100, min_obs = 1), 999)) %>%  mutate(strk_sum_c =sequence(rle(sum_callup)$lengths)) %>%  mutate(strk_sum_s =sequence(rle(sum_sentdown)$lengths)) %>% mutate(recent_callup100 = ifelse(sum_callup > sum_sentdown, TRUE, ifelse(sum_sentdown > sum_callup, FALSE, ifelse(sum_callup == 0, FALSE, ifelse(strk_sum_c > strk_sum_s, FALSE, TRUE)))))

```

### Identify highly correlated variables to omit from model

```r
numerics <- sapply(df_p2, is.numeric)
df_numeric <- df[ , numerics] %>% select(-y55)
df_without_na <- na.omit(df_numeric)
cor_matrix <- cor(df_without_na)
high_cor <- findCorrelation(cor_matrix, 0.9, names = TRUE)
print(high_cor)

[1] "vert_break"         "release_speed"      "horz_release_angle"

```
### Initial specification
```r
logit100 <- glm(is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) + as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break + horz_break + plate_height + plate_side + as.factor(pitch_type)  + rel_height + rel_side + rel_height + extension + vert_release_angle + vert_approach_angle + horz_approach_angle + as.factor(recent_callup100), data = df_p2, family = "binomial")
summary(logit100)

Call:
glm(formula = is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) +
    as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break +
    horz_break + plate_height + plate_side + as.factor(pitch_type) +
    rel_height + rel_side + rel_height + extension + vert_release_angle +
    vert_approach_angle + horz_approach_angle + as.factor(recent_callup100),
    family = "binomial", data = df_p2)

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-1.8000  -1.1004  -0.7856   1.1350   2.8941  

Coefficients:
                                 Estimate Std. Error z value Pr(>|z|)    
(Intercept)                      5.048836   0.331917  15.211  < 2e-16 ***
as.factor(pitcher_side)Right    -0.033410   0.011979  -2.789 0.005288 **
as.factor(pitcher_side)S         0.107961   0.300403   0.359 0.719305    
as.factor(batter_side)Right      0.058018   0.004138  14.022  < 2e-16 ***
as.factor(batter_side)S         -1.475181   0.811991  -1.817 0.069256 .  
as.factor(outs)1                 0.014451   0.004813   3.003 0.002678 **
as.factor(outs)2                -0.009824   0.004868  -2.018 0.043591 *  
as.factor(outs)3                 0.481057   0.347832   1.383 0.166660    
as.factor(count)0-1              0.679513   0.006855  99.133  < 2e-16 ***
as.factor(count)0-2              0.861786   0.008665  99.459  < 2e-16 ***
as.factor(count)0-3              6.661261  16.184717   0.412 0.680649    
as.factor(count)1-0              0.515509   0.007276  70.853  < 2e-16 ***
as.factor(count)1-1              0.885498   0.007327 120.857  < 2e-16 ***
as.factor(count)1-2              1.110702   0.007626 145.643  < 2e-16 ***
as.factor(count)2-0              0.551275   0.010902  50.565  < 2e-16 ***
as.factor(count)2-1              1.021391   0.009249 110.429  < 2e-16 ***
as.factor(count)2-2              1.352774   0.008157 165.834  < 2e-16 ***
as.factor(count)3-0             -1.269933   0.027751 -45.762  < 2e-16 ***
as.factor(count)3-1              0.959840   0.013116  73.183  < 2e-16 ***
as.factor(count)3-2              1.599787   0.010109 158.257  < 2e-16 ***
as.factor(count)4-1              7.301061  16.180978   0.451 0.651836    
zone_speed                      -0.044683   0.002757 -16.209  < 2e-16 ***
induced_vert_break              -0.056943   0.002822 -20.176  < 2e-16 ***
horz_break                       0.001617   0.001665   0.971 0.331384    
plate_height                     0.050533   0.015248   3.314 0.000919 ***
plate_side                       0.019249   0.018015   1.069 0.285282    
as.factor(pitch_type)CU         -0.352748   0.012193 -28.931  < 2e-16 ***
as.factor(pitch_type)FA         -0.166502   0.008633 -19.287  < 2e-16 ***
as.factor(pitch_type)KN          0.153555   0.048009   3.198 0.001382 **
as.factor(pitch_type)SI         -0.146233   0.010049 -14.551  < 2e-16 ***
as.factor(pitch_type)SL         -0.136814   0.008178 -16.729  < 2e-16 ***
rel_height                       0.064898   0.015602   4.160 3.19e-05 ***
rel_side                         0.007743   0.018114   0.427 0.669042    
extension                       -0.057658   0.004830 -11.936  < 2e-16 ***
vert_release_angle              -0.267525   0.016243 -16.470  < 2e-16 ***
vert_approach_angle              0.326020   0.016219  20.102  < 2e-16 ***
horz_approach_angle              0.004581   0.016736   0.274 0.784302    
as.factor(recent_callup100)TRUE  0.046294   0.012600   3.674 0.000239 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1515183  on 1096508  degrees of freedom
Residual deviance: 1440514  on 1096471  degrees of freedom
  (1217 observations deleted due to missingness)
AIC: 1440590

Number of Fisher Scoring iterations: 5

pdata2 <- predict(logit100, newdata = df_p2, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = df_p2$is_swing)

Confusion Matrix and Statistics

          Reference
Prediction      0      1
         0 388304 233911
         1 196591 277703

               Accuracy : 0.6074          
                 95% CI : (0.6065, 0.6083)
    No Information Rate : 0.5334          
    P-Value [Acc > NIR] : < 2.2e-16       

                  Kappa : 0.2076          

 Mcnemar's Test P-Value : < 2.2e-16       

            Sensitivity : 0.6639          
            Specificity : 0.5428          
         Pos Pred Value : 0.6241          
         Neg Pred Value : 0.5855          
             Prevalence : 0.5334          
         Detection Rate : 0.3541          
   Detection Prevalence : 0.5675          
      Balanced Accuracy : 0.6033          

       'Positive' Class : 0               
```

Results indicate that being called up in the last 100 pitches is significant at a .001 level. However, the model only has an accuracy rate of 60.74%, not much of an improvement over blind guessing (53.34% swings).

### Model improvement

We now try a number of ways to improve the model. First we take the absolute value of all horizontal break and location measures to accurately account for pitcher handedness. Next we normalize plate_height to distance from the y-center of the zone.

```r
logit100updated <- glm(is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) + as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break + abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) + as.factor(pitch_type)  + rel_height + abs(rel_side)  + extension + vert_approach_angle + abs(horz_approach_angle) + as.factor(recent_callup100), data = df_p2, family = "binomial")
summary(logit100updated)

Call:
glm(formula = is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) +
    as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break +
    abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) +
    as.factor(pitch_type) + rel_height + abs(rel_side) + extension +
    vert_approach_angle + abs(horz_approach_angle) + as.factor(recent_callup100),
    family = "binomial", data = df_p2)

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-2.6296  -0.8584  -0.2241   0.8561   5.5662  

Coefficients:
                                  Estimate Std. Error  z value Pr(>|z|)    
(Intercept)                      0.5068970  0.0738575    6.863 6.73e-12 ***
as.factor(pitcher_side)Right     0.0236837  0.0055337    4.280 1.87e-05 ***
as.factor(pitcher_side)S        -0.1313278  0.3481332   -0.377  0.70600    
as.factor(batter_side)Right      0.0651939  0.0046961   13.883  < 2e-16 ***
as.factor(batter_side)S         -1.1687334  0.9924357   -1.178  0.23894    
as.factor(outs)1                 0.0321804  0.0055731    5.774 7.73e-09 ***
as.factor(outs)2                 0.0149611  0.0056383    2.653  0.00797 **
as.factor(outs)3                 0.1233684  0.4060969    0.304  0.76129    
as.factor(count)0-1              1.0458018  0.0079724  131.177  < 2e-16 ***
as.factor(count)0-2              1.7711613  0.0106574  166.191  < 2e-16 ***
as.factor(count)0-3              7.1530588 16.1860754    0.442  0.65854    
as.factor(count)1-0              0.5945484  0.0082524   72.046  < 2e-16 ***
as.factor(count)1-1              1.1923622  0.0085047  140.201  < 2e-16 ***
as.factor(count)1-2              1.8792194  0.0092996  202.076  < 2e-16 ***
as.factor(count)2-0              0.5800086  0.0123538   46.950  < 2e-16 ***
as.factor(count)2-1              1.2434878  0.0106766  116.469  < 2e-16 ***
as.factor(count)2-2              1.9372676  0.0097746  198.194  < 2e-16 ***
as.factor(count)3-0             -1.6056270  0.0291417  -55.097  < 2e-16 ***
as.factor(count)3-1              1.0583205  0.0149532   70.775  < 2e-16 ***
as.factor(count)3-2              1.9801410  0.0118090  167.680  < 2e-16 ***
as.factor(count)4-1              5.8865668 16.1809788    0.364  0.71601    
zone_speed                       0.0142146  0.0008382   16.958  < 2e-16 ***
induced_vert_break               0.0014941  0.0006265    2.385  0.01709 *  
abs(horz_break)                 -0.0047736  0.0005748   -8.304  < 2e-16 ***
abs(plate_height - 2.55)        -1.5238067  0.0047746 -319.150  < 2e-16 ***
abs(plate_side)                 -2.0366886  0.0056774 -358.733  < 2e-16 ***
as.factor(pitch_type)CU         -0.6463817  0.0148978  -43.388  < 2e-16 ***
as.factor(pitch_type)FA         -0.3794398  0.0102592  -36.985  < 2e-16 ***
as.factor(pitch_type)KN         -0.3168596  0.0553838   -5.721 1.06e-08 ***
as.factor(pitch_type)SI         -0.3583327  0.0116182  -30.842  < 2e-16 ***
as.factor(pitch_type)SL         -0.2627492  0.0112135  -23.432  < 2e-16 ***
rel_height                      -0.0388431  0.0057030   -6.811 9.69e-12 ***
abs(rel_side)                    0.0375606  0.0039288    9.560  < 2e-16 ***
extension                       -0.0262398  0.0048867   -5.370 7.89e-08 ***
vert_approach_angle             -0.0573945  0.0027093  -21.184  < 2e-16 ***
abs(horz_approach_angle)        -0.0253998  0.0027802   -9.136  < 2e-16 ***
as.factor(recent_callup100)TRUE  0.0225541  0.0145650    1.549  0.12150    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 1515183  on 1096508  degrees of freedom
Residual deviance: 1135759  on 1096472  degrees of freedom
  (1217 observations deleted due to missingness)
AIC: 1135833

Number of Fisher Scoring iterations: 5

pdata2 <- predict(logit100updated, newdata = df_p2, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = df_p2$is_swing)

Confusion Matrix and Statistics

          Reference
Prediction      0      1
         0 439522 138656
         1 145373 372958

               Accuracy : 0.741           
                 95% CI : (0.7401, 0.7418)
    No Information Rate : 0.5334          
    P-Value [Acc > NIR] : < 2.2e-16       

                  Kappa : 0.48            

 Mcnemar's Test P-Value : < 2.2e-16       

            Sensitivity : 0.7515          
            Specificity : 0.7290          
         Pos Pred Value : 0.7602          
         Neg Pred Value : 0.7195          
             Prevalence : 0.5334          
         Detection Rate : 0.4008          
   Detection Prevalence : 0.5273          
      Balanced Accuracy : 0.7402          

       'Positive' Class : 0           
```

Accuracy rate jumps way up to 74.1%, and AIC falls from 1440518 to 1135774. More importantly, being called up in the last 100 pitches is now not significant at any level.

### Pitch type partitions

In a final attempt to improve the model, we partition based on pitch_type.

```r
df_p2_fa <- df_p2 %>% filter(pitch_type == "FA")
logit100fa <- glm(is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) + as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break + abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) + rel_height + abs(rel_side) + extension + vert_approach_angle + abs(horz_approach_angle) + as.factor(recent_callup100), data = df_p2_fa, family = "binomial")

summary(logit100fa)

Call:
glm(formula = is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) +
    as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break +
    abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) +
    rel_height + abs(rel_side) + extension + vert_approach_angle +
    abs(horz_approach_angle) + as.factor(recent_callup100), family = "binomial",
    data = df_p2_fa)

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-2.8758  -0.7798  -0.1392   0.7819   6.3408  

Coefficients:
                                 Estimate Std. Error  z value Pr(>|z|)    
(Intercept)                      6.859747   0.124760   54.983  < 2e-16 ***
as.factor(pitcher_side)RHP       0.108765   0.008140   13.362  < 2e-16 ***
as.factor(batter_side)Right      0.067203   0.006875    9.774  < 2e-16 ***
as.factor(batter_side)S          1.122767   1.282724    0.875    0.381    
as.factor(outs)1                 0.053678   0.008163    6.576 4.84e-11 ***
as.factor(outs)2                 0.056459   0.008290    6.810 9.74e-12 ***
as.factor(outs)3                -0.314573   0.644767   -0.488    0.626    
as.factor(count)0-1              1.033245   0.012281   84.133  < 2e-16 ***
as.factor(count)0-2              1.609061   0.016056  100.217  < 2e-16 ***
as.factor(count)1-0              0.685201   0.011805   58.043  < 2e-16 ***
as.factor(count)1-1              1.239317   0.013115   94.493  < 2e-16 ***
as.factor(count)1-2              1.781900   0.014501  122.884  < 2e-16 ***
as.factor(count)2-0              0.635401   0.016139   39.371  < 2e-16 ***
as.factor(count)2-1              1.371385   0.015189   90.289  < 2e-16 ***
as.factor(count)2-2              1.946853   0.014956  130.176  < 2e-16 ***
as.factor(count)3-0             -1.844073   0.033325  -55.336  < 2e-16 ***
as.factor(count)3-1              1.153716   0.019014   60.678  < 2e-16 ***
as.factor(count)3-2              2.157658   0.016658  129.528  < 2e-16 ***
zone_speed                      -0.050921   0.001330  -38.292  < 2e-16 ***
induced_vert_break              -0.061300   0.001053  -58.234  < 2e-16 ***
abs(horz_break)                 -0.009922   0.000965  -10.283  < 2e-16 ***
abs(plate_height - 2.55)        -2.135521   0.008271 -258.208  < 2e-16 ***
abs(plate_side)                 -2.477219   0.009019 -274.657  < 2e-16 ***
rel_height                       0.493782   0.009673   51.050  < 2e-16 ***
abs(rel_side)                    0.070237   0.006152   11.417  < 2e-16 ***
extension                       -0.093928   0.007308  -12.853  < 2e-16 ***
vert_approach_angle              0.390999   0.004446   87.935  < 2e-16 ***
abs(horz_approach_angle)        -0.126821   0.004717  -26.886  < 2e-16 ***
as.factor(recent_callup100)TRUE -0.004018   0.021993   -0.183    0.855    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 759404  on 548929  degrees of freedom
Residual deviance: 529117  on 548901  degrees of freedom
  (279 observations deleted due to missingness)
AIC: 529175

Number of Fisher Scoring iterations: 5

pdata2 <- predict(logit100fa, newdata = df_p2_fa, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = df_p2_fa$is_swing)

Confusion Matrix and Statistics

          Reference
Prediction      0      1
         0 221753  61124
         1  67407 198646

               Accuracy : 0.7659         
                 95% CI : (0.7647, 0.767)
    No Information Rate : 0.5268         
    P-Value [Acc > NIR] : < 2.2e-16      

                  Kappa : 0.5309         

 Mcnemar's Test P-Value : < 2.2e-16      

            Sensitivity : 0.7669         
            Specificity : 0.7647         
         Pos Pred Value : 0.7839         
         Neg Pred Value : 0.7466         
             Prevalence : 0.5268         
         Detection Rate : 0.4040         
   Detection Prevalence : 0.5153         
      Balanced Accuracy : 0.7658         

       'Positive' Class : 0              
```

When considering only fastballs, accuracy jumps to 76.59%. Being recently called up is certainly not statistically significant, with a p-value of .855.

We now try the other pitch types to see if recently called up batters' swing discipline is different for some pitches. The other pitch models all have similar accuracy rates, but sure enough, we do see a significantly positive effect of recently being called up on swings for change-ups only.  

```r
df_p2_ch <- df_p2 %>% filter(pitch_type == "CH") #change-ups
logit100ch <- glm(is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) + as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break + abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) + rel_height + abs(rel_side) + extension + vert_approach_angle + abs(horz_approach_angle) + as.factor(recent_callup100), data = df_p2_ch, family = "binomial")
summary(logit100ch)

Call:
glm(formula = is_swing ~ as.factor(pitcher_side) + as.factor(batter_side) +
    as.factor(outs) + as.factor(count) + zone_speed + induced_vert_break +
    abs(horz_break) + abs(plate_height - 2.55) + abs(plate_side) +
    rel_height + abs(rel_side) + extension + vert_approach_angle +
    abs(horz_approach_angle) + as.factor(recent_callup100), family = "binomial",
    data = df_p2_ch)

Deviance Residuals:
    Min       1Q   Median       3Q      Max  
-2.8591  -0.8057   0.2372   0.8012   5.2847  

Coefficients:
                                 Estimate Std. Error  z value Pr(>|z|)    
(Intercept)                     -0.334252   0.227592   -1.469  0.14193    
as.factor(pitcher_side)Right    -0.016514   0.018773   -0.880  0.37903    
as.factor(pitcher_side)S        -0.810201   0.635885   -1.274  0.20262    
as.factor(batter_side)Right      0.001321   0.016141    0.082  0.93478    
as.factor(batter_side)S         -1.219734   1.972817   -0.618  0.53640    
as.factor(outs)1                 0.028571   0.016185    1.765  0.07752 .  
as.factor(outs)2                 0.016600   0.016342    1.016  0.30974    
as.factor(outs)3                 7.614295  32.949973    0.231  0.81725    
as.factor(count)0-1              0.921496   0.022189   41.530  < 2e-16 ***
as.factor(count)0-2              1.623926   0.032739   49.603  < 2e-16 ***
as.factor(count)1-0              0.502180   0.024079   20.856  < 2e-16 ***
as.factor(count)1-1              1.136692   0.023056   49.302  < 2e-16 ***
as.factor(count)1-2              1.782366   0.026754   66.619  < 2e-16 ***
as.factor(count)2-0              0.395212   0.043162    9.157  < 2e-16 ***
as.factor(count)2-1              1.128700   0.031430   35.912  < 2e-16 ***
as.factor(count)2-2              1.883191   0.028190   66.804  < 2e-16 ***
as.factor(count)3-0             -0.809798   0.201017   -4.028 5.61e-05 ***
as.factor(count)3-1              0.866096   0.062933   13.762  < 2e-16 ***
as.factor(count)3-2              1.908670   0.039146   48.758  < 2e-16 ***
zone_speed                       0.023211   0.002470    9.398  < 2e-16 ***
induced_vert_break               0.016567   0.001911    8.668  < 2e-16 ***
abs(horz_break)                 -0.005788   0.001748   -3.311  0.00093 ***
abs(plate_height - 2.55)        -2.213110   0.017357 -127.504  < 2e-16 ***
abs(plate_side)                 -2.116919   0.016075 -131.693  < 2e-16 ***
rel_height                      -0.283839   0.019766  -14.360  < 2e-16 ***
abs(rel_side)                    0.180457   0.010526   17.145  < 2e-16 ***
extension                       -0.059157   0.013493   -4.384 1.16e-05 ***
vert_approach_angle             -0.395974   0.010847  -36.505  < 2e-16 ***
abs(horz_approach_angle)        -0.326973   0.009790  -33.400  < 2e-16 ***
as.factor(recent_callup100)TRUE  0.121861   0.042915    2.840  0.00452 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 195387  on 140946  degrees of freedom
Residual deviance: 138761  on 140917  degrees of freedom
  (76 observations deleted due to missingness)
AIC: 138821

Number of Fisher Scoring iterations: 8

pdata2 <- predict(logit100ch, newdata = df_p2_ch, type = "response")
confusionMatrix(data = as.factor(as.numeric(pdata2>0.5)), reference = df_p2_ch$is_swing)

Confusion Matrix and Statistics

          Reference
Prediction     0     1
         0 51155 15380
         1 18836 55576

               Accuracy : 0.7572         
                 95% CI : (0.755, 0.7595)
    No Information Rate : 0.5034         
    P-Value [Acc > NIR] : < 2.2e-16      

                  Kappa : 0.5143         

 Mcnemar's Test P-Value : < 2.2e-16      

            Sensitivity : 0.7309         
            Specificity : 0.7832         
         Pos Pred Value : 0.7688         
         Neg Pred Value : 0.7469         
             Prevalence : 0.4966         
         Detection Rate : 0.3629         
   Detection Prevalence : 0.4721         
      Balanced Accuracy : 0.7571         

       'Positive' Class : 0       


# Pull the percent change in odds of a swing for change-ups

percents <- logit100ch$coefficients %>% tibble() %>% mutate(odds = exp(.), percent = (odds -1)* 100 )
percents[[27,3]]
# 12.89139        
```

### Interpretation/Take-aways

As is seen above, it first appears that recently being promoted has no effect on swing patterns for batters who have seen 100 pitches or less at the new level.

However, when considering separate models for each pitch type the results are slightly different. Factors like break, speed, approach angle, etc. all have very different effects for different pitches (a 75mph fastball is a very different pitch than a 75mph curveball), so it makes sense to consider these pitches under different models.

While splitting the models by pitch type doesn’t show evidence of an “adjustment period” for most pitches, batters are more likely to swing at change-ups, all other factors constant, if they have recently been promoted. Under the model, a batter is **12.89% more likely** to swing at a change-up when they were recently called up, compared to if they had not just been recently promoted.

This makes sense — change-ups are a pitch that test hitters’ patience at the plate, and recently promoted batters are more likely to press and have less plate discipline on good change-ups.
