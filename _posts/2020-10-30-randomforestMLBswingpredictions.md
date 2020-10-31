---
title: "Predicting baseball swings with pitch tracking data"
date: 2020-10-30
tags: [Baseball research]
header:
  image: "/images/mookie.jpeg"
excerpt: "Random forest classification predictions"
---

## Background

With Trackman pitch-tracking data, more baseball information is available than ever before.
Today, I'll try to use that information to best predict whether a batter will swing at a given pitch.


## Initial variable construction and data cleaning

I first loaded the data, cleaned out some erroneous observations and made a categorical variable for count and swing.

```r
df <- df %>% mutate(is_swing = as.factor(as.integer((
  pitch_call == "FoulBall" | pitch_call == "InPlay" | pitch_call == "StrikeSwinging"))))
df <- df %>% unite(col = count, balls, strikes, sep = "-") %>% drop_na()

# Check counts
df %>% group_by(count) %>% summarise(ct = n())
# Remove impossible counts
df <-  df %>% filter(count != "0-3", count != "4-1")

# Check outs
df %>% group_by(outs) %>% summarise(ct = n())
# Remove impossible outs
df <-  df %>% filter(outs != 3)

# Check batter handedness
df %>% group_by(batter_side) %>% summarise(ct = n())
# Remove outliers
df <-  df %>% filter(batter_side != "S")
```
## Identify highly correlated variables

Then, I built a correlation matrix between the continuous variables to identity features that were more than 90% correlated with another variable —  these were then omitted from the modeling.

```r
library(caret)
set.seed(12)
nums <- sapply(df, is.numeric)
data.numeric <- df[ , nums] %>% select(-y55)
data.without_na <- na.omit(data.numeric)
cor_matrix <- cor(data.without_na)
high_cor <- findCorrelation(cor_matrix, 0.9, names = TRUE)
print(high_cor)
```
## Splitting data into test and training sets

I then split the training data into a sub-split of test and training, to verify my out-of-bag accuracy rate was consistent and that I’d avoided overfitting after the model was constructed.

```r
library(caTools)
set.seed(1234)
sample = sample.split(df$pitcher_id, SplitRatio = .7)
df_train = subset(df, sample == TRUE)
df_test  = subset(df, sample == FALSE)
```


## Random Forest specification

Random forest was chosen as the modeling method here for a number of reasons:

1. Random forests allow for conditionality that other methods struggle to account for. When predicting swings and non-swings, many of the variables in the provided data are not straightforward predictors. Depending on the pitcher’s handedness, the batter’s handedness and the pitch type, all the Trackman level measurements can have vastly different implications depending on the pitch. Thus, I made the decision that a random forest would be best suited to capturing these conditional probabilities.
2. This task doesn’t require model interpretation — we’re only looking for high predictive power. While techniques like logistic regression are more easily interpretable at a variable-specific level (the reason I used a logit model for problem #2), random forests are not as simple to interpret.
3. The dataset contains numerous important categorical and continuous variables — while random forests are known to favor the selection of continuous features with more options than categorical or binary ones, they do handle both kinds quickly and require almost no preprocessing.
4. Random forests are relatively simple to tune. With the caret package in R the optimal mtry, splitting rule and minimum tree depth can be easily identified.

### Tuning

Using the caret package I then tuned the random forest for the optimal hyperparameters to maximize accuracy.

```r

grid <-  expand.grid(mtry = c(3,4,5), min.node.size = c(1, 2,3), splitrule = "gini")

fitControl <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE)

fit <-  train(
  x = (df_train %>% select(pitcher_side, batter_side, outs, count, vert_release_angle,
  spin_rate, spin_axis, rel_side, rel_height, extension,  induced_vert_break, horz_break,
  plate_height, plate_side, zone_speed, vert_approach_angle, horz_approach_angle, pitch_type)),
  y = as.factor(df_train$is_swing),
  method = 'ranger',
  num.trees = 200,
  #200 folds is specified due to lack of computing power and quickly diminishing increases in accuracy after 100 trees
  tuneGrid = grid,
  trControl = fitControl,
  respect.unordered.factors = TRUE,
  seed = 1234
)
print(fit)

```

### Training model

Maximum CV accuracy was

```r

library(ranger)
modelfinal <- ranger(
  formula = is_swing ~ pitcher_side + batter_side + outs + count + vert_release_angle + spin_rate+ spin_axis + rel_side + rel_height + extension +  induced_vert_break + horz_break + plate_height + plate_side + zone_speed + vert_approach_angle + horz_approach_angle + pitch_type,
  data = df_train,
  num.trees = 200,
  mtry = 5,
  min.node.size = 3,
  splitrule = "gini",
  respect.unordered.factors = TRUE,
  seed = 1234)




predicts <- predict(modelfinalquestionmark, data = _test)
cf<- table(df_test$is_swing, predicts1$predictions)
sum(cf[1,1], cf[2,2])/sum(cf[1,1],cf[1,2],cf[2,1],cf[2,2])
Accuracy on test set is 0.7785082

```

### Further Considerations

To expand on this modeling approach, I would like to include controls for batter-level features like height, walk rate and other performance metrics.
I would also include information on the previous pitch to see if it has a predictive effect on batter swings.
