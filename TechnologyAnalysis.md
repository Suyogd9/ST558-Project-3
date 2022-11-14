ST 558 Project 3: Technology Analysis Report
================
Suyog Dharmadhikari; Bennett McAuley
2022-11-02

## Introduction

The goal of this report is to create and compare predictive models using
[Online News
Popularity](https://archive.ics.uci.edu/ml/datasets/Online+News+Popularity)
data from the UCI Machine Learning Repository.

This data contains information about articles published by Mashable over
a period of two years–specifically the statistics associated with them.

The purpose of this analysis is to use linear regression and ensemble
tree-based methods to predict the number of `shares`–our target
variable–subset by data channel. For more details about these methods,
see the *Modeling* section.

According to the data description, there are 39,797 observations and 61
variables present. 58 of them are predictors, but using all of them
would be inefficient and show glaring redundancies. Instead, the ones
that will be used were chosen intuitively–what might encourage a user to
share an article, what underlying forces may influence them, and what
would make for good exploratory analysis:

1.  `average_token_length` - Average length of the words in the content
2.  `avg_negative_polarity` - Avg. polarity of negative words
3.  `global_rate_positive_words` - Rate of positive words in the content
4.  `global_sentiment_polarity` - Text sentiment polarity
5.  `global_subjectivity` - Text subjectivity
6.  `is_weekend` - Was the article published on the weekend?
7.  `kw_avg_max` - Best keyword (avg. shares)
8.  `kw_avg_min` - Worst Keyword (avg. shares)
9.  `kw_max_max` - Best keyword (max shares)
10. `kw_min_min` - Worst keyword (min shares)
11. `n_non_stop_words` - Rate of non-stop words in the content
12. `n_tokens_content` - Number of words in the content
13. `n_tokens_title` - Number of words in the title
14. `n_unique_tokens` - Rate of unique words in the content
15. `num_hrefs` - Number of links
16. `num_imgs` - Number of images
17. `num_self_hrefs` - Number of links to other articles published by
    Mashable
18. `num_videos` - Number of videos
19. `self_reference_avg_shares` - Avg. shares of referenced articles in
    Mashable
20. `title_sentiment_polarity` - Title polarity
21. `weekday_is_monday` - Was the article published on a Monday?
22. `weekday_is_tuesday` - Was the article published on a Tuesday?
23. `weekday_is_wednesday` - Was the article published on a Wednesday?
24. `weekday_is_thursday` - Was the article published on a Thursday?
25. `weekday_is_friday` - Was the article published on a Friday?
26. `weekday_is_saturday` - Was the article published on a Saturday?
27. `weekday_is_sunday` - Was the article published on a Sunday?

Which we store in a vector for fast retrieval:

``` r
news.vars <- c('n_tokens_title', 'n_tokens_content', 'n_non_stop_words', 'num_hrefs',
               'num_self_hrefs', 'num_imgs', 'num_videos', 'average_token_length',
               'kw_max_max', 'kw_min_min', 'kw_avg_max', 'kw_avg_min',
               'self_reference_avg_sharess','is_weekend', 'global_subjectivity', 'global_sentiment_polarity',
               'global_rate_positive_words', 'avg_negative_polarity', 'n_unique_tokens', 'title_sentiment_polarity',
               'weekday_is_sunday', 'weekday_is_monday', 'weekday_is_tuesday', 'weekday_is_wednesday',
               'weekday_is_thursday', 'weekday_is_friday', 'weekday_is_saturday'
               )
```

## The Data

For this analysis, we will consider a single source for the data
channel. In other words, we subset the data to the *Technology* channel.

So, we read in the data, perform the filter, and convert `is_weekend`
into a binary factor:

``` r
news <- read_csv("OnlineNewsPopularity.csv")
```

    ## Rows: 39644 Columns: 61
    ## ── Column specification ───────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (1): url
    ## dbl (60): timedelta, n_tokens_title, n_tokens_content, n_unique_tokens, n_non_stop_words, n_non...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
fltr <- paste0("data_channel_is_", params$channel)

news.tbl <- news %>% filter(get(fltr) == 1) %>% 
  select(all_of(news.vars), shares) %>%
  mutate(is_weekend = as.factor(is_weekend))

head(news.tbl)
```

    ## # A tibble: 6 × 28
    ##   n_token…¹ n_tok…² n_non…³ num_h…⁴ num_s…⁵ num_i…⁶ num_v…⁷ avera…⁸ kw_ma…⁹ kw_mi…˟ kw_av…˟ kw_av…˟
    ##       <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1        13    1072    1.00      19      19      20       0    4.68       0       0       0       0
    ## 2        10     370    1.00       2       2       0       0    4.36       0       0       0       0
    ## 3        12     989    1.00      20      20      20       0    4.62       0       0       0       0
    ## 4        11      97    1.00       2       0       0       0    4.86       0       0       0       0
    ## 5         8    1207    1.00      24      24      42       0    4.72       0       0       0       0
    ## 6        13    1248    1.00      21      19      20       0    4.69       0       0       0       0
    ## # … with 16 more variables: self_reference_avg_sharess <dbl>, is_weekend <fct>,
    ## #   global_subjectivity <dbl>, global_sentiment_polarity <dbl>, global_rate_positive_words <dbl>,
    ## #   avg_negative_polarity <dbl>, n_unique_tokens <dbl>, title_sentiment_polarity <dbl>,
    ## #   weekday_is_sunday <dbl>, weekday_is_monday <dbl>, weekday_is_tuesday <dbl>,
    ## #   weekday_is_wednesday <dbl>, weekday_is_thursday <dbl>, weekday_is_friday <dbl>,
    ## #   weekday_is_saturday <dbl>, shares <dbl>, and abbreviated variable names ¹​n_tokens_title,
    ## #   ²​n_tokens_content, ³​n_non_stop_words, ⁴​num_hrefs, ⁵​num_self_hrefs, ⁶​num_imgs, ⁷​num_videos, …

## Summarizations

Now that our data has been manipulated and filtered, we can perform a
simple EDA.

### Summary Statistics

First and foremost, we want to know what range of values we might expect
when predicting for `shares`. The following table shows basic summary
statistics about the variable:

``` r
s <- summary(news.tbl$shares)

tibble(var = 'shares', min = s[1], max = s[6], mean = s[3], stddev = sd(news.tbl$shares))
```

    ## # A tibble: 1 × 5
    ##   var    min     max     mean    stddev
    ##   <chr>  <table> <table> <table>  <dbl>
    ## 1 shares 36      663600  1700     9024.

If the maximum is tremendously larger in scale than the mean, then it is
very likely an outlier. If the standard deviation is larger than the
mean, then the number of shares varies greatly. Furthermore, the higher
it is, the more disperse they are from the mean. The inverse also
applies for a smaller standard deviation (i.e. the smaller, the less
disperse).

It would also be good to know relationships between some of the
variables present–and not just to `shares`. Observe the following
correlation matrix of variables related to content sentiment. A value of
0 indicates no correlation and 1 indicates perfect correlation. The
closer a value is to 1(+/-), the stronger the relationship is. The
closer to 0, the weaker.

``` r
c <- news.tbl %>%
  select(avg_negative_polarity,
         global_rate_positive_words,
         global_sentiment_polarity,
         global_subjectivity,
         shares)

round(cor(c),2)
```

    ##                            avg_negative_polarity global_rate_positive_words
    ## avg_negative_polarity                       1.00                      -0.04
    ## global_rate_positive_words                 -0.04                       1.00
    ## global_sentiment_polarity                   0.27                       0.55
    ## global_subjectivity                        -0.26                       0.32
    ## shares                                     -0.03                      -0.02
    ##                            global_sentiment_polarity global_subjectivity shares
    ## avg_negative_polarity                           0.27               -0.26  -0.03
    ## global_rate_positive_words                      0.55                0.32  -0.02
    ## global_sentiment_polarity                       1.00                0.34  -0.02
    ## global_subjectivity                             0.34                1.00   0.01
    ## shares                                         -0.02                0.01   1.00

The new variable **popularity** is set up so that news is considered
`popular` if it receives more shares than the median shares. Otherwise,
it is considered `unpopular`.`table()` gives details about **count** of
popular and unpopular news.

``` r
news.tbl$popularity <- ifelse(news.tbl$shares < mean(news.tbl$shares),
                              "Unpopular", "Popular")

table(news.tbl$popularity)
```

    ## 
    ##   Popular Unpopular 
    ##      1803      5543

The contingency table below depicts the relationship between the
**popularity** of a news channel and whether or not it was **published
on the weekend**. It informs us whether or not popular news was
published over the weekend. Whether or not unpopular news was published
over the weekend.

``` r
table(news.tbl$popularity,news.tbl$is_weekend)
```

    ##            
    ##                0    1
    ##   Popular   1471  332
    ##   Unpopular 4954  589

### Plots

For visual analysis, we first construct a density plot to compare the
densities of `shares` between the weekend (`is_weekend = 1`) and
weekdays (`is_weekend = 0`). Observe the patterns generated for
similarities and/or differences.

``` r
g <- ggplot(data = news.tbl, aes(x = shares))

g + geom_density(adjust = 0.5, alpha = 0.5, aes(fill = is_weekend)) +
  xlim(0, 10000) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Density Plot for Shares",
       subtitle = paste("channel = ", params$channel),
       x = "# of Shares",
       y = "Density")
```

    ## Warning: Removed 307 rows containing non-finite values (stat_density).

![](TechnologyAnalysis_files/figure-gfm/density-1.png)<!-- -->

The second visual is a scatterplot with a trend line. The number of
words in the content is on the x-axis and the rate of unique words on
the y-axis. If the data points and trend line show an upward trend, then
articles with more words tend to have a larger rate of unique ones. If
there is a downward trend, then articles with more words tend to have a
smaller rate of unique ones.

``` r
p <- ggplot(news.tbl, aes(x = n_tokens_content, y = n_unique_tokens))

p + geom_point() +
  geom_smooth(method = glm, col = "Blue") +
  labs(title = "Scatterplot for n_tokens_content vs n_unique_tokens",
       subtitle = paste("channel = ", params$channel),
       x = "# of Words in Article",
       y = "Rate of Unique Words in Article")
```

    ## `geom_smooth()` using formula 'y ~ x'

![](TechnologyAnalysis_files/figure-gfm/scatter-1.png)<!-- -->

This subsequent bar plot shows a comparison between the total number of
articles published on each day of the week and the total shares of those
articles. Observe any interestingly sized columns that one might think
would be higher or lower than others. Articles published on which day
tend to get shared the most? Which one gets the least?

``` r
plt <- news.tbl %>% pivot_longer(cols = starts_with("weekday_is_"),
               names_to = "day",
               names_prefix = "weekday_is_",
               names_transform = as.factor,
               values_to = "count") %>%
  filter(count == 1) %>%
  select(-count, -starts_with("weekday_is_"))

#Reordering days
plt$day <- factor(plt$day,
    levels = c("monday", "tuesday", "wednesday", "thursday",
               "friday", "saturday", "sunday"))


plt <- plt %>%
  select(day, shares) %>%
  group_by(day) %>%
  summarise(articles = n(), shares = sum(shares))

plt <- reshape2::melt(plt, id.vars = c("day"))

p <- ggplot(plt, aes(x = day, y = value, fill = day))

p + geom_col() +
  facet_wrap(vars(variable), scales = "free") +
  scale_y_continuous(labels = scales::comma) +
  guides(x = guide_axis(angle = 45), fill = "none") +
  labs(title = "Total # of Articles vs Total # of Shares per Publishing Day",
       subtitle = paste("channel = ", params$channel),
       x = "Day",
       y = '')
```

![](TechnologyAnalysis_files/figure-gfm/box-1.png)<!-- -->

The code chunk below, tells us about the number of shares of a
particular published news with number of images in the published news.
It also tells us about the popularity of the news published with images.

``` r
ggplot(news.tbl, aes(fill= popularity, y=shares, x=num_imgs)) +
  geom_bar(position='dodge', stat='identity',  width = 0.98) +
  ggtitle('Relation between number of images and shares') +
  xlab('Number of images in the news published') +
  ylab('Shares') +
  ylim(0, 100000)
```

    ## Warning: Removed 2 rows containing missing values (geom_bar).

![](TechnologyAnalysis_files/figure-gfm/barPlot-1.png)<!-- -->

Correlation plots are used to find out the **correlation** between two
variables in the given data set. In the code chunk below, we have used
`ggcorrplt()` to plot the correlation between all the variables in the
data set and identify the most **correlated variables**. A **violet**
color circle shows there is **negative** correlation between the two
respective variables and a **dark orange** color is **positive**
correlation between two variables.

``` r
corr <- cor(select_if(news.tbl, is.numeric))

ggcorrplot(corr,
           method = "circle",
           type = "lower",
           outline.color = "black",
           lab_size = 6)
```

![](TechnologyAnalysis_files/figure-gfm/corrPlot-1.png)<!-- -->

In the code chunk a scatter plot with a trend line. The number of words
in the content is on the x-axis and the number of shares on the y-axis.
If the data points and trend line show an upward trend, then articles
with more words tend to have more number of shares. If there is a
downward trend, then articles with more words tend to have a smaller
number of shares.

``` r
ggplot(news.tbl, aes(y=shares, x=n_tokens_content)) + 
    geom_point(aes(color=popularity)) +
    geom_smooth() + 
    ylim(0, 100000)
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](TechnologyAnalysis_files/figure-gfm/scattPlot-1.png)<!-- -->

## Modeling

The goal now is to create models for predicting the number of `shares`
using linear regression and ensemble trees. Before that, the data is
split into a training (70%) and test (30%) set.

``` r
i <- createDataPartition(news.tbl[[1]], p = 0.7, list = FALSE)

train <- news.tbl[i,]
test <- news.tbl[-i,]
```

### Linear Regression

Linear regression is a statistical technique that aims to demonstrate a
relationship between variables. A dependent variable’s nature and degree
of connection with a group of independent variables are assessed using
linear regression. Symbolically:
$$Y_i = \beta_0 + \beta_1 x_i + \epsilon_i$$ where $Y_i$ is the
dependent variable/response value, $\beta_0$ is the intercept, and
$\beta_1$ is the slope coefficient for observation value $x_i$

We can make predictions of `shares` using linear regression models.

#### Model 1

The linear regression model is fit using variables chosen from forward
stepwise selection. The leaps library’s `regsubsets()` function performs
best subset selection by identifying the best model with a given number
of predictors, where best is quantified using $RSS$. The syntax is
identical to that of `lm()`. The summary() command returns the best set
of variables for each model size. In the code chunk below, we find the
best predictors subset, then $R^2$ value is used to determine the number
of predictors that can give best model results. Then we create a formula
that can be used to train the model by using `as.formula()` and
`paste0()`.

``` r
fwdVars <- regsubsets(shares ~ ., train, method = "forward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in, : 2 linear
    ## dependencies found

    ## Reordering variables and trying again:

``` r
fwdSumm <- summary(fwdVars)

adjRNum <- which.max(fwdSumm$adjr2)

bestModels <- fwdSumm$which[adjRNum,-1]

varNames <- names(which(bestModels == TRUE))

predictorFormulaPt2 <- paste(varNames, collapse = "+")

modelFormula <- as.formula(paste0("shares", "~", predictorFormulaPt2))

modelFormula
```

    ## shares ~ n_tokens_title + n_tokens_content + num_hrefs + num_self_hrefs + 
    ##     num_imgs + kw_avg_max + global_rate_positive_words + weekday_is_wednesday + 
    ##     popularityUnpopular
    ## <environment: 0x000002049a07a298>

In the code chunk below, firstly we have converted categorical variables
into numerical variables using `dummyVars()`. `gsub()` is used to rename
the column names in order to match with the column names generated by
`regsubsets()` in the function above. The `caret` package was used to
train a linear regression model by using the best predictors obtained
from `regsubsets()`. We have standardized the model using
`center and scale`. Cross validation is used to avoid over fitting and
add generalization in our model.

``` r
# convert categorical variables into numeric variables i.e one hot encoding
dummies <- dummyVars(shares ~ is_weekend+popularity, data = train)

trainDF <- as_tibble(predict(dummies, newdata = train)) %>%
    bind_cols(select(train,-is_weekend,-popularity))

# Replace . with spaces in column names to match the output names from regsubsets
names(trainDF) <- gsub(x = names(trainDF), pattern = "\\.", replacement = "") 

forwardFit <- train(modelFormula,
                    data = trainDF,
                    method = "lm",
                    metric = "Rsquared",
                    preProcess = c("center","scale"),
                    trControl = trainControl(method = "cv", number = 10))

summary(forwardFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -13381  -1133   -144    650 650022 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                  3127.0      139.5  22.413  < 2e-16 ***
    ## n_tokens_title                241.9      142.7   1.695 0.090113 .  
    ## n_tokens_content              663.9      179.8   3.693 0.000224 ***
    ## num_hrefs                     630.7      185.5   3.400 0.000679 ***
    ## num_self_hrefs               -377.5      174.6  -2.162 0.030675 *  
    ## num_imgs                     -428.5      167.7  -2.555 0.010658 *  
    ## kw_avg_max                   -160.5      142.5  -1.126 0.260172    
    ## global_rate_positive_words   -312.8      142.2  -2.200 0.027823 *  
    ## weekday_is_wednesday          254.6      139.8   1.822 0.068518 .  
    ## popularityUnpopular         -2777.9      140.8 -19.734  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10010 on 5133 degrees of freedom
    ## Multiple R-squared:  0.08392,    Adjusted R-squared:  0.08232 
    ## F-statistic: 52.25 on 9 and 5133 DF,  p-value: < 2.2e-16

Now we use the model to make predictions:

``` r
# convert categorical variables into numeric variables i.e one hot encoding
dummies <- dummyVars(shares ~ is_weekend+popularity, data = test)

testDF <- as_tibble(predict(dummies, newdata = test)) %>%
    bind_cols(select(test,-is_weekend,-popularity))

  predLR <- predict(forwardFit,newdata = testDF)
  
  predResultsFwd <- postResample(predLR,obs = testDF$shares)
  
  predResultsFwd
```

    ##         RMSE     Rsquared          MAE 
    ## 3403.2114754    0.3433277 1667.2762052

#### Model 2

This second linear regression model is fit using variables chosen from
*backwards* stepwise selection.

The `regsubsets` function is called, and the summary is returned to grab
the models of $n$ terms that have the optimal values for $R^2$, Mallows’
Cp, and BIC. As performance will vary, the mode value between the three
is chosen as the ‘best’.

``` r
backVars <- regsubsets(shares ~ ., train, method = "backward")
```

    ## Warning in leaps.setup(x, y, wt = wt, nbest = nbest, nvmax = nvmax, force.in = force.in, : 2 linear
    ## dependencies found

    ## Reordering variables and trying again:

``` r
backSumm <- summary(backVars, all.best = FALSE)

met <- c(R2 = which.max(backSumm$rsq), 
     Cp = which.min(backSumm$cp),
     BIC = which.min(backSumm$bic)
)

M <- which.max(tabulate(met))
```

The most frequent number is 2. The variables present in this size model
are:

``` r
terms <- coef(backVars, M)
terms
```

    ##         (Intercept)    n_tokens_content weekday_is_saturday 
    ##         2132.996606            1.669484          586.216795

Now, the model can be trained. The coefficient names are grabbed from
the backwards selection, and input as a subset of the training data. The
variables are standardized to account for the vastly different
magnitudes between them, and to prevent any extreme coefficient values
because of this.

``` r
prd <- names(terms)

#Assuming these terms appear in the model, but just a warning otherwise
prd <- replace(prd, prd %in% c('(Intercept)', 'is_weekend1'), c('shares', 'is_weekend'))
```

    ## Warning in x[list] <- values: number of items to replace is not a multiple of replacement length

``` r
BackwardFit <- train(shares ~ .,
                    data = train[,prd],
                    method = "lm",
                    metric = "Rsquared",
                    preProcess = c("scale", "center"),
                    trControl = trainControl(method = "cv", number = 10))

summary(BackwardFit)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -8765  -1859  -1237    -22 657141 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           3127.0      145.2  21.534  < 2e-16 ***
    ## n_tokens_content       810.3      145.4   5.575  2.6e-08 ***
    ## weekday_is_saturday    153.5      145.4   1.056    0.291    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10410 on 5140 degrees of freedom
    ## Multiple R-squared:  0.006334,   Adjusted R-squared:  0.005948 
    ## F-statistic: 16.38 on 2 and 5140 DF,  p-value: 8.081e-08

The model is then tested against the testing data:

``` r
predLRBck <- predict(BackwardFit, newdata = test)
  
predResultsBck <- postResample(predLRBck, obs = test$shares)
predResultsBck
```

    ##         RMSE     Rsquared          MAE 
    ## 4.146634e+03 4.021955e-03 2.257755e+03

### Random Forest

Random forest uses the same idea as bagging tree models. It is a
bootstrap aggregation method, but the difference is that random forests
*randomly* select a subset of predictors instead of using all the
predictors. This is because if a really strong predictor exists, *every*
bootstrap tree will probably use it for the first split. This will
result in making bagged trees predictions more correlated, and variance
will be large in aggregation. Hence, in random forest we avoid this
limitation by using a random subset of predictors for each bootstrap
sample/tree fit.

In the code chunk below, `caret` package was used to train the rf forest
model and the evaluation metric used for the fit is `Rsquared`. We have
tuned the model by using tuning parameter `mtry` ad used **5 folds cross
validation** for finding best fit.

``` r
#5 folds cross validation
control <- trainControl(method='cv', 
                        number=5)

tunegrid <- data.frame(mtry = 1:9)
rfFit <- train(modelFormula, 
                      data=trainDF, 
                      method='rf', 
                      metric='Rsquared', 
                      tuneGrid=tunegrid, 
                      trControl=control)

rfFit$bestTune
```

    ##   mtry
    ## 1    1

``` r
varImp(rfFit$finalModel) %>% arrange(desc(Overall))
```

    ##                                Overall
    ## n_tokens_content           78157577716
    ## global_rate_positive_words 55500580714
    ## num_hrefs                  49019416629
    ## popularityUnpopular        34808481416
    ## kw_avg_max                 33095410090
    ## n_tokens_title             19054062319
    ## num_self_hrefs             15864340644
    ## weekday_is_wednesday       11917101668
    ## num_imgs                   10762555122

``` r
# convert categorical variables into numeric variables i.e one hot encoding
dummies <- dummyVars(shares ~ is_weekend+popularity, data = test)

testDF <- as_tibble(predict(dummies, newdata = test)) %>%
    bind_cols(select(test,-is_weekend,-popularity))

predRF <- predict(rfFit,newdata = testDF)
  
predResultsRF <- postResample(predRF,obs = testDF$shares)
predResultsRF
```

    ##         RMSE     Rsquared          MAE 
    ## 3417.4680340    0.3127766 1455.1162193

### Boosted Tree Model

Boosting centers around the idea of sequential learning–taking
information from preceding iterations as a basis for calculating the
current iteration. Boosted trees are ensemble tree models consisting of
a group of trees that are built in sequence, including errors from
previous trees for predictions.

Boosted trees also have a unique characteristic of *slow* training. This
is controlled by $\lambda$, a shrinkage parameter. The two other notable
parameters for training a boosted tree are $B$ (the number of times the
algorithm performs its procedure; `n.trees` in R) and $d$ (the number of
splits; `interaction.depth` in R), both of which can be selected with
cross-validation.

For regression, the boosting algorithm proceeds as follows:

1.  The predictions are initialized to 0–$\hat{y}(x)=0$
2.  The residuals between observed and predicted values are calculated
3.  A tree with $d$ splits and $d+1$ terminal nodes is fit using the
    residuals as the response–denoted by $\hat{y}^b(x)$
4.  The overall prediction is updated by recursively adding these
    pseudo-responses–$\hat{y}(x) +=\lambda\hat{y}^b(x)$
5.  Residuals for new predictions are updated
6.  Repeat $B$ times

This model is fit using `cv = 10`, and the `train` function will
determine the optimal values of the parameters for us.

``` r
#Output for this chunk is hidden as it is rather long and does not provide information needed for reporting
boostFit <- train(shares ~ .,
              method = "gbm",
              data = train,
              trControl = trainControl(method = "cv", number = 10))
```

During training, the optimal values for the tuning parameters were found
to be:

``` r
boostFit$bestTune
```

    ##   n.trees interaction.depth shrinkage n.minobsinnode
    ## 4      50                 2       0.1             10

Since coefficients aren’t interpretable for boosted trees, the
calculation of variable importance for the final model is shown instead,
in descending order:

``` r
varImp(boostFit$finalModel) %>% arrange(desc(Overall))
```

    ##                                 Overall
    ## n_unique_tokens            471731562749
    ## popularityUnpopular         94894916220
    ## n_tokens_content            78867187827
    ## global_sentiment_polarity   21170696116
    ## num_hrefs                    6309969044
    ## num_videos                   3477865917
    ## n_non_stop_words             1676922486
    ## kw_avg_max                    746222900
    ## self_reference_avg_sharess    663326827
    ## n_tokens_title                        0
    ## num_self_hrefs                        0
    ## num_imgs                              0
    ## average_token_length                  0
    ## kw_max_max                            0
    ## kw_min_min                            0
    ## kw_avg_min                            0
    ## is_weekend1                           0
    ## global_subjectivity                   0
    ## global_rate_positive_words            0
    ## avg_negative_polarity                 0
    ## title_sentiment_polarity              0
    ## weekday_is_sunday                     0
    ## weekday_is_monday                     0
    ## weekday_is_tuesday                    0
    ## weekday_is_wednesday                  0
    ## weekday_is_thursday                   0
    ## weekday_is_friday                     0
    ## weekday_is_saturday                   0

Now, the results of applying the boosted tree to the test data:

``` r
predBoost <- predict(boostFit,newdata = test)
  
predResultsBoost <- postResample(predBoost,obs = test$shares)
predResultsBoost
```

    ##         RMSE     Rsquared          MAE 
    ## 3739.7455598    0.2485637 1492.3824534

## Model Comparison

The metric results of each model are returned in the table below. The
metric that they are “judged” against is RMSE, and the model with the
minimum value is the “winner”.

``` r
rmseModels <- c(predResultsFwd[[1]],predResultsBck[[1]],
                predResultsRF[[1]],predResultsBoost[[1]])

modelName <- c("Linear Regression (Forward Var. Selection)", "Linear Regression (Backward Var. Selection)",
               "Random foresF", "Boosted Tree")

resultsDF <- tibble(modelName, rmseModels)
resultsDF
```

    ## # A tibble: 4 × 2
    ##   modelName                                   rmseModels
    ##   <chr>                                            <dbl>
    ## 1 Linear Regression (Forward Var. Selection)       3403.
    ## 2 Linear Regression (Backward Var. Selection)      4147.
    ## 3 Random foresF                                    3417.
    ## 4 Boosted Tree                                     3740.

``` r
bestModelName <- filter(resultsDF,rmseModels == min(rmseModels))[[1]]

paste0("The best model for the ", params$channel, " channel is: ", bestModelName)
```

    ## [1] "The best model for the tech channel is: Linear Regression (Forward Var. Selection)"
