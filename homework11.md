homework11
================
2024-11-15

## Homework 11

Rose Wilfong

# Part 1

## Data Used

The data was collected by the Honolulu Heart Study and is a prospective
examination of coronary heart disease and stroke among men of Japanese
ancestry born between 1900 and 1919 and residing on the island of Oahu
in 1965.

| Column Name | Description                         |
|-------------|-------------------------------------|
| Age         | Age at Exam 1                       |
| agegrp      | Age in groups                       |
| bmi         | Body Mass Index                     |
| sbp         | Systolic Blood Pressure             |
| currsmok    | Current Smoker (0=No, 1=Yes)        |
| chd10       | 10-year CHD incidence (0=No, 1=Yes) |

Description of Dataset Variables

``` r
# read in the data 
data_1 <- read.csv("/Users/rwilfong/Downloads/BME501/data/BME 501 Homework 11-1 dataset.csv")
head(data_1)
```

    ##    id age  bmi sbp chd10 currsmok agegrp
    ## 1 112  53 23.2 163    NO       NO  50-54
    ## 2 119  58 26.2 182    NO       NO  55-59
    ## 3 121  50 22.1 194    NO       NO  50-54
    ## 4 126  52 24.1 118    NO      YES  50-54
    ## 5 132  51 24.9 131    NO       NO  50-54
    ## 6 141  57 23.8 117    NO       NO  55-59

Now, convert the CHD10 and currsmok entries numerical entries

``` r
data_1$chd10 <- factor(data_1$chd10, levels = c("NO", "YES"))
data_1$chd10 <- as.numeric(data_1$chd10) - 1 
data_1$currsmok <- factor(data_1$currsmok, levels = c("NO", "YES"))
data_1$currsmok <- as.numeric(data_1$currsmok) - 1
head(data_1)
```

    ##    id age  bmi sbp chd10 currsmok agegrp
    ## 1 112  53 23.2 163     0        0  50-54
    ## 2 119  58 26.2 182     0        0  55-59
    ## 3 121  50 22.1 194     0        0  50-54
    ## 4 126  52 24.1 118     0        1  50-54
    ## 5 132  51 24.9 131     0        0  50-54
    ## 6 141  57 23.8 117     0        0  55-59

### Question 1

Estimate the odds ratio, adjusted for age, BMI, and systolic blood
pressure, for developing CHD within 10 years for a smoker compared to a
non-smoker

``` r
# fit logistic regression model 
logit_model <- glm(chd10 ~ age + bmi + sbp + currsmok, data = data_1, family = binomial)
summary(logit_model)
```

    ## 
    ## Call:
    ## glm(formula = chd10 ~ age + bmi + sbp + currsmok, family = binomial, 
    ##     data = data_1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0740  -0.3925  -0.3158  -0.2530   2.7574  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -10.208662   1.870626  -5.457 4.83e-08 ***
    ## age           0.052429   0.026247   1.998   0.0458 *  
    ## bmi           0.035380   0.039222   0.902   0.3670    
    ## sbp           0.026039   0.005519   4.718 2.39e-06 ***
    ## currsmok      0.393811   0.252771   1.558   0.1192    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 527.80  on 1070  degrees of freedom
    ## Residual deviance: 492.61  on 1066  degrees of freedom
    ## AIC: 502.61
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# calculate the odds ratio and the 95% confidence interval
exp(cbind(OR = coef(logit_model), confint(logit_model)))
```

    ## Waiting for profiling to be done...

    ##                       OR        2.5 %      97.5 %
    ## (Intercept) 3.684974e-05 9.219125e-07 0.001439751
    ## age         1.053828e+00 9.995805e-01 1.108269987
    ## bmi         1.036013e+00 9.588686e-01 1.118433080
    ## sbp         1.026381e+00 1.015311e+00 1.037583697
    ## currsmok    1.482620e+00 9.028657e-01 2.440632280

The estimated odds ratio adjusted for age, BMI, and systolic blood
pressure is 1.482620.

### Question 2

Determine a 95% confidence interval for the adjusted odds ratio,
accounting for all risk factors (age, systolic blood pressure, and BMI).

``` r
exp(cbind(OR = coef(logit_model), confint(logit_model)))
```

    ## Waiting for profiling to be done...

    ##                       OR        2.5 %      97.5 %
    ## (Intercept) 3.684974e-05 9.219125e-07 0.001439751
    ## age         1.053828e+00 9.995805e-01 1.108269987
    ## bmi         1.036013e+00 9.588686e-01 1.118433080
    ## sbp         1.026381e+00 1.015311e+00 1.037583697
    ## currsmok    1.482620e+00 9.028657e-01 2.440632280

The 95% confidence interval for the adjusted odds ratio accounting for
all risk factors is (0.9028657, 2.440632280).  

### Question 3

Estimate the probability of developing CHD within 10 years for an
“average” smoker man (age = 54.6, BMI=23.2, systolic blood pressure =
134.4), along with a 95% confidence interval for this probability

``` r
# construct a new dataset of the "average" smoker man 
average_smoker <- data.frame(
  age = 54.6,
  bmi = 23.2,
  sbp = 134.4,
  currsmok = 1
)
average_smoker
```

    ##    age  bmi   sbp currsmok
    ## 1 54.6 23.2 134.4        1

``` r
# predict from the previous model on the new data 
predicted_prob <- predict(logit_model, newdata = average_smoker, type = "response", se.fit = TRUE)
```

``` r
# calculate the 95% confidence interval
prob <- predicted_prob$fit
se <- predicted_prob$se.fit

z_value <- qnorm(0.975)  # 95% confidence level
logit_ci <- c(prob - z_value * se, prob + z_value * se)

# transform to a CI
ci <- exp(logit_ci) / (1 + exp(logit_ci))

# print the results
cat("Predicted probability of developing CHD for an average smoker:", prob, "\n")
```

    ## Predicted probability of developing CHD for an average smoker: 0.06711948

``` r
cat("95% CI for the probability:", ci[1], ",", ci[2], "\n")
```

    ## 95% CI for the probability: 0.5110732 , 0.5224696

# Part 2

You are interested in developing a simple diagnostic method to reliably
predict multiple sclerosis using MRI data. In this study, white matter
areas were measured from 150 patients as a predictor, and the presence
of multiple sclerosis was confirmed by lumbar puncture results
(considered the gold standard).

``` r
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
data_2 <- read.csv("/Users/rwilfong/Downloads/BME501/data/BME 501 Homework 11-2 dataset.csv")
head(data_2)
```

    ##   Area Multiple.sclerosis
    ## 1  165                  1
    ## 2  140                  1
    ## 3  154                  1
    ## 4  139                  1
    ## 5  134                  1
    ## 6  154                  1

### Question 1

Write your own code (using Python, Matlab, R, or any preferred language)
to plot the ROC curve and calculate the AUC

``` r
roc_curve <- roc(data_2$Multiple.sclerosis, data_2$Area)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls > cases

``` r
plot(roc_curve, main="ROC Curve for Multiple Sclerosis Prediction", col="blue", lwd=2)
```

![](homework11_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
auc_value <- auc(roc_curve)
auc_value
```

    ## Area under the curve: 0.8754

### Question 2

Using logistic regression, plot an ROC curve and calculate the AUC

``` r
# fit logistic regression model
logit_model <- glm(Multiple.sclerosis ~ Area, data = data_2, family = binomial)

# get predicted probabilities
pred_probs <- predict(logit_model, type = "response")

# plot the ROC curve
roc_curve <- roc(data_2$Multiple.sclerosis, pred_probs)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(roc_curve, main="ROC Curve for Logistic Regression Model", col="purple", lwd=2)
```

![](homework11_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# calculate the AUC (Area Under the Curve)
auc_value <- auc(roc_curve)
auc_value
```

    ## Area under the curve: 0.8754

# Part 3

The textbook introduces the Ille-et-Vilaine study on esophageal cancer
and alcohol (Breslow and Day, 1980). In this study, data are stratified
by ten-year age groups, clearly showing that esophageal cancer incidence
increases significantly with age. The Ille-et-Vilaine dataset
categorizes daily alcohol consumption into four levels: 0–39 g, 40–79 g,
80–119 g, and ≥120 g. Similarly, daily tobacco consumption is divided
into four levels: 0–9 g, 10–19 g, 20–29 g, and ≥30 g.

``` r
# read in the data 
data_3 <- read.csv("/Users/rwilfong/Downloads/BME501/data/5.5.EsophagealCa.csv")
head(data_3)
```

    ##   age alcohol tobacco cancer patients heavy
    ## 1   1       1       1      0       40     0
    ## 2   1       1       1      1        0     0
    ## 3   1       1       2      0       10     0
    ## 4   1       1       2      1        0     0
    ## 5   1       1       3      0        6     0
    ## 6   1       1       3      1        0     0

### Question 1

Using multiple logistic regression, build a model to predict the risk of
esophageal cancer, including all risk factors in the dataset. Define
dummy variables for categorical variables as needed.

``` r
# fit a multiple logistic model 
model <- glm(cancer ~ factor(alcohol) + factor(age), 
                        data = data_3, family = binomial, weight = patients)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = cancer ~ factor(alcohol) + factor(age), family = binomial, 
    ##     data = data_3, weights = patients)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -4.858  -1.092   0.000   1.180   5.492  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -6.1472     1.0419  -5.900 3.63e-09 ***
    ## factor(alcohol)2   1.4343     0.2448   5.859 4.64e-09 ***
    ## factor(alcohol)3   2.0071     0.2776   7.230 4.84e-13 ***
    ## factor(alcohol)4   3.6800     0.3763   9.778  < 2e-16 ***
    ## factor(age)2       1.6311     1.0800   1.510 0.130973    
    ## factor(age)3       3.4258     1.0389   3.297 0.000976 ***
    ## factor(age)4       3.9435     1.0346   3.811 0.000138 ***
    ## factor(age)5       4.3568     1.0413   4.184 2.87e-05 ***
    ## factor(age)6       4.4242     1.0914   4.054 5.04e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 989.49  on 134  degrees of freedom
    ## Residual deviance: 727.42  on 126  degrees of freedom
    ## AIC: 745.42
    ## 
    ## Number of Fisher Scoring iterations: 6

### Question 2

Perform a goodness-of-fit test (e.g., Pearson chi-squared test) to
assess the model’s fit.

``` r
pearson_residuals <- residuals(model, type = "pearson")

# sum of squared Pearson residuals
pearson_chi_sq <- sum(pearson_residuals^2)

# degrees of freedom = total number of patterns - the number of independent variables - 1
dof <- dim(data_3)[1] - length(coef(model))

# calculate p-value from chi-squared distribution
p_value <- pchisq(pearson_chi_sq, dof, lower.tail = FALSE)

# print results
cat("Pearson Chi-squared:", pearson_chi_sq, "\n")
```

    ## Pearson Chi-squared: 859.636

``` r
cat("Degrees of Freedom:", dof, "\n")
```

    ## Degrees of Freedom: 183

``` r
cat("P-value:", p_value, "\n")
```

    ## P-value: 3.945976e-88

### Question 3

Evaluate the model’s performance by plotting a receiver operating
characteristic (ROC) curve and calculating the area under the curve
(AUC).

``` r
# expand the data
df_expand <- data_3[rep(1:nrow(data_3), data_3$patients), ]
head(df_expand)
```

    ##     age alcohol tobacco cancer patients heavy
    ## 1     1       1       1      0       40     0
    ## 1.1   1       1       1      0       40     0
    ## 1.2   1       1       1      0       40     0
    ## 1.3   1       1       1      0       40     0
    ## 1.4   1       1       1      0       40     0
    ## 1.5   1       1       1      0       40     0

``` r
# predict probabilities
predicted_probabilities <- predict(model, newdata = df_expand, type = "response")

# convert probabilities to binary predictions with threshold 0.5
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
```

``` r
# generate ROC curve
roc_curve <- roc(df_expand$cancer, predicted_probabilities)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
```

    ## [1] "AUC: 0.840777419354839"

``` r
# plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue")
text(0.5, 0.5, paste("AUC =", round(auc_value, 4)), col = "red")
```

![](homework11_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Question 4

Calculate the accuracy (%) of your logistic regression model in
correctly classifying esophageal cancer when the predicted probability
is ≥ 0.5

``` r
# calculate number of correct predictions using the predicted classes
correct_predictions <- sum(predicted_classes == df_expand$cancer)

# calculate accuracy
accuracy <- correct_predictions / nrow(df_expand)

# print the accuracy as a percentage
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))
```

    ## [1] "Accuracy: 82.26 %"
