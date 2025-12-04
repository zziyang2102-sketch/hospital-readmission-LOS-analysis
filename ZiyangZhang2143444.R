## Task 1.1: Histogram and transformation for LOS

# Read data
readmission <- read.csv("readmission.csv")

# Histogram of the original LOS
hist(readmission$LOS,
     breaks = 15,
     main = "Histogram of LOS",
     xlab  = "Length of stay (days)")

# Define transformed variable: log(LOS)
readmission$log_LOS <- log(readmission$LOS)

# Histogram of transformed LOS
hist(readmission$log_LOS,
     breaks = 15,
     main = "Histogram of log(LOS)",
     xlab  = "log(LOS)")

install.packages("e1071")  
library(e1071)

skewness(readmission$LOS)
skewness(readmission$log_LOS)

## Task 1.2: Plot transformed LOS by a categorical predictor

# Use DRG.Class as the categorical predictor and draw boxplots
boxplot(log_LOS ~ DRG.Class,
        data = readmission,
        main = "Boxplot of log(LOS) by DRG.Class",
        xlab = "DRG.Class",
        ylab = "log(Length of stay)")

# Group means of log(LOS) by DRG.Class
tapply(readmission$log_LOS, readmission$DRG.Class, mean)


## Task 2.1: Baseline multiple linear regression model m0

# Fit baseline model: log_LOS as response, others as predictors
m0 <- lm(log_LOS ~ Age + ER + HCC.Riskscore +
           Gender + Race + DRG.Class + DRG.Complication,
         data = readmission)

# Model summary
summary(m0)


## Task 2.2: Reduced model based on 5% significance level

# Based on summary(m0), remove ER, Gender, Race
m_red <- lm(log_LOS ~ Age + HCC.Riskscore +
              DRG.Class + DRG.Complication,
            data = readmission)

# Summary of reduced model
summary(m_red)

# Compare the two models (nested F-test)
anova(m_red, m0)


## Task 2.3: Diagnostic plots for the reduced model m_red

# Standard diagnostic plots
par(mfrow = c(2, 2))
plot(m_red)
par(mfrow = c(1, 1))


## Task 2.4: Detect unusual observations for m_red

# Standardised residuals
r_std <- rstandard(m_red)

# Leverage (hat values)
h <- hatvalues(m_red)

# Cook's distance
cook <- cooks.distance(m_red)

# Sample size n and number of predictors p (excluding intercept)
n <- nobs(m_red)
p <- length(coef(m_red)) - 1

# Average leverage: hbar = (p + 1) / n  (same as in the handout)
hbar <- (p + 1) / n

## Benchmarks (following the LRM R programming notes)

# (a) Outliers: |standardised residual| > 3
out_idx <- which(abs(r_std) > 3)

# (b) High leverage: h_i > 4 * hbar
lev_threshold <- 4 * hbar
lev_idx <- which(h > lev_threshold)

# (c) Influential points: Cook's distance > 4 / (n - p - 1)
cook_threshold <- 4 / (n - p - 1)
inf_idx <- which(cook > cook_threshold)

# Inspect indices
out_idx        # potential outliers
lev_idx        # high leverage points
inf_idx        # influential points

# Optional: look at the corresponding rows in the dataset
readmission[out_idx, ]
readmission[lev_idx, ]
readmission[inf_idx, ]

## Task 2.5: Assess multicollinearity for m_red using VIF

# Need car package for VIF

library(car)

vif(m_red)


## Task 2.6: Stepwise selection from the baseline model m0

# Stepwise selection using AIC (both directions)
m_sel <- step(m0,
              direction = "both",  # can also be "backward"
              k = 2)               # k = 2 corresponds to AIC

# Summary of selected model
summary(m_sel)

# Compare with reduced model m_red from Task 2.2
summary(m_red)

AIC(m0, m_red, m_sel)



## Part B – Task 3.1: Baseline logistic GLM for readmission

# Proportion of readmissions
prop.table(table(readmission$Readmission.Status))

# Fit baseline logistic regression model g0
g0 <- glm(Readmission.Status ~ Age + ER + HCC.Riskscore +
            Gender + Race + DRG.Class + DRG.Complication,
          family = binomial(link = "logit"),
          data   = readmission)

# Model summary
summary(g0)


## Part B – Task 3.2: Reduced logistic model based on 10% level

# From summary(g0), variables not significant at 10%:
# Age, Gender, Race
# Remove them and fit reduced model drop_g
drop_g <- glm(Readmission.Status ~ ER + HCC.Riskscore +
                DRG.Class + DRG.Complication,
              family = binomial(link = "logit"),
              data   = readmission)

# Summary of reduced model
summary(drop_g)

# Likelihood ratio test comparing drop_g and g0 (nested models)
anova(drop_g, g0, test = "Chisq")

# Compare AIC
AIC(g0, drop_g)



## Task 3.3: Compare residual types and choose one for diagnostics

# statmod package is needed for quantile residuals
library(statmod)

## 1. Compute three types of residuals
dev_res   <- residuals(drop_g, type = "deviance")
pear_res  <- residuals(drop_g, type = "pearson")
q_res     <- qresid(drop_g)   # quantile residuals (as in the handout)

## 2. Compare densities with N(0,1)

# Grid for standard normal density
x_grid <- seq(-4, 4, length = 200)
dnorm_grid <- dnorm(x_grid)

par(mfrow = c(1, 3))

# (a) Deviance residuals
plot(density(dev_res),
     main = "Density of deviance residuals",
     xlab = "Deviance residual")
lines(x_grid, dnorm_grid, lty = 2)  # dashed: standard normal density

# (b) Pearson residuals
plot(density(pear_res),
     main = "Density of Pearson residuals",
     xlab = "Pearson residual")
lines(x_grid, dnorm_grid, lty = 2)

# (c) Quantile residuals
plot(density(q_res),
     main = "Density of quantile residuals",
     xlab = "Quantile residual")
lines(x_grid, dnorm_grid, lty = 2)

par(mfrow = c(1, 1))

## 3. Q–Q plots of the three residual types

par(mfrow = c(1, 3))

qqnorm(dev_res,  main = "Q-Q: deviance residuals")
qqline(dev_res)

qqnorm(pear_res, main = "Q-Q: Pearson residuals")
qqline(pear_res)

qqnorm(q_res,    main = "Q-Q: quantile residuals")
qqline(q_res)

par(mfrow = c(1, 1))

# Fitted linear predictor eta = logit(p_hat)
eta_hat <- predict(drop_g, type = "link")

par(mfrow = c(1, 2))

# (a) Quantile residuals vs linear predictor
plot(eta_hat, q_res,
     xlab = "Fitted linear predictor (eta)",
     ylab = "Quantile residuals",
     main = "Quantile residuals vs fitted eta")
abline(h = 0, lty = 2)

# (b) Q–Q plot of quantile residuals
qqnorm(q_res, main = "Q-Q plot of quantile residuals")
qqline(q_res)

par(mfrow = c(1, 1))


## Task 3.4: Detect outliers (|standardised deviance residual| > 2.5)
##          and refit final model fin_g

## 1. Compute standardised deviance residuals
r_std_dev <- rstandard(drop_g, type = "deviance")   # as in the handout

## 2. Use benchmark |r_i| > 2.5 to flag outliers
cutoff  <- 2.5
out_idx <- which(abs(r_std_dev) > cutoff)

# Number and indices of outliers
length(out_idx)          # number of outliers
out_idx                  # row indices
readmission[out_idx, ]   # optional: inspect these observations

## 3. Remove outliers to obtain reduced dataset
if (length(out_idx) == 0) {
  # No observation exceeds 2.5: use original data
  readmission_clean <- readmission
} else {
  readmission_clean <- readmission[-out_idx, ]
}

nrow(readmission_clean)  # sample size after deletion

## 4. Refit the same logistic model on the reduced dataset to obtain fin_g
fin_g <- glm(Readmission.Status ~ ER + HCC.Riskscore +
               DRG.Class + DRG.Complication,
             family = binomial(link = "logit"),
             data   = readmission_clean)

summary(fin_g)

## 5. Compare fin_g with original model drop_g
AIC(drop_g, fin_g)
anova(drop_g, fin_g, test = "Chisq")


## Part B – Task 3.5: Predictive probability for a new patient

# Inspect factor levels in the final model (for constructing newdata)
fin_g$xlevels

# Construct a new patient:
# ER = 2, HCC.Riskscore = 1.5,
# DRG.Class = second level (e.g. "SURG"),
# DRG.Complication = fourth level (e.g. "SurgMCC.CC")
new_patient <- data.frame(
  ER            = 2,
  HCC.Riskscore = 1.5,
  DRG.Class     = factor(fin_g$xlevels$`DRG.Class`[2],
                         levels = fin_g$xlevels$`DRG.Class`),
  DRG.Complication = factor(fin_g$xlevels$`DRG.Complication`[4],
                            levels = fin_g$xlevels$`DRG.Complication`)
)

new_patient

# Use final model fin_g to predict readmission probability
pred_prob <- predict(fin_g, newdata = new_patient, type = "response")
pred_prob


