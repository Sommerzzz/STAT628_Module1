layout(1)
rm(list = ls())

# read the data
dat = read.csv("../data/BodyFat.csv", row.names = 1)

# find some abnormal points which bodyfat does not have a linear relationship of 1 / density
plot(y = dat$BODYFAT, x = 1 / dat$DENSITY, ylab = "bodyfat percentage", xlab = "body density", 
     main = "BODYFAT vs. 1 / DENSITY")
#identify(y = dat$BODYFAT, x = 1 / dat$DENSITY, n = 3)

# remove the DENSITY variable
dat = dat[, -2]

# find some potential outlier with histograms of each variable
layout(matrix(1:16, nrow = 4, byrow = TRUE))
for (i in 1:15) {
  hist(dat[, i], freq = FALSE, main = paste("Histogram of", colnames(dat)[i]),
       xlab = colnames(dat)[i], breaks = 100)
}

# three points with probably wrong data
print(dat[39, ])
print(dat[42, ])
print(dat[182, ])

# remove some points
dat = dat[c(-39, -42, -48, -96, -76, -182), ]
rownames(dat) = NULL
write.csv(dat, "BodyFat_clean.csv")

# find some points with high cook's distance
model = lm(BODYFAT ~ ., data = dat)
layout(1)
plot(model, which = 4)

dat[215, ]

# perform a outlier test
library(car, quietly = TRUE)
outlierTest(model)

# consider the Box-Cox transformation
library(MASS, quietly = TRUE)
MASS::boxcox(model)

# a function to perform a cross validation process
cross_validation = function(dat, model) {
  sub_index = list()
  n = dim(dat)[1]
  v = dim(dat)[2]
  l = round(n / 5)
  inds = 1:n
  
  MSEs = 1:5
  
  sub_index[[1]] = sample(inds, l)
  inds = setdiff(inds, sub_index[[1]])
  
  sub_index[[2]] = sample(inds, l)
  inds = setdiff(inds, sub_index[[2]])
  
  sub_index[[3]] = sample(inds, l)
  inds = setdiff(inds, sub_index[[3]])
  
  sub_index[[4]] = sample(inds, l)
  inds = setdiff(inds, sub_index[[4]])
  
  sub_index[[5]] = inds
  
  for(i in 1:5) {
    training = setdiff(1:5, i)
    training_index = numeric()
    
    for (t in training) {
      training_index = append(training_index, sub_index[[t]])
    }
    
    test_index = sub_index[[i]]
    
    m = lm(model, dat = dat[training_index, ])
    
    test_X = dat[test_index, 2:v]
    test_Y = dat[test_index, 1]
    
    predicted_Y = predict(m, test_X)
    MSEs[i] = sum((test_Y - predicted_Y)^2) / length(test_Y)
  }
  
  #print(MSEs)
  return(mean(MSEs))
}

# a function to do cross validation simulation
cal_cv = function(dat, model, S=100) {
  x = 0
  
  for (i in 1:S) {
    x = x + cross_validation(dat, model)
  }
  
  return(x / S)
}


# set a seed to make the simulations repeatable
set.seed(628)

# full model
full_model = BODYFAT ~ .
full_cv = cal_cv(dat, full_model, 100)
full_lm = lm(full_model, dat)


# stepwise variable selection
full = lm(BODYFAT ~ ., data = dat)
base = lm(BODYFAT ~ 1, data = dat)
# AIC 
# backward
model_aic_1 = step(full, direction = "backward", trace = 0)$call$formula
print(model_aic_1)
aic_cv_1 = cal_cv(dat, model_aic_1, 100)
aic_lm_1 = lm(model_aic_1, dat)
# forward
model_aic_2 = step(base, direction = "forward", trace = 0, 
                   scope = list(lower = base, upper = full))$call$formula
print(model_aic_2)
aic_cv_2 = cal_cv(dat, model_aic_2, 100)
aic_lm_2 = lm(model_aic_2, dat)

# BIC
# backward
n = dim(dat)[1]
model_bic_1 = step(full, direction = "backward", trace = 0, k = log(n))$call$formula
print(model_bic_1)
bic_cv_1 = cal_cv(dat, model_bic_1, 100)
bic_lm_1 = lm(model_bic_1, dat)
# forward
model_bic_2 = step(base, direction = "forward", trace = 0, 
                   scope = list(lower = base, upper = full), 
                   k = log(n))$call$formula
print(model_bic_2)
bic_cv_2 = cal_cv(dat, model_bic_2, 100)
bic_lm_2 = lm(model_bic_2, dat)


# mallow's cp
library(leaps, quietly = TRUE)
library(faraway, quietly = TRUE)
X = dat[, 2:15]
Y = dat$BODYFAT
g = leaps(X, Y, nbest = 1)
Cpplot(g)
print(colnames(dat)[c(1, 3, 6, 7, 12, 14) + 1])
model_cp = BODYFAT ~ AGE + HEIGHT + CHEST + ABDOMEN +  BICEPS + WRIST
cp_cv = cal_cv(dat, model_cp, 100)
cp_lm = lm(model_cp, dat)


# LASSO
library(glmnet, quietly = TRUE)
x = model.matrix(full)[, -1]
y = dat$BODYFAT
set.seed(628)
model.lasso = glmnet(x, y, alpha = 1, family = "gaussian")
plot(model.lasso, xvar = "lambda")
cv.lasso = cv.glmnet(x, y, alpha = 1, family = "gaussian")
plot(cv.lasso)
best.lambda.lasso = cv.lasso$lambda.min
print(best.lambda.lasso)
predict(model.lasso, s=best.lambda.lasso, type="coefficients")
model_lasso = BODYFAT ~ AGE + HEIGHT + NECK + ABDOMEN + BICEPS + FOREARM + WRIST
lasso_cv = cal_cv(dat, model_lasso, 100)
lasso_lm = lm(model_lasso, dat)

# print the table of all methods and their MSEs
print(data.frame(method_name = c("full model", "backward selection w/ aic", "forward selection w/ aic",
                                 "backward selection w/ bic", "forward selection w/ bic", "Mallow's Cp", 
                                 "Lasso"),
                 test_set_MSE = c(full_cv, aic_cv_1, aic_cv_2, bic_cv_1, bic_cv_2, cp_cv, lasso_cv)))

# use the Mallow's Cp
summary(cp_lm)

# remove one variable, BICEPS
model_cp_2 = BODYFAT ~ AGE + HEIGHT + CHEST + ABDOMEN + WRIST
cp_lm_2 = lm(model_cp_2, dat)
summary(cp_lm_2)
anova(cp_lm_2, cp_lm)

# remove one variable, CHEST
model_cp_3 = BODYFAT ~ AGE + HEIGHT + ABDOMEN + WRIST
cp_lm_3 = lm(model_cp_3, dat)
summary(cp_lm_3)
anova(cp_lm_3, cp_lm)
anova(cp_lm_3, cp_lm_2)

# MSE of the new model
print(cal_cv(dat, model_cp))
print(cal_cv(dat, model_cp_2))
print(cal_cv(dat, model_cp_3))

# dignostics
layout(matrix(1:4, byrow = TRUE, nrow = 2))
plot(cp_lm_3)
