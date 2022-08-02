library(ISLR)
library(tidyverse)
library(glmnet)
library(caret)
library(gridExtra)

#data_prepration
summary(College)

College <- College %>% 
  filter(Grad.Rate <= 100,
         PhD <= 100)

College %>% 
  mutate(Private = as.numeric(Private),
         across(everything(), scale)) %>% 
  pivot_longer(everything(), values_to = "z-score", names_to = "variable") %>% 
  ggplot(aes(`z-score`, variable)) +
  geom_boxplot() +
  theme_minimal()

College <- College %>% 
  mutate(Private = as.numeric(Private),
         across(c(PhD, Top25perc, Terminal), ~{101 - .}),
         across(c(2, 3, 4, 5, 7, 8, 11, 12, 13, 14, 17), ~ log(. + 1)),
         across(everything(), scale)) 

College  %>% 
  pivot_longer(everything(), values_to = "z-score", names_to = "variable") %>% 
  ggplot(aes(`z-score`, variable)) +
  geom_boxplot() +
  theme_minimal()


par(mfrow = c(2, 2))
for(i in 2:17)plot(College[, c(i, 18)], cex = .7)

# train/test set
set.seed(1)
train <- createDataPartition(y = College$Grad.Rate, p = .75, list = FALSE)

#Filter method
cor(College[train, ])[-18, 18] %>%  round(3) 
vars <- which(abs(cor(College[train, ])[-18, 18]) > .3)
train_filter <- lm(Grad.Rate ~ ., data = College[train, c(vars, 18)])
summary(train_filter)
filter_pred <- predict(train_filter, newdata = College[-train, ])
filter_mse  <- mean((College[-train, "Grad.Rate"] - filter_pred)^2)

#Wrappermethod

train_step <- step(lm(Grad.Rate ~ ., College[train, ]))
summary(train_step)

pred_step  <- predict(train_step, newdata = College[-train, ])

(step_mse <- mean((College[-train, "Grad.Rate"] - pred_step)^2))

##Embedded method

train_ridge <- glmnet(x = as.matrix(College[train, -18]), 
                      y = College[train, 18], 
                      alpha = 0)

train_lasso <- glmnet(x = as.matrix(College[train, -18]), 
                      y = College[train, 18], 
                      alpha = 1)
par(mfrow=c(2,1))
plot(train_ridge, label = T)
plot(train_lasso, label = T)

cv_ridge <- cv.glmnet(x = as.matrix(College[train, -18]), 
                      y = College[train, 18], 
                      alpha = 0, 
                      type.measure = "mse")

cv_lasso <- cv.glmnet(x = as.matrix(College[train, -18]), 
                      y = College[train, 18], 
                      alpha = 1, 
                      type.measure = "mse")

par(mfrow=c(1, 2))
plot(cv_ridge)
plot(cv_lasso)

round(cbind(coef(lm(Grad.Rate ~ . , College[train, ])), coef(cv_ridge), coef(cv_lasso)), 3)


pred_ridge <- predict(cv_ridge, newx = as.matrix(College[-train, -18]))

pred_lasso <- predict(cv_lasso, newx = as.matrix(College[-train, -18]))


ridge_mse  <- mean((College[-train, "Grad.Rate"] - pred_ridge)^2)

lasso_mse  <- mean((College[-train, "Grad.Rate"] - pred_lasso)^2)

data.frame(filter_mse, step_mse, ridge_mse, lasso_mse)

cv_lasso2 <- cv.glmnet(x = model.matrix(Grad.Rate ~ .^2, College)[train, -1], 
                       y = College$Grad.Rate[train], 
                       alpha = 1, 
                       type.measure = "mse")
plot(cv_lasso2)
round(coef(cv_lasso2) , 3)
pred_lasso2 <- predict(cv_lasso2, newx = model.matrix(Grad.Rate ~ .^2, College)[-train, -1])
(lasso2_mse   <- mean((College[-train, "Grad.Rate"] - pred_lasso2)^2))