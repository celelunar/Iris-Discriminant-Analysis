#### LIBRARIES ####
library(MASS)
library(datasets)
library(ggplot2)
library(dplyr)

#### LOAD DATASET ####
data(iris)

#### MODELLING ####

##### 1. LDA Model #####
lda_model = lda(Species ~ ., data = iris)
lda_model
  # z1 = 0.8293776 * Sepal.Length + 1.5344731 * Sepal.Width - 2.2012117 * Petal.Length - 2.8104603 * Petal.Width
  # z2 = - 0.02410215 * Sepal.Length - 2.16452123 * Sepal.Width + 0.93192121 * Petal.Length - 2.83918785 * Petal.Width
  # z3 = 0.8293776 * Sepal.Length + 1.5344731 * Sepal.Width - 2.2012117 * Petal.Length - 2.8104603 * Petal.Width

##### 2. QDA Model #####
qda_model = qda(Species ~ ., data = iris)
qda_model
  # the qda() function does not directly provide the coefficients for the quadratic discriminant functions.

#### VISUALIZATION ####

##### 1. LDA Model #####
lda_prob <- predict(lda_model, iris)$posterior
lda_data <- cbind(iris, lda_prob)
ggplot(lda_data, aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +
  geom_point() +
  labs(title = "LDA Results on Iris Dataset", x = "Petal Length", y = "Petal Width") +
  theme_minimal()

##### 2. QDA Model #####
qda_prob <- predict(qda_model, iris)$posterior
qda_data <- cbind(iris, qda_prob)
ggplot(qda_data, aes(x = Petal.Length, y = Petal.Width, color = Species, shape = Species)) +
  geom_point() +
  labs(title = "QDA Results on Iris Dataset", x = "Petal Length", y = "Petal Width") +
  theme_minimal()

#### PREDICTION ####
new_data = data.frame(Sepal.Length = c(5.7, 6.3), 
                      Sepal.Width = c(2.5, 2.8), 
                      Petal.Length = c(6.4, 4.9), 
                      Petal.Width = c(0.2, 1.5))

##### 1. LDA Predictions #####
lda_pred = predict(lda_model, new_data)$class
lda_pred

##### 2. QDA Predictions #####
qda_pred = predict(qda_model, new_data)$class
qda_pred