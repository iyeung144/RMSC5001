## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dpi = 300, fig.width = 7)


## ----set-options, echo=FALSE, cache=FALSE--------------------------------
options(width = 150)


## ----load packages, message=FALSE, warning=FALSE-------------------------
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(rworldmap)
library(knitr)
library(rpart)
library(rpart.plot)
library(tidyverse)


## ----data field, echo=FALSE, results='asis'------------------------------
income <- read.csv("worldbank_income.csv")
print(income)
kable(income, caption = "World Bank definition on the income group")

field <- read.csv("worldbank_field.csv")
print(field)
kable(field, caption = "Field information")


## ----read data-----------------------------------------------------------
d <- read.csv("world.csv", header = TRUE)
n <- nrow(d) # get sample size
set.seed(12345) # set random seed
r <- 0.80 # set sampling ratio
id <- sample(1:n, size = round(r * n)) # generate id
train <- d[id, ] # training dataset
test <- d[-id, ]
print(head(d))


## ----world map-----------------------------------------------------------
income.df <- joinCountryData2Map(d,
  joinCode = "NAME",
  nameJoinColumn = "COUNTRY"
)

mapParams <- mapCountryData(income.df,
  nameColumnToPlot = "INCOME_GROUP",
  catMethod = "categorical",
  colourPalette = "diverging",
  addLegend = "FALSE",
  mapTitle = "Different Income Levels around the World"
)

do.call(
  addMapLegendBoxes,
  c(mapParams,
    x = "bottomleft",
    horiz = FALSE,
    title = "",
    bg = "NA",
    col = "NA"
  )
)


## ----plot income group number--------------------------------------------
# All data
print(prop.table(table(d$INCOME_GROUP)))
all_plot <- d[, -1] %>%
  count(INCOME_GROUP) %>%
  ggplot() +
  geom_col(aes(x = INCOME_GROUP, y = n),
    fill = "#002244",
    colour = "#002244"
  ) +
  ggtitle("Income group (All data)") +
  coord_flip() +
  theme_fivethirtyeight()

# Training data
print(prop.table(table(train$INCOME_GROUP)))
train_plot <- train[, -1] %>%
  count(INCOME_GROUP) %>%
  ggplot() +
  geom_col(aes(x = INCOME_GROUP, y = n),
    fill = "#002244",
    colour = "#002244"
  ) +
  ggtitle("Income group (Training data)") +
  coord_flip() +
  theme_fivethirtyeight()

# Testing data
print(prop.table(table(test$INCOME_GROUP)))
test_plot <- test[, -1] %>%
  count(INCOME_GROUP) %>%
  ggplot() +
  geom_col(aes(x = INCOME_GROUP, y = n),
    fill = "#002244",
    colour = "#002244"
  ) +
  ggtitle("Income group (Testing data)") +
  coord_flip() +
  theme_fivethirtyeight()

grid.arrange(all_plot, train_plot, test_plot, nrow = 3, ncol = 1)


## ----grow classification tree--------------------------------------------
ctree <- rpart(INCOME_GROUP ~
REGION + GDP +
  LABOUR_FORCE +
  LIFE_EXPECTANCY +
  MILITARY_EXPENSE +
  HIGH_TECHNOLOGY_EXPORT +
  CORRUPTION,
data = train,
method = "class",
control = rpart.control(cp = 0)
)


## ----plot classification tree with probability---------------------------
# plot the tree with option 104 for probability
rpart.plot(ctree, extra = 104)


## ----plot classification tree with numbers-------------------------------
# plot the tree with option 101 for numbers
rpart.plot(ctree, extra = 101)


## ----print the classification tree---------------------------------------
print(ctree)


## ----print rules---------------------------------------------------------
rpart.rules(ctree)


## ----rule results, echo=FALSE, results='asis'----------------------------
rule.results <- read.csv("rule_results.csv")
kable(rule.results, caption = "Classification Tree Explanatory Power")
print(rule.results)

## ----tree complexity-----------------------------------------------------
printcp(ctree)
plotcp(ctree)


## ----training prediction-------------------------------------------------
predict_train2 <- predict(ctree, train, type = "class")
print(table(predict_train2, train$INCOME_GROUP))


## ----testing prediction--------------------------------------------------
predict_test2 <- predict(ctree, test, type = "class")
print(table(predict_test2, test$INCOME_GROUP))


## ----base classification accuracy----------------------------------------
base_accuracy <- mean(predict_test2 == test$INCOME_GROUP)
print(base_accuracy)


## ----train prediction, echo=FALSE, results='asis'------------------------
prediction.training <- read.csv("prediction_training.csv")
kable(prediction.training, caption = "Results on the prediction of training data")
print(prediction.training)


## ----test prediction, echo=FALSE, results='asis'-------------------------
prediction.testing <- read.csv("prediction_testing.csv")
kable(prediction.testing, caption = "Results on the prediction of training data")
print(prediction.testing)


## ----pre-pruning---------------------------------------------------------
# Grow a tree with minsplit of 100 and max depth of 8
ctree_preprun <- rpart(INCOME_GROUP ~
REGION + GDP +
  LABOUR_FORCE +
  LIFE_EXPECTANCY +
  MILITARY_EXPENSE +
  HIGH_TECHNOLOGY_EXPORT +
  CORRUPTION,
data = train, method = "class",
control = rpart.control(cp = 0, maxdepth = 3, minsplit = 20)
)
print(ctree_preprun)
# Compute the accuracy of the pruned tree
test$pred <- predict(ctree_preprun, test, type = "class")
accuracy_preprun <- mean(test$pred == test$INCOME_GROUP)


## ----post-pruning--------------------------------------------------------
# Prune the ctree_model based on the optimal cp value
ctree_pruned <- prune(ctree, cp = 0.046)
print(ctree_pruned)
# Compute the accuracy of the pruned tree
test$pred <- predict(ctree_pruned, test, type = "class")
accuracy_postprun <- mean(test$pred == test$INCOME_GROUP)
print(data.frame(base_accuracy, accuracy_preprun, accuracy_postprun))

