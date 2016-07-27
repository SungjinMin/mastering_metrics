library(stringr)
library(dplyr)
library(ggplot2)
library(textir)
library(stringr)
library(multiwayvcov)
library(readstata13)
library(plm)
library(lmtest)
library(dplyr)
library(caret)

setwd("C:/Users/Stephen/Desktop/stata/mastering_metrics/deaths_ch5_DD")

mlda <- read.dta13("deaths.dta")
dim(mlda)

p_mlda <- pdata.frame(mlda, index = c("state", "year"), drop.index = F, row.names = T)
p_mlda$year <- as.numeric(as.character(p_mlda$year))
p_mlda <- filter(p_mlda, dtype == "all", agegr == "18-20 yrs", year <= 1983)

# confirms with stata
m1 <- lm(mrate ~ legal, data = p_mlda)
coeftest(m1)

# caret gets same answer
m1.2 <- train(mrate ~ legal, data = p_mlda, method = "lm")
m1.2$finalModel

# with state-specific trend and pop weights, clustered on states
# same as stata
m2 <- lm(mrate ~ legal + factor(year) + factor(state) + factor(state)*year, data = p_mlda, weights = pop)
m2 <- lm(mrate ~ legal + factor(year) + factor(state) * year, data = p_mlda, weights = pop)
coeftest(m2)
m2.vcovCL <- cluster.vcov(m2, p_mlda$state)
coeftest(m2, m2.vcovCL)

# 
m2.2 <- train(mrate ~ legal + factor(year) + factor(state) + factor(state)*year, method = "lm",
              data = p_mlda, weights = pop)
summary(m2.2)
m2.2.vcovCL <- cluster.vcov(m2.2, p_mlda$state)
coeftest(m2, m2.2.vcovCL)
