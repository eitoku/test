library(tidyverse)
library(mlbench)

data(BostonHousing)

df <- BostonHousing %>% mutate_if(is.factor, as.numeric) %>% 
  mutate_at(-14, scale)

head(df, 3)

data(Sonar)

library(caret)
ca.fit <- train(Class ~ ., data = Sonar, method = "rf", ntree = 100)
varImp(ca.fit)
plot(varImp(ca.fit), top = 10)

######################## Hadley

library(tidyverse)
library(ggplot2)
library(nycflights13)

jan1 <- flights %>% filter(month == 1)
# filter arrange mutate, transmute select summarize

flights %>% group_by(year, month, day) %>% summarize(delay = mean(dep_delay, 
  na.rm = T))

delay <- flights %>% group_by(dest) %>% summarize(count = n(), 
  dist = mean(distance, na.rm = T), delay = mean(arr_delay, 
    na.rm = T)) %>% filter(count > 20, dest != "HNL")

delay %>% ggplot(mapping = aes(x = dist, y = delay)) + geom_point(aes(size = count), 
  alpha = 1/3) + geom_smooth(se = F)

not_cancelled <- flights %>% filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>% group_by(tailnum) %>% summarize(delay = mean(arr_delay), 
  n = n())
delays %>% ggplot(mapping = aes(x = delay)) + geom_freqpoly(binwidth = 10)
delays %>% ggplot(mapping = aes(x = n, y = delay)) + geom_point(alpha = 1/10)
delays %>% filter(n > 25) %>% ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

flights %>% group_by(dest, carrier) %>% summarize(n = n()) %>% 
  summarize(n = n()) %>% filter(n >= 2)


diamonds %>% ggplot(mapping = aes(x = price)) + geom_freqpoly(mapping = aes(color = cut), 
  binwidth = 500)

diamonds %>% ggplot() + geom_bar(mapping = aes(x = cut))
diamonds %>% ggplot(mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
diamonds %>% ggplot(aes(x = cut, y = price)) + geom_boxplot()
reg <- step(lm(price ~ carat + depth + table + x + y + z, diamonds))

flights %>% filter(is.na(dep_time)) %>% ggplot(aes(x = sched_dep_time)) + 
  geom_histogram()
flights %>% filter(!is.na(dep_time)) %>% ggplot(aes(x = sched_dep_time)) + 
  geom_histogram()
flights %>% mutate(cancelled = is.na(dep_time), sched_hour = sched_dep_time%/%100, 
  sched_min = sched_dep_time%%100, sched_dep_time = sched_hour + 
    sched_min/60) %>% ggplot(aes(x = sched_dep_time, y = ..density..)) + 
  geom_freqpoly(aes(color = cancelled), binwidth = 1/4)

test <- diamonds %>% filter(z < 10 & y < 12 & x > 3) %>% mutate_if(is.factor, 
  as.numeric) %>% mutate_at(-2 - 7, scale)
test$cut <- diamonds$cut
reg <- step(glm(price ~ . - y - z, test, family = gaussian()))
reg <- step(lm(price ~ . - x - y - z, test))
summary(reg)
pred <- predict(reg, test)


ggplot(test, aes(x = carat)) + geom_freqpoly(aes(color = cut))

diamonds %>% ggplot() + geom_count(aes(x = cut, y = color))
diamonds %>% count(cut, color) %>% ggplot(aes(x = cut, y = color)) + 
  geom_tile(aes(fill = n))

faithful %>% ggplot() + geom_point(aes(eruptions, waiting))

############################# 
library(modelr)

mod <- lm(log(price) ~ log(carat), diamonds)

diamonds2 <- diamonds %>% add_residuals(mod) %>% mutate(resid = exp(resid))
ggplot(diamonds2) + geom_point(aes(x = carat, y = resid))

