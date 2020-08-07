# RPA Capstone Time-Series Analysis
setwd("E:/OneDrive - lmu.edu/Course_MSBA Capstone 2020/RPA")

library(dplyr)
library(ggplot2)
library(zoo)
library(scales)
library(ggthemes)
library(stargazer)
# get the dataset
df <- read.csv("all_values.csv")


# transfer the date info into date formate
df$date <- as.Date(df$date, format = "%m/%d/%Y")
#extract month and year from the date
df$year <- format(df$date, "%Y")
# we can use months(), the built-in function 
# df$month <- months(df$date)
df$month <- format(df$date, "%m")
# create year-month
df$year_month <- format(df$date, "%Y/%m")
# create quarter
df$quarter <- quarters(df$date)
# crate year-quarter
df$year_quarter <- as.yearqtr(df$date, format = "%m/%d/%Y")

# write.csv(df, "all_values_with_all_dates.csv")



# visulize the values
# status
df_plot <- df %>% group_by(year_quarter) %>% summarize(mean_status = mean(status_pred)) %>% arrange(year_quarter)
ggplot(data = df_plot %>% filter(year_quarter >= "2018 Q1"), aes(x = year_quarter, y = mean_status)) + geom_point() + geom_smooth() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Status Value Frequency") + xlab("Time (Year-Quarter)") + 
  ggtitle("Dynamics of Status Value (2018 Q1 - 2020 Q3)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), size = 15)) 

  
# thrift
df_plot <- df %>% group_by(year_quarter) %>% summarize(mean_status = mean(thrift_pred)) %>% arrange(year_quarter)
ggplot(data = df_plot %>% filter(year_quarter >= "2018 Q1"), aes(x = year_quarter, y = mean_status)) + geom_point() + geom_smooth() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Thrift Value Frequency") + xlab("Time (Year-Quarter)") + 
  ggtitle("Dynamics of Thrift Value (2018 Q1 - 2020 Q3)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), size = 15)) 


# health
df_plot <- df %>% group_by(year_quarter) %>% summarize(mean_status = mean(health_pred)) %>% arrange(year_quarter)
ggplot(data = df_plot %>% filter(year_quarter >= "2018 Q1"), aes(x = year_quarter, y = mean_status)) + geom_point() + geom_smooth() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Health Value Frequency") + xlab("Time (Year-Quarter)") + 
  ggtitle("Dynamics of Health Value (2018 Q1 - 2020 Q3)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), size = 15)) 

# social
df_plot <- df %>% group_by(year_quarter) %>% summarize(mean_status = mean(social_pred)) %>% arrange(year_quarter)
ggplot(data = df_plot %>% filter(year_quarter >= "2018 Q1"), aes(x = year_quarter, y = mean_status)) + geom_point() + geom_smooth() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Social Connectedness Value Frequency") + xlab("Time (Year-Quarter)") + 
  ggtitle("Dynamics of Social Connectedness Value (2018 Q1 - 2020 Q3)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), size = 15)) 


# Individual
df_plot <- df %>% group_by(year_quarter) %>% summarize(mean_status = mean(ind_pred)) %>% arrange(year_quarter)
ggplot(data = df_plot %>% filter(year_quarter >= "2018 Q1"), aes(x = year_quarter, y = mean_status)) + geom_point() + geom_smooth() +
  theme_economist() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ylab("Individuality Value Frequency") + xlab("Time (Year-Quarter)") + 
  ggtitle("Dynamics of Individuality Value (2018 Q1 - 2020 Q3)") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), size = 15),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 0), size = 15)) 


# logit analysis
bar = 0.5

df <- df %>% mutate(status = ifelse(status_pred >= bar, 1, 0), 
                    ind = ifelse(ind_pred >= bar, 1, 0),
                    social = ifelse(social_pred >= bar, 1, 0),
                    thrift = ifelse(thrift_pred >= bar, 1, 0),
                    health = ifelse(health_pred >= bar, 1, 0))

df <- df %>% mutate(helpful = ifelse(numberOfHelpful >= 1, 1, 0))

df <- df %>% mutate(pandemic = ifelse((year == "2020" & (quarter == "Q2" | quarter == "Q3")), 1, 0))

df_new <- df %>% filter(year == "2020" | year == "2019")

typeof(df$year)

df_new$year = as.factor(df_new$year)

a <- glm(thrift ~ pandemic +helpful + year + quarter +  category , data = df_new, family = "binomial")
summary(a)

b <- glm(status ~ pandemic +helpful + year + quarter +  category, data = df_new, family = "binomial")
summary(b)

c <- glm(ind ~ pandemic +helpful + year + quarter +  category, data = df_new, family = "binomial")
summary(c)

d <- glm(social ~ pandemic +helpful + year + quarter +  category, data = df_new, family = "binomial")
summary(d)

e <- glm(health ~ pandemic +helpful + year + quarter +  category, data = df_new, family = "binomial")
summary(e)


stargazer(a, d, c, e, b, type="text",
          dep.var.labels=c("Thrift", "Social Conncectedness", "Individuality", "health", "Status"),
          covariate.labels=c("Pandemic", "Helpful", "Year 2020", "Quarter Q2", "Quarter Q3", "Quarter Q4",
                             "Electronics", "Home Improvement", "Personal healthcare", "Software"), out="models.txt")

# stepwise regression on status
status1 <- glm(status ~ pandemic + helpful, data = df_new, family = "binomial")
status2 <- glm(status ~ pandemic + helpful + year , data = df_new, family = "binomial")
status3 <- glm(status ~ pandemic + helpful + year + quarter, data = df_new, family = "binomial")
status4 <- glm(status ~ pandemic + helpful + year + quarter +  category, data = df_new, family = "binomial")

stargazer(status1, status2, status3, status4, type="text",
          dep.var.caption = c("Dependent Variable: Status Value"),
          model.labels = c("1", "2", "3", "4"),
          covariate.labels=c("Pandemic", "Helpful", "Year 2020", "Quarter Q2", "Quarter Q3", "Quarter Q4",
                             "Electronics", "Home Improvement", "Personal healthcare", "Software"), out="status.txt")




# not using

# average value of year-quarter
status_2020_Q1 = df %>% filter(year == 2020, quarter == "Q1") %>% select(status_pred)
status_2019_Q1 = df %>% filter(year == 2019, quarter == "Q1") %>% select(status_pred)
status_2020_Q2 = df %>% filter(year == 2020, quarter == "Q2") %>% select(status_pred)
status_2019_Q2 = df %>% filter(year == 2019, quarter == "Q2") %>% select(status_pred)

# t-test 
# year to year comparison 2019 Q1 Q2 to 2020 Q1 Q2 respectively.
t.test(status_2019_Q1$status_pred, status_2020_Q1$status_pred)
t.test(status_2019_Q2$status_pred, status_2020_Q2$status_pred)
