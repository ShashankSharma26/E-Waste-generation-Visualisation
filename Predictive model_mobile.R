library(glmnet)
library(tidyverse)
library(broom)

##reading the new file
df = read.csv("/Users/shashanksharma/Documents/sem 4/datafinal.csv")
#df[-c(1,2),]
#head(df)
#cor(df)
## 75% of the sample size
pairs(df)



##standardising year
df$new_year = df$Year - 2003

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]

###model for mobiles per 100 people in Australia
model <- lm(Mobile._100 ~ Population + X...GDP.PPE  +new_year + I(new_year^2) , data = train)
summary(model)

##predicting new values
predict(model, newdata=test)


sst <- sum((test$Mobile._100 - mean(test$Mobile._100))^2)
sse <- sum((predict(model, newdata=test) - test$Mobile._100)^2)
# R squared calculation
rsq <- 1 - sse / sst
rsq   ##0.9882


##reading test data
df_test = read.csv("/Users/shashanksharma/Documents/sem 4/data_test.csv")

##predicting new values
df_test$Mobile._100 = predict(model, newdata=df_test)

###
model_2 <- lm(Mobile ~ Population+ X...GDP.PPE  +new_year + I(new_year^2) , data = train)
summary(model_2)

predict(model_2, newdata=test)
sst <- sum((test$Mobile - mean(test$Mobile))^2)
sse <- sum((predict(model_2, newdata=test) - test$Mobile)^2)
# R squared
rsq <- 1 - sse / sst
rsq   ##.991

df_test$Mobile =  predict(model_2, newdata=df_test)

df_test = df_test[c(1:7),]

final_df = rbind(df,df_test)

write.csv(final_df, file = "/Users/shashanksharma/Documents/sem 4/data.csv")

df = read.csv("/Users/shashanksharma/Documents/sem 4/data.csv")

#plotting new values
plot1= ggplot(df, aes(Year, Mobile._100, colour=(Legend))) + geom_point()+
  geom_line() +labs(colour="Legend") +  theme(legend.text=element_text(size=15),axis.text=element_text(size=12),
                                              axis.title=element_text(size=14,face="bold")) + 
  ylab("Number of mobiles per 100 people in Australia") + scale_x_continuous(breaks = seq(1980, 2030, by = 5)) + scale_y_continuous(breaks = seq(0, 170, by = 10)) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  )
ggsave("/Users/shashanksharma/Documents/sem 4/plot1.png", plot=plot1, width=14, height=7)

ggplot(df, aes(Year, Mobile, colour=(Value))) + 
  geom_path()