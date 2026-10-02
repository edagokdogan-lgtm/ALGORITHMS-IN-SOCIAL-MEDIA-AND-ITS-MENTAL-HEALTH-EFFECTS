# SOC 225 Data Project 
# Eda Gokdogan 
# The goal of this project is to examine social media and mental health by investigating the correlation between the social media usage and its impact on mental health
# The project aims to address computer algorithms and the content people see on social media. The research mainly focuses on the age group from 18 to 29.  
# The research examines whether social media usage has an effect on mental health variables. These variables show the reflections of users' interactions with social media platforms and its psychological effects of the interactions
# Algorithm-Driven content shown on the social media to maximize user engagement could lead to involvement of comparisons, feeling of inadequacy and depression.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

social_media_data <- read.csv("smmh.csv")
# The data is from Souvik Ahmed aiming to analyze the correlation between mental health and social media usage. 
# The variables of the data are: 
# Age
# gender
# relationship status
# occupational status
# affiliated organisations
# social media used
# time spent on social media in hours.

# The questions asked for potential mental health issues are: 
# How often do you find yourself using Social media without a specific purpose?
# How often do you get distracted by Social media when you are busy doing something?
# Do you feel restless if you haven't used Social media in a while?
# How easily distracted are you?
# How much are you bothered by worries?
# Do you find it difficult to concentrate on things?
# How often do you compare yourself to other successful people through the use of social media?
# Following the previous question, how do you feel about these comparisons, generally speaking?
# How often do you feel depressed or down?
# How frequently does your interest in daily activities fluctuate?
# How often do you face issues regarding sleep?

# The data uses 1 as the least and 5 as the most scale. 

#Loading libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(rpart)

# Calculating the distribution for  the average time spent on social media by age group using dplyr package 
# This represents the age group from 13 to 99 and the time that they spent on social media without a specific purpose.
average_time_spent <- social_media_data %>% 
  group_by(X1..What.is.your.age.) %>%  summarize(mean_time_spent = mean(X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose., na.rm = TRUE))

# Creating a plot for the visualization 
ggplot(average_time_spent, aes(x = X1..What.is.your.age., y = mean_time_spent)) +
  geom_bar(stat = "identity", fill = "orange2", width = 0.95) +
  labs(title = "Distribution of Average Time Spent on Social Media Without A Specific Purpose",
       x = "Age",
       y = "Average Time Spent (1-5 Scale)"
       ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold"), 
    axis.title = element_text(size = 10, color = "violet",face="bold"),
    axis.text = element_text(size = 10)) 


# This is the mean for the following questions: 
# How often do you find yourself using Social media without a specific purpose?
# How often do you compare yourself to other successful people through the use of social media?
# Following the previous question, how do you feel about these comparisons, generally speaking?
# How often do you feel depressed or down?
descriptive_stats <- social_media_data %>% 
  reframe(
    avg_age = mean(X1..What.is.your.age., na.rm = TRUE),
    avg_time_on_social_media = mean(X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose., na.rm = TRUE),
    avg_social_media_comparison = mean(X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media., na.rm = TRUE),
    avg_feeling_depressed = mean(X18..How.often.do.you.feel.depressed.or.down., na.rm = TRUE),
    avg_comparison_feeling = mean(X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking., na.rm=TRUE),
    avg_seek_validation = mean(X17..How.often.do.you.look.to.seek.validation.from.features.of.social.media.,na.rm=TRUE)
  ) 

print(descriptive_stats)
# The average age is noted as 26 years old for this data. 
# The average for feeling depressed and down is 3.55/5. This high value suggest that there is high indication of depressive feelings in the sample. 

library(ggplot2)
library(tidyr)
boxplot_data <- social_media_data %>% 
  select(X1..What.is.your.age., 
         X9..How.often.do.you.find.yourself.using.Social.media.without.a.specific.purpose., 
         X15..On.a.scale.of.1.5..how.often.do.you.compare.yourself.to.other.successful.people.through.the.use.of.social.media., 
         X18..How.often.do.you.feel.depressed.or.down., 
         X16..Following.the.previous.question..how.do.you.feel.about.these.comparisons..generally.speaking., 
         X17..How.often.do.you.look.to.seek.validation.from.features.of.social.media.)
# Changing the column names for better interpretation of the data 
colnames(boxplot_data) <- c("Age", "Social Media Usage Time without Specific Purpose", "Comparing  with Others", 
                            "Feeling Depressed or Down", "Comparison Feelings", "Seeking Validation")

# this is the pivot longer function that I searched online for the boxplot
boxplot_data_long <- boxplot_data %>% 
  pivot_longer(cols = -Age, names_to = "Variable", values_to = "Value")
ggplot(boxplot_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(title = "Box Plots of Social Media Usage and Mental Health Variables",
       x = "Questions about Mental Health ",
       y = "Responses (1-5 Scale)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, color="black"))
# Key Takeaways from the descriptive statistics, histogram and boxplot: 
# The box plot shows that except from the seeking validation through social media, there is a high indication that 
# The social media usage create comparison with others, feeling negative about this comparison feeling and leads to feel depressed and down. 
# The boxplot shows the frequency of users' engagement with social media without a specific purpose. 
# High engagement levels without specific purpose could show that the algorithms are forcing the user to engage with the platform. 
# Comparison with others is the question that shows how frequently users compare themselves to others on social media. These algorithms could prioritize highly popular content 
# This could amplify the comparisons and also having an effect on depressive feelings due to too much exposure of the content or content that triggers negative emotional responses. 

# Machine Learning Social Media Data Filtering 
social_media_data_filtered <- social_media_data %>% 
  select(-X3..Relationship.Status, -X2..Gender, -X4..Occupation.Status,-X5..What.type.of.organizations.are.you.affiliated.with., -X6..Do.you.use.social.media.,-X7..What.social.media.platforms.do.you.commonly.use.)
social_media_data_filtered$X8..What.is.the.average.time.you.spend.on.social.media.every.day. <- as.factor(social_media_data_filtered$X8..What.is.the.average.time.you.spend.on.social.media.every.day.)


# Based on LAB 13 and testing rpart and other modeling to test the best machine learning to determine depressed correlation 
# Split the dataset into training and testing sets
social_media_data$depressed <- ifelse(social_media_data$X18..How.often.do.you.feel.depressed.or.down. >= 3.5, 1, 0)
set.seed(123)
train_index <- sample(1:nrow(social_media_data), 0.7 * nrow(social_media_data))
train_data <- social_media_data[train_index,]
test_data <- social_media_data[-train_index,]
rpart_model <- rpart(depressed ~ ., data = train_data, method = "class")
print(rpart_model)
test_predictions_rpart <- predict(rpart_model, data = test_data, method = "class")
total_predictions <- length(test_predictions_rpart)
correct_predictions <- sum(test_predictions_rpart == test_data$depressed)
accuracy_1 <- correct_predictions / total_predictions
print(accuracy_1)
# The results shows that the data needs improvement in order to asses the correlation between social media usage and mental health. 
# The results shows 0.50297 accuracy for the test. 

# Calculating the same result using glm modeling 
glm_model <- glm(depressed ~ ., data = train_data, family = binomial)
test_probabilities <- predict(glm_model, data = test_data, method = "response")
test_predictions_glm <- ifelse(test_probabilities > 0.5, 1, 0)
test_data$depressed <- as.factor(test_data$depressed)
correct_predictions <- sum(test_predictions_glm == test_data$depressed)
total_predictions <- length(test_predictions_glm)
accuracy_2 <- correct_predictions / total_predictions
print(accuracy_2)
# The accuracy using glm model shows a similar accuracy with 0.5059524. 




