library(tidyverse)
library(readxl)
ucl <- read.csv(file.choose())
# Unpacking the required packages
library(dlookr)
View(ucl)
str(ucl)
diagnose_outlier(ucl)
library(missRanger)
library(visdat)
plot_na_pareto(ucl)
vis_dat(ucl)
vis_miss(ucl)

# Decode EDUCATION variable
ucl_decoded <- ucl %>% 
  mutate(SEX = ifelse(SEX == 1, "male", "female"),
    EDUCATION = case_when(
      EDUCATION == 1 ~ "graduate school",
      EDUCATION == 2 ~ "university",
      EDUCATION == 3 ~ "high school",
      EDUCATION == 4 ~ "others",
      EDUCATION == 5 ~ "unknown",
      EDUCATION == 6 ~ "unknown",
      EDUCATION == 0 ~ "unknown",
      TRUE ~ as.character(EDUCATION)  # keep other values unchanged
    ),
MARRIAGE = case_when( MARRIAGE ==1 ~"married",MARRIAGE== 2 ~"single", MARRIAGE == 3~"others"),
PAY_0 = case_when(PAY_0==-1~"pay duly",
                  PAY_0 ==1~"payment delay for one month",
                  PAY_0==2~"payment delay for two months",
                  PAY_0 ==3~"payment delay for three months",
                  PAY_0 ==4~"payment delay for four months",
                  PAY_0 ==5~"payment delay for five months",
                  PAY_0 ==6~"payment delay for six months",
                  PAY_0 ==7~"payment delay for seven months",
                  PAY_0 ==8~"payment delay for eight months", 
                  PAY_0 ==9~"payment delay for nine months and above",
                  PAY_0 ==0~"Neutral",
                  PAY_0 ==-2~"Pay twice duly",
TRUE ~ as.character(PAY_0)),
PAY_2 = case_when(PAY_2==-1~"pay duly",
                  PAY_2 ==1~"payment delay for one month",
                  PAY_2==2~"payment delay for two months", 
                  PAY_2 ==3~"payment delay for three months",
                  PAY_2 ==4~"payment delay for four months",
                  PAY_2 ==5~"payment delay for five months",
                  PAY_2 ==6~"payment delay for six months",
                  PAY_2 ==7~"payment delay for seven months",
                  PAY_2 ==8~"payment delay for eight months", 
                  PAY_2 ==9~"payment delay for nine months and above",
                  PAY_2 ==0~"Neutral",
                  PAY_2 ==-2~"Pay twice duly",
TRUE ~ as.character(PAY_2)),
PAY_3 =  case_when(PAY_3==-1~"pay duly",
                   PAY_3 ==1~"payment delay for one month",
                   PAY_3==2~"payment delay for two months", 
                   PAY_3 ==3~"payment delay for three months",
                   PAY_3 ==4~"payment delay for four months",
                   PAY_3 ==5~"payment delay for five months",
                   PAY_3 ==6~"payment delay for six months",
                   PAY_3 ==7~"payment delay for seven months",
                   PAY_3 ==8~"payment delay for eight months", 
                   PAY_3 ==9~"payment delay for nine months and above",
                   PAY_3 ==0~"Neutral",
                   PAY_3 ==-2~"Pay twice duly",
TRUE ~ as.character(PAY_3)),
PAY_4 =  case_when(PAY_4==-1~"pay duly",
                   PAY_4 ==1~"payment delay for one month",
                   PAY_4==2~"payment delay for two months", 
                   PAY_4 ==3~"payment delay for three months",
                   PAY_4 ==4~"payment delay for four months",
                   PAY_4 ==5~"payment delay for five months",
                   PAY_4 ==6~"payment delay for six months",
                   PAY_4 ==7~"payment delay for seven months",
                   PAY_4 ==8~"payment delay for eight months", 
                   PAY_4 ==9~"payment delay for nine months and above",
                   PAY_4 ==0~"Neutral",
                   PAY_4 ==-2~"Pay twice duly",
TRUE ~ as.character(PAY_4)),
PAY_5 = case_when(PAY_5==-1~"pay duly",
                  PAY_5 ==1~"payment delay for one month",
                  PAY_5==2~"payment delay for two months", 
                  PAY_5 ==3~"payment delay for three months",
                  PAY_5 ==4~"payment delay for four months",
                  PAY_5 ==5~"payment delay for five months",
                  PAY_5 ==6~"payment delay for six months",
                  PAY_5 ==7~"payment delay for seven months",
                  PAY_5 ==8~"payment delay for eight months", 
                  PAY_5 ==9~"payment delay for nine months and above",
                  PAY_5 ==0~"Neutral",
                  PAY_5 ==-2~"Pay twice duly",
TRUE ~ as.character(PAY_5)),
PAY_6 =  case_when(PAY_6==-1~"pay duly",
                   PAY_6 ==1~"payment delay for one month",
                   PAY_6==2~"payment delay for two months", 
                   PAY_6 ==3~"payment delay for three months",
                   PAY_6 ==4~"payment delay for four months",
                   PAY_6 ==5~"payment delay for five months",
                   PAY_6 ==6~"payment delay for six months",
                   PAY_6 ==7~"payment delay for seven months",
                   PAY_6 ==8~"payment delay for eight months", 
                   PAY_6 ==9~"payment delay for nine months and above",
                   PAY_6 ==0~"Neutral",
                   PAY_6 == -2~"Pay twice duly",
TRUE ~ as.character(PAY_6))
)
View(ucl_decoded)

sex_descr <- ucl_decoded %>% select(default.payment.next.month, SEX)%>% 
  group_by(default.payment.next.month) %>% count(SEX) %>% mutate(per = n/sum(n)*100)

# Calculate percentages by SEX
sex_per<- ucl_decoded %>%
  count(SEX) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(sex_per, aes(x = percentage , y = SEX)) +
  geom_bar(stat = "identity", fill="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Adjust width as needed
            hjust = 4, # Adjust vertical position
            size = 6,      # Adjust text size
            color = "white", 
            fontface = "bold") +  # Make text bold
  labs(title = "Distribution of gender ",
       y = "Percentage") +
  theme(axis.text.x = element_blank(),  
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(color = "black", face = "bold", size =30))




# Education

ucl_decoded %>% select(default.payment.next.month, EDUCATION)%>% 
  group_by(default.payment.next.month) %>% count(EDUCATION) %>% mutate(per = n/sum(n)*100)

edu_per <- ucl_decoded %>% 
  count(EDUCATION) %>%
  mutate(percentage = n / sum(n) * 100)


ggplot(edu_per, aes(x = percentage , y = EDUCATION
                    )) +
  geom_bar(stat = "identity", fill="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Adjust width as needed
            hjust = 0,  # Adjust vertical position
            size = 4,      # Adjust text size
            color = "black", 
            fontface = "bold") +  # Make text bold
  labs(title = "Distribution of education ",
       y = "Percentage") +
  theme(axis.text.x = element_blank(),  
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(color = "black", face = "bold"))


# Marriage
ucl_decoded %>% select(default.payment.next.month, MARRIAGE)%>% 
  filter(MARRIAGE != "NA") %>% group_by(default.payment.next.month) %>% count(MARRIAGE) %>% mutate(per = n/sum(n)*100)



edu_per <- ucl_decoded %>% filter(MARRIAGE != "NA") %>% 
  count(MARRIAGE) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(edu_per, aes(x = percentage , y =MARRIAGE)) +
         geom_bar(stat = "identity", fill="red") +
         geom_text(aes(label = paste0(round(percentage, 1), "%")),
                    # Adjust width as needed
                   hjust = 0,  # Adjust vertical position
                   size = 4,      # Adjust text size
                   color = "black", 
                   fontface = "bold") +  # Make text bold
         labs(title = "Distribution of marriage ",
              y = "Percentage") +
         theme(axis.text.x = element_blank(),  
               plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
               axis.title.y = element_blank(), 
               axis.text.y = element_text(color = "black", face = "bold"))
# PAY_0
library(summarytools)
summarytools::descr(ucl_decoded)
k <-xtabs(~PAY_0+default.payment.next.month, data = ucl_decoded)
round(k/sum(k)*100,2)


ucl_decoded %>% select(default.payment.next.month, PAY_0)%>% 
   group_by(default.payment.next.month) %>% count(PAY_0) %>% mutate(per = n/sum(n)*100)


PAY_0_per <- ucl_decoded %>%
  count(PAY_0) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_0_per, aes(x = percentage, y = PAY_0)) +
  geom_bar(stat = "identity", fill="red")+
  geom_text(aes(label = paste0(round(percentage, 1), "%")),  # Adjust width as needed
            hjust = 0,  # Adjust vertical position
            size = 5,      # Adjust text size
            color = "black", 
            fontface = "bold") +  # Make text bold
  labs(title = "Distribution of marriage ",
       y = "Percentage") +
  theme(axis.text.x = element_blank(),  
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title.y = element_blank(), 
        axis.text.y = element_text(color = "black", face = "bold"))


# PAY_2
k1 <-xtabs(~PAY_2+default.payment.next.month, data = ucl_decoded)
round(k1/sum(k1)*100,2)
PAY_2_per <- ucl_decoded %>%
  count(PAY_2) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_2_per, aes(x = percentage, y = PAY_2)) +
  geom_bar(stat = "identity", fill ="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = 0) +
  labs(title = "Distribution of Repayment status in August, 2005   ",
       y = "Percentage")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
                               axis.title.y = element_blank(), 
                               axis.text.y = element_text(color = "black", face = "bold")
                               ) # Remove grid lines



# PAY_3
k2 <-xtabs(~PAY_3+default.payment.next.month, data = ucl_decoded)
round(k2/sum(k2)*100,2)
PAY_3_per <- ucl_decoded %>%
  count(PAY_3) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_3_per, aes(x = percentage, y = PAY_3)) +
  geom_bar(stat = "identity", fill ="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = 0) +
  labs(title = "Distribution of Repayment status in July, 2005    ",
       y = "Percentage")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
                               axis.title.y = element_blank(), 
                               axis.text.y = element_text(color = "black", face = "bold")
                               ) # Remove grid lines




# PAY_4
k3 <-xtabs(~PAY_4+default.payment.next.month, data = ucl_decoded)
round(k3/sum(k3)*100,2)

PAY_4_per <- ucl_decoded %>%
  count(PAY_4) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_4_per, aes(x =percentage, y =  PAY_4)) +
  geom_bar(stat = "identity", fill="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust=0)+
  labs(title = "Distribution of Repayment status in June, 2005     ",
       y = "Percentage")+
   theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, 
                                  color = "black", face = "bold"), axis.title.y = element_blank(), 
        axis.text.y = element_text(color = "black", face = "bold")
        
        
  )

# PAY_5
k4 <-xtabs(~PAY_5+default.payment.next.month, data = ucl_decoded)
round(k4/sum(k4)*100,2)

PAY_5_per <- ucl_decoded %>%
  count(PAY_5) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_5_per, aes(x =  percentage , y =PAY_5)) +
  geom_bar(stat = "identity", fill="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust=0) +
  labs(title = "Distribution of Repayment status in May, 2005 ", y="Percentage")+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                        plot.title = element_text(hjust = 0.5, 
              color = "black", face = "bold"), axis.title.y = element_blank(), 
        axis.text.y = element_text(color = "black", face = "bold")
        

)

# PAY_6
k5 <-xtabs(~PAY_6+default.payment.next.month, data = ucl_decoded)
round(k5/sum(k5)*100,2)


PAY_6_per <- ucl_decoded %>%
  count(PAY_6) %>%
  mutate(percentage = n / sum(n) * 100)

# Create the bar plot with percentage scale on the y-axis
ggplot(PAY_6_per, aes(x = percentage  , y = PAY_6)) +
  geom_bar(stat = "identity", fill ="red") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = 0) +
  labs(title = "Distribution of Repayment status in April, 2005 ", y="")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) # Remove grid lines



# Plotting of the continous variables 
ucl_decoded <- ucl_decoded %>% mutate( default.payment.next.month= 
                                         ifelse(default.payment.next.month ==1, "Yes", "No"))
# ggplot(ucl_decoded, aes(x = BILL_AMT1  , y= ..density.., fill = 
                          #default.payment.next.month))  +geom_density()
R <- ucl_decoded %>% select(default.payment.next.month, LIMIT_BAL,                
                            BILL_AMT1 ,                
                            BILL_AMT2 ,                
                            BILL_AMT3 ,                
                            BILL_AMT4 ,                
                            BILL_AMT5 ,                 
                            BILL_AMT6 ,                
                            PAY_AMT1 ,               
                            PAY_AMT2 ,                  
                            PAY_AMT3,            
                            PAY_AMT4,               
                            PAY_AMT5,                
                            PAY_AMT6, AGE )  
R %>% filter(default.payment.next.month=="No") %>% descr()
R %>% filter(default.payment.next.month=="Yes") %>% descr()

ggplot(ucl_decoded, aes(y = BILL_AMT1, fill = 
                          default.payment.next.month))  +geom_boxplot()+
  labs(title = "Distribution of amount of bill statement in September, 2005 (NT dollar) ")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(y = BILL_AMT2  ,fill =
                          default.payment.next.month))  + geom_boxplot()+
  labs(title = "Amount of bill statement in August, 2005 (NT dollar) ")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(y = BILL_AMT3 , fill =
                          default.payment.next.month)) +geom_boxplot() +
  labs(title = "Amount of bill statement in July, 2005 (NT dollar) ")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(y = BILL_AMT4 , fill = 
                          default.payment.next.month)) +geom_boxplot()+
labs(title = "Amount of bill statement in June, 2005 (NT dollar) ")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                             plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 

ggplot(ucl_decoded, aes(y = BILL_AMT5 , fill = 
                          default.payment.next.month)) +geom_boxplot()+
  labs(title = "Amount of bill statement in May, 2005 (NT dollar) ")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 

ggplot(ucl_decoded, aes(y = BILL_AMT6  ,  fill = 
                          default.payment.next.month))  +geom_boxplot()+
  labs(title = "Amount of bill statement in April, 2005 (NT dollar)")+theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                               plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 

ggplot(ucl_decoded, aes(y= PAY_AMT1  , fill = 
                          default.payment.next.month)) +geom_boxplot()+
  labs(title = "Amount of previous payment in September, 2005 (NT dollar)")+
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
                    plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 


ggplot(ucl_decoded, aes(y= PAY_AMT2  , fill = 
                          default.payment.next.month))  + geom_boxplot()+
labs(title = "Amount of previous payment in August, 2005 (NT dollar)") +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(y= PAY_AMT3  , fill = 
                          default.payment.next.month)) + geom_boxplot()+
  labs(title = "Amount of previous payment in July, 2005 (NT dollar)") +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 

ggplot(ucl_decoded, aes(y= PAY_AMT4  , fill = 
                          default.payment.next.month)) + geom_boxplot()+
  labs(title = "Amount of previous payment in June, 2005 (NT dollar)") +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 


ggplot(ucl_decoded, aes(y= PAY_AMT5  , fill = 
                          default.payment.next.month))  +geom_boxplot()+
  labs(title = "Amount of previous payment in May, 2005 (NT dollar)") +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(y= PAY_AMT6  , fill = 
                          default.payment.next.month))  +geom_boxplot()+
  labs(title = "Amount of previous payment in April, 2005 (NT dollar)") +
  theme(axis.text.x = element_blank(),  # Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(x= LIMIT_BAL  , y= ..density.., fill = 
                          default.payment.next.month))  +geom_density()+
  labs(title = "Distribution of amount of given credit in NT dollars \n (includes individual and family/supplementary credit") +
  theme(# Remove x-axis labels  # Make axis lines black
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 



ggplot(ucl_decoded, aes(x= AGE  , y= ..density.., fill = 
                          default.payment.next.month))  +geom_density()+
labs(title = "Distribution of Age in years") +
  theme(# Remove x-axis labels  # Make axis lines black
    plot.title = element_text(hjust = 0.5, color = "black", face = "bold")) 

# Correlation Analysis

## Unpack the GGCORRPLOT PACKAGE
 library(ggcorrplot)
selected <- ucl %>% select( LIMIT_BAL,                
                            BILL_AMT1 ,                
                            BILL_AMT2 ,                
                            BILL_AMT3 ,                
                            BILL_AMT4 ,                
                            BILL_AMT5 ,                 
                            BILL_AMT6 ,                
                            PAY_AMT1 ,               
                            PAY_AMT2 ,                  
                            PAY_AMT3,            
                            PAY_AMT4,               
                            PAY_AMT5,                
                             PAY_AMT6 ) 
x = cor(selected)
ggcorrplot(corr = x, method = "square", lab = T)


# Partioning the dataset into subset 75/ 25
## Unpack the rewuired package
library(caTools)
model_data <- ucl %>% select(-ID)
sample <- sample.split(model_data$LIMIT_BAL, SplitRatio = 0.75)
Train <- subset(model_data, sample== T)
Test <- subset(model_data, sample== F)


library(effects)
library(performance)
library(sjPlot)
## Linear probability model (LPM)
LPM <- lm(default.payment.next.month~. , data = Train)
summary(LPM)

## Logistic regression model
Logit <- glm(default.payment.next.month~., family = binomial(link="logit") , data = Train)
summary(Logit)

## Predictions
LPM_predict <- predict(LPM, newdata = Test)
# Predicting on the test dataset
prediction <- predict(Logit, newdata = test, type = "response")

# Converting probabilities to binary predictions (0 or 1)
binary_prediction <- ifelse(prediction > 0.5, 1, 0)
# Creating the confusion matrix
conf_matri <- table(test$default.payment.next.month, binary_prediction)

# Printing the confusion matrix
print("Confusion Matrix:")
print(conf_matri)
plot(conf_matri)

# Calculating accuracy
accurac <- sum(diag(conf_matri)) / sum(conf_matri)
print(paste("Accuracy:", round(accurac * 100, 2), "%"))

# Convert confusion matrix to data frame with appropriate column names
conf_matrixd <- as.data.frame.table(conf_matrix)
conf_matrixd$Var1 <- factor(conf_matrixd$Var1, levels = rownames(conf_matri))
conf_matrixd$Var2 <- factor(conf_matrixd$Var2, levels = colnames(conf_matri))

# Generate plot using ggplot2
ggplot(data = conf_matrixd) +
  geom_tile(aes(Var1, binary_predictions, fill = Freq)) +
  geom_text(aes(Var1, binary_predictions, label = Freq), color = "white", size = 8) +
  scale_fill_gradient(low = "lightcoral", high = "red") +  # Adjust colors as needed
  labs(x = "Actual Class", y = "Predicted Class", title = "Confusion Matrix") +
  theme_minimal() +  # Change theme as needed
  theme(axis.text.y =
          element_text(size=20),axis.text.x = element_text( hjust = 1, size = 20), 
        plot.title = element_text(hjust = 0.5))  # Center title

library(pROC)

# Create ROC curve
roc_curve <- roc(test$default.payment.next.month, prediction)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add 45-degree line for reference
lines(x = c(0,1), y = c(0,1), col = "red", lty = 2)

# Calculate AUC
auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), adj = 0)

# Add legend
legend("bottomright", legend = c("ROC Curve", "Random Guess"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))



# Install the writexl package if you haven't already
 install.packages("writexl")

# Load the writexl library
library(writexl)

# Write the dataset to an Excel file
write_xlsx(ucl_decoded, "ucl_decoded_dataset.xlsx")

# Load necessary libraries
library(pROC)

# Predict probabilities for the test set
logit_pred <- predict(Logit, newdata = test, type = "response")

# Create ROC curve
roc_curve <- roc(test$default.payment.next.month, logit_pred)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# Add 45-degree line for reference
lines(x = c(0,1), y = c(0,1), col = "red", lty = 2)

# Calculate AUC
auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), adj = 0)

# Add legend
legend("bottomright", legend = c("ROC Curve", "Random Guess"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))


######probit
probit <- glm(default.payment.next.month~., data = Train, family = binomial(link="probit"))
summary(probit)

# Load necessary libraries
library(pROC)

# Predict probabilities for the test set
logit_pred <- predict(logit, newdata = test, type = "response")


####Gaussian
probit <- glm(default.payment.next.month~., data = Train, family = gaussian(link = "identity"))
summary(probit)
plot(allEffects(LPM))
plot( LPM)
plot_model(LPM,show.values = T)
plot_residuals(LPM)





# Corrected model after all insignificant features has been remove

significant_data <-ucl %>% select(
AGE, PAY_0 , PAY_2, PAY_3 , BILL_AMT1, BILL_AMT3, PAY_AMT1,
PAY_AMT2, PAY_AMT6, PAY_AMT5, PAY_AMT4, default.payment.next.month)

## Partioning the dataset into subset 75/ 25

sample_2 <- sample.split(significant_data$AGE, SplitRatio = 0.75)
Train_2 <- subset(significant_data, sample_2== T)
test_2 <- subset(significant_data, sample_2== F)


## Linear probability model
LPM_2 <- lm(default.payment.next.month~. , data = Train_2)
summary(LPM_2)



## Logistic regression model
Train_2$default.payment.next.month <- as.factor(Train_2$default.payment.next.month)
test_2$default.payment.next.month <- as.factor(test_2$default.payment.next.month)
Logit_2 <- glm(default.payment.next.month~., family = binomial(link="logit") , data = Train_2)
summary(Logit_2)
plot(allEffects(Logit_2))
plot(Logit_2)
plot_model(Logit_2,show.values = T)
plot_residuals(Logit_2)


## Predicting on the test dataset
predictions <- predict(Logit_2, newdata = test_2, type = "response")

## Converting probabilities to binary predictions (0 or 1)
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

## Creating the confusion matrix
conf_matrix <- table(test_2$default.payment.next.month, binary_predictions)

## Printing the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)
plot(conf_matrix)

## Calculating accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

## Convert confusion matrix to data frame with appropriate column names
conf_matrix_df <- as.data.frame.table(conf_matrix)
conf_matrix_df$Var1 <- factor(conf_matrix_df$Var1, levels = rownames(conf_matrix))
conf_matrix_df$Var2 <- factor(conf_matrix_df$Var2, levels = colnames(conf_matrix))

## Generate plot using ggplot2
ggplot(data = conf_matrix_df) +
  geom_tile(aes(Var1, binary_predictions, fill = Freq)) +
  geom_text(aes(Var1, binary_predictions, label = Freq), color = "white", size = 8) +
  scale_fill_gradient(low = "lightcoral", high = "red") +  # Adjust colors as needed
  labs(x = "Actual Class", y = "Predicted Class", title = "Confusion Matrix") +
  theme_minimal() +  # Change theme as needed
  theme(axis.text.y =
          element_text(size=20),axis.text.x = element_text( hjust = 1, size = 20), 
        plot.title = element_text(hjust = 0.5))  # Center title


library(pROC)

Logit_2_predict <- predict(Logit_2, newdata = test_2, type="response")

### Create ROC curve
roc_curve_logit_2 <- roc(test_2$default.payment.next.month, Logit_2_predict)

### Plot ROC curve
plot(roc_curve_logit_2, main = "ROC Curve", col = "blue", lwd = 2)

### Add 45-degree line for reference
lines(x = c(0,1), y = c(0,1), col = "red", lty = 2)

### Calculate AUC
auc_value_logit_2 <- auc(roc_curve_logit_2)
text(0.8, 0.2, paste("AUC =", round(auc_value_logit_2, 2)), adj = 0)

### Add legend
legend("bottomright", legend = c("ROC Curve", "Random Guess"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))


## probit
probit_2 <- glm(default.payment.next.month~., data = Train_2, family = binomial(link="probit"))
summary(probit_2)
plot(allEffects(probit_2))
plot(probit_2)
plot_model(probit_2,show.values = T)
plot_residuals(probit_2)
logit_pred <- predict(probit_2, newdata = test)

probit_pred_2 <- predict(probit_2, newdata = test_2, type = "response")

### Create ROC curve
roc_curve_2_logit <- roc(test_2$default.payment.next.month, probit_pred_2)

### Plot ROC curve
plot(roc_curve_2_logit, main = "ROC Curve", col = "blue", lwd = 2)

### Add 45-degree line for reference
lines(x = c(0,1), y = c(0,1), col = "red", lty = 2)

### Calculate AUC
auc_value_probit <- auc(roc_curve_2_logit)
text(0.8, 0.2, paste("AUC =", round(auc_value_probit, 2)), adj = 0)

### Add legend
legend("bottomright", legend = c("ROC Curve", "Random Guess"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))


## Gaussian
gaus <- glm(default.payment.next.month~., data = Train, family = gaussian(link = "identity"))
summary(gaus)
plot(allEffects(gaus))
plot( gaus)
plot_model(gaus,show.values = T)
plot_residuals(gaus)
