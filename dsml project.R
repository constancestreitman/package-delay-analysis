#dsml project!

library(tidyverse)
library(randomForest)
library(caret)
library(pROC)
library(FactoMineR)
library(dplyr)
library(broom)
library(multcomp)
library(MASS)

### AN ASIDE - TESTING COST FOR NORMALITY
plot(df$Cost)
Q1 <- quantile(df$Cost, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Cost, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower <- Q1 - 1.5 * IQR
upper <- Q3 + 1.5 * IQR
df_no_outliers <- subset(df, Cost >= lower & Cost <= upper)
hist(df_no_outliers$Cost)
shapiro.test(df_no_outliers$Cost)
######################################################

###PCA

dfnumeric = df[8:11]
prcomp(dfnumeric, scale. = TRUE)
PCA(dfnumeric, scale= TRUE) #literally this easy. so dumb


################ cleanup for log reg
df$Shipment_Date <- as.Date(df$Shipment_Date, format = "%Y-%m-%d") #idk

##########################################################
####LOGISIC REGRESSION
CatReg <- glm(D_or_L ~ Origin_Warehouse + Destination + Carrier + ShipWeekday,
             data = df, family = binomial)
summary(CatReg) #NOTE: WArehouse_NYC and Boston have pval low. Also UPS. but
#regressions are weird(compares to random val in dataset as baseline) 
#so those p-vals need tested. also TUESDAY
anova(CatReg, test = "Chisq") #pvals all high as total vars, (not individ vals)


uglydf = df #so df isnt polluted by bs columns
# predicted probabilities
preds <- augment(CatReg, type.predict = "response", newdata = df)
uglydf$pred_prob <- preds$.fitted

# average predicted prob by destination
uglydf %>%
  group_by(Destination) %>%
  summarise(mean_delay_prob = mean(pred_prob))
#### HOW LIKELY IT IS TO GET DELAY/LOST IN EACH DESTINATION LOCATION!!!
#low is 8%, high is 20% (Boston)
# average predicted prob by WAREHOUSE!!!!!!!!!!
uglydf %>%
  group_by(Origin_Warehouse) %>%
  summarise(mean_delay_prob = mean(pred_prob))
### low of 8% high of 16%

#####
summary(glht(CatReg, linfct = mcp(Destination = "Tukey")))
#if this does nothing, its bc it takes forever to load
#basically inconclusive, high ass p values. get rid of it? idk

#REGRESSION FOR NUMERIC VARS
NumReg <- glm(D_or_L ~ Weight_kg + Distance_miles + Cost + Transit_Days,
                 data = df, family = binomial)
summary(NumReg) #pval low for transit days(duh) and for distance_miles


#Regression for everything ig
AllReg <- glm(D_or_L ~ Weight_kg + Distance_miles + Cost + Transit_Days +
                           Origin_Warehouse + Destination + Carrier + ShipWeekday,
                         data = df, family = binomial)
summary(AllReg) #FedEx and OnTrac suddenly become significant
#Sussy !


uglydf$UPS_flag <- ifelse(df$Carrier == "UPS", 1, 0)
uglydf$NYC_flag <- ifelse(df$Origin_Warehouse == "Warehouse_NYC", 1, 0)
uglydf$Boston_flag = ifelse(df$Destination == "Boston", 1, 0)
uglydf$Tuesday_flag = ifelse(df$ShipWeekday == "Tuesday", 1, 0)

#testing w another regression w the flagts to see if "rly significant"
model_flags <- glm(D_or_L ~ UPS_flag + NYC_flag + Boston_flag + Tuesday_flag,
                   data = uglydf, family = binomial) 
summary(model_flags) #only Boston still significant!?


# Get 95% confidence intervals for these hoes. IF THE LEFT IS OVER 1 OR THE
#RIGHT IS UNDER 1 THEN ITS SIGNIFICANT
exp(confint(model_flags)) #NYC, Boston
exp(confint(AllReg)) #distance, transit days(duh), Tuesday, OnTrac, FedEx
#yes the results are different; no idk

######## CV stuff
set.seed(2253221)
cv_ctrl = trainControl(method = "cv", number = 5)  # 8-fold CV

df$D_or_L <- as.factor(df$D_or_L)

cvmodel = train(
  D_or_L ~ Weight_kg + Distance_miles + Cost + Transit_Days +
    Origin_Warehouse + Destination + Carrier + ShipWeekday,
  data = df,
  method = "glm",
  family = "binomial",
  trControl = cv_ctrl
)

cvmodel

stepmodel = step(AllReg, direction = "both") ##############stepwise
summary(stepmodel)
#aka. only use these two vars ig

varImp(cvmodel)


########## GETTING RID OF TRANSIT DAYS
# Its stupid and I dont like it. Boom good talk. Gonna ask Niu abt it actually

NoTransit = glm(
  D_or_L ~ Weight_kg + Distance_miles + Cost +
    Origin_Warehouse + Destination + Carrier + ShipWeekday,
  data = df,
  family = binomial
)

summary(NoTransit)
stepNOTRANSIT = step(NoTransit, direction = "both")
summary(stepNOTRANSIT) #literally nothing lolxd

##cving this
set.seed(2253221)

NOTRANSIT.ctrl = trainControl(
  method = "cv",
  number = 8,       
  sampling = "up",     
  savePredictions = "final",
  classProbs = TRUE,   #lets me do other stuff idk
)

#aside stuff so the cv model works
uglydf$D_or_L <- as.character(df$D_or_L)
uglydf$D_or_L[df$D_or_L %in% c("0", 0)] <- "OnTime"
uglydf$D_or_L[df$D_or_L %in% c("1", 1)] <- "Delayed"

NOTRANSIT.cvmodel = train(
  D_or_L ~ Weight_kg + Distance_miles + Cost +
    Origin_Warehouse + Destination + Carrier + ShipWeekday,
  data = uglydf, #############################################
  method = "glm",
  family = "binomial",
  trControl = NOTRANSIT.ctrl
)

NOTRANSIT.cvmodel #its not looking too hot tbh.

#########################
#####     LDA     ######
##########################

#dummy-code categoricals for LDA
dmy = dummyVars(~ Origin_Warehouse + Destination + Carrier + ShipWeekday,
                data = df, fullRank = TRUE)
Xcat = predict(dmy, newdata = df)

#assemble modeling frame (numeric + dummies)
df_lda = cbind(
  D_or_L = df$D_or_L,
  Weight_kg = df$Weight_kg,
  Distance_miles = df$Distance_miles,
  Cost = df$Cost,
  Xcat
)
df_lda = as.data.frame(df_lda)
df_lda = na.omit(df_lda)

#scale predictors (keep target untouched)
num_cols = setdiff(names(df_lda), "D_or_L")

#set up 8-fold CV
set.seed(2253221)
lda_ctrl = trainControl(
  method = "cv",
  number = 8,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  sampling = "smote" ##########try with and without this
)

#normalize target labels for caret
table(df_lda$D_or_L, useNA = "ifany")
df_lda$D_or_L = as.character(df_lda$D_or_L)
df_lda$D_or_L[df_lda$D_or_L %in% c("2", 2, "Lost", "Delayed/Lost", "Delayed")] = "Delayed"
df_lda$D_or_L[df_lda$D_or_L %in% c("1", 1, "OnTime", "on time", "ontime")] = "OnTime"
df_lda$D_or_L = factor(df_lda$D_or_L, levels = c("Delayed", "OnTime"))

# 1. Calculate current class proportions
class_counts = table(df_lda$D_or_L)
prop_delayed = class_counts["Delayed"] / sum(class_counts)
prop_ontime = class_counts["OnTime"] / sum(class_counts)

# 2. Assign the prior probabilities (e.g., assuming a 50/50 split is desired)
# Set your target prior as 0.5 for Delayed and 0.5 for OnTime, 
# even if the data is imbalanced.
lda_prior = c(0.5, 0.5) 
names(lda_prior) = levels(df_lda$D_or_L) # Ensure names match levels


#fit LDA
lda_cv = train(
  D_or_L ~ .,
  data = df_lda,
  method = "lda",
  trControl = lda_ctrl,
  metric = "ROC",
  preProcess = c("zv", "center", "scale"),
)

lda_cv
varImp(lda_cv)
#conclusoin: WITHOUT SMOTE, the sens is 0 and spec is .999. With Smote
#The sens goes up to .4 but the spec goes down to .58. Not good. Try diff model.

#########################
####  RANDOM FOREST  ####
##########################

df$D_or_L = as.character(df$D_or_L)
df$D_or_L[df$D_or_L %in% c("1", 1, "Delayed", "Lost", "Delayed/Lost")] = "Delayed"
df$D_or_L[df$D_or_L %in% c("0", 0, "OnTime", "on time", "ontime")] = "OnTime"
df$D_or_L = factor(df$D_or_L, levels = c("Delayed", "OnTime"))

#CV controls
set.seed(2253221)
rf_ctrl = trainControl(
  method = "cv",
  number = 8,
  sampling = "up",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final",
  verboseIter = TRUE
)

#train RF
rf_model = train(
  D_or_L ~ Weight_kg + Distance_miles + Cost + Transit_Days +
    Origin_Warehouse + Destination + Carrier + ShipWeekday,
  data = df,
  method = "rf",
  metric = "ROC",
  trControl = rf_ctrl,
  tuneLength = 5
)

rf_model
plot(rf_model)
varImp(rf_model)


df$D_or_L = ifelse(df$D_or_L == "Delayed", 1, 0)
df$D_or_L = as.integer(df$D_or_L)
cor(df$Weight_kg, df$D_or_L)
