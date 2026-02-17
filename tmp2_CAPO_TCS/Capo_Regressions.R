## Loading the data
capo<-read.csv(file = "C:\\Users\\bushc\\OneDrive\\Desktop\\DS-450\\capo_tcs_data_cleaned.csv",header = TRUE,sep=",")

library(tidyverse) #used for filtering and selecting
library(ggplot2)  #used for plotting and visuals

capo_processed<- capo %>%    #this calls the tidyverse into action
  select(-starts_with("lab_")) #then this takes out the lab data to only keep demographic (dem_), exam (exam_) and chest x-ray (cx_) data

## Survival Analysis 
library(survival)  #loading this to make calculations easy
survival<-Surv(time=capo_processed$TCS,event = capo_processed$stable_7) #creating objective
#Fitting Kaplan_Meir Curve
km<-survfit(survival~1)
#Converting to a tidyverse data frame for ggplots
km_df <- data.frame(
  time  = km$time,
  surv  = km$surv,
  n_risk = km$n.risk,
  n_event = km$n.event) %>%
  filter(time <= 8)  # limiting to 8 days

## THE PLOT
ggplot(km_df,aes(x=time,y=surv))+geom_step(color='darkblue')+geom_point(color='red2')+labs(x="Days to Stability",y="Proportion Not Yet Stable")


## Creating Linear Regression Model
capo_lm<-lm(TCS~dem_age+dem_sex+dem_pregnant+exam_height+exam_weight+exam_hr+exam_rr+
            exam_sbp+exam_dbp+exam_temp+exam_o2sat+exam_o2satvalue+exam_fio2+exam_mental+
              cx_rul+cx_rml+cx_rll+cx_lul+cx_lll+cx_cav+cx_pe,data=capo_processed)  #The addition of all the columns after the ~ creates the multiple regression model

## Checking the Residuals 
qqnorm(capo_lm$residuals)
qqline(capo_lm$residuals)
# This plot shows that since most of the points are in middle and mostly follow the line most of the data is approx normal. However the tails are heavy which could make the model less reliable.
hist(capo_lm$residuals)
# This histogram shows that the data is somewhat bimodal/flat. This confirms what we saw in the Q-Q plot, which is that the residuals aren't perectly normal.
plot(capo_lm$residuals~capo_lm$fitted.values)
abline(h=0)
# This plot should show randomly scattered data but it does not. This means that the model may not fully capture the relation between the predictors and TCS.

## Getting R Squared Value 
summary(capo_lm)
# This summary shows me that many of the factors are significant when predicting TCS. 
# These are: dem_age,exam_height,exam_weight,exam_hr,exam_rr,exam_sbp,exam_temp,exam_o2sat,exam_o2satvalue,exam_mental,cx_rll,cx_lul,cx_lll,cx_cav,cx_pe.
# This also shows me that only 8.4% of the variation in TCS is explained by these predictors.

## Predict TCS for the original dataset while adding a new column 
capo_processed$pred_TCS<-pmax(predict(capo_lm,newdata=capo_processed),0) 
# Quick look at predicted vs actual
head(capo_processed %>% select(TCS, pred_TCS))

## Independent Hypothetical
# Example new patient data
new_patient <- data.frame(
  dem_age = 65,
  dem_sex = 1,          
  dem_pregnant = 0,
  exam_height = 170,
  exam_weight = 75,
  exam_hr = 90,
  exam_rr = 22,
  exam_sbp = 120,
  exam_dbp = 80,
  exam_temp = 37.8,
  exam_o2sat = 95,
  exam_o2satvalue = 0.95,
  exam_fio2 = 0.21,
  exam_mental = 1,
  cx_rul = 0,
  cx_rml = 0,
  cx_rll = 1,
  cx_lul = 0,
  cx_lll = 0,
  cx_cav = 0,
  cx_pe = 0)
# Predict TCS for this patient and forcing value to be 0 or greater
pmax(predict(capo_lm, newdata = new_patient),0)



## Creating Logistic Model For Stable
capo_glm_stable<-glm(stable_7~dem_age+dem_sex+dem_pregnant+exam_height+exam_weight+exam_hr+exam_rr+
                exam_sbp+exam_dbp+exam_temp+exam_o2sat+exam_o2satvalue+exam_fio2+exam_mental+
                cx_rul+cx_rml+cx_rll+cx_lul+cx_lll+cx_cav+cx_pe,data=capo_processed, family=binomial)
summary(capo_glm_stable)
# From this summary it can be seen that the predictors help explain stability since the deviance dropped from null to residual
# The variables dem_age,dem_sex,exam_hr,exam_mental,cx_rll,cx_lul, and cx_pe are statistically significant to the model and are negative which means they decrease the probability of stability.
# The variables exam_sbp,exam_o2sat,exam_o2satvalue, and cx_lll are also statistically significant to the model however they are positive so they increase the probability of stability.

## Looking at the Model's Accuracy
pred_prob <- predict(capo_glm_stable, type="response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)   #the 0.5 represents cutoff for predicting both stable and not stable and then 1 and 0 represent if they will stabilize or not.

mean(pred_class == capo_processed$stable_7)
# The output of the test says that 69.17% of the data was predicted correctly which is not bad but not amazing either. 



## Creating Logistic Model For Stable Using "lab" Columns
capo_with_lab_glm_stable<-glm(stable_7~dem_age+dem_sex+dem_pregnant+exam_height+exam_weight+exam_hr+exam_rr+
                         exam_sbp+exam_dbp+exam_temp+exam_o2sat+exam_o2satvalue+exam_fio2+exam_mental+
                         cx_rul+cx_rml+cx_rll+cx_lul+cx_lll+cx_cav+cx_pe+lab_hematocrit+lab_hemoglobin+
                         lab_wbc+lab_platelets+lab_na+lab_k+lab_bun+lab_creatinine+lab_bicarb+lab_glucose+
                         lab_albumin+lab_trop3+lab_ckmb1+lab_cholesterol+lab_triglycerides+lab_ldh+lab_crp+
                         lab_abg+lab_abgph+lab_abgpao2+lab_abgbicarb+lab_abgfio2,data=capo, family=binomial)
summary(capo_with_lab_glm_stable)
# This summary shows that with the lab variables included residual deviance dropped meaning a better fit, along with AIC dropping which which means that adding the lab variables improved prediction.

## Looking at the Model's Accuracy
pred_prob <- predict(capo_with_lab_glm_stable, type="response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
mean(pred_class == capo$stable_7)
# The percentage being similar means that lab columns improved fit but not classification a bunch.



## Creating Logistic Model For Early TCS
capo_glm_early<-glm(early_tcs~dem_age+dem_sex+dem_pregnant+exam_height+exam_weight+exam_hr+exam_rr+
                       exam_sbp+exam_dbp+exam_temp+exam_o2sat+exam_o2satvalue+exam_fio2+exam_mental+
                       cx_rul+cx_rml+cx_rll+cx_lul+cx_lll+cx_cav+cx_pe,data=capo_processed, family=binomial)
summary(capo_glm_early)
# From this summary it can be seen that the predictors help explain stability since the deviance dropped from null to residual
# This summary also shows that the variables dem_age,exam_hr,exam_temp,exam_mental,cx_rll,cx_lul,cx_pe are staistically significant and decrease chance of early tcs
# The other significant variables dem_sex,exam_height,exam_weight,exam_sbp,exam_o2satvalue,cx_lll increase the chance of early tcs 

## Looking at the Model's Accuracy
pred_prob <- predict(capo_glm_early, type="response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
mean(pred_class == capo_processed$early_tcs)
# The output of the test says that 65.38% of the data was predicted correctly which is not bad but not amazing either. 


## Creating Logistic Model For Stable Using "lab" Columns
capo_with_lab_glm_early<-glm(early_tcs~dem_age+dem_sex+dem_pregnant+exam_height+exam_weight+exam_hr+exam_rr+
                                exam_sbp+exam_dbp+exam_temp+exam_o2sat+exam_o2satvalue+exam_fio2+exam_mental+
                                cx_rul+cx_rml+cx_rll+cx_lul+cx_lll+cx_cav+cx_pe+lab_hematocrit+lab_hemoglobin+
                                lab_wbc+lab_platelets+lab_na+lab_k+lab_bun+lab_creatinine+lab_bicarb+lab_glucose+
                                lab_albumin+lab_trop3+lab_ckmb1+lab_cholesterol+lab_triglycerides+lab_ldh+lab_crp+
                                lab_abg+lab_abgph+lab_abgpao2+lab_abgbicarb+lab_abgfio2,data=capo, family=binomial)
summary(capo_with_lab_glm_early)
# This summary shows that with the lab variables included residual deviance dropped meaning a better fit, along with AIC dropping which which means that adding the lab variables improved prediction.

## Looking at the Model's Accuracy
pred_prob <- predict(capo_with_lab_glm_early, type="response")
pred_class <- ifelse(pred_prob > 0.5, 1, 0)
mean(pred_class == capo$early_tcs)
# The percentage being similar means that lab columns improved fit but not classification a bunch.