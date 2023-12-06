# Code to calculate AUC for the best Flickr model
# Based on code from David Dalpiaz (2020) "R for Statistical Learning", https://daviddalpiaz.github.io/r4sl/logistic-regression.html#roc-curves

#install.packages("ISLR")
#install.packages("caret")
#install.packages("pROC")

library(ISLR)
#library(tibble)
library(caret) # For the confusion matrix
library(pROC)

#### Calculate AUC for the model of Flickr data ####------------------------------------------------------------------------------

# test-train split

set.seed(52)
nrow(my_data)
nrow(my_data)/2
mydata_idx   = sample(nrow(my_data), 3175)
mydata_trn = my_data[mydata_idx, ]
mydata_tst = my_data[-mydata_idx, ]


# Function which allows use to make predictions based on different probability cutoffs:

get_logistic_pred = function(mod, data, res = "y", pos = 1, neg = 0, cut = 0.5) {
  probs = predict(mod, newdata = data, type = "response")
  ifelse(probs > cut, pos, neg)
}

# Predictions using a low, medium, and high cutoff. (0.1, 0.5, and 0.9)
my_test_pred_10 = get_logistic_pred(Flickr.bin_best_model, data = mydata_tst, res = "Fli_T_F", 
                                    pos = 1, neg = 0, cut = 0.1)
my_test_pred_50 = get_logistic_pred(Flickr.bin_best_model, data = mydata_tst, res = "Fli_T_F", 
                                    pos = 1, neg = 0, cut = 0.5)
my_test_pred_90 = get_logistic_pred(Flickr.bin_best_model, data = mydata_tst, res = "Fli_T_F", 
                                    pos = 1, neg = 0, cut = 0.9)

# Evaluate accuracy, sensitivity, and specificity
my_test_tab_10 = table(predicted = my_test_pred_10, actual = mydata_tst$Fli_T_F)
my_test_tab_50 = table(predicted = my_test_pred_50, actual = mydata_tst$Fli_T_F)
my_test_tab_90 = table(predicted = my_test_pred_90, actual = mydata_tst$Fli_T_F)

my_test_con_mat_10 = confusionMatrix(my_test_tab_10, positive = "1")
my_test_con_mat_50 = confusionMatrix(my_test_tab_50, positive = "1")
my_test_con_mat_90 = confusionMatrix(my_test_tab_90, positive = "1")

#my_test_tab_10
#my_test_con_mat_10
#my_test_con_mat_50


metrics = rbind(
  
  c(my_test_con_mat_10$overall["Accuracy"], 
    my_test_con_mat_10$byClass["Sensitivity"], 
    my_test_con_mat_10$byClass["Specificity"]),
  
  c(my_test_con_mat_50$overall["Accuracy"], 
    my_test_con_mat_50$byClass["Sensitivity"], 
    my_test_con_mat_50$byClass["Specificity"]),
  
  c(my_test_con_mat_90$overall["Accuracy"], 
    my_test_con_mat_90$byClass["Sensitivity"], 
    my_test_con_mat_90$byClass["Specificity"])
  
)

rownames(metrics) = c("c = 0.10", "c = 0.50", "c = 0.90")
metrics


# Instead of manually checking cutoffs, we can create an ROC curve (receiver operating characteristic curve) which will sweep through all possible cutoffs, and plot the sensitivity and specificity.

my_test_prob = predict(Flickr.bin_best_model, newdata = mydata_tst, type = "response")
head(my_test_prob)

my_test_roc = roc(mydata_tst$Fli_T_F ~ my_test_prob, plot = TRUE, print.auc = TRUE)

Flickr_model_AUC <- as.numeric(my_test_roc$auc) 

Flickr_model_AUC #A good model will have a high AUC, that is as often as possible a high sensitivity and specificity.


