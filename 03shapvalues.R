library(SHAPforxgboost)
library(dplyr)
library(readxl)
library(caret)
library(xgboost)
library(iml)
library(fastDummies)
library(shapper)

datosmodelo <- read_excel("datos/datosmodelo.xlsx")
datosmodelo<- datosmodelo %>% mutate(fraud_reported = as.factor(fraud_reported))

variables_cat <- c("incident_severity", "collision_type", "incident_type", "authorities_contacted")

datosmodelo <- dummy_cols(datosmodelo, select_columns = variables_cat, remove_selected_columns = TRUE  )

# Dividimos el conjunto en train y test:

set.seed(1234)
indice <- createDataPartition(datosmodelo$fraud_reported, p = 0.8, list=FALSE)
entrenamiento <- datosmodelo[indice,]
test <- datosmodelo[-indice,]


# https://www.r-bloggers.com/2021/06/shap-analysis-in-9-lines/#google_vignette

modelo_xgb<- readRDS("datos/modeloXGB")

xgb_model <- modelo_xgb$finalModel

X1 = as.matrix(entrenamiento %>% select(-fraud_reported))


mod1 = xgboost::xgboost(
  data = X1, label = entrenamiento$fraud_reported, gamma = 0, eta = 1,
  lambda = 0, nrounds = 1, verbose = FALSE, nthread = 1)


shap_values <- shap.values(xgb_model = mod1, X_train = X1)

shap_values$mean_shap_score


shap_values_iris <- shap_values$shap_score



# shap.prep() returns the long-format SHAP data from either model or
shap_long_iris <- shap.prep(xgb_model = mod1, X_train = X1)
# is the same as: using given shap_contrib
shap_long_iris <- shap.prep(shap_contrib = shap_values_iris, X_train = X1)
               
shap.plot.summary(shap_long_iris, scientific = TRUE)
shap.plot.summary(shap_long_iris, x_bound  = 1.5, dilute = 10)

