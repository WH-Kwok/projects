# Data Extraction and Filtering for the Year 2017

bo<-list("Book2.xlsx", "Book3.xlsx", "Book4.xlsx", "Book5.xlsx", "Book6.xlsx", "Book7.xlsx", "Book8.xlsx", "Book9.xlsx", "Book10.xlsx", "Book11.xlsx", "Book12.xlsx", "Book13.xlsx", "Book14.xlsx", "Book15.xlsx", "Book16.xlsx", "Book17.xlsx", "Book18.xlsx", "Book19.xlsx", "Book20.xlsx", "Book21.xlsx", "Book22.xlsx")

library(tidyverse)

master<-map(bo, \(b) 
            map( excel_sheets(b) , \(s)
                 read_excel( b , sheet = s)%>% select(IndicatorCode,	SpatialDim,	TimeDim, Value)
            ) %>% do.call(rbind,.)
)%>% do.call(rbind,.) %>% filter(TimeDim ==2017)

master_copy<-master


# Cleaning and Standardizing Missing Data Values
fun2<-function(x){
  x[x=="Not Available"]<-NA
  x[x=="Not available"]<-NA
  x[x=="Not applicable" ]<-NA
  x[x=="not applicable" ]<-NA
  x[x=="Figure not available" ]<-NA
  x
}

master$Value <- fun2(master$Value)


# Reshaping Data into a Wide Format
df_wide <- master %>% select(-TimeDim) %>%
  pivot_wider(names_from = IndicatorCode, values_from = Value)


## Converting Columns to Appropriate Data Types
fun3 <-function(x) if(all(is.na(as.numeric(x)))) as.character(x) else as.numeric(x)

df_wide2 <-df_wide %>% map(fun3) %>% as.data.frame()

## Data Cleaning and Assigning Country Variables
x<-df_wide2$GDO_q10x2_TECH
x[x=="Not Reimbursable" | is.na(x)] <-"No"
df_wide2$GDO_q10x2_TECH<-x

x<-df_wide2$GDO_q14x2_4
x[is.na(x)]<-"No"
df_wide2$GDO_q14x2_4<-x

df_wide2$SpatialDim
countries <- c("Australia", "Qatar", "Malta", "Hungary", "Oman", "Brazil", "Vietnam", "Lithuania", "Slovenia", "Fiji", "Tunisia", "Turkey", "Iran", "Lebanon", "Austria", "South Korea", "Belize", "Bangladesh", "Guyana", "Chile", "Eswatini", "Saint Lucia", "Japan", "Trinidad and Tobago", "India", "Maldives", "Brunei", "United Kingdom", "United States of America", "Singapore", "Cook Islands", "Canada", "Italy", "France", "Morocco", "Mauritius", "Israel", "Czech Republic", "Ireland", "China", "Myanmar", "Finland", "Estonia", "Pakistan", "Thailand", "Belgium", "Sweden", "Malaysia", "Dominican Republic", "Togo", "Jordan", "Poland", "Denmark", "Cyprus", "Switzerland", "Germany", "Netherlands", "Greece", "South Africa", "Grenada", "Norway", "Costa Rica")
library(VIM)

## Final Predictor Selection, Imputation, and Output
Predictors <- df_wide2 %>%  select(-SpatialDim,-GDO_q10x2_TECH,-GDO_q14x2_4)%>%
  select_if(~ sum(is.na(.x))/length(.x) <=.3)%>%
  select_if(~ .x %>% unique() %>%length() >1)%>%
  mutate_if(is.character, factor)%>%  kNN(imp_var=F)

Predictors%>% write.csv(paste("Predictors_10Dec2024",".csv"))

## Defining a Custom Modeling Function
fun_model<-function( Output,  my_method, Grid ) {
  
  data <- data.frame(Predictors,label = Output) # view(data1)
  rownames(data)<-countries
  require(caret)
  fun_seeds <- function( my_grid ){
    set.seed(123)
    seeds <- vector(mode = "list", length = 11)
    for (i in 1:10) { seeds[[i]] <- sample.int(1000, nrow(my_grid) ) } 
    seeds[[11]] <- sample.int(1000, 1)
    seeds
  }
  set.seed(124)  # For reproducibility
  id <- createDataPartition(Output, p = 0.7, list = F)
  train_data <- data[id, ]  
  test_data <- data[-id, ]  
  
  model <- train(label ~ ., data = train_data, 
                 method = my_method , 
                 trControl = trainControl(method = "cv", classProbs = T, 
                                          seeds = fun_seeds(Grid)),
                 tuneGrid = Grid)
  
  return(list(train_data=train_data,test_data=test_data,model=model))
}

## Running and Saving Models and Outputs
df_wide2 %>%
  select(GDO_q10x2_TECH, GDO_q14x2_4) %>%
  write.csv("GDO_q10x2_TECH_q14x2_4.csv")    

Output<- list(
  
  m1= fun_model(df_wide2$GDO_q10x2_TECH,"rf" , expand.grid(mtry = 2:9 ) ),
  m2=  fun_model(df_wide2$GDO_q10x2_TECH,"mlp", expand.grid(size = 2:9 ) ),
  m3=  fun_model(df_wide2$GDO_q14x2_4, "rf" , expand.grid(mtry = 2:9 ) ),
  m4=  fun_model(df_wide2$GDO_q14x2_4, "mlp", expand.grid(size = 2:9 ) )
)

## Extracting Best Hyperparameters and Variable Importance
library(caret)
Output$m1 $model$bestTune
Output$m3$model$bestTune

var_imp <- varImp(Output$m1$model)
var_imp_tab<-var_imp$importance %>% top_n(20) %>% arrange(desc(.)) 
var_imp_tab%>%write.csv(paste(Sys.time() %>% gsub(":","_",.), "Variable importance.csv"))


## Visualizing Variable Importance and Evaluating Model Performance
# Load ggplot2
library(ggplot2)
# Create the bar plot
var_imp_tab%>%
  ggplot( aes(x = reorder(rownames(.), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Variable importance", x = "Variables", y = "Importance") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))

confusionMatrix(
  predict(Output$m1$model,
          Output$m1$test_data, 
          type="raw")  ,
  factor ( Output$m1$test_data$label),
  
  "Reimbursable"
)

## Generating and Evaluating the ROC Curve and AUC
# Predict probabilities and classes
pred_probs <- predict(Output$m1$model, newdata = Output$m1$test_data, type = "prob")[, 2]  # Probabilities for positive class
pred_classes <- predict(Output$m1$model, newdata = Output$m1$test_data, type = "raw")
library(pROC)

# Generate ROC curve
roc_curve <- roc(Output$m1$test_data $label, pred_probs)

# Plot ROC curve
plot(roc_curve, main = "ROC Curve")
auc(roc_curve)  # Compute AUC

## Assessing Model Calibration and Computing the Brier Score
library(ggplot2)
library(caret)

calibration_data <- calibration(factor(label) ~ pred_probs, 
                                data = Output$m1$test_data, 
                                class = "Reimbursable")

ggplot(calibration_data, aes(midpoint, Percent)) +
  geom_line() +
  geom_point() +
  labs(title = "Calibration Plot", x = "Predicted Probability", y = "Observed Frequency")


brier_score <- mean((pred_probs - as.numeric(Output$m1$test_data $label == "Reimbursable"))^2)
print(brier_score)


## Merging Datasets and Saving Indicator Names
left_join(Book27_20var,Book28_allvar,
          join_by("...1" == "IndicatorCode")
) %>% write.csv(
  
  paste(Sys.time() %>% gsub(":","_",.), "IndicatorName.csv")
  
)

## Additional Data Processing (Commented Out Code)
# df_wide_chr<-df_wide2%>%select_if(is.character)%>%
#   map(function(x){ x[is.na(x)]<-"NA"; x})%>%as.data.frame()
# 
# df_wide_num<-df_wide2%>%select_if(is.numeric)%>%
#   map(function(x){ x[is.na(x)]<-0; x})%>%as.data.frame()

# test<-df_wide3%>% 
#   select(SpatialDim,GDO_q10x2_TECH,GDO_q14x2_4) %>% 
#   mutate(countries=countries)%>%filter(GDO_q10x2_TECH  =="NA")

