library(tidyverse)
library(readxl)

# Set working directory (redacted to avoid identifiable information)
setwd("REDACTED_PATH")

# File names redacted and generalized
f <- c("DATA_EXPORT_REDACTED.xlsx", "JULY_DATA_FINAL_REDACTED.xlsx")

# General labels for file names
fn <- c("sep", "jul")

# Redacted sheet names
s <- c("REDACTED_DATE", "REDACTED_SHEET_NAME_1", "REDACTED_SHEET_NAME_2")

# Function to read specific sheets from the redacted files
out <- map(f, function(f) {
  logi <- excel_sheets(f) %in% s
  p <- excel_sheets(f)[logi]
  out <- map(p, function(p) read_excel(f, p))
  names(out) <- p
  out
})
names(out) <- fn
out

# Redacting sensitive identifiers
out$sep$`REDACTED_SHEET_NAME_2`$`Question ID` <-
  out$sep$`REDACTED_SHEET_NAME_2`$`Question ID` %>%
  paste0("fs1.Q", .)

out

# Function for data processing with redacted content
f1 <- function(data) {
  f <- function(c) sum(is.na(c)) / length(c)
  m <- map_dbl(data, f) %>% sort
  
  v <- integer()
  for (i in 1:(length(m) - 1)) v[i] <- m[i + 1] - m[i]
  v2 <- integer()
  for (i in 1:(length(v) - 1)) v2[i] <- v[i + 1] - v[i]
  
  wm <- which.max(abs(v2))
  
  f1 <- function(c) n_distinct(c, na.rm = T) < 12
  f2 <- function(c) unique(c) %>% sort %>% paste(., collapse = "; ")
  f3 <- function(d) (map_dbl(d, f) <= m[[wm]]) %>% d[,.]
  f4 <- function(c) fct_explicit_na(c, "NA")
  
  d <- data %>% mutate_if(f1, as.factor)
  
  # Writing output to a generalized file name
  chr <- d %>% select_if(is.character) %>% map_df(f2) %>% write.csv("REDACTED_TEXT_ANSWERS.csv")
  
  num <- d %>% select_if(is.numeric) %>% f3 
  
  ftr <- d %>% select_if(is.factor) 
  
  ftr1 <- ftr %>% f3
  
  ftr2 <- ftr %>% select(!names(ftr1)) %>% mutate_all(f4) %>%
    fastDummies::dummy_cols(remove_selected_columns = T) %>% 
    select(!ends_with("_NA"))
  
  df <- data.frame(ftr1, ftr2) %>% VIM::kNN(imp_var = F)
  
  return(as.list(environment()))
}

d <- f1(out$sep$`REDACTED_DATE`)

d$df %>% dim

# Function with redacted parameter details
f2 <- function(data,
               pos = "fs1.Q_REDACTED",
               neg = c("fs1.Q_REDACTED", "fs1.Q_REDACTED", "fs1.Q_REDACTED"),
               max = 5,
               cut = 15,
               dir = c("higher", "lower"),
               means = c("inclined", "disinclined"),
               wants = c("better", "worse")) {
  
  d <- data[c(neg, pos)] %>% mutate_all(as.numeric)
  
  f <- function(x) d[x] %>% rowSums()
  fr <- function(x) (max + 1) * length(x) - f(x)
  
  tot1 = f(pos) + fr(neg)
  tot2 = f(neg) + fr(pos)
  
  if (dir == "higher") {
    if (means == "inclined") {
      if (wants == "better") tot = tot1 else tot = tot2
    } else {
      if (wants == "better") tot = tot2 else tot = tot1
    }
  } else {
    if (means == "inclined") {
      if (wants == "better") tot = tot2 else tot = tot1
    } else {
      if (wants == "better") tot = tot1 else tot = tot2
    }
  }
  
  Class <- ifelse(tot < cut, "Low", "High") %>% as.factor()
  Dummy <- ifelse(tot < cut, 0, 1) %>% as.numeric()
  return(as.list(environment()))
}

# Example function call with redacted parameter values
b <- f2(d$df, dir = "higher", means = "inclined", wants = "worse")
library(tidyverse)
library(Hmisc)
library(caret)

# Selecting columns with redacted identifiers
fe <- select(d$df, -b$pos, -b$neg)

# Calculating correlations with redacted variable names
Cor <- fe %>% mutate(b$Dummy) %>% as.matrix() %>% Hmisc::rcorr(., type = "s")
Cor2 <- Cor$r[, ncol(Cor$r)] %>% .[!is.na(.) & abs(.) > .2]
Cor2 %>% write.csv("REDACTED_CORRELATION_RESULTS.csv")

# Redacting column names for output processing
N <- names(Cor2) %>% data.frame(x = .) %>%
  separate(x, c("f", "q", "v"), remove = FALSE) %>%
  separate(x, c("FQ", "V"), sep = "_", remove = FALSE)
N
N[24, -1] <- "Class"
N

# Process output with redacted sheet names
out

pmap(
  list(
    list(
      out$sep$head,
      out$sep$`Data Label Legend`,
      out$jul$head
    ),
    list("var", "Question ID", "v2"),
    list("REDACTED_COR1.csv", "REDACTED_COR2.csv", "REDACTED_COR3.csv")
  ),
  function(d, c, f) left_join(N, d, c("FQ" = c)) %>% write.csv(f)
)

# Heatmap function with redacted labels
f_hmap <- function(data, low = "white", high = "red", name = "Score", x = "Items", y = "Participant ID") {
  d <- data %>% as.matrix %>% reshape2::melt()
  a <- aes(Var2, Var1, fill = as.numeric(value))
  
  ggplot(d, a) + geom_tile() +
    scale_fill_gradient(low = low, high = high, name = name) +
    labs(x = x, y = y) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))
}

# Generating heatmaps with redacted content
fe %>% mutate(b$Dummy) %>% select(N$x) %>% { names(.) <- N$q; . } %>% f_hmap
ggsave("REDACTED_SUBSET_HEATMAP.png", width = 8, height = 8)

Cor$r[N$x, N$x] %>% { colnames(.) <- rownames(.) <- N$q; . } %>%
  f_hmap(., high = "blue", name = "Spearman's rho", y = "Items")
ggsave("REDACTED_CORR_HEATMAP.png", width = 8, height = 8)

# Selecting and structuring subset with redacted labels
Subset <- fe %>% select_if(names(.) %in% N$x) %>%
  mutate(b$Class) %>% { names(.) <- N$q; . }
str(Subset)

# Set up random seeds with redacted configurations
set.seed(1)
id <- Subset[[1]] %>% createDataPartition(1, .7, F)
train_data <- Subset[id, ]
test_data <- Subset[-id, ]

# Function to set up random seeds with redacted parameters
setSeeds <- function(method = "cv", numbers = 10, repeats = 1, tunes = 10, seed = 1, npar = 5) {
  B <- if (method == "cv") numbers else if (method == "repeatedcv") numbers * repeats else NULL
  
  if (is.null(length)) {
    seeds <- NULL
  } else {
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = B)
    s1 <- numbers + ifelse(is.null(tunes), 0, tunes^npar)
    s2 <- 1
    f <- function(x) sample.int(n = 10^7, size = s1)
    seeds <- lapply(seeds, f)
    seeds[[length(seeds) + 1]] <- sample.int(n = 10^7, size = s2)
  }
  seeds
}
library(caret)
library(epiR)
library(pROC)
library(CalibrationCurves)
library(data.table)

# Setting up train control with redacted model parameters
tc <- trainControl(
  method = "repeatedcv", 
  classProbs = TRUE,
  seeds = setSeeds("repeatedcv") 
)

# Function for performance evaluation with redacted output names
f_perf <- function(data, model, model_name, response, class) {
  f.1 <- function(c) round(c, 3)
  f.2 <- function(d) mutate_if(d, is.numeric, f.1)
  f.3 <- function(est, low, up) paste0(est, " [", low, ", ", up, "]")
  f.4 <- function(x) x %>% t %>% as.data.frame()
  f.5 <- function(ftr, class) as.character(ftr) %>% {.[. == class] <- 1; .[. != "1"] <- 0; .} %>% as.numeric
  
  pred <- predict(model, data)
  prob <- predict(model, data, "prob")
  
  lab <- data[[response]]
  tab <- table(pred, lab)
  
  cm <- confusionMatrix(tab, positive = class, mode = "everything")
  
  ca <- cm$overall[c("Accuracy", "AccuracyLower", "AccuracyUpper")]
  ck <- cm$overall["Kappa"]
  cc <- cm$byClass[c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Precision", "Recall", "F1")]
  
  Accuracy_CI <- ca %>% f.4 %>% f.2 %>% transmute(Accuracy_CI = f.3(.[[1]], .[[2]], .[[3]]))
  ckc <- c(ck, cc) %>% f.4 %>% f.2
  perf <- data.frame(Accuracy_CI, ckc)
  
  ep <- epiR::epi.tests(tab) %>% .$detail
  logi <- ep$statistic %in% c("diag.ac", "se", "sp", "pv.pos", "pv.neg", "youden")
  ep2 <- ep %>% { rownames(.) <- .$statistic; . } %>% .[logi, ] %>% f.2 %>% transmute(CI = f.3(.$est, .$lower, .$upper))
  ep3 <- data.table::transpose(ep2) %>% { colnames(.) <- rownames(ep2); . }
  
  Roc <- roc(lab, prob[[class]])
  AUC_CI <- ci.auc(Roc) %>% f.4 %>% f.2 %>% .[1, ] %>% as.data.frame %>%
    transmute(AUC_CI = f.3(.[[2]], .[[1]], .[[3]]))
  
  # Redacted output plot generation
  paste(model_name, "Calibration plot.png") %>% png(., 500, 500)
  
  cal <- val.prob.ci.2(prob[[class]], f.5(lab, class))
  cal <- cal %>% f.4 %>% f.2 %>% 
    select("C (ROC)", "R2", "Brier", "Intercept", "Slope")
  dev.off()
  
  df <- data.frame(perf, ep3, AUC_CI, cal)
  
  return(as.list(environment()))
}

# Function for training and evaluating model performance with redacted model names
f_train_perf <- function(model, model_name, response = "Class", class = "High") {
  m <- train(
    form = reformulate(".", response),
    data = train_data,
    method = model,
    tuneLength = 10,
    trControl = tc
  )
  
  Perf <- f_perf(
    data = test_data,
    model = m,
    model_name,
    response,
    class
  )
  
  return(as.list(environment()))
}

# Redacted model lists
model_list <- c(
  "bayesglm",
  "naive_bayes",
  "svmRadial",
  "knn",
  "rpart",
  "rf",
  "nnet",
  "gbm",
  "adaboost",
  "xgbTree"
)

model_name_list <- c(
  "Logistic regression",
  "Naive Bayes",
  "Support vector machines, Radial",
  "K-nearest neighbors",
  "Decision tree",
  "Random forest",
  "Neural network",
  "Gradient boosting machine",
  "AdaBoost",
  "Extreme gradient boosting"
)
library(doParallel)

# Setting up parallel processing with redacted cluster setup
cl <- (detectCores() * 0.8) %>% round() %>% makeCluster()
registerDoParallel(cl)

# Running model performance evaluation with redacted function calls
system.time(
  output <- map2(
    model_list,
    model_name_list,
    function(x, y) try(f_train_perf(x, y, "Class", "High"))
  )
)

# Redacting output manipulations and assignments
names(output) <- model_name_list

# Function for tuning parameter formatting with redacted content
f_tune_par <- function(d) {
  n <- colnames(d)
  d <- d %>% mutate_if(is.numeric, function(x) round(x, 3))
  m <- map2(n, d, function(n, d) paste(n, "=", paste(d, collapse = ", "))) %>%
    paste(collapse = "; ")
}

# Plotting ROC curves with redacted filenames and output
output %>% map(function(x) x$Perf$Roc) %>%
  ggroc(c("linetype", "color")) + theme_classic() + labs(col = 'Model', lty = "Model") 
ggsave("REDACTED_ROC_CURVES.png", width = 9, height = 6)

# Writing performance data to redacted CSV file
output %>% map_df(function(x) x$Perf$df) %>%
  data.frame(model_name_list = model_name_list, .) %>% write.csv("REDACTED_PERFORMANCE_DATA.csv")

# Writing best tuning parameters to a redacted CSV file
output %>% map_df(function(x) x$m$bestTune %>% f_tune_par) %>% write.csv("REDACTED_BEST_TUNES.csv")

# Writing tuning ranges to a redacted CSV file
output %>% map_df(function(x) x$m$results %>% select(-c("Accuracy", "Kappa", "AccuracySD", "KappaSD")) %>%
                    f_tune_par) %>% write.csv("REDACTED_TUNE_RANGES.csv")

# Stopping and deregistering the parallel cluster
stopCluster(cl)
registerDoSEQ()
