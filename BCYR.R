

library(tidyverse)
library(readxl)

setwd("D:/Users/SK/Documents/jayne kotz")

f <-c("00. DATA EXPORT 29092022_with_Previous_Version column Mapping2.xlsx",
      "July Data_FINAL_2.xlsx")

fn<-c("sep","jul")

s <-c("30-09-2022", "head", "Data Label Legend") 


out<-map(f, function(f){
  
  logi = excel_sheets(f) %in% s
  p = excel_sheets(f)[logi]
  out<-map(p, function(p) read_excel(f,p))
  names(out)<-p
  out
  
}
)
names(out)<-fn
out

out$sep$`Data Label Legend`$`Question ID`<-
  out$sep$`Data Label Legend`$`Question ID`%>%
  paste0("fs1.Q",.)

out


f1<-function(data){
  
  f <-function(c) sum(is.na(c))/length(c)
  m <- map_dbl(data, f) %>% sort
  
  v<-integer()
  for(i in 1:length(m)-1) v[i]<-m[i+1]-m[i]
  v2<-integer()
  for(i in 1:length(v)-1) v2[i]<-v[i+1]-v[i]
  
  wm = which.max(abs(v2))
  
  f1<-function(c) n_distinct(c, na.rm = T) < 12
  f2<-function(c) unique(c) %>% sort %>% paste(., collapse = "; ")
  f3<-function(d) (map_dbl(d,f) <= m[[wm]]) %>% d[,.]
  f4<-function(c) fct_explicit_na(c, "NA")
  
  d <- data %>% mutate_if(f1, as.factor)
  
  chr<-d %>% select_if(is.character) %>% map_df(f2) %>% write.csv("text answers.csv")
  
  num<-d %>% select_if(is.numeric) %>% f3 
  
  ftr<-d %>% select_if(is.factor) 
  
  ftr1<-ftr %>% f3
  
  ftr2<-ftr %>% select(!names(ftr1)) %>% mutate_all(f4) %>%
    fastDummies::dummy_cols(remove_selected_columns = T) %>% 
    select(!ends_with("_NA"))
  
  df <- data.frame(ftr1, ftr2) %>% VIM::kNN(imp_var = F)
  
  return(as.list(environment()))
  
}

d = f1(out$sep$`30-09-2022`)

d$df %>% dim




f2<-
  function(data ,
           pos = "fs1.Q223",
           neg = c("fs1.Q216", "fs1.Q217", "fs1.Q218", "fs1.Q219","fs1.Q221", "fs1.Q222"),
           max = 5,
           cut = 15,
           dir = c("higher","lower"),
           means= c("inclined","disinclined"),
           wants= c("better","worse")
  ){
    
    d <- data[c(neg, pos)] %>% mutate_all(as.numeric)
    
    f <-function(x) d[x] %>% rowSums()
    fr<-function(x) (max+1)*length(x) - f(x)
    
    tot1= f(pos) + fr(neg) 
    tot2= f(neg) + fr(pos)
    
    if(dir=="higher"){
      if(means=="inclined"){
        if(wants=="better") tot= tot1 else tot=tot2
      }else{
        if(wants=="better") tot= tot2 else tot=tot1
      } 
    }else{
      if(means=="inclined"){
        if(wants=="better") tot= tot2 else tot=tot1
      }else{
        if(wants=="better") tot= tot1 else tot=tot2
      } 
    }
    
    Class<-ifelse(tot < cut,"Low","High") %>% as.factor()
    Dummy<-ifelse(tot < cut, 0, 1) %>% as.numeric()
    return(as.list(environment()))
  }

b  <-f2(d$df, dir ="higher", means = "inclined" , wants = "worse")


fe   <- select(d$df , -b$pos, -b$neg)

Cor <- fe %>% mutate(b$Dummy) %>% as.matrix %>%  Hmisc::rcorr(.,  type="s")
Cor2 <- Cor$r [ , ncol(Cor$r) ] %>% .[ !is.na(.) & abs(.) > .2] 
Cor2 %>% write.csv("BCYR corr resutls.csv")


N <-names(Cor2) %>% data.frame(x = .) %>% 
  separate(x, c("f","q","v"), remove = F) %>% 
  separate(x, c("FQ","V"), sep="_", remove = F)
N
N[24,-1] <- "Class"
N

out


pmap(
  list(
    list(
      out$sep$head,
      out$sep$`Data Label Legend`,
      out$jul$head
    ),
    list("var", "Question ID", "v2"),
    list("cor1.csv", "cor2.csv", "cor3.csv")
  ),
  function(d, c, f) left_join(N, d, c("FQ"=c)) %>% write.csv(f)
)





f_hmap <- function(data
                   , low = "white"
                   , high = "red"
                   , name = "Score"
                   , x = "Items"
                   , y = "Participant ID"
){
  d <- data %>% as.matrix %>% reshape2::melt()
  a =  aes(Var2, Var1, fill = as.numeric(value))
  
  ggplot(d,a) + geom_tile() +
    scale_fill_gradient(low=low, high=high, name=name) +
    labs(x=x, y=y) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90))
} 



fe %>% mutate( b$Dummy ) %>% select( N$x ) %>% { names(.)<-N$q ; . } %>% f_hmap

ggsave("Subset heatmap.png", width = 8,height = 8)

Cor$r [N$x, N$x] %>% { colnames(.)<-rownames(.)<-N$q ; . } %>%
  f_hmap(., high = "blue", name = "Spearman's rho", y = "Items")

ggsave("Subset corr heatmap.png", width = 8, height = 8)



Subset = fe %>% select_if(names(.) %in% N$x) %>% 
  mutate(b$Class) %>% { names(.)<-N$q; . }
str(Subset)






library(caret)
set.seed(1)
id <-Subset[[1]]  %>% createDataPartition(1, .7, F)
train_data = Subset[id,]
test_data = Subset[-id,]

# function to set up random seeds
setSeeds <- function(method = "cv", numbers = 10, repeats = 1, 
                     tunes = 10, seed = 1, npar=5) {
  #B is the number of resamples and integer vector of M (numbers + tune length if any)
  B <- if (method == "cv") numbers
  else if(method == "repeatedcv") numbers * repeats
  else NULL
  
  if(is.null(length)) {
    seeds <- NULL
  } else {
    
    set.seed(seed = seed)
    seeds <- vector(mode = "list", length = B)
    
    s1=numbers + ifelse(is.null(tunes), 0, tunes^npar)
    s2=1
    
    f=function(x) sample.int(n = 10^7, size = s1)
    
    seeds <- lapply(seeds, f)
    
    seeds[[length(seeds) + 1]] <-
      sample.int(n = 10^7, size = s2)
  }
  # return seeds
  seeds
}




#modelLookup(model = "avNNet")
tc <- trainControl(
  method = "repeatedcv", 
  classProbs = T,
  seeds = setSeeds("repeatedcv") 
)

f_perf <- 
  function(data
           , model
           , model_name
           , response
           , class
  ){
    
    f.1<-function(c) round(c,3)
    f.2<-function(d) mutate_if(d, is.numeric, f.1)
    f.3<-function(est,low,up) paste0(est," [",low,", ",up,"]")
    f.4<-function(x) x %>% t %>% as.data.frame()
    f.5<-function(ftr, class) as.character(ftr) %>%{.[.==class]<-1; .[.!="1"]<-0; .} %>% as.numeric
    
    pred <-predict(model,data)
    prob <-predict(model,data,"prob") 
    
    lab = data[[response]]
    tab <- table(pred, lab) 
    
    cm <- confusionMatrix(tab, positive = class, mode = "everything")
    
    ca=  cm$overall [c("Accuracy","AccuracyLower","AccuracyUpper")]
    ck=  cm$overall ["Kappa"]
    cc=  cm$byClass [c("Sensitivity","Specificity","Pos Pred Value", "Neg Pred Value","Precision", "Recall","F1")]
    
    Accuracy_CI = ca %>% f.4 %>% f.2 %>% transmute(Accuracy_CI = f.3(.[[1]], .[[2]], .[[3]]))
    ckc =  c(ck, cc) %>% f.4 %>% f.2
    perf =  data.frame(Accuracy_CI, ckc)
    
    ep <- epiR::epi.tests(tab)   %>% .$detail
    logi = ep$statistic %in% c("diag.ac","se","sp","pv.pos","pv.neg","youden")
    ep2 = ep %>% { rownames(.)<- .$statistic; . } %>% .[logi,] %>% f.2 %>% transmute(CI = f.3(.$est, .$lower, .$upper)) 
    ep3 <- data.table::transpose(ep2) %>% { colnames(.) <-rownames(ep2); . }
    
    require(pROC)
    
    Roc <- roc(lab, prob[[class]] )
    AUC_CI = ci.auc(Roc)%>% f.4 %>% f.2 %>% .[1,] %>% as.data.frame %>%
      transmute(AUC_CI = f.3(.[[2]], .[[1]], .[[3]]))
    
    paste(model_name, "Calibration plot.png") %>% png(., 500, 500)
    
    require(CalibrationCurves)
    cal <- val.prob.ci.2( prob[[class]], f.5(lab, class)) 
    cal =  cal %>% f.4 %>% f.2 %>% 
      select("C (ROC)","R2","Brier","Intercept","Slope") 
    dev.off()
    
    df<-data.frame(perf, ep3, AUC_CI,  cal)
    
    return(as.list(environment()))
  }




f_train_perf<-function(
    model
    , model_name
    , response = "Class"
    , class = "High"
){
  
  m <-train(form = reformulate(".", response)
            , data = train_data
            , method = model
            , tuneLength = 10
            , trControl = tc
  )
  
  Perf <-f_perf(data = test_data
                , model=m
                , model_name
                , response
                , class
  )
  
  return(as.list(environment()))
  
}

model_list<-c("bayesglm"
              , "naive_bayes"
              , "svmRadial"
              , "knn"
              , "rpart"
              , "rf"
              ,"nnet"
              ,"gbm"
              ,"adaboost"
              , "xgbTree"
              
)

model_name_list<-c(
  "Logistic regression"
  , "Na?ve Bayes"
  , "Support vector machines, Radial"
  , "K-nearest neighbors"
  , "Decision tree"
  , "Random forest"
  ,"Neural network"
  ,"Gradient boosting machine"
  ,"AdaBoost"
  , "Extreme gradient boosting"
)





library(doParallel)
cl <-(detectCores()*.8) %>% round() %>% makeCluster() 
registerDoParallel(cl)

system.time(
  output <-map2(model_list,
                model_name_list,
                function(x,y) try(f_train_perf(x,y, "Class", "High")))
)



# output[[1]]$out$df %>%data.table::transpose(keep.names = "x")
# names(output)<-new_names
# output$`Logistic regression`$m
# apply(1, function(x) paste(x["statistic"],   x["est"], x["lower"], x["upper"], x["id"])) %>% as.data.frame()

names(output) <- model_name_list


f_tune_par = function(d){
  
  n = colnames(d)
  d = d %>% mutate_if(is.numeric, function(x) round(x,3))
  
  m = map2(n,d, function(n,d) paste(n,"=",paste(d, collapse = ", "))) %>%
    paste(collapse ="; ")
  
}



output %>% map(function(x) x$Perf$Roc) %>%
  ggroc(c("linetype", "color")) +theme_classic() +labs(col='Model',lty="Model") 
ggsave("roc curves.png", width = 9, height = 6)

output %>% map_df(function(x) x$Perf$df) %>%
  data.frame(model_name_list = model_name_list, .) %>%write.csv("out perf df.csv")

output %>% map_df(function(x) x$m$bestTune %>% f_tune_par) %>% write.csv("best tunes.csv")

output %>% map_df(function(x) x$m$results %>% select(-c("Accuracy","Kappa","AccuracySD","KappaSD")) %>%
                    f_tune_par) %>% write.csv("tune ranges.csv")



stopCluster(cl)
registerDoSEQ()
