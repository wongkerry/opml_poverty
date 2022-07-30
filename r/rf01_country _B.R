library(data.table)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(ROCR)

## overall work plan
## use country A to develop code
## apply to country B and C
this_country <- "B"

hhold_filename <- paste0("01 input/", this_country, "_hhold_train.csv")
indiv_filename <- paste0("01 input/", this_country, "_indiv_train.csv")

hhold <- as.data.table(read.csv(hhold_filename))
indiv <- as.data.table(read.csv(indiv_filename))
indiv[, country := NULL]
setcolorder(indiv, c("id", "iid", "poor"))

#search duplicated col names
  indiv_names <- as.data.table(names(indiv))
  indiv_names$duplicated <- 0
  for (i in 4:length(names(indiv))){
    indiv_names$duplicated[i] <- sum(grepl(names(indiv)[i], names(hhold), fixed = TRUE))
  }
  indiv_names[duplicated==1] 
  hhold[, wJthinfa := NULL]

#merge indiv and hhold to form a dataset with all potential factors
indiv <- as.data.table(merge(indiv, hhold[, c("poor") := NULL][], by = "id") )
table(indiv$poor)
setcolorder(indiv, c("id", "iid", "poor"))

#inspect col with a lot of missing
indiv_miss <- indiv[, c("id", "iid") := NULL]
miss <- data.table(colSums(is.na(indiv_miss)))
miss[, varname := names(indiv_miss)]
miss[, V1 := V1*100/nrow(indiv_miss)]
miss_col <- miss[V1>25]$varname
#remove columns with >25% missing
indiv1 <- indiv[, (miss_col):=NULL]

#inspect no variability in cols (factor columns only)
num <- indiv1[ , .SD, .SDcols =  is.numeric] #6
fac <- indiv1[ , .SD, .SDcols = !is.numeric] #378 columns
cols <- names(fac)
fac <- data.frame(fac)

var_list <- list()
for(i in 2:ncol(fac)) {  
  plot_var <- data.table(fac[, i])
  plot_var <- plot_var[, .(table(V1)*100/.N)]
  plot_var[, varname := names(fac)[i]]
  
  var_list[[i]] <- plot_var
}
var_list <- rbindlist(var_list)
setorder(var_list, -N)

var_list_99 <- var_list[N>99, .(varname)]
drop <- var_list_99$varname
fac <- data.table(fac)
fac[, (drop) := NULL]  #left with 314 columns 

indiv2 <- cbind(fac, num)

indiv2_full <- indiv2[complete.cases(indiv2), ]
table(indiv2_full$poor)

indiv2_full[, poor := as.factor(poor)]

##run rfs
set.seed(20220726)
train <- sample(nrow(indiv2_full), 0.7*nrow(indiv2_full), replace = FALSE)
TrainSet <- indiv2_full[train,]
ValidSet <- indiv2_full[-train,]
summary(TrainSet)
summary(ValidSet)

##generate a list of rf's, each with different ntree-mtry
Sys.time()
rf_250_08 <- randomForest(poor ~ ., data=TrainSet, ntree=250, mtry=08, importance=TRUE); Sys.time()
rf_250_16 <- randomForest(poor ~ ., data=TrainSet, ntree=250, mtry=16, importance=TRUE); Sys.time()
rf_250_20 <- randomForest(poor ~ ., data=TrainSet, ntree=250, mtry=20, importance=TRUE); Sys.time()
rf_250_32 <- randomForest(poor ~ ., data=TrainSet, ntree=250, mtry=32, importance=TRUE); Sys.time()

rf_500_08 <- randomForest(poor ~ ., data=TrainSet, ntree=500, mtry=08, importance=TRUE); Sys.time()
rf_500_16 <- randomForest(poor ~ ., data=TrainSet, ntree=500, mtry=16, importance=TRUE); Sys.time()
rf_500_20 <- randomForest(poor ~ ., data=TrainSet, ntree=500, mtry=20, importance=TRUE); Sys.time()
rf_500_32 <- randomForest(poor ~ ., data=TrainSet, ntree=500, mtry=32, importance=TRUE); Sys.time()

rf_750_08 <- randomForest(poor ~ ., data=TrainSet, ntree=750, mtry=08, importance=TRUE); Sys.time()
rf_750_16 <- randomForest(poor ~ ., data=TrainSet, ntree=750, mtry=16, importance=TRUE); Sys.time()
rf_750_20 <- randomForest(poor ~ ., data=TrainSet, ntree=750, mtry=20, importance=TRUE); Sys.time()
rf_750_32 <- randomForest(poor ~ ., data=TrainSet, ntree=750, mtry=32, importance=TRUE); Sys.time()

#check against the validation set
table(observed=ValidSet[, poor], predicted=predict(rf_250_08,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_250_16,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_250_20,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_250_32,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_500_08,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_500_16,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_500_20,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_500_32,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_750_08,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_750_16,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_750_20,ValidSet[,-1]))
table(observed=ValidSet[, poor], predicted=predict(rf_750_32,ValidSet[,-1]))

#importance
imp_500_08 <- data.table(importance(rf_500_08), row.names=rownames(importance(rf_500_08)))
imp_500_16 <- data.table(importance(rf_500_16), row.names=rownames(importance(rf_500_16)))
imp_500_20 <- data.table(importance(rf_500_20), row.names=rownames(importance(rf_500_20)))
imp_500_32 <- data.table(importance(rf_500_32), row.names=rownames(importance(rf_500_32)))

setorder(imp_500_08, -MeanDecreaseAccuracy)
setorder(imp_500_16, -MeanDecreaseAccuracy)
setorder(imp_500_20, -MeanDecreaseAccuracy)
setorder(imp_500_32, -MeanDecreaseAccuracy)

