library(data.table);library(magrittr);library(parallel);library(fst);library(stats)

#setwd("~/ShinyApps/doctorssi/ranitidine")

names.study <- list.files("res")

#list.balance <- lapply(names.study, function(x){
#  fread(file.path("res",x, "covariate_balance.csv"), integer64 = "numeric")
#}) %>% rbindlist %>% write_fst("covariate_balance_all.fst")


data.balance <- read_fst("covariate_balance_all.fst", as.data.table = T)

#list.covariate <- lapply(names.study, function(x){
#  fread(file.path("res",x, "covariate.csv"), integer64 = "numeric")
#})

data.covariate <- read_fst("covariate_all.fst", as.data.table = T)


data.result <- lapply(names.study, function(x){
  fread(file.path("res",x, "cohort_method_result.csv"), integer64 = "numeric")
}) %>% rbindlist

data.interaction <- lapply(names.study, function(x){
  fread(file.path("res",x, "cm_interaction_result.csv"), integer64 = "numeric")
}) %>% rbindlist


#names(list.interaction) <- names(list.result) <- names.study




## ID info

list.idinfo <- lapply(c("exposure_of_interest.csv", "outcome_of_interest.csv", "cohort_method_analysis.csv"), function(x){
  info.id <- fread(file.path("res", names.study[1], x))[, 1:2]
  if (grepl("exposure", x)){
    split.comparator <- data.result[, unique(comparator_id), keyby = "database_id"] %>% split(.$database_id)
    info.id <- info.id[exposure_id %in% c(9991, sort(lapply(split.comparator, function(x){unique(x$V1)}) %>% Reduce(union, .)))]
  }
  vec.id <- info.id[[1]]
  names(vec.id) <- info.id[[2]]
  return(vec.id)
})

names(list.idinfo) <- c("exposure", "outcome", "analysis")

analysis.originalN <- c(1, 1, 1, 1, 15, 15, 15, 15, 37, 39, 37, 39, 37, 39, 37, 39, rep(39, 5))
names(analysis.originalN) <- sort(lapply(data.result[, unique(analysis_id), keyby = "database_id"] %>% split(.$database_id), function(x){unique(x$V1)}) %>% Reduce(intersect, .))




## KM info
data.km <- lapply(names.study, function(x){
  fread(file.path("res",x, "kaplan_meier_dist.csv"), integer64 = "numeric")
}) %>% rbindlist

## ps distribution
data.ps <- lapply(names.study, function(x){
  fread(file.path("res",x, "preference_score_dist.csv"), integer64 = "numeric")
}) %>% rbindlist

## power
data.fudist <- lapply(names.study, function(x){
  fread(file.path("res",x, "cm_follow_up_dist.csv"), integer64 = "numeric")
}) %>% rbindlist

data.cma <- lapply(names.study, function(x){
  dd <- fread(file.path("res",x, "cohort_method_analysis.csv"), integer64 = "numeric")
  dd$database_id <- x
  return(dd)
}) %>% rbindlist


data.negres <- lapply(names.study, function(x){
  dd <- fread(file.path("res",x, "negative_control_outcome.csv"), integer64 = "numeric")
  dd$database_id <- x
  return(dd)
}) %>% rbindlist

## Analysis type: for sensitivity analysis
type.analysis <- data.table(analysis_id = list.idinfo$analysis,
                            Adjustment = sapply(strsplit(names(list.idinfo$analysis), ", "), `[[`, 1), 
                            TAR = sapply(strsplit(names(list.idinfo$analysis), ", "), function(x){
                              res <- NULL
                              if (length(x) == 2){
                                res <- x[2]
                              } else{
                                res <- paste(x[2:3], collapse = ", ")
                              }
                              return(gsub("on", "On", res))
                            }),
                            Interaction = grepl("interaction", names(list.idinfo$analysis)))

type.analysis[, `:=`(Adjustment = factor(Adjustment, levels = c("No PS matching", "1:1 PS matching", "Variable-ratio PS matching", "PS stratification")),
                     TAR = factor(TAR))] 

## Group outcome: for sensitivity analysis
name.cancer <- unique(sapply(strsplit(sapply(strsplit(names(list.idinfo$outcome), " cancer| Cancer"), `[[`, 1), ", "), `[[`, 1))
type.cancer <- sapply(name.cancer[-length(name.cancer)], function(x){
  if (x == "Overall"){
    list.idinfo$outcome[c(grep(x, names(list.idinfo$outcome), value = T), "Cancer mortality")]
  } else{
    list.idinfo$outcome[grep(x, names(list.idinfo$outcome), value = T)]
  }
})



## Adding calibrated confidential intervals
library(CohortMethod)
library(EmpiricalCalibration)

change_columns <- c("calibrated_rr","calibrated_ci_95_lb","calibrated_ci_95_ub","calibrated_log_rr","calibrated_se_log_rr")
data.result[,(change_columns):=lapply(.SD,as.numeric),.SDcols=change_columns]

for(databaseId in names.study){
    singleCohortMethodResult<-data.result[database_id==databaseId,,]
    rowindex<-data.result[,database_id==databaseId,]
    tcos <- unique(singleCohortMethodResult[, c("target_id", "comparator_id", "outcome_id")])
    tcos <- tcos[tcos$outcome_id %in% list.idinfo$outcome, ]
    tcs<-unique(tcos[,c("target_id","comparator_id")])

    for (analysisId in unique(data.result$analysis_id)){
        for (i in seq(nrow(tcs))){
            tc<- tcs[i,]
            index<-singleCohortMethodResult$target_id==tc$target_id&
                singleCohortMethodResult$comparator_id==tc$comparator_id&
                singleCohortMethodResult$analysis_id==analysisId&
                singleCohortMethodResult$database_id==databaseId&
                !is.na(singleCohortMethodResult$log_rr) &
                !is.na(singleCohortMethodResult$se_log_rr)
            
            if(sum(index, na.rm=T)==0) next
            negativeData<-singleCohortMethodResult[index &
                                                 singleCohortMethodResult$outcome_id %in% unique(data.negres$outcome_id),]
            null<-EmpiricalCalibration::fitNull(negativeData$log_rr,
                                                negativeData$se_log_rr)

            model<-EmpiricalCalibration::convertNullToErrorModel(null)

            calibratedCi<-EmpiricalCalibration::calibrateConfidenceInterval(logRr=singleCohortMethodResult[index,]$log_rr,
                                                                            seLogRr=singleCohortMethodResult[index,]$se_log_rr,
                                                                            model=model,
                                                                            ciWidth = 0.95)

            singleCohortMethodResult[index,]$calibrated_log_rr<-calibratedCi$logRr
            singleCohortMethodResult[index,]$calibrated_se_log_rr<-calibratedCi$seLogRr
            singleCohortMethodResult[index,]$calibrated_ci_95_lb<-exp(calibratedCi$logLb95Rr)
            singleCohortMethodResult[index,]$calibrated_ci_95_ub<-exp(calibratedCi$logUb95Rr)
            singleCohortMethodResult[index,]$calibrated_rr<-exp(calibratedCi$logRr)
        }
    }
    data.result[rowindex,]<-singleCohortMethodResult
}

data.result[,calibrated_p:=2*(pmin(pnorm(calibrated_log_rr/calibrated_se_log_rr),1-pnorm(calibrated_log_rr/calibrated_se_log_rr))),]

