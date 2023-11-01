### ElasticNet linear model with Confidence facets - alpha selection

# libraries
library(glmnet)
library(dplyr)
library(caret)
library(boot)


root <- rprojroot::is_rstudio_project
basename(getwd())               

####### data frame with filters already applied
filepath <- root$find_file("Data/df_total_filtered.Rda")
load(file= filepath)

source(root$find_file("Analysis/AuxiliaryFunctions/DataFrame_subset.R"))
d <- DataFrame_subset(df_total_filtered)

### preprocessing
d$gender <- ifelse(d$gender == "Male",1,0)

# transforming the df to a matrix, without a few variables that don't matter
d_caret <- d %>%
  select(!c(Participant,
            DomainNegativeAffect,
            DomainDetachment,
            DomainAntagonism,
            DomainDisinhibition,
            DomainPsychoticism,
            PC,
            ConfSD,
            ReacTimeMean_DiscTask,
            ReacTimeSD_DiscTask,
            ReacTimeMean_ConfTask,
            ReacTimeSD_ConfTask,
            mc))

### selecting the alpha and the lambda using caret
loocv = trainControl(method = "LOOCV")

m_elnet  = train(
  ConfMean ~ ., data = d_caret,
  method = "glmnet",
  trControl = loocv,
  tuneLength = 50
)

alpha_selected <- m_elnet$bestTune[[1]] # 0.2285714
lambda_selected <- m_elnet$bestTune[[2]] # 0.1555395

## runing the model regression with the selected alpha and lambda

d_mat <- d %>%
  select(!c(Participant,
            DomainNegativeAffect,
            DomainDetachment,
            DomainAntagonism,
            DomainDisinhibition,
            DomainPsychoticism,
            PC,
            ConfMean,
            ConfSD,
            ReacTimeMean_DiscTask,
            ReacTimeSD_DiscTask,
            ReacTimeMean_ConfTask,
            ReacTimeSD_ConfTask,
            mc)) %>%
  data.matrix()



fit <- glmnet(d_mat, d$ConfMean, 
              family = "gaussian",
              alpha = alpha_selected,
              lambda = lambda_selected)

print(fit)

a <- coef(fit)

save(a, file = "Data/Regression_Results/Confidence_Results/Conf_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
