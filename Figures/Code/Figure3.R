### Figure3

# set root
root <- rprojroot::is_rstudio_project
basename(getwd())  

# library
library(jtools)
library(tidyverse)
library(glmnet)
library(boot)

### reg beta

filepath <- root$find_file("Data/Regression_Results/Metacognitive_sensitivity_Results/mc_PID_facets_Beta_linear_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- term <- c("Anxiousness", "EmotionalLability")
coeff <- c(unname(sum_a$coefficients$mean[3,"Estimate"]),unname(sum_a$coefficients$mean[10,"Estimate"]))
se <- c(unname(sum_a$coefficients$mean[3,"Std. Error"]), unname(sum_a$coefficients$mean[10,"Std. Error"]))
model <- rep("Beta Multitrait",2)


df.models <- data.frame(terms = term,
                        coeff = coeff,
                        se = se,
                        model = model)

### reg unitrait beta

# Anxiety 

filepath <- root$find_file("Data/Regression_Results/Metacognitive_sensitivity_Results/individual_mc_PID_facets_beta_model/Anxiousness.norm_mc_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "Anxiousness"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "Beta Unitrait"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

# EmotionalLability 

filepath <- root$find_file("Data/Regression_Results/Metacognitive_sensitivity_Results/individual_mc_PID_facets_beta_model/EmotionalLability.norm_mc_PID_facets_beta_model.RData")
load(file= filepath)

sum_a <- summary(a)
term <- "EmotionalLability"
coeff <- unname(sum_a$coefficients$mean[2,"Estimate"])
se <- unname(sum_a$coefficients$mean[2,"Std. Error"])
model <- "Beta Unitrait"

df.beta <- data.frame(terms = term,
                      coeff = coeff,
                      se = se,
                      model = model)

df.models <- rbind(df.models, df.beta)

#### elastic net 
filepath <- root$find_file("Data/Regression_Results/Metacognitive_sensitivity_Results/mc_PID_facets_fit_elasticNet_alpha_lambda_loocv.RData")
load(file= filepath)

term <- term <- c("Anxiousness", "EmotionalLability")
coeff <- c(a[4],a[11])
se <- rep(0,2) ## sd is not usefull in regularized regressions, could cause misinterpretations
model <- rep("Elastic-Net",2)

df.elastic <- data.frame(terms = term,
                         coeff = coeff,
                         se = se,
                         model = model)


df.models <- rbind(df.models, df.elastic)

##### preprocessing to plot

df.models <- df.models %>%
  mutate(model = fct_relevel(model,
                             "Beta Multitrait",
                             "beta unitrait",
                             "Elastic-Net"
                             ))

## plot models
ggplot(df.models , aes(coeff, terms, color=model)) + 
  geom_point(aes(shape=model),size=4, 
             position=position_dodge(width=0.3)) +
  geom_vline(xintercept= 0, linetype='dashed', color= "black")+
  scale_color_manual(name="model",
                     values=c("darkred", "darkblue", "darkgreen")) +
  scale_shape_manual(name="model",values=c(17,18,19)) + 
  scale_x_continuous("Regression Coefficients") +
  scale_y_discrete(labels = c("Anxiousness", "Emotional Lability"), expand = c(1, 0)) + # Ajusta el expand
  geom_errorbar(aes(xmin= coeff - 2* se,xmax= coeff + 2* se),
                width=0.1,
                position=position_dodge(width=0.3), size = 1)+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      plot.margin = margin(1, 1,1, 1, "cm"),
      legend.text =  element_text(size = 15),
      legend.title =  element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15), 
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 15))

ggsave("Figures/Figures/Figure3.png", 
       width = 10, height = 6)
