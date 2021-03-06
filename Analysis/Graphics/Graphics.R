#################
### DataFrame ###
#################

# voy a la carpeta del proyecto
root <- rprojroot::is_rstudio_project
basename(getwd())

# load the function to get the df list
source(root$find_file("Analysis/AuxiliaryFunctions/Graphics/DataFrames_ForGraphics.R"))

# get the df list
# experimento = ExperimentOnlySurvey, ExperimentComplete, ambos
DF_list <- DataFrame_ForGraphics(experimento = "ambos", 
                                  filtroRT_Disc_Sup = 5000,
                                  filtroRT_Disc_Inf = 100,
                                  filtroRT_Conf_Sup = 5000,
                                  filtroRT_Conf_Inf = 100,
                                  filtroTrial = 20)

# DF_list:
# a df_total # todos los datos, trial a trial
# b d.sin.normalizar # solo los datos unicos, sino el trial a trial
# c d # lo mismo que el anterior pero normalizado
# d d.mc.filter # ahora sin los que tienen AUROC2 menor a 0.5
# e d.sin.normalizar.solo.FyM ## solo hombres mujeres
# f d.sin.normalizar.solo.FyM.mc.filter 
# g d.solo.FyM.mc.filter
# h df_total.sin.normalizar.solo.FyM.mc.filter

d.sin.normalizar <- DF_list$b
df_total <- DF_list$a
###############
### library ###
###############
library(tidyverse)
library(ggridges)

################
### Graphics ###
################

####### histograms







#### AQ by sex

# a
d.sin.normalizar.solo.FyM.mc.filter[d.sin.normalizar.solo.FyM.mc.filter=="Masculino"] <- 'Male' 
d.sin.normalizar.solo.FyM.mc.filter[d.sin.normalizar.solo.FyM.mc.filter=="Femenino"] <- 'Female' 

# a1
ggplot(d.sin.normalizar.solo.FyM.mc.filter,aes(x=aq)) + 
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Female'),
           fill = "red", alpha = 0.2) +
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Male'),
           fill = "blue", alpha = 0.2) +
  facet_grid(rows = vars(Im))+
  xlab("AQ") +
  ylab("Participants")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

# a2
ggplot(d.sin.normalizar.solo.FyM.mc.filter,aes(x=aq)) + 
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Female'),
           fill = "red", alpha = 0.2) +
  geom_bar(data=subset(d.sin.normalizar.solo.FyM.mc.filter,
                       Im == 'Male'),
           fill = "blue", alpha = 0.2) +
  facet_grid(cols = vars(Im))+
  xlab("AQ") +
  ylab("Participants")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))
        

## Histograms of reaction times 
RT_disk_task = df_total$t_ensayo_discriminacion
RT_conf_task = df_total$t_ensayo_confianza
RT_disk_task_label <- rep('RT_disk_task',length(RT_disk_task))
RT_conf_task_label <- rep('RT_conf_task', length(RT_conf_task))

RT_task <- c(RT_disk_task,RT_conf_task)
RT_task_labels <- c(RT_disk_task_label,RT_conf_task_label)

d <- data.frame(RT_task = RT_task,
                RT_task_labels = RT_task_labels)


# a1
# disc task
ggplot(df_total, aes(x=t_ensayo_discriminacion))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in discrimination task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 

# conf task
ggplot(df_total, aes(x=t_ensayo_confianza))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("RT in confidence task (ms)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 

# b1
ggplot(d,aes(x=RT_task)) + 
  geom_histogram(data=subset(d,RT_task_labels == 'RT_disk_task'),
           fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(d,RT_task_labels == 'RT_conf_task'),
           fill = "blue", alpha = 0.2) +
  facet_grid(cols = vars(RT_task_labels))+
  xlab("RT_task") +
  ylab("Trials")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

# b2
ggplot(d,aes(x=RT_task)) + 
  geom_histogram(data=subset(d,RT_task_labels == 'RT_disk_task'),
                 fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(d,RT_task_labels == 'RT_conf_task'),
                 fill = "blue", alpha = 0.2) +
  facet_grid(rows = vars(RT_task_labels))+
  xlab("RT_task") +
  ylab("Trials")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        strip.text = element_text(size = 20),
        axis.title.x = element_text(size = 30))

####### histogram age

# age
ggplot(d.sin.normalizar, aes(x=edad))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("count")+
  xlab("Age (years)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 25)) 


####### metacognition and performance plot


mc.sorted <-  d.sin.normalizar[order(d.sin.normalizar$mc),]
subjects <- 1:nrow(mc.sorted)
mc.sorted$s <- subjects

ggplot(mc.sorted, aes(s)) +                   
  geom_point(aes(x = s, y=mc , colour="AUROC2")) +  
  geom_point(aes(x = s, y=pc , colour="Performance"), shape = 17) +  
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  labs(x="Participants", y="", color = "") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        #legend.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 

####### density plots

DomainPsychoticism <- d.sin.normalizar$DomainPsychoticism
DomainDisinhibition  <- d.sin.normalizar$DomainDisinhibition
DomainAntagonism <- d.sin.normalizar$DomainAntagonism
DomainDetachment <- d.sin.normalizar$DomainDetachment
DomainNegativeAffect <- d.sin.normalizar$DomainNegativeAffect

DomainValues <- c(DomainPsychoticism,DomainDisinhibition,DomainAntagonism,
                  DomainDetachment,DomainNegativeAffect)

DomainPsychoticism_label <- rep("DomainPsychoticism",length(DomainPsychoticism))
DomainDisinhibition_label <- rep("DomainDisinhibition", length(DomainDisinhibition)) 
DomainAntagonism_label <- rep("DomainAntagonism", length(DomainAntagonism))
DomainDetachment_label <- rep("DomainDetachment", length(DomainDetachment))
DomainNegativeAffect_label <- rep("DomainNegativeAffect", length(DomainNegativeAffect))

DomainLabels <- c(DomainPsychoticism_label,DomainDisinhibition_label,
                  DomainAntagonism_label, DomainDetachment_label,
                  DomainNegativeAffect_label)

d1 <- data.frame(DomainValues = DomainValues,
                 DomainLabels = DomainLabels)

ggplot(d1, aes(x = DomainValues, fill = DomainLabels)) + 
  geom_density(alpha = 0.2)


ggplot(d.sin.normalizar.solo.FyM.mc.filter, aes(x = mc, y = aq.quartile, fill = Im, colour = Im, alpha=0.5)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.title = element_text(colour="blue", size=10, 
                                      face="bold"))+
  theme(legend.text = element_text(colour="blue", size=10, 
                                     face="bold"))

# by sex
l <- d.sin.normalizar.solo.FyM.mc.filter
for (i in 1:nrow(l)) {
  if(l$Im[i] == 'Femenino'){l$Im[i]= 'Female'}
  if(l$Im[i] == 'Masculino'){l$Im[i]= 'Male'}
}

l$Gender <-l$Im 

# Use semi-transparent fill
p<-ggplot(l, aes(x=aq, fill=Gender))+ xlab("AQ") +
  geom_density(alpha=0.4) +
  scale_y_continuous(expand = expansion(mult = c(0,0)))
p
p+  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size=30),
        axis.title.x = element_text(size = 30))+
  scale_fill_grey()

# distribucion de confianza para trials correctas e incorrectas 

p<-ggplot(df_total, aes(x=AQ, fill=genero))+ xlab("AQ") +
  geom_density(alpha=0.4)
p
p+  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26),
        axis.title.y = element_text(size = 31),
        legend.title = element_text(size=31),
        legend.text = element_text(size=26),
        axis.title.x = element_text(size = 31))+
  scale_fill_grey()


##### probando un grafico de tr por sujeto y cantidad de trials menores a 100ms

Cant_Trial_Disc <- rep(NA,length(unique(df_total$sujetos)))
Cant_Trial_Conf <- rep(NA,length(unique(df_total$sujetos)))
ExistingSubjects <- unique(df_total$sujetos)

for (s in 1:length(ExistingSubjects)){
  subj <- df_total[df_total$sujetos== ExistingSubjects[s],] # getting data by subject
  Cant_Trial_Disc[s] <- nrow(subj[subj$t_ensayo_discriminacion < 100,])
  Cant_Trial_Conf[s] <- nrow(subj[subj$t_ensayo_confianza < 100,])
}

d1 <- data.frame(Cant_trial_conf = Cant_Trial_Conf,
                 Cant_trial_disc = Cant_Trial_Disc)
d.sin.normalizar <- cbind(d.sin.normalizar, d1)

ggplot(d1, aes(x=Cant_Trial_Conf))+
  geom_histogram(color="darkred", fill="red", bins = 100)+
  ylab("Cantidad de participantes")+
  xlab("Cantidad de trials menores a 100 ms (confidence task)")+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20)) 



Cant_trial_conf.sorted <-  d.sin.normalizar[order(d.sin.normalizar$Cant_trial_conf),]
subjects <- 1:nrow(Cant_trial_conf.sorted)
Cant_trial_conf.sorted$s <- subjects

ggplot(Cant_trial_conf.sorted, aes(Cant_trial_conf)) +                   
  geom_point(aes(x = Cant_trial_conf, y=mc)) +  
  #scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.margin = margin(1, 1,1, 1, "cm"),
        panel.background = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.title.x = element_text(size = 30)) 
  

library(arm)

a=lm(mc ~ Cant_trial_disc, data = d.sin.normalizar)
summary(a)
display(a)

par(mar = c(5, 5, 5, 5))
plot(d.sin.normalizar$Cant_trial_disc,
     d.sin.normalizar$mc, 
     pch = 16, cex = 1, col = "black",
     xlab = "Cant fast Trials (<100ms) Disc Task", ylab = "AUROC2", 
     cex.axis = 1.7, cex.lab = 1.8)
abline(lm(d.sin.normalizar$mc ~ d.sin.normalizar$Cant_trial_disc),
       col="grey", lwd=3)


