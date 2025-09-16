library(readxl)
library(dplyr)
library(purrr)
library(broom)


#

cvm <- read_excel("C:/Users/andresmv/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/phaneuf_cvm.xlsx")
cvm<-cvm%>%mutate(choicef=as.factor(choice))
cvm_v1<-cvm%>%filter(version==1)
freq<-table(cvm_v1$choice,cvm_v1$cost)
freq
prop.table(freq,margin=2)

m1<-glm(choicef~cost,data=cvm_v1,family = "binomial")
summary(m1)
DAP<--m1$coefficients[1]/m1$coefficients[2]


#Bootstrap

#install.packages("tidymodels")
library(tidymodels)

set.seed(123)
cvm_v1_boot<-bootstraps(cvm_v1,times=1000)

fit_boot<-cvm_v1_boot%>%
  mutate(model=map(splits,~glm(choicef~cost,data=.x,family = "binomial")))
fit_boot

boot_coefs<-fit_boot%>%
  mutate(coefs=map(model,tidy))

coefs<-boot_coefs %>%
  unnest(coefs)
coefs_w<-coefs%>%select(c(id,term,estimate))%>%pivot_wider(names_from = term,values_from = estimate)
coefs_w<-coefs_w%>%mutate(dap=-1*`(Intercept)`/cost)

#Calcular intervalo de confianza del ratio
coefs_w %>%
  summarize(
    lower = quantile(dap, 0.025),
    upper = quantile(dap, 0.975),
    mean_dap = mean(dap),
    median_dap=quantile(dap,0.5)
  )


#Modelo 2

m2<-glm(choicef~cost+recreator,data=cvm_v1,family = "binomial")
summary(m2)
DAP_nr<--m2$coefficients[1]/m2$coefficients[2]
DAP_r<--(m2$coefficients[1]+m2$coefficients[3])/m2$coefficients[2]

#Bootstrap

#install.packages("tidymodels")
set.seed(123)
cvm_v1_boot<-bootstraps(cvm_v1,times=1000)

fit_boot2<-cvm_v1_boot%>%
  mutate(model2=map(splits,~glm(choicef~cost+recreator,data=.x,family = "binomial")))

boot_coefs2<-fit_boot2%>%
  mutate(coefs=map(model2,tidy))

coefs2<-boot_coefs2 %>%
  unnest(coefs)
coefs2_w<-coefs2%>%select(c(id,term,estimate))%>%pivot_wider(names_from = term,values_from = estimate)
coefs2_w<-coefs2_w%>%mutate(dap_nr=-1*`(Intercept)`/cost,
                            dap_r=-1*(`(Intercept)`+recreator)/cost)

#Calcular intervalo de confianza del ratio
coefs2_w %>%
  summarize(
    lower_nr = quantile(dap_nr, 0.025),
    upper_nr = quantile(dap_nr, 0.975),
    mean_dap_nr = mean(dap_nr),
    median_dap_nr=quantile(dap_nr,0.5),
    lower_r = quantile(dap_r, 0.025),
    upper_r = quantile(dap_r, 0.975),
    mean_dap_r = mean(dap_r),
    median_dap_r=quantile(dap_r,0.5)
      )






#DCchoice
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")
install.packages("DCchoice", repos = c("https://cran.r-universe.dev", "https://cloud.r-project.org"))
library(DCchoice)


m1sb<-sbchoice(choice~1|cost,data=cvm_v1, dist="logistic")
m1sb
krCI(m1sb)
bootCI(m1sb)
