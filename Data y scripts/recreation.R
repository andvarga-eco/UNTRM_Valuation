library(readxl)
library(dplyr)
library(tidyr)
library(AER)
library(MASS)
library(pglm)

NC_beach <- read_excel("C:/Users/andre/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/NC_beach.xls")

#Formato largo

NC_long <- NC_beach %>%
  pivot_longer(
    cols = -c(id, inc),
    names_to = c("variable", "beach"),
    names_pattern = "([a-z]+)(\\d+)",
    values_to = "valor"
  )%>%
  pivot_wider(
    names_from = variable,
    values_from = valor
  )

# Single site: Seleccionar playa 10

NC_b10<-NC_long%>%filter(beach==10)
table(NC_b10$tr)
hist(NC_b10$tr,breaks=50)

m1<-glm(tr~pr,data=NC_b10,family="poisson")
summary(m1)
dispersiontest(m1)

m2<-glm(tr~pr+inc,data=NC_b10,family="poisson")
summary(m2)
dispersiontest(m2)

# Compensating variation: ingreso que compensa la pérdida de acceso al sitio

NC_b10<-NC_b10%>%mutate(E_trips=exp(m1$coefficients[1]+m1$coefficients[2]*pr))
CV_full<-mean(NC_b10$E_trips)/(-1*m1$coefficients[2])
visited<-NC_b10%>%filter(tr>0)
CV_visited<-mean(visited$tr)/(-1*m1$coefficients[2])
B_pertrip<--1/m1$coefficients[2]

# Pérdida de bienestar por tarifa de acceso $5

NC_b10<-NC_b10%>%mutate(E_trips_fee=exp(m1$coefficients[1]+m1$coefficients[2]*(pr+5)))
NC_b10<-NC_b10%>%mutate(CV_fee=(E_trips_fee-E_trips)/m1$coefficients[2])
CV_fee<-mean(NC_b10$CV_fee)

#Poisson Panel data

m1pglm<-pglm(tr~pr+width+park,index=c("beach"),model="pooling", effect="individual",data=NC_long,
             family="poisson")
summary(m1pglm)
