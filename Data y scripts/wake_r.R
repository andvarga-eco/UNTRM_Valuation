library(readxl)
library(dplyr)

wake_exercise <- read_excel("C:/Users/andre/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/wake_exercise.xlsx")

wake_exercise<-wake_exercise%>%mutate(logdist=ifelse(lakedist>0,log(lakedist),NA))

m1<-lm(price~lakedist+living_area+baths+age+fireplace+garage_area+condA+condB+condC,data=wake_exercise)
summary(m1)

m2<-lm(lprice~lakedist+living_area+baths+age+fireplace+garage_area+condA+condB+condC,data=wake_exercise)
summary(m2)

m3<-lm(lprice~logdist+living_area+baths+age+fireplace+garage_area+condA+condB+condC,data=wake_exercise)
summary(m3)

wake_exercise$daphat<-m2$coefficients[2]*wake_exercise$price
mean(wake_exercise$daphat)

