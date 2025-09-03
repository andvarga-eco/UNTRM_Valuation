library(readxl)
library(dplyr)
library(tidyr)
library(mlogit)

choice<- read_excel("C:/Users/andresmv/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/WI_100.xls", 
                     sheet = "choice")

price<- read_excel("C:/Users/andresmv/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/WI_100.xls", 
                    sheet = "price")

attributes<-read_excel("C:/Users/andresmv/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/WI_100.xls", 
                       sheet = "attributes")

household<-read_excel("C:/Users/andresmv/OneDrive - Universidad del Norte/Drive/Uninorte/Uninorte-docencia/Doctorado Perú/Valoración 202530/data y script/WI_100.xls", 
           sheet = "household")


#Data en formato largo

data<-choice%>%left_join(price,by="id")
data<-data%>%pivot_longer(!c(id,choice),names_to = "site",names_prefix="tc",values_to = "tc")
data<-data%>%mutate(dchoice=ifelse(choice==site,1,0))

attributes<-attributes%>%mutate(site=as.character(seq(from=1,to=100,by=1)))

data<-data%>%left_join(attributes,by="site")
data<-data%>%left_join(household,by="id")

data_rum<-dfidx(data,idx=c("id","site"))

m1<-mlogit(dchoice~tc+walleye+salmon+panfish|0,data=data_rum)
summary(m1)
dapwem1<--m1$coefficients[2]/m1$coefficients[1]

m2<-mlogit(dchoice~tc+walleye+salmon+panfish+ramp+restroom|0,data=data_rum)
summary(m2)
dapwem2<--m2$coefficients[2]/m2$coefficients[1]

m3<-mlogit(dchoice~tc+walleye+salmon+panfish+ramp+restroom+ramp:boat+restroom:kids|0,data=data_rum)
summary(m3)


