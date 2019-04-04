#CUBE QC report: South Africa
#Created by: Nicole Dear
#Created on: 2019-04-03

install.packages("tidyverse")
install.packages("lubridate")
install.packages("gmodels")
install.packages("rmarkdown")
library(tidyverse)
library(lubridate)
library(gmodels)
library(rmarkdown)

#set working directory
setwd("H:/CUBE/QC")

#load and reduce dataset
sa<-read.csv("CUBEContraceptiveUse_DATA_2019-04-03_1240.csv")
sa_mo6<-sa[c(1:346)]
names(sa_mo6)

#criteria 1: exit method must match method at 1mo post exit visit
sa_mo6$flag_exitmethod<-ifelse((sa_mo6$contraception_at_exit==1&sa_mo6$month_injectable3___1==1)|(sa_mo6$contraception_at_exit==2&sa_mo6$copper_iud___1==1)|(sa_mo6$contraception_at_exit==3&sa_mo6$rod_implant_jadelle___1==1),1,0)
table(sa_mo6$flag_exitmethod)
a<-subset(sa_mo6, sa_mo6$flag_exitmethod==FALSE)
print(a$pid)

#criteria 2: exited on iud or implant, discontinued iud or implant
  #for those discont iud
sa_mo6_exitiud<-subset(sa_mo6, sa_mo6$contraception_at_exit==2&any_method_discont==1)
sa_mo6_exitiud$flag_discontiud<-ifelse(sa_mo6_exitiud$method_discontinued==5,1,0)
b<-subset(sa_mo6, sa_mo6_exitiud$flag_discontiud==FALSE)
print(b$pid)

sa_mo6_exitiud$flag_discontuse<-ifelse(sa_mo6_exitiud$discont_use_iud==1,1,0)
c<-subset(sa_mo6, sa_mo6_exitiud$flag_discontuse==FALSE)
print(c$pid)

#for those discont implant
sa_mo6_exitimp<-subset(sa_mo6, sa_mo6$contraception_at_exit==3&any_method_discont==1)
sa_mo6_exitimp$flag_discontimp<-ifelse(sa_mo6_exitimp$method_discontinued==3,1,0)
d<-subset(sa_mo6, sa_mo6_exitiud$flag_discontimp==FALSE)
print(d$pid)

sa_mo6_exitimp$flag_discontuse<-ifelse(sa_mo6_exitimp$discont_use_iud==1,1,0)
e<-subset(sa_mo6, sa_mo6_exitimp$flag_discontuse==FALSE)
print(e$pid)

#criteria 3: exited on iud or implant, did not discontinue iud or implant
  #for those on iud or impant at exit

sa_mo6$discont_iudimp_no<-ifelse((sa_mo6$contraception_at_exit==2&sa_mo6$any_method_discont==0&sa_mo6$discont_use_iud==0)|(sa_mo6$contraception_at_exit==3&sa_mo6$any_method_discont==0&sa_mo6$discont_use_iud==1),1,0)
table(sa_mo6$discont_iudimp_no)
b<-subset(sa_mo6, sa_mo6$discont_iudimp_no==TRUE)

