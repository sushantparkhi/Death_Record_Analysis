library(dplyr)
library(ggplot2)
library(sampling)
library(gridExtra)

DeathRecords<-read.csv("US_Deaths/DeathRecords/DeathRecords.csv",stringsAsFactors = F,header = T)

#Total Death Proportion
data<-table(DeathRecords$MannerOfDeath)/length(DeathRecords$MannerOfDeath)
data<-data[2:7]
data<-as.data.frame(data)
var_type<-c("Accident","Suicide","Homicide","Pending","Cannot-determine","Natural")
All<-cbind(data,var_type)

ggplot(data=data, aes(x=var_type, y=data, fill=var_type)) +
  geom_bar(stat="identity")+xlab("Type of Death")+ylab("Proportion")+ggtitle("Proportion of Total Deaths in United States")+ scale_fill_hue(name="Type of Death")

#Death Cases by Age

Death_By_Age<-DeathRecords %>%
  filter(AgeType==1)

Death_By_Age<- Death_By_Age %>%
  group_by(Age,Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id))

ggplot(Death_By_Age,aes(x=Age,y=Percentage,fill=Sex))+
  geom_bar(stat="identity")+scale_fill_brewer(palette="Set1")+scale_y_continuous(name="Percentage")+
  labs(title="Percentage of Death Cases \n According to Ages in 2014")+xlim(c(0,130))+facet_grid(.~Sex)

#Suicide Cases
Suicide<-DeathRecords %>%
  filter(MannerOfDeath==2)

#Removing the Records which has no Ages 
Suicide<-Suicide %>%
  filter(AgeType!=9)

#Group By Sex using function in dplyr and using summarise to get the summary
SuicideSex<- Suicide %>%
  group_by(Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
SuicideSex

#Finding the Distribution of the Suicide Cases with Sex and adding Column of Mean
SuicideDist<- Suicide %>%
  group_by(Sex) %>%
  mutate(Mean=mean(Age))

ggplot(SuicideDist,aes(x=Age,y=..density.., fill=Sex))+ylab("Density")+geom_density()+
  facet_grid(.~Sex)+scale_fill_brewer()+labs(title="US Suicide Cases Distribution in 2014")

#Suicide Cases on Education Level
ByEdu<-Suicide %>%
  group_by(Education2003Revision,Sex) %>%
  summarise(Sum=n(),Percentage=100*n()/length(.$Id))
ByEdu

ggplot(ByEdu,aes(x=factor(Education2003Revision),y=Percentage,fill=Sex))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set1")+
  scale_x_discrete(name="Education Level 2003 Rev.Code")+
  labs(title="Percentage of Suicide Cases \n per Education Level in 2014")

#Suicide Cases Grouped by Marital Status
ByMarital<-Suicide %>%
  group_by(MaritalStatus,Sex) %>%
  summarise(Cases_Suicide=n(),Percentage_Suicide=100*n()/length(.$Id))

ggplot(ByMarital,aes(x=MaritalStatus,y=Percentage_Suicide,fill=Sex))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Marital Status",labels=c("Divorced","Married","Single","unknown","Widowed"))+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(name="Percentage")+labs(title="Percentage of Suicide Cases on Marital Status")

#Accident Cases
Accident<-DeathRecords %>%
  filter(MannerOfDeath==1)

#Removing the Records which has no Ages 
Accident<-Accident %>%
  filter(AgeType!=9)

#Group By Sex using function in dplyr and using summarise to get the summary
AccidentSex<- Accident %>%
  group_by(Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
AccidentSex

#Finding the Distribution of the Accident Cases with Sex and adding Column of Mean
AccidentDist<- Accident %>%
  group_by(Sex) %>%
  mutate(Mean=mean(Age))

ggplot(AccidentDist,aes(x=Age,y=..density.., fill=Sex))+ylab("Density")+geom_density()+
  facet_grid(.~Sex)+scale_fill_brewer()+labs(title="US Accident Cases Distribution in 2014")+xlim(0,130)

#Accident Cases on Education Level
ByEdu<-Accident %>%
  group_by(Education2003Revision,Sex) %>%
  summarise(Sum=n(),Percentage=100*n()/length(.$Id))
ByEdu

ggplot(ByEdu,aes(x=factor(Education2003Revision),y=Percentage,fill=Sex))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set1")+
  scale_x_discrete(name="Education Level 2003 Rev.Code")+
  labs(title="Percentage of Accident Cases \n per Education Level in 2014")

#Accident Cases Grouped by Marital Status
ByMarital<-Accident %>%
  group_by(MaritalStatus,Sex) %>%
  summarise(Cases_Suicide=n(),Percentage_Accident=100*n()/length(.$Id))

ggplot(ByMarital,aes(x=MaritalStatus,y=Percentage_Accident,fill=Sex))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Marital Status",labels=c("Divorced","Married","Single","unknown","Widowed"))+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(name="Percentage")+labs(title="Percentage of Accident Cases on Marital Status")

#Stroke Cases Filtering by ICD10Code
#I64 in Stroke Cases
Strokes<-DeathRecords %>%
  filter(Icd10Code=="I64")

Strokes<-Strokes %>%
  filter(AgeType!=9)

StrokeSex<- Strokes %>%
  group_by(Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
StrokeSex

StrokeDist<- Strokes %>%
  group_by(Sex) %>%
  mutate(Mean=mean(Age))

ggplot(StrokeDist,aes(x=Age,y=..density.., fill=Sex))+ylab("Density")+geom_density()+
  facet_grid(.~Sex)+scale_fill_brewer()+labs(title="US Stroke Cases Distribution in 2014")+xlim(0,130)

ByMarital<-Strokes %>%
  group_by(MaritalStatus,Sex) %>%
  summarise(Cases_=n(),Percentage_Stroke=100*n()/length(.$Id))

ggplot(ByMarital,aes(x=MaritalStatus,y=Percentage_Stroke,fill=Sex))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Marital Status",labels=c("Divorced","Married","Single","unknown","Widowed"))+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(name="Percentage")+labs(title="Percentage of Stroke Cases on Marital Status")

ByPlace<-Strokes %>%
  group_by(PlaceOfInjury,Sex) %>%
  filter(PlaceOfInjury!=99)%>%
  filter(PlaceOfInjury!=9)%>%
  summarise(Cases_Strokes=n(),Percentage_Strokes=100*n()/length(.$Id))

ggplot(ByPlace,aes(x=PlaceOfInjury,y=Percentage_Strokes,fill=Sex))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+
  scale_x_continuous(name="Place of Injury",breaks=seq(0, 8, by = 1),labels=c("Home","Residential","School","Sports","Street","Service Area","Industrial","Farm","Other"))+
  scale_y_continuous(name="Percentage%")+
  scale_fill_brewer(palette="Set1")

#HIV/AIDS Cases Filtered by ICD10Code
Aids<-DeathRecords %>%
  filter(Icd10Code=="B24")

Aids<-Aids %>%
  filter(AgeType!=9)

Aids_Sex<- Aids %>%
  group_by(Sex)%>%
  summarise(Cases=n(),Percentage=100*n()/length(.$Id),Mean=mean(Age),Std=sd(Age))
Aids_Sex

AidsDist<- Aids %>%
  group_by(Sex) %>%
  mutate(Mean=mean(Age))

ggplot(AidsDist,aes(x=Age,y=..density.., fill=Sex))+ylab("Density")+geom_density()+
  facet_grid(.~Sex)+scale_fill_brewer()+labs(title="US HIV/AIDS Cases Distribution in 2014")+xlim(0,130)

ByMarital<-Aids %>%
  group_by(MaritalStatus,Sex) %>%
  summarise(Cases_=n(),Percentage_Aids=100*n()/length(.$Id))

ggplot(ByMarital,aes(x=MaritalStatus,y=Percentage_Aids,fill=Sex))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="Marital Status",labels=c("Divorced","Married","Single","unknown","Widowed"))+
  theme(axis.text.x=element_text(angle = 90,vjust=1))+  scale_fill_brewer(palette="Set1")+
  scale_y_continuous(name="Percentage")+labs(title="Percentage of HIV/AIDS Cases on Marital Status")

#Central Limit Theorem
samples<-10000
sample.size<-100
xbar_100<-numeric(samples)
for(i in 1:samples)
{
  xbar_100[i]<-mean(sample(DeathRecords$Age,size=sample.size,replace=TRUE))
}

samples<-10000
sample.size<-200
xbar_200<-numeric(samples)
for(i in 1:samples)
{
  xbar_200[i]<-mean(sample(DeathRecords$Age,size=sample.size,replace=TRUE))
}

samples<-10000
sample.size<-300
xbar_300<-numeric(samples)
for(i in 1:samples)
{
  xbar_300[i]<-mean(sample(DeathRecords$Age,size=sample.size,replace=TRUE))
}

samples<-10000
sample.size<-400
xbar_400<-numeric(samples)
for(i in 1:samples)
{
  xbar_400[i]<-mean(sample(DeathRecords$Age,size=sample.size,replace=TRUE))
}

graph1<-ggplot(as.data.frame(xbar_100), aes(x=xbar_100))+geom_histogram(aes(y = ..density..),fill="orange",alpha=I(.6),binwidth=0.5) + geom_density()+ylab("Density")+xlim(c(65,90))+ggtitle("Sample Size = 100")+xlab("Age")

graph2<-ggplot(as.data.frame(xbar_200), aes(x=xbar_200))+geom_histogram(aes(y = ..density..),fill="darkorange",alpha=I(.6),binwidth=0.5) + geom_density()+ylab("Density")+xlim(c(65,90))+ggtitle("Sample Size = 200")+xlab("Age")

graph3<-ggplot(as.data.frame(xbar_300), aes(x=xbar_300))+geom_histogram(aes(y = ..density..),fill="red",alpha=I(.6),binwidth=0.5) + geom_density()+ylab("Density")+xlim(c(65,90))+ggtitle("Sample Size = 300")+xlab("Age")

graph4<-ggplot(as.data.frame(xbar_400), aes(x=xbar_400))+geom_histogram(aes(y = ..density..),fill="darkred",alpha=I(.6),binwidth=0.5) + geom_density()+ylab("Density")+xlim(c(65,90))+ggtitle("Sample Size = 400")+xlab("Age")

grid.arrange(graph1, graph2, graph3, graph4, ncol=2)

cat("(Sample Size = 100) Mean = ",mean(xbar_100),"Standard Deviation = ",sd(xbar_100),"\n")
cat("(Sample Size = 200) Mean = ",mean(xbar_200),"Standard Deviation = ",sd(xbar_200),"\n")
cat("(Sample Size = 300) Mean = ",mean(xbar_300),"Standard Deviation = ",sd(xbar_300),"\n")
cat("(Sample Size = 400) Mean = ",mean(xbar_400),"Standard Deviation = ",sd(xbar_400),"\n")

#Sampling
#Simple Sampling
s<-srswr(100,nrow(DeathRecords))
rows<-(1:nrow(DeathRecords))[s!=0]
rows<-rep(rows,s[s!=0])
sample.1<-DeathRecords[rows,]
fr<-as.data.frame(table(sample.1$MannerOfDeath))
fr<-cbind(fr,c("Non Specified","Accident","Homicide","Pending Investigation","Natural"))
colnames(fr)<-c("Manner of Death","Frequency","Description")
fr


#Systematic Sampling
N<-nrow(DeathRecords)
n<-1000
k<-ceiling(N/n)
r<-sample(k,1)
s<-seq(r,by=k,length=n)

sample.systematic<-DeathRecords[s!=0,]
fr<-as.data.frame(table(sample.systematic$MannerOfDeath))
fr<-cbind(fr,c("Non Specified","Accident","Suicide","Homicide","Pending Investigation","Could not determine","Natural"))
colnames(fr)<-c("Manner of Death","Frequency","Description")
fr


#Confidence Interval
conf<-c(80,90)
alpha<-1-conf/100

sd.sample.means<-sd(DeathRecords$Age)/sqrt(nrow(DeathRecords))
xbar<-mean(DeathRecords$Age)

for(i in alpha)
{
  str<-sprintf("%.2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",100*(1-i),i,xbar-qnorm(1-i/2)*sd.sample.means,xbar+qnorm(1-i/2)*sd.sample.means)
  cat(str,"\n")
}
  
#Systematic Sampling Confidence Interval
conf<-c(80,90)
alpha<-1-conf/100

sd.sample.means<-sd(sample.systematic$Age)/sqrt(nrow(sample.systematic))
xbar<-mean(sample.systematic$Age)

for(i in alpha)
{
  str<-sprintf("%.2d%% Conf Level (alpha = %.2f), CI = %.2f - %.2f",100*(1-i),i,xbar-qnorm(1-i/2)*sd.sample.means,xbar+qnorm(1-i/2)*sd.sample.means)
  cat(str,"\n")
}


