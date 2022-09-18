# load library
library(tidyverse)
#load data
titanic_train<-read.csv("train.csv",sep=",")
str(titanic_train)
#check Na
check_na <- function(df){
  for(i in colnames(df)){
    print(paste(i," ",sum(is.na(df[i]))))
  }
}
check_na(titanic_train)
# only age var 177 missing values
View(titanic_train)
# If we look at the View, there are another variable with blank value that doesn't count
# as missing
# convert all blank value as NA
# copy dataset
titan_df <- titanic_train %>% mutate_all(list(~na_if(.,"")))
check_na(titan_df)
# Fow now, we ignore all NA's value,and focus to explore each variable as it is
# The reason is, even we drop all missing value, we still have enough sample size (n>30) statistically

str(titan_df)
# PassengerId---> doesn't weight any amount of information, we can ignore it
# Survived, this is the goal variable. 
# Pclass---> Variable of cabin class, consists of 3 class, start from class 1 as the most
# luxurious
# plot for Pclass
# convert Survived and Pclass as Factor
titan_df$Survived <- factor(titan_df$Survived)
titan_df$Pclass<- factor(titan_df$Pclass)
titan_df %>% ggplot(aes(Pclass, fill=Survived))+
  geom_bar()+
  xlab("Cabin Class")+
  ylab("Total")+
  ggtitle("Titanic Survival Status")+
  geom_text(aes(label=scales::percent(prop.table(stat(count)))),stat="count", 
            position = position_stack(), vjust=1.5)+
  scale_fill_discrete(name="Survival Status",labels=c("Not Survived","Survived"))
# Apply chi-square test to determine if two variables (Survived and Pclass)
# related to each other
titan_df_sur_cab <- titan_df %>% select(Survived,Pclass)
titan_df_sur_cab<- table(titan_df_sur_cab$Survived,titan_df_sur_cab$Pclass)
chisq.test(titan_df_sur_cab, correct = FALSE)
fisher.test(titan_df_sur_cab)

# Analysis on Name Variable
summary(titan_df$Name)
head(titan_df$Name)  
# try to extract the title
titan_df <-titan_df %>% mutate(title=str_extract(titan_df$Name,'[a-zA-Z]+(?=\\.)'))
titan_df %>% filter(title=="Rev")
# Total Passenger by Title
titan_df %>% ggplot(aes(x=fct_infreq(title))) + 
  geom_bar(fill="green")+
  xlab("Passenger's Title")+
  ylab("Total Passengers by Title")+
  ggtitle("Total Titanic Passengers by Title")+
  geom_text(aes(label=..count..), stat="count")+
  theme(axis.text.x = element_text(angle=45, hjust=1))
# Survavibility by title
titan_df %>% filter(title=="Mr"|title=="Miss"|title=="Mrs"|title=="Master")%>% 
  ggplot(aes(x=fct_infreq(title), fill=as.factor(Survived))) + 
  geom_bar(position = position_dodge())+
  xlab("Passenger's Title")+
  ylab("Total Passengers by Title")+
  ggtitle("Passenger Survivability by Title")+
  geom_text(aes(label=..count..), stat="count",position = position_jitterdodge(),vjust=0.5)+
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "top")+
  labs(fill="Survival Status")+
  scale_fill_hue(labels=c("Not Survived","Survived"))
  
# Analysis by Passenger's Surname
titan_df_surname_anz<- titan_df %>% mutate(surname=str_extract(titan_df$Name,"[a-zA-Z]+(?=,)"))
head(titan_df_surname_anz)
titan_df_surname_anz %>% arrange(surname) %>% 
  select(Survived,Pclass,title,Parch,SibSp,Age,Cabin,surname)
# Analisa menggunakan surname kelihatannya tidak memberikan pola yang jelas 
# baik untuk memprediksi kabin atau hubungan keluarga

# Proceed to analyze sex variable
str(titan_df)
titan_df$Sex<- as.factor(titan_df$Sex)
titan_df %>% ggplot(aes(x=Sex))+
  geom_bar(fill="magenta")+
  ggtitle("Total Passengers by Sex")+
  ylab("Total Passenger")+
  geom_text(aes(label=..count..),stat="count",vjust=1.5)
# Survivability by Sex
titan_df %>% ggplot(aes(x=Sex, fill=as.factor(Survived)))+
  geom_bar()+
  ggtitle("Total Passengers by Sex")+
  ylab("Total Passenger")+
  geom_text(aes(label=..count..),stat="count",vjust=1.5,position=position_stack())+
  theme(legend.position = "top")+
  labs(fill="Survival Status")+
  scale_fill_hue(labels=c("Not Survived","Surived"))+
  ggtitle("Survivability by Sex")
# There strong indication that Sex has relation with survivability.
# Do Statistical test
titan_df_sex <- titan_df %>% select(Sex,Survived)
titan_df_sex<- table(titan_df_sex)
chisq.test(titan_df_sex)
fisher.test(titan_df_sex)
# both test prove that sex has correlantion with survivability

#Proceed to analyze Age variable
titan_df %>% ggplot(aes(x=Age))+
  geom_histogram(binwidth = 4)
mean(titan_df$Age,na.rm=TRUE)
# Rigth-skewed distribution
# For now, let's just drop all NA value from Age
# copy dataset
med <- median(titand_df_age_na$Age)
titan_df_age_na<-titan_df %>% filter(!is.na(Age))
# calculate stat summary for boxplot
get_box_stat<- function(y,upper_limit = max(titan_df_age_na$Age)*1.15){
  return(data.frame(
    y=0.95 * upper_limit,
    label= paste(
      "Median = ",round(median(y),2),"\n",
      "Mean = ", round(mean(y),2),"\n"
    )
  ))
}
titan_df_age_na %>% ggplot(aes(x=Survived, y=Age))+
  geom_boxplot(fill=c("red","blue"))+
  ggtitle("Age Distribution of Survivability Status")+
  xlab("Survivability Status")+
  ylab("Age")+
  stat_summary(fun.data=get_box_stat, geom="text", vjust=1)+
  scale_x_discrete(labels=c("Not Survived", "Survived"))

# t-test 
t.test(Age~Survived, data=titan_df_age_na, var.equal=FALSE)
# wilcox test non parametric
res_age<-wilcox.test(Age~Survived, data=titan_df_age_na)
res_age
# the result of non parametric wilcox show us that there age distribution of survival is identical

# Proceed to analyze Sibling and Spouse
titan_df %>% group_by(SibSp) %>% summarise(count=n())
titan_df %>% ggplot(aes(x=as.factor(SibSp),fill=SibSp)) +
  geom_bar()+
  ylab("Total Passengers")+
  xlab("Number of Sibling or Spouse Travelled Together")+
  geom_text(aes(label=..count..),stat="count",vjust=-0.2)+
  theme(legend.position = "none")
# group SibSp into categorical var consist of "with sibsp" and "without sibsp"  
# copy data
titan_df_sibsp <- titan_df %>% mutate(sibsp_count = case_when(SibSp==0~"Without sibsp",
                                                        TRUE~"With sibsp"))
titan_df_sibsp %>% ggplot(aes(x=as.factor(sibsp_count), fill=Survived))+
  geom_bar()+
  ylab("Survivability")+
  xlab("Number of Sibling or Spouse")+
  geom_text(aes(label=..count..),stat="count",position=position_stack(),vjust=1)+
  theme(legend.position = "top")+
  labs(fill="Survivability Status")+
  scale_fill_hue(labels=c("Not Survived", "Survived"))+
  ggtitle("Survivability of Passenger With / Without Sibling-Spouse")

# statistic test (chisquare and fisher)
# convert data into contigency table 
titan_df_sibsp$sibsp_count <- as.factor(titan_df_sibsp$sibsp_count)
titan_df_sibsp_tab<- titan_df_sibsp %>% select(sibsp_count, Survived)
titan_df_sibsp_tab<- table(titan_df_sibsp_tab)
sibsp_chis_res<-chisq.test(titan_df_sibsp_tab)
sibsp_fisher_res <-fisher.test(titan_df_sibsp_tab)
# it turns out sibsp has correlation with survivability of its passangers

# Proceed to analyze Parch Variable
str(titan_df)
summary(titan_df$Parch)
titan_df %>% ggplot(aes(x=as.factor(Parch)))+
  geom_bar()+
  xlab("Number of Parent or Children")+
  ylab("Total Passenger")+
  ggtitle("Total Passenger by number of parent or children bring along")
# same as SibSp, there are huge difference between people who bring parch and not
# We can grouping the people with parent and children together
titan_df_parch <- titan_df %>% 
  mutate(parch_status = case_when(Parch==0~"No Parch",
                                  TRUE ~ "With Parch"))
head(titan_df_parch)
# Visualize
titan_df_parch %>% ggplot(aes(parch_status, fill=Survived))+
  geom_bar()+
  xlab("parch status")+
  ylab("Total Passenger")+
  ggtitle("Survivability by Parch Status")+
  geom_text(aes(label=..count..), stat="count", vjust=1.5, position=position_stack())+
  theme(legend.position = "top")+
  labs(fill="Survival Status")+
  scale_fill_hue(labels=c("Not Survived","Survived"))
# Statistic Test
str(titan_df_parch)
titan_df_parch$parch_status<- as.factor(titan_df_parch$parch_status)
# Convert to contingency table
titan_df_parch_tab <- titan_df_parch %>% select(parch_status,Survived)
titan_df_parch_tab<-table(titan_df_parch_tab)
titan_parch_chires <- chisq.test(titan_df_parch_tab)
titan_parch_fisres <- fisher.test(titan_df_parch_tab)
print(titan_parch_fisres)

# Proceed to analyze Fare
summary(titan_df$Fare)
titan_df %>% filter(Fare==0)
# There are few Passengers whom posses ticket for "free". It's doesn't seem right
titan_df %>% ggplot(aes(Fare))+
  geom_histogram(binwidth = 10)
titan_df %>% ggplot(aes(Survived,Fare))+
  geom_boxplot(aes(fill=Survived))+
  stat_summary(fun.data=get_box_stat_2, geom="text", vjust=1)+
  ggtitle("Survivability Based on Ticket Fare")+
  scale_fill_hue(labels=c("Not Survived","Survived"))
# analyze without outliers
titan_df_fare_no_outlier<- titan_df %>% select(Fare,Survived)
q_fare<- quantile(titan_df_fare_no_outlier$Fare)
iqr_fare<- IQR(titan_df_fare_no_outlier$Fare)
titan_df_fare_no_outlier<- titan_df_fare_no_outlier %>% 
  filter(Fare > (q_fare["25%"] - 1.5*iqr_fare) & Fare < (q_fare["75%"] + 1.5*iqr_fare))
summary(titan_df_fare_no_outlier)
#Visualize without oulier
titan_df_fare_no_outlier %>% ggplot(aes(Fare))+
  geom_histogram(binwidth = 4)
titan_df_fare_no_outlier %>% ggplot(aes(Survived,Fare))+
  geom_boxplot(aes(fill=Survived))+
  stat_summary(fun.data=get_box_stat_2, geom="text", vjust=1)+
  ggtitle("Survivability Based on Ticket Fare")+
  scale_fill_hue(labels=c("Not Survived","Survived"))

# box stat
get_box_stat_2<- function(y,upper_limit = max(titan_df_fare_no_outlier$Fare)*1.15){
  return(data.frame(
    y=0.95 * upper_limit,
    label= paste(
      "Median = ",round(median(y),2),"\n",
      "Mean = ", round(mean(y),2),"\n"
    )
  ))
}

# Statistic test
fare_wilcox_res<-wilcox.test(Fare~Survived, data=titan_df_fare_no_outlier)
titan_df_fare_no_outlier %>% ggplot(aes(Fare))+
  geom_histogram(binwidth = 5)+
  ggtitle("Fare Distribution")


#Proceed to analyze Cabin
summary(titan_df$Cabin)
sum(is.na(titan_df$Cabin))
head(titan_df$Cabin)
titan_df %>% filter(!is.na(Cabin)) %>% 
  mutate(cabin_suf = str_extract_all(Cabin,"[A-Z]"), cabin_num=str_extract_all(Cabin,"\\d+")) %>% 
  filter(nchar(cabin_suf)>3) %>% 
  select(Sex,Age,SibSp,Parch,Cabin,cabin_suf,cabin_num,Survived)
# Take only first Cabin Suffix
titan_df_cabin<-titan_df %>% filter(!is.na(Cabin)) %>% 
  mutate(cabin_suf=str_extract(Cabin,"^[A-Z]"))
titan_df_cabin$cabin_suf<-as.factor(titan_df_cabin$cabin_suf)
#Visualize
titan_df_cabin %>% ggplot(aes(cabin_suf, fill=Survived))+
  geom_bar()+
  xlab("Cabin Suffix")+
  ylab("Total Passengers")+
  geom_text(aes(label=..count..), stat="count", vjust=-0.1, position = position_stack())+
  ggtitle("Survivability by Cabin / Deck (omit NA)")+
  labs(fill="Survival Status")+
  theme(legend.position = "bottom")+
  scale_fill_hue(labels=c("Not Surived","Survived"))
# statisticel test
titan_df_cabin_tab<- titan_df_cabin %>% 
  select(cabin_suf,Survived)
titan_df_cabin_tab <- table(titan_df_cabin_tab)
titan_df_cabin_tab
cabin_chires <- chisq.test(titan_df_cabin_tab)
# Based on the result, we fail to reject null hypothesis, so it could be said that
# Suvived and Cabin/Deck is indenpendent


# Proceed to analyze Embarked
sum(is.na(titan_df$Embarked))
titan_df %>% filter(is.na(Embarked))
titan_df %>% filter(str_detect(Name,"Stone"))
# Data sourcing from https://www.encyclopedia-titanica.org/cabins.html
titan_df$Embarked[titan_df$Name=="Stone, Mrs. George Nelson (Martha Evelyn)"]<- "S"
titan_df$Embarked[titan_df$Name=="Icard, Miss. Amelie"]<- "S"
#visualize
titan_df$Embarked <- as.factor(titan_df$Embarked)
str(titan_df$Embarked)
titan_df %>% ggplot(aes(fct_infreq(Embarked)))+
  geom_bar()+
  xlab("Departure City")+
  ylab("Total Passenger")+
  geom_text(aes(label=..count..),stat="count", vjust=-.1, position=position_stack())+
  ggtitle("Survivability by Departure City")+
  scale_fill_hue(labels=c("Not Survived","Survived"))+
  theme(legend.position = "top")+
  labs(fill="Survivability")+
  facet_grid(cols=vars(Pclass))
titan_df_embark<-titan_df %>% dplyr::select(Embarked, Survived)
titan_df_embark_tab<-table(titan_df_embark)
titan_df_embark_tab
titan_embark_chires <-chisq.test(titan_df_embark_tab)
titan_embark_chires$expected
titan_embark_chires
# Exploration finished
check_na(titan_df)

# all significant variable : Pclass, Sex, SibSp,Parch, Fare, and Embarked
# Error in data
# We know that not all fare is the individual ticket fare, so we need to transform it
# First, let's try to find out the fare for passenger who travel alone
titan_fare_pc1 <- titan_df %>% select(Pclass, SibSp,Parch, Fare, Embarked) %>% 
  filter(Pclass==1 & SibSp==0 & Parch==0)
titan_fare_pc1 %>% group_by(Embarked) %>% 
  summarise(Mean=mean(Fare),Median=median(Fare), Max=max(Fare),Min=min(Fare))


titan_fare_pc1 %>% ggplot(aes(x=Embarked, y=newFare))+
  geom_boxplot(fill=c("red","blue"))+
  xlab("Departure City")+
  ylab("Fare")+
  stat_summary(fun.data=get_box_stat_fare, geom="text", vjust=1)

# Calculate mean and median by class and departure city
titan_fare_pc1 %>% group_by(Embarked, Pclass) %>% 
  summarise(mean=mean(Fare),median=median(Fare))
mean_c_class1 <- 93.5
mean_s_class1 <- 45.7
titan_fare_pc1 <- titan_fare_pc1 %>% 
  mutate(newFare = case_when(Embarked=="C" & Fare==0~mean_c_class1,
                             Embarked=="S" & Fare==0~mean_s_class1,
                             TRUE~Fare))
#change the incorrent newFare value with minimum fare  
which(titan_fare_pc1$newFare==5) # find out the indices
titan_fare_pc1$newFare[107] <-26 # Change the value
titan_fare_pc1
# Kelihatannya ada kesalahan nilai tiket. Ada satu tiket kelas 1 dari Southampton bernilai 5 pound
get_box_stat_fare<- function(y,upper_limit = max(titan_fare_pc1$newFare)*1.15){
  return(data.frame(
    y=0.95 * upper_limit,
    label= paste(
      "Median = ",round(median(y),2),"\n",
      "Mean = ", round(mean(y),2),"\n",
      "Max = ", round(max(y),2),"\n",
      "Min = ", round(min(y),2),"\n",
      "Q1 = ", round(quantile(y,prob=c(.25)),2),"\n",
      "Q3 = ", round(quantile(y,prob=c(.75)),2),"\n"
    )
  ))
}

titan_fare_pc1_sibpar <- titan_df %>% select(Pclass, SibSp,Parch, Fare, Embarked) %>% 
  filter(Pclass==1 & SibSp!=0 & Parch!=0)
titan_fare_pc1_sibpar %>% mutate(total_group = SibSp+Parch+1, 
                                 fare_per = round((Fare/total_group),2))

# Class 2
# Single passenger
titan_fare_pc2 <- titan_df %>% select(Pclass, SibSp,Parch, Fare, Embarked) %>% 
  filter(Pclass==2 & SibSp==0 & Parch==0)
titan_fare_pc2 %>% group_by(Embarked, Pclass) %>% 
  summarise(mean=mean(Fare),median=median(Fare))
mean_c_class2<-13.8
mean_q_class2<-12.4
mean_s_class2<-14.1
titan_fare_pc2 <- titan_fare_pc2 %>% 
  mutate(newFare=case_when(Embarked=="C" & Fare==0~mean_c_class2,
                           Embarked=="Q" & Fare==0~mean_q_class2,
                           Embarked=="S" & Fare==0~mean_s_class2,
                           TRUE~Fare))
titan_fare_pc2_sibpar <- titan_df %>% select(Pclass, SibSp,Parch, Fare, Embarked) %>% 
  filter(Pclass==2 & SibSp!=0 & Parch!=0)
titan_fare_pc2_sibpar %>% mutate(total_group = SibSp+Parch+1, 
                                 fare_per = round((Fare/total_group),2))
titan_fare_pc2_sibpar

# Engineering Fare per Passenger by their ticket number

titan_df %>% select(Ticket, Fare) %>% arrange(Ticket)
titan_df %>% filter(Fare==0)
# ada beberapa penumpang dengan data harga tiket 0 pound. Kemungkinan besar ini adalah kesalahan pengimputan data
# kita akan mengisi nilai ini dengan rata-rata berdasarkan kota dan kelas  
fare_summary<-titan_df %>% filter(SibSp==0 & Parch==0) %>% group_by(Embarked,Pclass) %>% 
  summarise(mean=round(mean(Fare),2),median=round(median(Fare),2))
fare_summary$median[which(fare_summary$Embarked=="C" & fare_summary$Pclass==1)]
fare_summary
titan_df <- titan_df %>% 
  mutate(newFare = case_when(
    Fare==0 & Embarked=="C" & Pclass==1~fare_summary$median[which(fare_summary$Embarked=="C" & fare_summary$Pclass==1)],
    Fare==0 & Embarked=="C" & Pclass==2~fare_summary$median[which(fare_summary$Embarked=="C" & fare_summary$Pclass==2)],
    Fare==0 & Embarked=="C" & Pclass==3~fare_summary$median[which(fare_summary$Embarked=="C" & fare_summary$Pclass==3)],
    Fare==0 & Embarked=="Q" & Pclass==2~fare_summary$median[which(fare_summary$Embarked=="Q" & fare_summary$Pclass==2)],
    Fare==0 & Embarked=="Q" & Pclass==3~fare_summary$median[which(fare_summary$Embarked=="Q" & fare_summary$Pclass==3)],
    Fare==0 & Embarked=="S" & Pclass==1~fare_summary$median[which(fare_summary$Embarked=="S" & fare_summary$Pclass==1)],
    Fare==0 & Embarked=="S" & Pclass==2~fare_summary$median[which(fare_summary$Embarked=="S" & fare_summary$Pclass==2)],
    Fare==0 & Embarked=="S" & Pclass==3~fare_summary$median[which(fare_summary$Embarked=="S" & fare_summary$Pclass==3)],
    TRUE~Fare
  ))
View(titan_df)
# After review the newFare, i recognize that ticket LINE and 239853 is ticket for multiple passengers
# So, assume that thet bought this together, it would be innaccurate to give Fare value based on one Ticket
# Here i change the value
titan_df$newFare[which(titan_df$Ticket=="LINE")]<- titan_df$newFare[which(titan_df$Ticket=="LINE")] * 3 
titan_df$newFare[which(titan_df$Ticket=="LINE")]
titan_df$newFare[which(titan_df$Ticket=="239853")] <- titan_df$newFare[which(titan_df$Ticket=="239853")] * 3
titan_df$newFare[which(titan_df$Ticket=="239853")]

#Now, we will engineer fare per passenger
str(titan_df_fare)
titan_df_fare <- titan_df %>% group_by(Ticket) %>% 
  summarise(total = n(), fareVal = max(newFare), farePerPass = round((fareVal/total),2))
View(titan_df_fare)
titan_df_new<-inner_join(x=titan_df,y=titan_df_fare, by='Ticket')
summary(titan_df_new$farePerPass)
# Check if it still significant
titan_df_new %>% ggplot(aes(Survived, farePerPass))+
  geom_boxplot()
wilcox.test(farePerPass~Survived, data=titan_df_new)
titan_df_new

#identified newFare Outlier
q_fare<- quantile(titan_df_new$farePerPass)
iqr_fare<- IQR(titan_df_new$farePerPass)
titan_df_new %>% ggplot(aes(farePerPass))+
  geom_histogram()
titan_df_new %>% 
  select(Survived,Pclass,Embarked,Fare,farePerPass, SibSp,Parch) %>% 
  filter(farePerPass<quantile(farePerPass,0.025)|farePerPass>quantile(farePerPass,0.975))
quantile(titan_df_new$farePerPass,0.025)
titan_df_new %>% filter(farePerPass > (q_fare["25%"] - 1.5*iqr_fare) & farePerPass < (q_fare["75%"] + 1.5*iqr_fare)) %>% 
  ggplot(aes(Pclass,farePerPass, fill=Survived))+
  geom_boxplot()
q_fare
iqr_fare
titan_df_new$newFare[which(titan_df_new$Name=="Carlsson, Mr. Frans Olof")] <- 16
titan_df_new$farePerPass[which(titan_df_new$Name=="Carlsson, Mr. Frans Olof")] <- 16/1
titan_df_new$farePerPass[which(titan_df_new$Name=="Carlsson, Mr. Frans Olof")]
titan_df_new %>% group_by(Pclass) %>% 
  summarise(median=median(farePerPass), max=max(farePerPass),min=min(farePerPass))

titan_df_new %>% ggplot(aes(x=Embarked,y=Survived, fill=farePerPass))+
                          geom_tile()+
  scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
  coord_fixed()
str(titan_df_new)

# Mutate new attributes compaCount <-- categorical var indicate whether the passanger travel alone or bring companion. Consist of 4 group : alone, onlySibSp, onlyParch, both
titan_df_new <- titan_df_new %>% 
  mutate(compaCount = case_when(
    SibSp == 0 & Parch==0 ~ "alone",
    SibSp > 0 & Parch==0 ~ "onlySibSp",
    SibSp == 0 & Parch > 0 ~ "onlyParch",
    SibSp > 0 & Parch > 0 ~ "both"
  ))
titan_df_new$compaCount <- as.factor(titan_df_new$compaCount)
summary(titan_df_new$compaCount)
titan_compa_tab <- titan_df_new %>% dplyr::select(compaCount,Survived) 
titan_compa_tab <- table(titan_compa_tab)
titan_compa_tab
chisq.test(titan_compa_tab)
# Model

# load test data
test_titan <- read.csv("test.csv",sep=",")
str(test_titan)
View(test_titan)
check_na(test_titan)
titan_test_df <- test_titan %>% mutate_all(list(~na_if(.,"")))
titan_test_df$Pclass <- as.factor(titan_test_df$Pclass)
titan_test_df$Sex <- as.factor(titan_test_df$Sex)
titan_test_df <- titan_test_df %>% 
  mutate(compaCount=case_when(
    SibSp == 0 & Parch==0 ~ "alone",
    SibSp > 0 & Parch==0 ~ "onlySibSp",
    SibSp == 0 & Parch > 0 ~ "onlyParch",
    SibSp > 0 & Parch > 0 ~ "both"
  ))
str(titan_test_df)
titan_test_df$Embarked <- as.factor(titan_test_df$Embarked)
titan_test_df$Fare[which(titan_test_df$Ticket=="3701")]<- 7.9
titan_test_df$Fare[which(titan_test_df$Ticket=="112051")] <- 30
titan_test_df$Fare[which(titan_test_df$Ticket=="112058")] <- 30*3
titan_test_df[which(titan_test_df$Fare==0),]
str(titan_test_df$Ticket)
test_titan_fare <- titan_test_df %>% group_by(Ticket) %>% 
  summarise(total = n(), fareVal = max(Fare), farePerPass = round((fareVal/total),2))
str(titan_test_df)

titan_test_new<-inner_join(x=titan_test_df,y=test_titan_fare, by='Ticket')
check_na(titan_test_new)
summary(test_df_fare)
# Model Evaluation
# There are 5 models that we will evaluate
# Linear Discriminant Analysis (LDA)
# Classification and Regression Trees (CART)
# Support Vector Machines
# Random Forest (RF)
# Logistic Regression (LR)



# Linear Discriminant Analysis
# Select only the significant variables
str(titan_df_new_train)
titan_df_new_train <- titan_df_new %>% 
  dplyr::select(Pclass, Sex, compaCount, Embarked, farePerPass, Survived)
# load library MASS
library(MASS)
train_fit_model<- lda(Survived~., data=titan_df_new_train)
train_fit_model
plot(train_fit_model)
lda.train.pred <- predict(train_fit_model, titan_df_new_train)$class
lda.train.pred
ldaTitanTrainPred <- table(predicted = lda.train.pred, actual=titan_df_new_train$Survived)
ldaTitanTrainPred
sum(diag(ldaTitanTrainPred))/sum(ldaTitanTrainPred)
# lda against test data
str(titan_test_df_new)
titan_test_df_new <- titan_test_new %>% 
  dplyr::select(Pclass,Sex,compaCount, Embarked, farePerPass)
lda.test.pred <- predict(train_fit_model, titan_test_df_new)$class
lda.test.pred
ldaModelTrain <- data.frame(PassengerId=titanic_train$PassengerId, Survived=lda.train.pred)
ldaModelTrain
ldaModelTest <- data.frame(PassengerId = test_titan$PassengerId,Survived=lda.test.pred)
ldaModelTest
# Write CSV for submission
write.csv(ldaModelTest,"ldaModelSubmission.csv", row.names = FALSE, quote = FALSE)

# Evaluating lda model
install.packages("caret")
library(caret)
# evaluating lda model against training data set
conf <- table(list(predicted = lda.train.pred, observed=titan_df_new_train$Survived))
conf
# Confussion Matrix
confusionMatrix(conf)
# Evaluate data with cross-validation
train_fit_model.2<- lda(Survived~., data=titan_df_new_train, CV=TRUE)
conf.2 <- table(list(predicted=train_fit_model.2$class, observed = titan_df_new_train$Survived))
confusionMatrix(conf.2)
# Scored 78.23% accuracy


# Classification and Regression Trees (CART)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
str(titan_df_new_train)
fit.model.cart <- rpart(Survived ~ Pclass+Sex+compaCount+Embarked+farePerPass, 
                        data=titan_df_new_train, method="class")
printcp(fit.model.cart)
plotcp(fit.model.cart)
print(fit.model.cart)
rsq.rpart(fit.model.cart)
summary(fit.model.cart)
rpart.plot(fit.model.cart, uniform = TRUE)
text(fit.model.cart, use.n = TRUE, all=TRUE,pretty = 0)
post(fit.model.cart)
# Model Accuracy
treeTitanPred <- predict(fit.model.cart, titan_df_new_train, type="class")
treeTitanPred
mean(treeTitanPred == titan_df_new_train$Survived)

# pruning
# Choose the lowest xerror
bestcp <- fit.model.cart$cptable[which.min(fit.model.cart$cptable[,"xerror"]),"CP"]
bestcp
fit.model.cart.2 <- prune(fit.model.cart, cp=bestcp)
rpart.plot(fit.model.cart.2)
treeTitanpred.2 <- predict(fit.model.cart.2, titan_df_new_train, type="class") 
treeTitanpred.2
confusionMatrix(treeTitanpred.2,titan_df_new_train$Survived)

# test model
treeTitanpred.test <- predict(fit.model.cart.2,titan_test_df_new, type="class")
treeTitanpred.test
summary(treeTitanpred.test)
treeTitanTest.pred <- data.frame(PassengerId = test_titan$PassengerId, Survived = treeTitanpred.test)
treeTitanTest.pred
write.csv(ldaModelTest,"ldaModelSubmission.csv", row.names = FALSE, quote = FALSE)

# Logistic Regression
install.packages("caTools")
install.packages("ROCR")
library(caTools)
library(ROCR)
# Model fitting
titan.logit.train <- glm(Survived~., data=titan_df_new_train, family = "binomial")
titan.logit.train
summary(titan.logit.train)
# test against training data
titan.train.prob <- predict(titan.logit.train, titan_df_new_train, type="response")
titan.train.prob
threshold <- 0.5
titan.train.pred <- as.numeric(titan.train.prob >= threshold)
confusionMatrix(as.factor(titan.train.pred), titan_df_new_train$Survived)
# test against test data
titan.test.prop <- predict(titan.logit.train, titan_test_df_new, type="response")
titan.test.pred <- as.numeric(titan.test.prop >= threshold)
titan.test.pred
titanLogitTestPred <- data.frame(PassengerId = test_titan$PassengerId, Survived=titan.test.pred)
titanLogitTestPred
write.csv(titanLogitTestPred, "titanLogitPredSubmission.csv", row.names = FALSE, quote = FALSE)
titan_test_df_new$Survived <- titan.test.pred
titan_test_df$Survived <- titan.test.pred
str(titan_test_df)
# try to run model  against only significant variable
titan.logit.fit.2 <- glm(Survived~Pclass+Sex+Embarked, data=titan_df_new_train, family="binomial")
summary(titan.logit.fit.2)
titan.train.prob.2 <- predict(titan.logit.fit.2, titan_df_new_train, type="response")
titan.train.pred.2 <- as.numeric(titan.train.prob.2 >= threshold)
titan.train.pred.2
confusionMatrix(as.factor(titan.train.pred.2), titan_df_new_train$Survived)
# Setelah pengurangan attribut, accuracy model justru menurun

# Evaluate model using ROC AUC Curve
ROCPred <- prediction(titan.train.pred, titan_df_new_train$Survived)
ROCPer <- performance(ROCPred, measure ="tpr", x.measure = "fpr")
plot(ROCPer, colorize = TRUE, main="TITANIC PREDICTION ROC CURVE")
auc <- performance(ROCPred, measure="auc")
auc <- auc@y.values[[1]]
auc <- round(auc,4)
legend(.6,.4,auc,title="AUC",cex=1)


# Random Forest
# Model Fitting
install.packages("randomForest")
library(randomForest)
titan.rf.fit <- randomForest(Survived~., data=titan_df_new_train, mtry=1,importance=TRUE)
titan.rf.fit
importance(titan.rf.fit)
varImpPlot(titan.rf.fit)
plot(titan.rf.fit)
#test performance against training data
titan.rf.train.pred <- predict(titan.rf.fit, titan_df_new_train)
confusionMatrix(titan.rf.train.pred, titan_df_new_train$Survived)
# Accuracy 82.23%
# Performance in test data
titan.rf.test.pred <- predict(titan.rf.fit, titan_test_df_new)
titan_test_df_new$compaCount <- as.factor(titan_test_df_new$compaCount)
titan.rf.test.pred
titanRandomForPred <- data.frame(PassengerId=test_titan$PassengerId, Survived = titan.rf.test.pred)
titanRandomForPred
write.csv(titanRandomForPred,"titanRandomSubmission.csv", row.names = FALSE, quote=FALSE)


# Support Vector Machine
library(e1071)
titan.svm.fit <- svm(Survived~., data=titan_df_new_train,
                     method = "C-classification", kernal="linear",
                     gamma=2, cost=10)
summary(titan.svm.fit)
titan.svm.fit$SV
titan.svm.fit$rho
# test against training data
titan.svm.train.pred <- predict(titan.svm.fit, titan_df_new_train)
titan.svm.train.pred
confusionMatrix(titan.svm.train.pred, titan_df_new_train$Survived)
# test against test data
titan.svm.test.pred <- predict(titan.svm.fit, titan_test_df_new)
titan.svm.test.pred
titanSVMPred <- data.frame(PassengerId = test_titan$PassengerId, Survived = titan.svm.test.pred)
titanSVMPred
str(titan_df_new_train)
write.csv(titanSVMPred, "titanSVMSubmission.csv",row.names = FALSE, quote=FALSE)

# C5.0 Decision Tree
install.packages("C50")
library(C50)
titan.c50.fit <- C5.0(titan_df_new_train[-6], titan_df_new_train$Survived)
summary(titan.c50.fit)
plot(titan.c50.fit)
# test against training data
titan.c50.train.pred <- predict(titan.c50.fit, titan_df_new_train)
confusionMatrix(titan.c50.train.pred, titan_df_new_train$Survived)
# predict the test data
titan.c50.test.pred <- predict(titan.c50.fit, titan_test_df_new)
titan.c50.test.pred
titanC50Pred <- data.frame(PassengerId = test_titan$PassengerId, Survived = titan.c50.test.pred)
titanC50Pred
write.csv(titanC50Pred,"titanC50Submission.csv",row.names = FALSE,quote=FALSE)

# gender_submission
gender_submission <- read.csv(file="gender_submission.csv", sep=",")
gender_submission


