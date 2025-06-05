getwd()
setwd()
list.files()
#Loading the data base
load("titanic_train.RDATA")
head(titanic.train)
sex = titanic.train$Sex

#Here I intend to find out the distribution of passengers in relation with their age 
library(ggplot2)
mean_age <- mean(titanic.train$Age)
print(mean_age)
#mean=29.1471
median_age <- median(titanic.train$Age)
print(median_age)
#median=28

ggplot(titanic.train, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") + 
  geom_density(color = "blue", size = 1) +  geom_vline(aes(xintercept = mean_age), color = "red", linetype = "dashed", size = 1, label="Mean") +  
  geom_vline(aes(xintercept = median_age), color = "green", linetype = "dashed", size = 1, label="Median")+
  labs(title = "Distribution of Age with Mean and Median", x = "Age", y = "Density") +
  theme_minimal() 
#From the graph we can infer that most passengers were in their 20s and 30s and there is a peak in the late 20s.
#We can observe that the number of passengers above 40 has dropped and especially after 50.
#The graph is also slightly right skewed
#This graph is a good representation of the passenger distribution in relation with their age. It gives us an overall view
#about the age distribution.

#Q1.Does gender influence the survival of the passengers?
##Finding out how many people of each sex were aboard
table(titanic.train$Sex)
#female= 243 , male = 425

#Finding out how many of female passengers survived

f_survivors=subset(titanic.train, sex=="female" & Survived==1)
nrow(f_survivors)
#Total 181 out of 243 female passengers survived

m_survivors=subset(titanic.train, sex=="male" & Survived==1)
nrow(m_survivors)
#75 out of 425 male passengers survived


library(ggplot2)
ggplot(data = titanic.train )+
  aes(x = Sex, fill = factor(Survived))+geom_bar()+
  labs(x = "Sex", fill= "Survived", y = "count", title = "Male v/s Female survival")
#This shows us the disparity of survival between both the sexes. Even though the number 
#of male passengers is a lot more than female passengers, the chances of survival is surprisingly
#really low comparing to the trend we see for women survival
#74.48% of the female passengers survived while only 17.65% of male passengers survived


#Q2 Does age play an important role in the survival of the passengers?
titanic.train$AgeGroup <- cut(titanic.train$Age,
                              breaks = c(-Inf,12,18,60, Inf),
                              labels = c("child","Teenager","Adult","Senior"))


ggplot()+aes(x=titanic.train$Survived, y=titanic.train$Age, color=titanic.train$AgeGroup)+
  geom_point()+
  scale_color_manual(values = c("child" = "pink","Teenager" = "skyblue","Adult"="lightgreen","Senior" ="darkgreen"))+
  labs(x="Survived",y = "age",title="Age v/s Survival")+theme_minimal()

#From this graphical representation I could infer that the mortality rate and survival rate are distributed evenly along 
#almost every age group except seniors. The graph represents that the number of adults who died is more than the 
#number of adults who survived.
#The number of adult passengers seems to be the highest of all the other passengers but it hasn't affected their survival rate

table(titanic.train$AgeGroup)
s_survivors=subset(titanic.train, AgeGroup=="Senior" & Survived==1)
nrow(s_survivors)
s_survivors=subset(titanic.train, AgeGroup=="Senior" & Survived==0)
nrow(s_survivors)
#Survived = 3
#Passed away = 15


t_survivors=subset(titanic.train, AgeGroup=="Teenager" & Survived==1)
nrow(t_survivors)
t_survivors=subset(titanic.train, AgeGroup=="Teenager" & Survived==0)
nrow(t_survivors)
#survived = 25
#Passed away= 32


c_survivors=subset(titanic.train, AgeGroup=="child" & Survived==1)
nrow(c_survivors)
c_survivors=subset(titanic.train, AgeGroup=="child" & Survived==0)
nrow(c_survivors)
#Survived = 32
#Passed away = 24
#Here the children who survived are slightly higher than the children who didn't

a_survivors=subset(titanic.train, AgeGroup=="Adult" & Survived==1)
nrow(a_survivors)
a_survivors=subset(titanic.train, AgeGroup=="Adult" & Survived==0)
nrow(a_survivors)
#survived = 196
#Passed away = 341
#The number of adults who didn't survive is a lot more than the number of adults who survived


#Does the gender of a person influence the survival of the passenger depending on their age?
filtered_data <- titanic.train %>% filter(AgeGroup == "Adult")
gender_survival_table <- table(filtered_data$Sex, filtered_data$Survived)
print(gender_survival_table)
#females dead=43 survived=140
#Male dead=298 survived=56
#High chances of survival if you are female 

filtered_data <- titanic.train %>% filter(AgeGroup == "child")
gender_survival_table <- table(filtered_data$Sex, filtered_data$Survived)
print(gender_survival_table)
#female dead=11 survived=15
#Male dead=13 survived=17
#It seems like children were more likely to survive


filtered_data <- titanic.train %>% filter(AgeGroup == "Senior")
gender_survival_table <- table(filtered_data$Sex, filtered_data$Survived)
print(gender_survival_table)
#All the senior passengers who survived are women. Out of 18 passengers only 3 survived. 15 senior men passed away.

filtered_data <- titanic.train %>% filter(AgeGroup == "Teenager")
gender_survival_table <- table(filtered_data$Sex, filtered_data$Survived)
print(gender_survival_table)
#Female dead = 8 survived= 23
#Male dead=24 survived=2

ggplot(titanic.train, aes(x = factor(Survived), fill = Sex)) +
  geom_bar(position = "dodge") +  
  facet_grid(AgeGroup ~ .) +      
  labs(x = "Survived", fill = "Gender", 
       y = "Count", title = "Gender Survival in Different Age Groups") +
  scale_fill_manual(values = c("lightblue", "pink")) +
  theme_classic(base_size = 15)
#This graph shows us the different age groups and the distribution of male and female passengers who survived and didn't survive


library(ggplot2)

ggplot(titanic.train, aes(x = factor(Survived), y = Age, color = AgeGroup)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +  
  scale_color_manual(values = c("child" = "pink", "Teenager" = "skyblue", "Adult" = "lightgreen", "Senior" = "darkgreen")) +
  labs(x = "Survived", y = "Age", title = "Age vs Survival by Age Group", color = "Age Group") +
  theme_minimal() 
library(dplyr)

#Here I am trying to establish the question whether the women who survived were accompanied by parents or childrem
female_with_family <- titanic.train %>% filter(Sex == "female" & (SibSp > 0 | Parch > 0))

print(female_with_family)

count(female_with_family)

female_with_family %>%
  group_by(AgeGroup) %>%
  summarize(count = n())
filter(titanic.train$sex=="female")

female_with_chil <- titanic.train %>% filter(Sex == "female" & ( Parch > 0))
print(female_with_chil)
female_with_chil %>%
  group_by(AgeGroup) %>%
  summarize(count = n())
num_survived <- sum(female_with_chil$Survived==1)
num_survived
#60 females who survived were accompanied by children or parents 


female_with_kids_or_parents <- subset(titanic.train, Sex == "female" & Parch > 0)

family_tickets <- female_with_kids_or_parents$Ticket

family_data <- subset(titanic.train, Ticket %in% family_tickets)

library(dplyr)

family_survival <- family_data %>%
  group_by(Ticket) %>%
  summarize(all_survived = all(Survived == 1), female_present = any(Sex == "female" & Parch > 0)) %>%
  filter(female_present & all_survived)

family_survival

num_families_all_survived <- nrow(family_survival)

num_families_all_survived
#37 people who accompanied these women also survived
#This shows us that women were considered important maybe because they took great part in giving care to their children
#and parents.

table(titanic.train$Embarked =="C" & titanic.train$Survival==1)
embarked <- subset(titanic.train,Embarked %in% c("C","S","D")) 
num<-sum(embarked$Survived==1)
num
embarked_c <- subset(titanic.train,Embarked %in% "C") 
num_c<-sum(embarked_c$Survived==1)
num_c
#76 people survived who embarked from Cherbourg

embarked_s <- subset(titanic.train,Embarked %in% "S") 
num_s<-sum(embarked_s$Survived==1)
num_s
#156 people who embarked from Southampton  survived

embarked_q <- subset(titanic.train,Embarked %in% "Q") 
num_q<-sum(embarked_q$Survived==1)
num_q
#24 people embarked from Queensland survived




embarked_CSD_survived <- subset(titanic.train, Embarked %in% c("C", "S", "D") & Survived == 1)

library(dplyr)

class_distribution <- embarked_CSD_survived %>%
  group_by(Embarked, Pclass) %>%
  summarize(count = n())


class_distribution


male_with_chil <- titanic.train %>% filter(Sex == "male" & ( Parch > 0))
print(male_with_chil)
male_with_chil %>%
  group_by(AgeGroup) %>%
  summarize(count = n())
num_survived <- sum(male_with_chil$Survived==1)
num_survived
#Checking whether there is any relation between males who survived and people accompanied
#It seems like 23 out of 75 males who survived were accompanied by parents or children
#That´s 30.6% of males who survived

class_by_embark <- table(titanic.train$Pclass, titanic.train$Embarked)
prop.table(class_by_embark, margin = 2)


ggplot(titanic.train, aes(x = Embarked, fill = as.factor(Pclass))) +
  geom_bar(position = "fill") +
  labs(x = "Embarkation Location", y = "Proportion", fill = "Pclass") +
  ggtitle("Class Distribution by Embarkation Location")
#This graph demonstrates that different classes were given embarkation priority in different harbours 
class_embark_counts <- table(titanic.train$Pclass, titanic.train$Embarked)
print(class_embark_counts)
        C   Q   S
1   0  72   2  93
2   0  15   3 120
3   0  45  52 266
#This could be mostly because of socio -economic reasons which existed back then.
#Most 3rd class passengers embarked from Southampton and Queensland.
#Historically during this time poor people immigrated to America for a better life so the significant number of 3rd class passengers could be due to that fact.
library(ggplot2)
library(dplyr)

class_embark_counts <- titanic.train %>%
  group_by(Pclass, Embarked) %>%
  summarise(Count = n()) %>%
  arrange(Embarked, Pclass)

print(class_embark_counts)

ggplot(class_embark_counts, aes(x = Embarked, y = Count, fill = as.factor(Pclass))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Embarkation Location", y = "Number of Passengers", fill = "Passenger Class") +
  ggtitle("Number of Passengers by Class and Embarkation Location")


third_class_passengers <- subset(titanic.train, Pclass == 3)


accompanied_third_class <- subset(third_class_passengers, SibSp > 0 | Parch > 0)


print(accompanied_third_class)

third_class_passengers$Accompanied <- ifelse(third_class_passengers$SibSp > 0 | third_class_passengers$Parch > 0, "Yes", "No")

table(third_class_passengers$Accompanied)
#124 3rd class passengers were accompanied by sib/spo or paren/chi
#239 traveled alone

n_accompanied_third_class <- subset(third_class_passengers, SibSp == 0 | Parch == 0)
print(n_accompanied_third_class)

third_class_passengers$Accompanied <- ifelse(third_class_passengers$SibSp == 0 | third_class_passengers$Parch == 0, "Yes", "No")
table(third_class_passengers$Accompanied)

# Filter for third-class passengers who traveled alone
third_class_alone <- subset(titanic.train, Pclass == 3 & SibSp == 0 & Parch == 0)

# Check their survival status
table(third_class_alone$Survived)
#only 50 of 239 3rd class passengers who traveled alone survived
third_class_males <- subset(titanic.train, Pclass == 3 & sex=="male")
num_third_class_males <- nrow(third_class_males)
print(num_third_class_males)

third_class_surv <- subset(titanic.train, Pclass == 3 & Survived==1)
num_third_class_surv <- nrow(third_class_surv)
print(num_third_class_surv)
#84 third class people survived
third_class_sur <- subset(titanic.train, Pclass == 3 & Survived==0)
num_third_class_sur <- nrow(third_class_sur)
print(num_third_class_sur)
library(ggplot2)

#This scatterplot shows how many passengers of different classes embarked from different ports
ggplot(class_embark_counts, aes(x = Embarked, y = Count, color = as.factor(Pclass))) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  labs(x = "Embarkation Location", y = "Count", color = "Passenger Class") +
  ggtitle("Dot Plot of Passenger Classes by Embarkation Location") +
  theme_minimal()

library(ggplot2)

ggplot(titanic.train, aes(x = as.factor(Pclass), y = Age, fill = as.factor(Pclass))) +
  geom_boxplot() +
  labs(x = "Passenger Class", y = "Age", fill = "Class") +
  ggtitle("Distribution of Age by Passenger Class") +
  theme_minimal()
age_third <- subset(titanic.train, Pclass==3 & AgeGroup == "Adult")
num_age_third <- nrow(age_third)
print(num_age_third)
#285 passengers who travelled in 3rd class were adults

age_third <- subset(titanic.train, Pclass==3 & Sex == "male")
num_age_third <- nrow(age_third)
print(num_age_third)#258

age_third <- subset(titanic.train, Pclass==3 & Sex == "female")
num_age_third <- nrow(age_third)
print(num_age_third)
#105
