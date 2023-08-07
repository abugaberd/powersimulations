#David's power simulation code
#generalized  linear mixed models and linear mixed models
#April 5, 2022

#load and install packages
#install.packages(lme4)
library(lme4)
library(car)

NumberOfSimulations<-10

#First, the binary-coded productive task
NumberOfItems<-5

#probability of tu/usted for each condition-group
HS_lower_prob<-0.20
HS_same_prob<-0.50
HS_higher_prob<-0.80

L2_lower_prob<-0.10
L2_same_prob<-0.20
L2_higher_prob<-0.30


VectorOfPvaluesSocialStatus<-NULL
VectorOfPvaluesHSorL2<-NULL
VectorOfPvaluesInteraction<-NULL

VectorOfPowerSocialStatus<-NULL
VectorOfPowerHSorL2<-NULL
VectorOfPowerInteraction<-NULL


for(n in 5:10){
  NumberOfParticipantsPerGroup<-n
  
  #build a list of participant IDs
  VectorOfParticipants<-NULL
  for(i in 1:(NumberOfParticipantsPerGroup)){
    VectorOfParticipants<-c(VectorOfParticipants,rep(i,NumberOfItems)  )
  }
  VectorOfParticipants<-rep(VectorOfParticipants,3)
  VectorOfParticipants<-c(VectorOfParticipants,VectorOfParticipants+30)
  
  
  for(i in 1:NumberOfSimulations){
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_HS_lower<-ifelse(VectorForNow>HS_lower_prob,1,0)
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_HS_same<-ifelse(VectorForNow>HS_same_prob,1,0)
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_HS_higher<-ifelse(VectorForNow>HS_higher_prob,1,0)
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_L2_lower<-ifelse(VectorForNow>L2_lower_prob,1,0)
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_L2_same<-ifelse(VectorForNow>L2_same_prob,1,0)
    
    VectorForNow<-runif(NumberOfParticipantsPerGroup*NumberOfItems)
    VectorOfScores_L2_higher<-ifelse(VectorForNow>L2_higher_prob,1,0)
    
    
    VectorOfLabels<-c(
      rep("HS_lower",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("HS_same",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("HS_higher",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_lower",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_same",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_higher",NumberOfParticipantsPerGroup*NumberOfItems)
    )
    
    ExampleDF<-cbind(VectorOfParticipants,VectorOfLabels,c(VectorOfScores_HS_lower,VectorOfScores_HS_same,VectorOfScores_HS_higher,VectorOfScores_L2_lower,VectorOfScores_L2_same,VectorOfScores_L2_higher))
    
    ExampleDF<-data.frame(ExampleDF)
    
    colnames(ExampleDF)<-c("Participant","Group","Score") 
    
    ExampleDF$HSorL2<-ifelse(grepl("HS",ExampleDF$Group),"HS","L2")
    
    ExampleDF$SocialStatus<-ifelse(grepl("lower",ExampleDF$Group),"lower",ifelse(grepl("same",ExampleDF$Group),"same","higher"))
    
    ExampleDF$Participant<-as.factor(ExampleDF$Participant)
    ExampleDF$SocialStatus<-as.factor(ExampleDF$SocialStatus)
    ExampleDF$HSorL2<-as.factor(ExampleDF$HSorL2)
    ExampleDF$Score<-as.numeric(ExampleDF$Score)
    
    
    #Model<-glm(Score~SocialStatus*HSorL2,data=ExampleDF, family=binomial(link = "logit"))
    #ModelSummary<-Anova(Model)
    #pvalueSocialStatus<-ModelSummary$`Pr(>Chisq)`[1]
    #pvalueHSorL2<-ModelSummary$`Pr(>Chisq)`[2]
    #pvalueInteraction<-ModelSummary$`Pr(>Chisq)`[3]
    
    
    #running without random effects because the following produces singular fit
    Model<-glmer(Score~SocialStatus*HSorL2+(1|Participant),data=ExampleDF, family=binomial(link = "logit"))
    ModelSummary<-Anova(Model)
      pvalueSocialStatus<-ModelSummary$`Pr(>Chisq)`[1]
      pvalueHSorL2<-ModelSummary$`Pr(>Chisq)`[2]
      pvalueInteraction<-ModelSummary$`Pr(>Chisq)`[3]
    
    VectorOfPvaluesSocialStatus<-c(VectorOfPvaluesSocialStatus,pvalueSocialStatus)
    VectorOfPvaluesHSorL2<-c(VectorOfPvaluesHSorL2,pvalueHSorL2)
    VectorOfPvaluesInteraction<-c(VectorOfPvaluesInteraction,pvalueInteraction)
    
  }
  
  VectorOfSignificantResultsSocialStatus<-ifelse(VectorOfPvaluesSocialStatus<.05,1,0)
  VectorOfSignificantResultsHSorL2<-ifelse(VectorOfPvaluesHSorL2<.05,1,0)
  VectorOfSignificantResultsInteraction<-ifelse(VectorOfPvaluesInteraction<.05,1,0)
  
  VectorOfPowerSocialStatus<-c(VectorOfPowerSocialStatus,mean(VectorOfSignificantResultsSocialStatus))
  VectorOfPowerHSorL2<-c(VectorOfPowerHSorL2,mean(VectorOfSignificantResultsHSorL2))
  VectorOfPowerInteraction<-c(VectorOfPowerInteraction,mean(VectorOfSignificantResultsInteraction))
  
}

plot(VectorOfPowerSocialStatus,main="Productive task - Power curve for effect of social status",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")

plot(VectorOfPowerHSorL2,main="Productive task - Power curve for effect of HS/L2 group",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")

plot(VectorOfPowerInteraction,main="Productive task - Power curve for effect of interaction",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")




## now do it all again for the Likert-scale task
NumberOfItems<-3

#Mean rating for each condition-group
HS_lower_prob<-40
HS_same_prob<-50
HS_higher_prob<-60

L2_lower_prob<-40
L2_same_prob<-45
L2_higher_prob<-50

#standard deviation (equal for all groups in this simulation)
SD=20


VectorOfPvaluesSocialStatus<-NULL
VectorOfPvaluesHSorL2<-NULL
VectorOfPvaluesInteraction<-NULL

VectorOfPowerSocialStatus<-NULL
VectorOfPowerHSorL2<-NULL
VectorOfPowerInteraction<-NULL


for(n in 5:50){
  NumberOfParticipantsPerGroup<-n
  
  #build a list of participant IDs
  VectorOfParticipants<-NULL
  for(i in 1:(NumberOfParticipantsPerGroup)){
    VectorOfParticipants<-c(VectorOfParticipants,rep(i,NumberOfItems)  )
  }
  VectorOfParticipants<-rep(VectorOfParticipants,3)
  VectorOfParticipants<-c(VectorOfParticipants,VectorOfParticipants+30)
  
  
  for(i in 1:NumberOfSimulations){
    
    VectorOfScores_HS_lower<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=HS_lower_prob,sd=SD)
    VectorOfScores_HS_same<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=HS_same_prob,sd=SD)
    VectorOfScores_HS_higher<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=HS_higher_prob,sd=SD)
    
    VectorOfScores_L2_lower<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=L2_lower_prob,sd=SD)
    VectorOfScores_L2_same<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=L2_same_prob,sd=SD)
    VectorOfScores_L2_higher<-rnorm(NumberOfParticipantsPerGroup*NumberOfItems,mean=L2_higher_prob,sd=SD)
    
    VectorOfLabels<-c(
      rep("HS_lower",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("HS_same",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("HS_higher",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_lower",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_same",NumberOfParticipantsPerGroup*NumberOfItems),
      rep("L2_higher",NumberOfParticipantsPerGroup*NumberOfItems)
    )
    
    ExampleDF<-cbind(VectorOfParticipants,VectorOfLabels,c(VectorOfScores_HS_lower,VectorOfScores_HS_same,VectorOfScores_HS_higher,VectorOfScores_L2_lower,VectorOfScores_L2_same,VectorOfScores_L2_higher))
    
    ExampleDF<-data.frame(ExampleDF)
    
    colnames(ExampleDF)<-c("Participant","Group","Score") 
    
    ExampleDF$HSorL2<-ifelse(grepl("HS",ExampleDF$Group),"HS","L2")
    
    ExampleDF$SocialStatus<-ifelse(grepl("lower",ExampleDF$Group),"lower",ifelse(grepl("same",ExampleDF$Group),"same","higher"))
    
    ExampleDF$Participant<-as.factor(ExampleDF$Participant)
    ExampleDF$SocialStatus<-as.factor(ExampleDF$SocialStatus)
    ExampleDF$HSorL2<-as.factor(ExampleDF$HSorL2)
    ExampleDF$Score<-as.numeric(ExampleDF$Score)
    
    ExampleDF$Score<-ifelse(ExampleDF$Score>100,100,ExampleDF$Score)
    ExampleDF$Score<-ifelse(ExampleDF$Score<0,0,ExampleDF$Score)
    
    #Model<-lm(Score~SocialStatus*HSorL2,data=ExampleDF)
    #ModelSummary<-Anova(Model)
    #pvalueSocialStatus<-ModelSummary$`Pr(>F)`[1]
    #pvalueHSorL2<-ModelSummary$`Pr(>F)`[2]
    #pvalueInteraction<-ModelSummary$`Pr(>F)`[3]
    
    
    #running with random effects
    Model<-lmer(Score~SocialStatus*HSorL2+(1|Participant),data=ExampleDF)
    ModelSummary<-Anova(Model)
      pvalueSocialStatus<-ModelSummary$`Pr(>Chisq)`[1]
      pvalueHSorL2<-ModelSummary$`Pr(>Chisq)`[2]
      pvalueInteraction<-ModelSummary$`Pr(>Chisq)`[3]
    
    VectorOfPvaluesSocialStatus<-c(VectorOfPvaluesSocialStatus,pvalueSocialStatus)
    VectorOfPvaluesHSorL2<-c(VectorOfPvaluesHSorL2,pvalueHSorL2)
    VectorOfPvaluesInteraction<-c(VectorOfPvaluesInteraction,pvalueInteraction)
    
  }
  
  VectorOfSignificantResultsSocialStatus<-ifelse(VectorOfPvaluesSocialStatus<.05,1,0)
  VectorOfSignificantResultsHSorL2<-ifelse(VectorOfPvaluesHSorL2<.05,1,0)
  VectorOfSignificantResultsInteraction<-ifelse(VectorOfPvaluesInteraction<.05,1,0)
  
  VectorOfPowerSocialStatus<-c(VectorOfPowerSocialStatus,mean(VectorOfSignificantResultsSocialStatus))
  VectorOfPowerHSorL2<-c(VectorOfPowerHSorL2,mean(VectorOfSignificantResultsHSorL2))
  VectorOfPowerInteraction<-c(VectorOfPowerInteraction,mean(VectorOfSignificantResultsInteraction))
  
}

plot(VectorOfPowerSocialStatus,main="Receptive task - Power curve for effect of social status",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")

plot(VectorOfPowerHSorL2,main="Receptive task - Power curve for effect of HS/L2 group",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")

plot(VectorOfPowerInteraction,main="Receptive task - Power curve for effect of interaction",xlab="Number of participants per group",ylab="Chance of detecting a significant effect")



#VISUALIZE WHAT THE DATA LOOKS LIKE
library(ggplot2)

ggplot(ExampleDF, aes(x=Score, fill=Group)) +
  geom_density(alpha=.2)

