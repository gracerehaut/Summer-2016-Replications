## Code for non-privacy protecting replication of Pew Research Center's 2014 study on India
## by Grace Rehaut (grehaut@princeton.edu)
## August 8, 2016

library(foreign)
setwd("~/Desktop/Harvard/India")
india<-read.spss("india.sav",to.data.frame=T)

#############
## Table 1 ##
#############

## Not private
bjp<-sum(india$weight[india$QIND5=="BJP "],na.rm=T)/sum(india$weight) 
inc<-sum(india$weight[india$QIND5=="INC"])/sum(india$weight) 
other<-sum(india$weight[india$QIND5=="Other" | india$QIND5=="None"])/sum(india$weight) 
dk<-sum(india$weight[india$QIND5=="Don’t know" | india$QIND5=="Refused "])/sum(india$weight) 

pie<-c(bjp,dk,other,inc)
col.pie<-c("paleturquoise1","paleturquoise2","paleturquoise3","paleturquoise4")
pie(x=pie,col=col.pie,labels=c("BJP \n 63%","Don't know \n 4% ","Other/None \n 14%","INC \n 19%"),
    clockwise=T,main="Which party should lead the next government?",radius=1)

## Differentially private

#############
## Table 2 ##
#############

##################################################################################################
## Row 1: Men
##################################################################################################

## Not private
men.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q164=="Male"],na.rm=T)/sum(india$weight[india$Q164=="Male"]) 
men.inc<-sum(india$weight[india$QIND5=="INC" & india$Q164=="Male"])/sum(india$weight[india$Q164=="Male"]) 
men.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q164=="Male"])/sum(india$weight[india$Q164=="Male"]) 
men.bjp-men.inc

## Differentially private

##################################################################################################
## Row 2: Women
##################################################################################################

## Not private
women.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q164=="Female"],na.rm=T)/sum(india$weight[india$Q164=="Female"]) 
women.inc<-sum(india$weight[india$QIND5=="INC" & india$Q164=="Female"])/sum(india$weight[india$Q164=="Female"]) 
women.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q164=="Female"])/sum(india$weight[india$Q164=="Female"]) 
women.bjp-women.inc

## Differentially private

##################################################################################################
## Row 3: Ages 18-29
##################################################################################################

## Not private
age1.bjp<-sum(india$weight[india$QIND5=="BJP " & (india$Q165>=18 & india$Q165<=29)],na.rm=T)/sum(india$weight[india$Q165>=18 & india$Q165<=29]) 
age1.inc<-sum(india$weight[india$QIND5=="INC" & (india$Q165>=18 & india$Q165<=29)])/sum(india$weight[india$Q165>=18 & india$Q165<=29]) 
age1.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & (india$Q165>=18 & india$Q165<=29)])/sum(india$weight[india$Q165>=18 & india$Q165<=29]) 
age1.bjp-age1.inc

## Differentially private

##################################################################################################
## Row 4: Ages 30-49
##################################################################################################

## Not private
age2.bjp<-sum(india$weight[india$QIND5=="BJP " & (india$Q165>=30 & india$Q165<=49)],na.rm=T)/sum(india$weight[india$Q165>=30 & india$Q165<=49]) 
age2.inc<-sum(india$weight[india$QIND5=="INC" & (india$Q165>=30 & india$Q165<=49)])/sum(india$weight[india$Q165>=30 & india$Q165<=49]) 
age2.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & (india$Q165>=30 & india$Q165<=49)])/sum(india$weight[india$Q165>=30 & india$Q165<=49]) 
age2.bjp-age2.inc

## Differentially private

##################################################################################################
## Row 5: Age 50+
##################################################################################################

## Not private
age3.bjp<-sum(india$weight[india$QIND5=="BJP " & (india$Q165>=50)],na.rm=T)/sum(india$weight[india$Q165>=50]) 
age3.inc<-sum(india$weight[india$QIND5=="INC" & (india$Q165>=50)])/sum(india$weight[india$Q165>=50]) 
age3.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & (india$Q165>=50)])/sum(india$weight[india$Q165>=50]) 
age3.bjp-age3.inc

## Differentially private

##################################################################################################
## Row 6: Primary school or less [educational level]
##################################################################################################

india$Q180INDIA<-ifelse(india$Q180INDIA=="Illiterate" | india$Q180INDIA=="Literate but no formal schooling" | india$Q180INDIA=="School up to 4 years" | india$Q180INDIA=="School up to 5 to 9 years",1,
                 ifelse(india$Q180INDIA=="SSC/HSC",2,
                 ifelse(india$Q180INDIA=="Some college but not graduated" | india$Q180INDIA=="Graduate/Post grad-Gen BA MSc BCOM etc" | india$Q180INDIA=="Graduate/Post grad-Prof BE MTech MBA MBBS etc",3,4)))

## Not private
psl.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q180INDIA==1],na.rm=T)/sum(india$weight[india$Q180INDIA==1]) 
psl.inc<-sum(india$weight[india$QIND5=="INC" & india$Q180INDIA==1])/sum(india$weight[india$Q180INDIA==1]) 
psl.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q180INDIA==1])/sum(india$weight[india$Q180INDIA==1]) 
psl.bjp-psl.inc

## Differentially private

##################################################################################################
## Row 7: Secondary school / higher secondary certificate [educational level]
##################################################################################################

## Not private
sshsc.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q180INDIA==2],na.rm=T)/sum(india$weight[india$Q180INDIA==2]) 
sshsc.inc<-sum(india$weight[india$QIND5=="INC" & india$Q180INDIA==2])/sum(india$weight[india$Q180INDIA==2]) 
sshsc.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q180INDIA==2])/sum(india$weight[india$Q180INDIA==2]) 
sshsc.bjp-sshsc.inc

## Differentially private

##################################################################################################
## Row 8: Some college or more [educational level]
##################################################################################################

## Not private
scm.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q180INDIA==3],na.rm=T)/sum(india$weight[india$Q180INDIA==3]) 
scm.inc<-sum(india$weight[india$QIND5=="INC" & india$Q180INDIA==3])/sum(india$weight[india$Q180INDIA==3]) 
scm.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q180INDIA==3])/sum(india$weight[india$Q180INDIA==3]) 
scm.bjp-scm.inc

## Differentially private

##################################################################################################
## Row 9: Low income level
##################################################################################################

india$income<-ifelse(india$Q183INDIA=="Up to Rs. 500 per month" | india$Q183INDIA=="Rs. 501 – Rs. 750" | india$Q183INDIA=="Rs. 751 – Rs. 1000" |
                     india$Q183INDIA=="Rs. 1001 – Rs. 1500" | india$Q183INDIA=="Rs. 1501 – Rs. 2000" | india$Q183INDIA=="Rs. 2001 – Rs. 2500" |
                     india$Q183INDIA=="Rs. 2501 – Rs. 3000" | india$Q183INDIA=="Rs. 3001 – Rs. 4000",1,
              ifelse(india$Q183INDIA=="Rs. 4001 – Rs. 5000" | india$Q183INDIA=="Rs. 5001 – Rs. 6000" | india$Q183INDIA=="Rs. 6001 – Rs. 10,000",2,
              ifelse(india$Q183INDIA=="Rs. 10,001 – Rs. 15,000" | india$Q183INDIA=="Rs. 15,001 – Rs. 20,000" |
                       india$Q183INDIA=="Rs. 20,001 – Rs. 30,000" | india$Q183INDIA=="Rs. 30,001 – Rs. 40,000" | india$Q183INDIA=="Rs. 40,001 – Rs. 50,000" | 
                       india$Q183INDIA=="Rs. 50,001 – Rs. 100,000" | india$Q183INDIA=="Rs. 100,001 and above",3,4)))

## Not private
income1.bjp<-sum(india$weight[india$QIND5=="BJP " & india$income==1],na.rm=T)/sum(india$weight[india$income==1]) 
income1.inc<-sum(india$weight[india$QIND5=="INC" & india$income==1])/sum(india$weight[india$income==1]) 
income1.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$income==1])/sum(india$weight[india$income==1]) 
income1.bjp-income1.inc

## Differentially private

##################################################################################################
## Row 10: Middle income level
##################################################################################################

## Not private
income2.bjp<-sum(india$weight[india$QIND5=="BJP " & india$income==2],na.rm=T)/sum(india$weight[india$income==2]) 
income2.inc<-sum(india$weight[india$QIND5=="INC" & india$income==2])/sum(india$weight[india$income==2]) 
income2.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$income==2])/sum(india$weight[india$income==2]) 
income2.bjp-income2.inc

## Differentially private

##################################################################################################
## Row 11: High income level
##################################################################################################

## Not private
income3.bjp<-sum(india$weight[india$QIND5=="BJP " & india$income==3],na.rm=T)/sum(india$weight[india$income==3]) 
income3.inc<-sum(india$weight[india$QIND5=="INC" & india$income==3])/sum(india$weight[india$income==3]) 
income3.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$income==3])/sum(india$weight[india$income==3]) 
income3.bjp-income3.inc

## Differentially private

##################################################################################################
## Row 12: Urban
##################################################################################################

## Not private
urban.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q217INDIA=="Urban"],na.rm=T)/sum(india$weight[india$Q217INDIA=="Urban"]) 
urban.inc<-sum(india$weight[india$QIND5=="INC" & india$Q217INDIA=="Urban"])/sum(india$weight[india$Q217INDIA=="Urban"]) 
urban.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q217INDIA=="Urban"])/sum(india$weight[india$Q217INDIA=="Urban"]) 
urban.bjp-urban.inc

## Differentially private

##################################################################################################
## Row 13: Rural
##################################################################################################

## Not private
rural.bjp<-sum(india$weight[india$QIND5=="BJP " & india$Q217INDIA=="Rural"],na.rm=T)/sum(india$weight[india$Q217INDIA=="Rural"]) 
rural.inc<-sum(india$weight[india$QIND5=="INC" & india$Q217INDIA=="Rural"])/sum(india$weight[india$Q217INDIA=="Rural"]) 
rural.other<-sum(india$weight[(india$QIND5=="Other" | india$QIND5=="None") & india$Q217INDIA=="Rural"])/sum(india$weight[india$Q217INDIA=="Rural"]) 
rural.bjp-rural.inc

## Differentially private

#############
## Table 4 ##
#############

##################################################################################################
## Row 1: Combating corruption
##################################################################################################

## Not private
corruption.bjp<-sum(india$weight[india$QIND2c=="BJP "])/sum(india$weight) 
corruption.inc<-sum(india$weight[india$QIND2c=="INC"])/sum(india$weight) 
corruption.bjp-corruption.inc

## Differentially private

##################################################################################################
## Row 2: Creating job opportunities
##################################################################################################

## Not private
jobs.bjp<-sum(india$weight[india$QIND2a=="BJP "])/sum(india$weight) 
jobs.inc<-sum(india$weight[india$QIND2a=="INC"])/sum(india$weight) 
jobs.bjp-jobs.inc

## Differentially private

##################################################################################################
## Row 3: Limiting rising prices
##################################################################################################

## Not private
prices.bjp<-sum(india$weight[india$QIND2d=="BJP "])/sum(india$weight) 
prices.inc<-sum(india$weight[india$QIND2d=="INC"])/sum(india$weight) 
prices.bjp-prices.inc

## Differentially private

##################################################################################################
## Row 4: Reducing terrorism
##################################################################################################

## Not private
terrorism.bjp<-sum(india$weight[india$QIND2b=="BJP "])/sum(india$weight) 
terrorism.inc<-sum(india$weight[india$QIND2b=="INC"])/sum(india$weight) 
terrorism.bjp-terrorism.inc

## Differentially private

##################################################################################################
## Row 5: Helping the poor
##################################################################################################

## Not private
poor.bjp<-sum(india$weight[india$QIND2f=="BJP "])/sum(india$weight) 
poor.inc<-sum(india$weight[india$QIND2f=="INC"])/sum(india$weight) 
poor.bjp-poor.inc

## Differentially private

##################################################################################################
## Row 6: Ending political deadlock
##################################################################################################

## Not private
deadlock.bjp<-sum(india$weight[india$QIND2e=="BJP "])/sum(india$weight) 
deadlock.inc<-sum(india$weight[india$QIND2e=="INC"])/sum(india$weight) 
deadlock.bjp-deadlock.inc

## Differentially private

#############
## Table 4 ##
#############

india$modi<-ifelse(india$QIND4e=="Very favorable" | india$QIND4e=="Somewhat favorable",1,0)
india$gandhi<-ifelse(india$QIND4d=="Very favorable" | india$QIND4d=="Somewhat favorable",1,0)

##################################################################################################
## Row 1: Men
##################################################################################################

## Not private
men.modi<-sum(india$weight[india$modi==1 & india$Q164=="Male"])/sum(india$weight[india$Q164=="Male"]) 
men.gandhi<-sum(india$weight[india$gandhi==1 & india$Q164=="Male"])/sum(india$weight[india$Q164=="Male"]) 
men.modi-men.gandhi

## Differentially private

##################################################################################################
## Row 2: Women
##################################################################################################

## Not private
women.modi<-sum(india$weight[india$modi==1 & india$Q164=="Female"])/sum(india$weight[india$Q164=="Female"]) 
women.gandhi<-sum(india$weight[india$gandhi==1 & india$Q164=="Female"])/sum(india$weight[india$Q164=="Female"]) 
women.modi-women.gandhi

## Differentially private

##################################################################################################
## Row 3: Ages 18-29
##################################################################################################

## Not private
age1.modi<-sum(india$weight[india$modi==1 & (india$Q165>=18 & india$Q165<=29)])/sum(india$weight[india$Q165>=18 & india$Q165<=29]) 
age1.gandhi<-sum(india$weight[india$gandhi==1 & (india$Q165>=18 & india$Q165<=29)])/sum(india$weight[india$Q165>=18 & india$Q165<=29]) 
age1.modi-age1.gandhi

## Differentially private

##################################################################################################
## Row 4: Ages 30-49
##################################################################################################

## Not private
age2.modi<-sum(india$weight[india$modi==1 & (india$Q165>=30 & india$Q165<=49)])/sum(india$weight[india$Q165>=30 & india$Q165<=49]) 
age2.gandhi<-sum(india$weight[india$gandhi==1 & (india$Q165>=30 & india$Q165<=49)])/sum(india$weight[india$Q165>=30 & india$Q165<=49]) 
age2.modi-age2.gandhi

## Differentially private

##################################################################################################
## Row 5: Ages 50+
##################################################################################################

## Not private
age3.modi<-sum(india$weight[india$modi==1 & (india$Q165>=50)])/sum(india$weight[india$Q165>=50]) 
age3.gandhi<-sum(india$weight[india$gandhi==1 & (india$Q165>=50)])/sum(india$weight[india$Q165>=50]) 
age3.modi-age3.gandhi

## Differentially private

##################################################################################################
## Row 6: Primary school or less [educational level]
##################################################################################################

## Not private
psl.modi<-sum(india$weight[india$modi==1 & india$Q180INDIA==1])/sum(india$weight[india$Q180INDIA==1]) 
psl.gandhi<-sum(india$weight[india$gandhi==1 & india$Q180INDIA==1])/sum(india$weight[india$Q180INDIA==1]) 
psl.modi-psl.gandhi

## Differentially private

##################################################################################################
## Row 7: Secondary school / Higher secondary certificate [educational level]
##################################################################################################

## Not private
sshsc.modi<-sum(india$weight[india$modi==1 & india$Q180INDIA==2])/sum(india$weight[india$Q180INDIA==2]) 
sshsc.gandhi<-sum(india$weight[india$gandhi==1 & india$Q180INDIA==2])/sum(india$weight[india$Q180INDIA==2]) 
sshsc.modi-sshsc.gandhi

## Differentially private

##################################################################################################
## Row 8: Some college or more [educational level]
##################################################################################################

## Not private
scm.modi<-sum(india$weight[india$modi==1 & india$Q180INDIA==3])/sum(india$weight[india$Q180INDIA==3]) 
scm.gandhi<-sum(india$weight[india$gandhi==1 & india$Q180INDIA==3])/sum(india$weight[india$Q180INDIA==3]) 
scm.modi-scm.gandhi

## Differentially private

##################################################################################################
## Row 9: Low income
##################################################################################################

## Not private
income1.modi<-sum(india$weight[india$modi==1 & india$income==1])/sum(india$weight[india$income==1]) 
income1.gandhi<-sum(india$weight[india$gandhi==1 & india$income==1])/sum(india$weight[india$income==1]) 
income1.modi-income1.gandhi

## Differentially private

##################################################################################################
## Row 10: Middle income
##################################################################################################

## Not private
income2.modi<-sum(india$weight[india$modi==1 & india$income==2])/sum(india$weight[india$income==2]) 
income2.gandhi<-sum(india$weight[india$gandhi==1 & india$income==2])/sum(india$weight[india$income==2]) 
income2.modi-income2.gandhi

## Differentially private

##################################################################################################
## Row 11: High income
##################################################################################################

## Not private
income3.modi<-sum(india$weight[india$modi==1 & india$income==3])/sum(india$weight[india$income==3]) 
income3.gandhi<-sum(india$weight[india$gandhi==1 & india$income==3])/sum(india$weight[india$income==3]) 
income3.modi-income3.gandhi

## Differentially private

##################################################################################################
## Row 12: Urban
##################################################################################################

## Not private
urban.modi<-sum(india$weight[india$modi==1 & india$Q217INDIA=="Urban"])/sum(india$weight[india$Q217INDIA=="Urban"]) 
urban.gandhi<-sum(india$weight[india$gandhi==1 & india$Q217INDIA=="Urban"])/sum(india$weight[india$Q217INDIA=="Urban"]) 
urban.modi-urban.gandhi

## Differentially private

##################################################################################################
## Row 13: Rural
##################################################################################################

## Not private
rural.modi<-sum(india$weight[india$modi==1 & india$Q217INDIA=="Rural"])/sum(india$weight[india$Q217INDIA=="Rural"]) 
rural.gandhi<-sum(india$weight[india$gandhi==1 & india$Q217INDIA=="Rural"])/sum(india$weight[india$Q217INDIA=="Rural"]) 
rural.modi-rural.gandhi

## Differentially private

#############
## Table 5 ##
#############

india$msingh<-ifelse(india$QIND4a=="Very favorable" | india$QIND4a=="Somewhat favorable",1,
                     ifelse(india$QIND4a=="Very unfavorable" | india$QIND4a=="Somewhat unfavorable",0,NA))
india$sgandhi<-ifelse(india$QIND4b=="Very favorable" | india$QIND4b=="Somewhat favorable",1,
                      ifelse(india$QIND4b=="Very unfavorable" | india$QIND4b=="Somewhat unfavorable",0,NA))
india$rsingh<-ifelse(india$QIND4c=="Very favorable" | india$QIND4c=="Somewhat favorable",1,
                     ifelse(india$QIND4c=="Very unfavorable" | india$QIND4c=="Somewhat unfavorable",0,NA))
india$rgandhi<-ifelse(india$QIND4d=="Very favorable" | india$QIND4d=="Somewhat favorable",1,
                      ifelse(india$QIND4d=="Very unfavorable" | india$QIND4d=="Somewhat unfavorable",0,NA))
india$modi<-ifelse(india$QIND4e=="Very favorable" | india$QIND4e=="Somewhat favorable",1,
                   ifelse(india$QIND4e=="Very unfavorable" | india$QIND4e=="Somewhat unfavorable",0,NA))
india$chidam<-ifelse(india$QIND4f=="Very favorable" | india$QIND4f=="Somewhat favorable",1,
                     ifelse(india$QIND4f=="Very unfavorable" | india$QIND4f=="Somewhat unfavorable",0,NA))
india$hazare<-ifelse(india$QIND4g=="Very favorable" | india$QIND4g=="Somewhat favorable",1,
                     ifelse(india$QIND4g=="Very unfavorable" | india$QIND4g=="Somewhat unfavorable",0,NA))

##################################################################################################
## Row 1: Narendra Modi
##################################################################################################

## Not private
unfav.modi<-sum(india$weight[india$modi==0],na.rm=T)/sum(india$weight) 
fav.modi<-sum(india$weight[india$modi==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 2: Anna Hazare
##################################################################################################

## Not private
unfav.hazare<-sum(india$weight[india$hazare==0],na.rm=T)/sum(india$weight) 
fav.hazare<-sum(india$weight[india$hazare==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 3: Manmohan Singh
##################################################################################################

## Not private
unfav.msingh<-sum(india$weight[india$msingh==0],na.rm=T)/sum(india$weight) 
fav.msingh<-sum(india$weight[india$msingh==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 4: Rahul Gandhi
##################################################################################################

## Not private
unfav.rgandhi<-sum(india$weight[india$rgandhi==0],na.rm=T)/sum(india$weight) 
fav.rgandhi<-sum(india$weight[india$rgandhi==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 5: Sonia Gandhi
##################################################################################################

## Not private
unfav.sgandhi<-sum(india$weight[india$sgandhi==0],na.rm=T)/sum(india$weight) 
fav.sgandhi<-sum(india$weight[india$sgandhi==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 6: Rajnath Singh
##################################################################################################

## Not private
unfav.rsingh<-sum(india$weight[india$rsingh==0],na.rm=T)/sum(india$weight) 
fav.rsingh<-sum(india$weight[india$rsingh==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Row 7: P. Chidambaram
##################################################################################################

## Not private
unfav.chidam<-sum(india$weight[india$chidam==0],na.rm=T)/sum(india$weight) 
fav.chidam<-sum(india$weight[india$chidam==1],na.rm=T)/sum(india$weight) 

## Differentially private

##################################################################################################
## Figure
##################################################################################################

## Not private
par(mfrow=c(1,2))

figure2<-c(-unfav.chidam,-unfav.rsingh,-unfav.sgandhi,-unfav.rgandhi,-unfav.msingh,-unfav.hazare,-unfav.modi)
figure3<-c(fav.chidam,fav.rsingh,fav.sgandhi,fav.rgandhi,fav.msingh,fav.hazare,fav.modi)
barplot(figure2,horiz=T,col="hotpink",main="                                          Unfavorable",xlim=c(-1,0))
text(-0.78,7.9,"Narendra Modi",col="black",cex=1.1)
text(-0.81,6.7,"Anna Hazare",col="black",cex=1.1)
text(-0.75,5.5,"Manmohan Singh",col="black",cex=1.1)
text(-0.8,4.3,"Rahul Gandhi",col="black",cex=1.1)
text(-0.8,3.1,"Sonia Gandhi",col="black",cex=1.1)
text(-0.8,1.9,"Rajnath Singh",col="black",cex=1.1)
text(-0.76,0.7,"P. Chidambaram",col="black",cex=1.1)
text(-0.08,7.9,"16%",col="white",cex=1.1)
text(-0.08,6.7,"17%",col="white",cex=1.1)
text(-0.08,5.5,"42%",col="white",cex=1.1)
text(-0.08,4.3,"43%",col="white",cex=1.1)
text(-0.08,3.1,"46%",col="white",cex=1.1)
text(-0.08,1.9,"34%",col="white",cex=1.1)
text(-0.08,0.7,"39%",col="white",cex=1.1)
barplot(figure3,horiz=T,col="forestgreen",main="Favorable                                          ",xlim=c(0,1))
text(0.08,7.9,"78%",col="white",cex=1.1)
text(0.08,6.7,"69%",col="white",cex=1.1)
text(0.08,5.5,"52%",col="white",cex=1.1)
text(0.08,4.3,"50%",col="white",cex=1.1)
text(0.08,3.1,"49%",col="white",cex=1.1)
text(0.08,1.9,"43%",col="white",cex=1.1)
text(0.08,0.7,"37%",col="white",cex=1.1)

## Differentially private