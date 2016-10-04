## Code for non-privacy protecting replication of Broockman (2013)
## by Grace Rehaut (grehaut@princeton.edu)
## August 8, 2016

library(cem)
load("~/Desktop/Harvard/Broockman/Broockman.RData") 
broockman<-x

##############
## Figure 1 ##
##############

##################################################################################################
## Performing coarsened exact matching (CEM) on data
##################################################################################################

broock.man<-cem(treatment="leg_black",data=broockman,drop=c("leg_state","leg_chamber","leg_party",
                "leg_gender","leg_white","leg_latino","leg_native","leg_arab","leg_asian",
                "leg_nonblackminority","treat_out","outcitiesisausedoutcity","code_some_response_given",
                "totalpop","whitepop","blackpop","nativepop","asianpop","islanderp","otherrace","hispanicp",
                "totalhouses","urbanhouses","ruralhousing","black_medianhh","latino_medianhh","white_medianhh",
                "statessquireindex", "blackvotingpe","nonblackvotin","district_overallturnout","leg_senator",
                "census_district_blackpercent","demvotepercent", "nonblacknonwhite","urbanpercent","helpful",
                "keepthis","south","chamber","dist_from_dist_to_out"))
broockman<-cbind(broockman,broock.man$w)
broockman$weights<-broockman$"broock.man$w"
broockman<-broockman[,-46]

##################################################################################################
## Part 1: Non-black legislators
##################################################################################################

## Not private
nb.id<-broockman[broockman$leg_black==0 & broockman$treat_out==0,]
nonblack.indistrict<-sum(nb.id$code_some_response_given*nb.id$weights)/sum(nb.id$weights)

nb.od<-broockman[broockman$leg_black==0 & broockman$treat_out==1,]
nonblack.outdistrict<-sum(nb.od$code_some_response_given*nb.od$weights)/sum(nb.od$weights)

## Differentially private


##################################################################################################
## Part 2: Black legislators
##################################################################################################

## Not private
b.id<-broockman[broockman$leg_black==1 & broockman$treat_out==0,]
black.indistrict<-sum(b.id$code_some_response_given*b.id$weights)/sum(b.id$weights)

b.od<-broockman[broockman$leg_black==1 & broockman$treat_out==1,]
black.outdistrict<-sum(b.od$code_some_response_given*b.od$weights)/sum(b.od$weights)

## Differentially private


##################################################################################################
## Part 3: Differences in Differences
##################################################################################################

## Not private
did<-(nonblack.indistrict - nonblack.outdistrict) - (black.indistrict - black.outdistrict)

## Differentially private


##################################################################################################
## Part 4: Figure!
##################################################################################################

Figure1<-c(did,black.outdistrict,black.indistrict,nonblack.outdistrict,nonblack.indistrict)
names.figure1<-c("Difference\nin Differences","Black\nIn-District","Black\nOut-of-District","Non-Black\nIn-District","Non-Black\nOut-of-District")
col.figure1=c("black","cadetblue1","cadetblue3","cadetblue1","cadetblue3")
borders.figure1=c("black","black","black","black","black")
barplot(Figure1,horiz=TRUE,col=col.figure1,border=borders.figure1,xlim=c(0,0.59),
        xlab="Percent of legislators responding",
        main="Rates of Reply by Treatment Group and Legislators’ Race",
        names.arg=names.figure1)
text(0.53,5.4,"51.6%",col="black",cex=1.2,srt=270)
text(0.16,4.3,"14.6%",col="black",cex=1.2,srt=270)
text(0.47,3.1,"45.1%",col="black",cex=1.2,srt=270)
text(0.31,1.9,"29.5%",col="black",cex=1.2,srt=270)
text(0.23,0.7,"21.4%",col="black",cex=1.2,srt=270)