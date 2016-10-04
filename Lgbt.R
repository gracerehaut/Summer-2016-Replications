## Code for non-privacy protecting and differentially private replications of McClendon (2013)
## by Grace Rehaut (grehaut@princeton.edu)
## August 7, 2016

setwd("~/Desktop/Harvard/Final Paper")
source("./PrivateZelig-replication2/dpModules/Jack/DP_Means.R") 

##################################################################################################
## Loading in, preparing data
##################################################################################################

lgbt<-load("~/Desktop/Harvard/LGBT/lgbt.RData") 
lgbt<-x

sims<-1000
results<-results1<-results2<-results3<-results4<-results5<-results6<-results7<-rep(NA,1000)
results8<-results9<-results10<-results11<-results12<-results13<-results14<-results15<-rep(NA,1000)
results16<-results17<-results18<-results19<-results20<-results21<-results22<-rep(NA,1000)
results23<-results24<-results25<-results26<-results27<-results28<-results29<-rep(NA,1000)
results30<-results31<-results32<-results33<-results34<-results35<-results36<-results37<-rep(NA,1000)

#############
## Table 1 ##
#############

##################################################################################################
## Effect of newsletter treatment // Intent to participate
##################################################################################################

newsletter<-subset(lgbt,subset=(lgbt$newsletter==1))
control<-subset(lgbt,subset=(lgbt$control==1))

## Not private
newsletter.ip<-mean(newsletter$intended)
control.ip<-mean(control$intended)
newsletter.ip-control.ip
t.test(newsletter$intended,control$intended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=newsletter$intended,
                        range=range(newsletter$intended))
  results[i]<-release$release
  newsletter.ip.DP<-median(results)
  release2<-Mean.release(eps=0.1,del=2e-20,data=control$intended,
                        range=range(control$intended))
  results1[i]<-release2$release
  control.ip.DP<-median(results1)
}
newsletter.ip.DP-control.ip.DP

##################################################################################################
## Effect of newsletter treatment // Actual participation
##################################################################################################

## Not private
newsletter.ap<-mean(newsletter$attended)
control.ap<-mean(control$attended)
newsletter.ap-control.ap
t.test(newsletter$attended,control$attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=newsletter$attended,
                        range=range(newsletter$attended))
  results2[i]<-release$release
  newsletter.ap.DP<-median(results2)
  release2<-Mean.release(eps=0.1,del=2e-20,data=control$attended,
                        range=range(control$attended))
  results3[i]<-release2$release
  control.ap.DP<-median(results3)
}
newsletter.ap.DP-control.ap.DP

##################################################################################################
## Effect of Facebook treatment // Intent to participate
##################################################################################################

facebook<-subset(lgbt,subset=(lgbt$facebook==1))

## Not private
facebook.ip<-mean(facebook$intended)
facebook.ip-control.ip
t.test(facebook$intended,control$intended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=facebook$intended,
                        range=range(facebook$intended))
  results4[i]<-release$release
  facebook.ip.DP<-median(results4)
}
facebook.ip.DP-control.ip.DP

##################################################################################################
## Effect of Facebook treatment // Actual participation
##################################################################################################

## Not private
facebook.ap<-mean(facebook$attended)
facebook.ap-control.ap
t.test(facebook$attended,control$attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=facebook$attended,
                        range=range(facebook$attended))
  results5[i]<-release$release
  facebook.ap.DP<-median(results5)
}
facebook.ap.DP-control.ap.DP

##################################################################################################
## Effect of pooled treatment // Intended participation
##################################################################################################

pooled<-subset(lgbt,subset=(lgbt$facebook==1) | (lgbt$newsletter==1))

## Not private
pooled.ip<-mean(pooled$intended)
pooled.ip-control.ip
t.test(pooled$intended,control$intended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=pooled$intended,
                        range=range(pooled$intended))
  results5[i]<-release$release
  pooled.ip.DP<-median(results5)
}
pooled.ip.DP-control.ip.DP

##################################################################################################
## Effect of pooled treatment // Actual participation
##################################################################################################

## Not private
pooled.ap<-mean(pooled$attended)
pooled.ap-control.ap
t.test(pooled$attended,control$attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=pooled$attended,
                        range=range(pooled$attended))
  results6[i]<-release$release
  pooled.ap.DP<-median(results6)
}
pooled.ap.DP-control.ap.DP

#############
## Table 3 ##
#############

##################################################################################################
## Row 1: Difference in rate of reported participation b/w newsletter and information-only
##################################################################################################

## Not private
newsletter.rp<-mean(newsletter$reported_attended,na.rm=T)
control.rp<-mean(control$reported_attended,na.rm=T)
newsletter.rp-control.rp
t.test(newsletter$reported_attended,control$reported_attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=newsletter$reported_attended,
                        range=range(newsletter$reported_attended,na.rm=T))
  results7[i]<-release$release
  newsletter.rp.DP<-median(results7)
  release2<-Mean.release(eps=0.1,del=2e-20,data=control$reported_attended,
                        range=range(control$reported_attended,na.rm=T))
  results8[i]<-release2$release
  control.rp.DP<-median(results8)
}
newsletter.rp.DP-control.rp.DP

##################################################################################################
## Row 2: Differences in rate of reported participation b/w Facebook and information-only
##################################################################################################

## Not private
facebook.rp<-mean(facebook$reported_attended,na.rm=T)
facebook.rp-control.rp
t.test(facebook$reported_attended,control$reported_attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=facebook$reported_attended,
                        range=range(facebook$reported_attended,na.rm=T))
  results9[i]<-release$release
  facebook.rp.DP<-median(results9)
}
facebook.rp.DP-control.rp.DP

##################################################################################################
## Row 3: Differences in rate of reported participation b/w pooled and information-only
##################################################################################################

## Not private
pooled.rp<-mean(pooled$reported_attended,na.rm=T)
pooled.rp-control.rp
t.test(pooled$reported_attended,control$reported_attended)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=pooled$reported_attended,
                        range=range(pooled$reported_attended,na.rm=T))
  results10[i]<-release$release
  pooled.rp.DP<-median(results10)
}
pooled.rp.DP-control.rp.DP

#############
## Table 4 ##
#############

##################################################################################################
## Attendees' difference in mean rates of choosing a reason for participation behavior
##################################################################################################

esteem.attend<-subset(lgbt,subset=(lgbt$esteem==1)&(lgbt$reported_attended==1))
control.attend<-subset(lgbt,subset=(lgbt$information==1)&(lgbt$reported_attended==1))

####################################################
## I went to promote a cause I care about
####################################################

## Not private
esteem.a.cause<-mean(esteem.attend$forcause,na.rm=T)
control.a.cause<-mean(control.attend$forcause,na.rm=T)
control.a.cause-esteem.a.cause

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$forcause,
                        range=range(esteem.attend$forcause))
  results11[i]<-release$release
  esteem.a.cause.DP<-median(results11)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$forcause,
                        range=range(control.attend$forcause))
  results12[i]<-release$release
  control.a.cause.DP<-median(results12)
}
control.a.cause.DP-esteem.a.cause.DP

####################################################
## I went for camaraderie, to meet people...
####################################################

## Not private
esteem.a.social<-mean(esteem.attend$forsocial,na.rm=T)
control.a.social<-mean(control.attend$forsocial,na.rm=T)
control.a.social-esteem.a.social

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$forsocial,
                        range=range(esteem.attend$forsocial,na.rm=T))
  results13[i]<-release$release
  esteem.a.social.DP<-median(results13)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$forsocial,
                        range=range(control.attend$forsocial,na.rm=T))
  results14[i]<-release$release
  control.a.social.DP<-median(results14)
}
control.a.social.DP-esteem.a.social.DP

####################################################
## I went to have fun
####################################################

## Not private
esteem.a.fun<-mean(esteem.attend$forfun,na.rm=T)
control.a.fun<-mean(control.attend$forfun,na.rm=T)
control.a.fun-esteem.a.fun

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$forfun,
                        range=range(esteem.attend$forfun,na.rm=T))
  results15[i]<-release$release
  esteem.a.fun.DP<-median(results15)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$forfun,
                        range=range(control.attend$forfun,na.rm=T))
  results16[i]<-release$release
  control.a.fun.DP<-median(results16)
}
control.a.fun.DP-esteem.a.fun.DP

####################################################
## I went because I had time...
####################################################

## Not private
esteem.a.time<-mean(esteem.attend$fortime,na.rm=T)
control.a.time<-mean(control.attend$fortime,na.rm=T)
control.a.time-esteem.a.time

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$fortime,
                        range=range(esteem.attend$fortime,na.rm=T))
  results17[i]<-release$release
  esteem.a.time.DP<-median(results17)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$fortime,
                        range=range(control.attend$fortime,na.rm=T))
  results18[i]<-release$release
  control.a.time.DP<-median(results18)
}
control.a.time.DP-esteem.a.time.DP

####################################################
## I went because... highly of people who attend
####################################################

## Not private
esteem.a.admiration<-mean(esteem.attend$foradmiration,na.rm=T)
control.a.admiration<-mean(control.attend$foradmiration,na.rm=T)
control.a.admiration-esteem.a.admiration

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$foradmiration,
                        range=range(esteem.attend$foradmiration,na.rm=T))
  results19[i]<-release$release
  esteem.a.admiration.DP<-median(results19)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$foradmiration,
                        range=range(control.attend$foradmiration,na.rm=T))
  results20[i]<-release$release
  control.a.admiration.DP<-median(results20)
}
control.a.admiration.DP-esteem.a.admiration.DP

####################################################
## I went because... a leadership position...
####################################################

## Not private
esteem.a.leadership<-mean(esteem.attend$forleadership,na.rm=T)
control.a.leadership<-mean(control.attend$forleadership,na.rm=T)
control.a.leadership-esteem.a.leadership

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$forleadership,
                        range=range(esteem.attend$forleadership,na.rm=T))
  results21[i]<-release$release
  esteem.a.leadership.DP<-median(results21)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$forleadership,
                        range=range(control.attend$forleadership,na.rm=T))
  results22[i]<-release$release
  control.a.leadership.DP<-median(results22)
}
control.a.leadership.DP-esteem.a.leadership.DP

####################################################
## I went because it was an important event
####################################################

## Not private
esteem.a.important<-mean(esteem.attend$forimportant,na.rm=T)
control.a.important<-mean(control.attend$forimportant,na.rm=T)
control.a.important-esteem.a.important

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.attend$forimportant,
                        range=range(esteem.attend$forimportant,na.rm=T))
  results23[i]<-release$release
  esteem.a.important.DP<-median(results23)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.attend$forimportant,
                        range=range(control.attend$forimportant,na.rm=T))
  results24[i]<-release$release
  control.a.important.DP<-median(results24)
}
control.a.important.DP-esteem.a.important.DP

##################################################################################################
## Nonattendees' difference in mean rates of choosing a reason for participation behavior
##################################################################################################

esteem.nonattend<-subset(lgbt,subset=(lgbt$esteem==1)&(lgbt$reported_attended==0))
control.nonattend<-subset(lgbt,subset=(lgbt$information==1)&(lgbt$reported_attended==0))

####################################################
## I did not think the cause was that important
####################################################

## Not private
esteem.n.cause<-1-mean(esteem.nonattend$forcause,na.rm=T)
control.n.cause<-1-mean(control.nonattend$forcause,na.rm=T)
control.n.cause-esteem.n.cause

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$forcause,
                        range=range(esteem.nonattend$forcause,na.rm=T))
  results25[i]<-1-release$release
  esteem.n.cause.DP<-median(results25)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$forcause,
                        range=range(control.nonattend$forcause,na.rm=T))
  results26[i]<-1-release$release
  control.n.cause.DP<-median(results26)
}
control.n.cause.DP-esteem.n.cause.DP

####################################################
## I did not know anyone else who was going
####################################################

## Not private
esteem.n.social<-1-mean(esteem.nonattend$forsocial,na.rm=T)
control.n.social<-1-mean(control.nonattend$forsocial,na.rm=T)
control.n.social-esteem.n.social

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$forsocial,
                        range=range(esteem.nonattend$forsocial,na.rm=T))
  results27[i]<-1-release$release
  esteem.n.social.DP<-median(results27)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$forsocial,
                        range=range(control.nonattend$forsocial,na.rm=T))
  results28[i]<-1-release$release
  control.n.social.DP<-median(results28)
}
control.n.social.DP-esteem.n.social.DP

####################################################
## I did not think it would be fun
####################################################

## Not private
esteem.n.fun<-1-mean(esteem.nonattend$forfun,na.rm=T)
control.n.fun<-1-mean(control.nonattend$forfun,na.rm=T)
control.n.fun-esteem.n.fun

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$forfun,
                        range=range(esteem.nonattend$forfun,na.rm=T))
  results29[i]<-1-release$release
  esteem.n.fun.DP<-median(results29)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$forfun,
                        range=range(control.nonattend$forfun,na.rm=T))
  results30[i]<-1-release$release
  control.n.fun.DP<-median(results30)
}
control.n.fun.DP-esteem.n.fun.DP

####################################################
## I did not have time...
####################################################

## Not private
esteem.n.time<-1-mean(esteem.nonattend$fortime,na.rm=T)
control.n.time<-1-mean(control.nonattend$fortime,na.rm=T)
control.n.time-esteem.n.time

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$fortime,
                        range=range(esteem.nonattend$fortime,na.rm=T))
  results31[i]<-1-release$release
  esteem.n.time.DP<-median(results31)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$fortime,
                        range=range(control.nonattend$fortime,na.rm=T))
  results32[i]<-1-release$release
  control.n.time.DP<-median(results32)
}
control.n.time.DP-esteem.n.time.DP

####################################################
## I did not feel that... would be admired...
####################################################

## Not private
esteem.n.admiration<-1-mean(esteem.nonattend$foradmiration,na.rm=T)
control.n.admiration<-1-mean(control.nonattend$foradmiration,na.rm=T)
control.n.admiration-esteem.n.admiration

## Differentially private
esteem.n.admiration.DP<-0
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$foradmiration,
                        range=range(control.nonattend$foradmiration,na.rm=T))
  results34[i]<-1-release$release
  control.n.admiration.DP<-median(results34)
}
control.n.admiration.DP-esteem.n.admiration.DP

####################################################
## I am not interested in a leadership position...
####################################################

## Not private
esteem.n.leadership<-1-mean(esteem.nonattend$forleadership,na.rm=T)
control.n.leadership<-1-mean(control.nonattend$forleadership,na.rm=T)
control.n.leadership-esteem.n.leadership

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$forleadership,
                        range=range(esteem.nonattend$forleadership,na.rm=T))
  results35[i]<-1-release$release
  esteem.n.leadership.DP<-median(results35)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$forleadership,
                        range=range(control.nonattend$forleadership,na.rm=T))
  results35[i]<-1-release$release
  control.n.leadership.DP<-median(results35)
}
control.n.leadership.DP-esteem.n.leadership.DP

####################################################
## I did not think it was an important event
####################################################

## Not private
esteem.n.important<-1-mean(esteem.nonattend$forimportant,na.rm=T)
control.n.important<-1-mean(control.nonattend$forimportant,na.rm=T)
control.n.important-esteem.n.important

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=esteem.nonattend$forimportant,
                        range=range(esteem.nonattend$forimportant,na.rm=T))
  results36[i]<-1-release$release
  esteem.n.important.DP<-median(results36)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=control.nonattend$forimportant,
                        range=range(control.nonattend$forimportant,na.rm=T))
  results37[i]<-1-release$release
  control.n.important.DP<-median(results37)
}
control.n.important.DP-esteem.n.important.DP

#############
## Plots   ##
#############

##################################################################################################
## Histogram of 1,000 DP Estimates
##################################################################################################

par(mfrow=c(1,2))

h<-hist(results5-results1, breaks = 40, plot=FALSE)
h$counts=(h$counts/sum(h$counts))*100
plot(h,col="white",border="black",ylim=c(0,10),
     main="Distribution of DP\nIntended Participation Differential",
     xlab="DP Differential Estimate",ylab="Frequency of Estimate")
abline(v=pooled.ip-control.ip,col="darkgreen",lwd=3)
text(0.017,19,"True \n Value",col="darkgreen")

g<-hist(results6-results3, breaks = 40, plot=FALSE)
g$counts=(g$counts/sum(g$counts))*100
plot(g,col="white",border="black",ylim=c(0,10),
     main="Distribution of DP\nActual Participation Differential",
     xlab="DP Differential Estimate",ylab="Frequency of Estimate")
abline(v=pooled.ap-control.ap,col="darkgreen",lwd=3)
text(0.005,19,"True \n Value",col="darkgreen")

##################################################################################################
## Error over 1,000 Bootstraps
##################################################################################################

mybootstrap<-function(data){
  n<-nrow(data)
  index<-1:n
  bootindex<-sample(x=index,size=n,replace=TRUE)
  return(data[bootindex,])
}

lgbt.c<-subset(control,select=c(attended,intended))
lgbt.p<-subset(pooled,select=c(attended,intended))

x<-matrix(nrow=1217,ncol=2)
bootstraps5DP<-bootstraps5NP<-bootstraps6DP<-bootstraps6NP<-rep(NA,1000)

z<-matrix(nrow=2430,ncol=2)
bootstraps7DP<-bootstraps7NP<-bootstraps8DP<-bootstraps8NP<-rep(NA,1000)

for (i in 1:1000) {
  x<-mybootstrap(lgbt.c)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$intended,
                        range=range(x$intended))
  bootstraps5DP[i]<-release$release
  bootstraps5NP[i]<-mean(x$intended)
  
}

for (i in 1:1000) {
  x<-mybootstrap(lgbt.c)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$attended,
                        range=range(x$attended))
  bootstraps6DP[i]<-release$release
  bootstraps6NP[i]<-mean(x$attended)
}

for (i in 1:1000) {
  z<-mybootstrap(lgbt.p)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=z$intended,
                        range=range(z$intended))
  bootstraps7DP[i]<-release$release
  bootstraps7NP[i]<-mean(z$intended)
}

for (i in 1:1000) {
  z<-mybootstrap(lgbt.p)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=z$attended,
                        range=range(z$attended))
  bootstraps8DP[i]<-release$release
  bootstraps8NP[i]<-mean(z$attended)
}

error.ip<-(bootstraps7DP - bootstraps5DP) - (bootstraps7NP - bootstraps5NP)

error.ap<-(bootstraps8DP - bootstraps8NP) - (bootstraps6DP - bootstraps6NP)

par(mfrow=c(1,2))

plot(error.ap,main="Intended Participation\nError in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error",ylim=c(-0.042,0.042))
abline(h=mean(error.ap),col="darkgreen",lwd=3,lty=1)
text(130,0.0035,"Mean",col="white",cex=1.5)

plot(error.ip,main="Actual Participation\nError in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error",ylim=c(-0.042,0.042))
abline(h=mean(error.ip),col="darkgreen",lwd=3,lty=1)
text(130,0.0035,"Mean",col="white",cex=1.5)

##################################################################################################
## RMSE Across Epsilon Values
##################################################################################################

rmse <- function (orig_value, dp_value) {
  return(sqrt(mean((orig_value-dp_value)^2)))
}

eps<-seq(from=0.01,to=1,by=0.01)

x<-rep(NA,500)
list5<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list6<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list7<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list8<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=control$intended,
                          range=range(control$intended))
    list5[[i]][j]<-release$release
  } 
}
for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=pooled$intended,
                          range=range(pooled$intended))
    list6[[i]][j]<-release$release
  } 
}
for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=control$attended,
                          range=range(control$attended))
    list7[[i]][j]<-release$release
  } 
}
for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=pooled$attended,
                          range=range(pooled$attended))
    list8[[i]][j]<-release$release
  } 
}

np_ip<-pooled.ip-control.ip

error<-rep(NA,100)
for (i in 1:100) {
  c_ip<-list5[[i]]
  p_ip<-list6[[i]]
  dp_ip<-p_ip-c_ip
  error[i]<-rmse(np_ip,dp_ip)
}

np_ap<-pooled.ap-control.ap

error2<-rep(NA,100)
for (i in 1:100) {
  c_ap<-list7[[i]]
  p_ap<-list8[[i]]
  dp_ap<-p_ap-c_ap
  error2[i]<-rmse(np_ap,dp_ap)
}

par(mfrow=c(1,2))

plot(x=eps,y=error,main="RMSE for Intended \n Participation Across Epsilon",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error,col="darkgreen",lwd=4)
points(x=eps[10],y=error[10],cex=2,pch=19,col="black")
text(0.25,0.011,"Typical\nEpsilon",col="black",cex=0.8)

plot(x=eps,y=error2,main="RMSE for Actual \n Participation Across Epsilon",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error2,col="darkgreen",lwd=4)
points(x=eps[10],y=error2[10],cex=2,pch=19,col="black")
text(0.25,0.011,"Typical\nEpsilon",col="black",cex=0.8)