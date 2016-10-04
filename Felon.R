## Code for non-privacy protecting and differentially private replications of Gerber et al (2015)
## by Grace Rehaut (grehaut@princeton.edu)
## August 8, 2016

setwd("~/Desktop/Harvard/Final Paper")
source("./PrivateZelig-replication2/dpModules/Jack/DP_Means.R") 

##################################################################################################
## Loading in, preparing data
##################################################################################################

library(foreign)
setwd("~/Desktop/Harvard/Felon")
felon<-read.dta("felon.dta")
felon$registered<-ifelse(felon$registered=="No",0,1)
felon$v_pres_general_12<-ifelse(felon$v_pres_general_12=="No",0,1)

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
## Column 1: Registration and turnout among control group
##################################################################################################

## Not private
reg.c<-mean(felon$registered[felon$treat_combined==0])
turn.c<-mean(felon$v_pres_general_12[felon$treat_combined==0],na.rm=TRUE)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$registered[felon$treat_combined==0],
                        range=range(felon$registered[felon$treat_combined==0]))
  results[i]<-release$release
  reg.c.DP<-median(results)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$v_pres_general_12[felon$treat_combined==0],
                        range=range(felon$v_pres_general_12[felon$treat_combined==0],na.rm=F))
  results2[i]<-release$release
  turn.c.DP<-median(results2)
}

##################################################################################################
## Column 2: Registration and turnout among assurance group
##################################################################################################

## Not private
reg.a<-mean(felon$registered[felon$treat1==1])
reg.a-reg.c
turn.a<-mean(felon$v_pres_general_12[felon$treat1==1],na.rm=TRUE)
turn.a-turn.c

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$registered[felon$treat1==1],
                        range=range(felon$registered[felon$treat1==1]))
  results3[i]<-release$release
  reg.a.DP<-median(results3)
}
reg.a.DP-reg.c.DP
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$v_pres_general_12[felon$treat1==1],
                        range=range(felon$v_pres_general_12[felon$treat1==1]))
  results4[i]<-release$release
  turn.a.DP<-median(results4)
}
turn.a.DP-turn.c.DP

##################################################################################################
## Column 3: Registration and turnout among expanded assurance group
##################################################################################################

## Not private
reg.ea<-mean(felon$registered[felon$treat2_felon==1])
reg.ea-reg.c
turn.ea<-mean(felon$v_pres_general_12[felon$treat2_felon==1],na.rm=TRUE)
turn.ea-turn.c

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$registered[felon$treat2_felon==1],
                        range=range(felon$registered[felon$treat2_felon==1]))
  results5[i]<-release$release
  reg.ea.DP<-median(results5)
}
reg.ea.DP-reg.c.DP
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$v_pres_general_12[felon$treat2_felon==1],
                        range=range(felon$v_pres_general_12[felon$treat2_felon==1]))
  results6[i]<-release$release
  turn.ea.DP<-median(results6)
}
turn.ea.DP-turn.c.DP

##################################################################################################
## Column 4: Registration and turnout among pooled assurance group
##################################################################################################

## Not private
reg.pa<-mean(felon$registered[felon$treat_combined==1])
reg.pa-reg.c
turn.pa<-mean(felon$v_pres_general_12[felon$treat_combined==1],na.rm=TRUE)
turn.pa-turn.c

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$registered[felon$treat_combined==1],
                        range=range(felon$registered[felon$treat_combined==1]))
  results7[i]<-release$release
  reg.pa.DP<-median(results7)
}
reg.pa.DP-reg.c.DP
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$v_pres_general_12[felon$treat_combined==1],
                        range=range(felon$v_pres_general_12[felon$treat_combined==1]))
  results8[i]<-release$release
  turn.pa.DP<-median(results8)
}
turn.pa.DP-turn.c.DP

##################################################################################################
## Column 4: Registration and turnout among pooled assurance (mail returned) group
##################################################################################################

## Not private
reg.pa.nr<-mean(felon$registered[felon$treat_combined==1 & felon$returned=="No"])
turn.pa.nr<-mean(felon$v_pres_general_12[felon$treat_combined==1 & felon$returned=="No"])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$registered[felon$treat_combined==1 & felon$returned=="No"],
                        range=range(felon$registered[felon$treat_combined==1  & felon$returned=="No"]))
  results10[i]<-release$release
  reg.pa.nr.DP<-median(results10)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=felon$v_pres_general_12[felon$treat_combined==1 & felon$returned=="No"],
                        range=range(felon$v_pres_general_12[felon$treat_combined==1  & felon$returned=="No"],na.rm=TRUE))
  results11[i]<-release$release
  turn.pa.nr.DP<-median(results11)
}

#############
## Plots   ##
#############

##################################################################################################
## Histogram of 1,000 DP Estimates
##################################################################################################

h<-hist(results7-results, breaks = 50, plot=FALSE)
h$counts=(h$counts/sum(h$counts))*100
plot(h,col="white",border="black",ylim=c(0,18),
     main="Plot 5: Distribution of DP \n  Estimates of Voter Registration Differential",
     xlab="Differentially Private Voter Registration Differential Estimate",ylab="Frequency of Estimate")
abline(v=reg.pa-reg.c,col="dodgerblue",lwd=3)
text(0.01790,17,"True \n Value",col="dodgerblue")

##################################################################################################
## Error over 1,000 Bootstraps
##################################################################################################

mybootstrap<-function(data){
  n<-nrow(data)
  index<-1:n
  bootindex<-sample(x=index,size=n,replace=TRUE)
  return(data[bootindex,])
}

fe.lon<-subset(felon,select=c(registered,treat_combined))

x<-matrix(nrow=6441,ncol=2)
bootstraps3DP<-bootstraps3NP<-bootstraps4DP<-bootstraps4NP<-rep(NA,1000)

for (i in 1:1000) {
  x<-mybootstrap(fe.lon)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$registered[x$treat_combined==0],
                        range=range(x$registered[x$treat_combined==0]))
  results3<-release$release
  bootstraps3DP[i]<-results3
  results4<-mean(x$registered[x$treat_combined==0])
  bootstraps3NP[i]<-results4
}

for (i in 1:1000) {
  x<-mybootstrap(fe.lon)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$registered[x$treat_combined==1],
                        range=range(x$registered[x$treat_combined==1]))
  results5<-release$release
  bootstraps4DP[i]<-results5
  results6<-mean(x$registered[x$treat_combined==1])
  bootstraps4NP[i]<-results6
}

DP.bs<-bootstraps4DP - bootstraps3DP
NP.bs<-bootstraps4NP - bootstraps3NP
error.bs<-DP.bs-NP.bs

plot(error.bs,main="Plot 6: Registration Differential Error in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error")
abline(h=mean(error.bs),col="dodgerblue",lwd=3,lty=1)
text(50,0.00005,"Mean",col="dodgerblue",cex=1.5)

##################################################################################################
## RMSE Across Epsilon Values
##################################################################################################

rmse <- function (orig_value, dp_value) {
  return(sqrt(mean((orig_value-dp_value)^2)))
}

eps<-seq(from=0.01,to=1,by=0.01)

x<-rep(NA,500)
list3<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list4<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=felon$registered[felon$treat_combined==0],
                          range=range(felon$registered[felon$treat_combined==0]))
    list3[[i]][j]<-release$release
  } 
}

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=felon$registered[felon$treat_combined==1],
                          range=range(felon$registered[felon$treat_combined==1]))
    list4[[i]][j]<-release$release
  } 
}

np2<-reg.pa-reg.c

error<-rep(NA,100)
for (i in 1:100) {
  c_reg<-list3[[i]]
  pa_reg<-list4[[i]]
  dp2<-pa_reg-c_reg
  error[i]<-rmse(np2,dp2)
}

plot(x=eps,y=error,main="Plot 7: RMSE for Registration Differential \n Calculation Across Epsilon Values",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error,col="dodgerblue",lwd=4)
points(x=eps[10],y=error[10],cex=2,pch=19,col="black")
text(0.19,0.000015,"Typical\nEpsilon",col="black")