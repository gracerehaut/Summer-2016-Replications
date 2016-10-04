## Code for non-privacy protecting and differentially private replications of Weitz-Shapiro & Winters (2015)
## by Grace Rehaut (grehaut@princeton.edu)
## August 8, 2016

setwd("~/Desktop/Harvard/Final Paper")
source("./PrivateZelig-replication2/dpModules/Jack/DP_Means.R") 

##################################################################################################
## Loading in, preparing data
##################################################################################################

library(foreign)
load("~/Desktop/Harvard/Brazil/Brazil.RData") 
brazil<-x
brazil$vinheta <- as.factor(brazil$vinheta)
sims<-1000
results<-results1<-results2<-results3<-results4<-results5<-results6<-results7<-rep(NA,1000)
results8<-results9<-results10<-results11<-results12<-results13<-results14<-results15<-rep(NA,1000)
results16<-results17<-results18<-results19<-results20<-results21<-results22<-rep(NA,1000)

#############
## Table 2 ##
#############

##################################################################################################
## Column 1: Average Response
##################################################################################################

## Not private
vote.ca<-mean(brazil$voteintent[brazil$cred_vs_less==1],na.rm=T)
vote.lc<-mean(brazil$voteintent[brazil$cred_vs_less==0],na.rm=T)
vote.ns<-mean(brazil$voteintent[brazil$nosource_vs_pure==1],na.rm=T)
vote.pc<-mean(brazil$voteintent[brazil$nosource_vs_pure==0],na.rm=T)

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil$voteintent[brazil$cred_vs_less==1],
                        range=range(brazil$voteintent[brazil$cred_vs_less==1],na.rm=T))
  results[i]<-release$release
  vote.ca.DP<-median(results)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil$voteintent[brazil$cred_vs_less==0],
                        range=range(brazil$voteintent[brazil$cred_vs_less==0],na.rm=T))
  results2[i]<-release$release
  vote.lc.DP<-median(results2)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil$voteintent[brazil$nosource_vs_pure==1],
                        range=range(brazil$voteintent[brazil$nosource_vs_pure==1],na.rm=T))
  results3[i]<-release$release
  vote.ns.DP<-median(results3)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil$voteintent[brazil$nosource_vs_pure==0],
                        range=range(brazil$voteintent[brazil$nosource_vs_pure==0],na.rm=T))
  results4[i]<-release$release
  vote.pc.DP<-median(results4)
}

##################################################################################################
## Column 2: Estimated Difference from Control Conditions
##################################################################################################

## Not private
vote.ca-vote.pc
t.test(brazil$voteintent[brazil$cred==1],brazil$voteintent[brazil$nosource_vs_pure==0])
vote.lc-vote.pc
t.test(brazil$voteintent[brazil$cred==0],brazil$voteintent[brazil$nosource_vs_pure==0])
vote.ns-vote.pc
t.test(brazil$voteintent[brazil$nosource_vs_pure==1],brazil$voteintent[brazil$nosource_vs_pure==0])

## Differentially private
vote.ca.DP-vote.pc.DP
vote.lc.DP-vote.pc.DP
vote.ns.DP-vote.pc.DP

##################################################################################################
## Column 3: Estimated Difference from Unsourced Accusations
##################################################################################################

## Not private
vote.ca-vote.ns
t.test(brazil$voteintent[brazil$cred==1],brazil$voteintent[brazil$nosource_vs_pure==1])
vote.lc-vote.ns
t.test(brazil$voteintent[brazil$cred==0],brazil$voteintent[brazil$nosource_vs_pure==1])

## Differentially private
vote.ca.DP-vote.ns.DP
vote.lc.DP-vote.ns.DP

##################################################################################################
## Column 4: Estimated Difference from Less Credible Accusations
##################################################################################################

## Not private
vote.ca-vote.lc
t.test(brazil$voteintent[brazil$cred==1],brazil$voteintent[brazil$cred==0])

## Differentially private
vote.ca.DP-vote.lc.DP

#############
## Table 3 ##
#############

##################################################################################################
## Column 1:  Illiterate / less than primary [education]
##################################################################################################

## Not private
brazil.1<-subset(brazil,subset=(brazil$education==0))
vote.1.lc<-mean(brazil.1$voteintent[brazil.1$lesscred==1],na.rm=T)
vote.1.ca<-mean(brazil.1$voteintent[brazil.1$cred==1],na.rm=T)
vote.1.lc-vote.1.ca  
t.test(brazil.1$voteintent[brazil.1$lesscred==1],brazil.1$voteintent[brazil.1$cred==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.1$voteintent[brazil.1$cred_vs_less==0],
                        range=range(brazil.1$voteintent[brazil.1$cred_vs_less==0],na.rm=T))
  results5[i]<-release$release
  vote.1.lc.DP<-median(results5)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.1$voteintent[brazil.1$cred_vs_less==1],
                        range=range(brazil.1$voteintent[brazil.1$cred_vs_less==1],na.rm=T))
  results6[i]<-release$release
  vote.1.ca.DP<-median(results6)
}
vote.1.lc.DP-vote.1.ca.DP

##################################################################################################
## Column 2: Complete primary; incomplete middle [education]
##################################################################################################

## Not private
brazil.2<-subset(brazil,subset=(brazil$education==1))
vote.2.lc<-mean(brazil.2$voteintent[brazil.2$lesscred==1],na.rm=T)
vote.2.ac<-mean(brazil.2$voteintent[brazil.2$cred==1],na.rm=T)
vote.2.lc-vote.2.ac    
t.test(brazil.2$voteintent[brazil.2$lesscred==1],brazil.2$voteintent[brazil.2$cred==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.2$voteintent[brazil.2$cred_vs_less==0],
                        range=range(brazil.2$voteintent[brazil.2$cred_vs_less==0],na.rm=T))
  results7[i]<-release$release
  vote.2.lc.DP<-median(results7)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.2$voteintent[brazil.2$cred_vs_less==1],
                        range=range(brazil.2$voteintent[brazil.2$cred_vs_less==1],na.rm=T))
  results8[i]<-release$release
  vote.2.ca.DP<-median(results8)
}
vote.2.lc.DP-vote.2.ca.DP

##################################################################################################
## Column 3: Complete middle; incomplete secondary [education]
##################################################################################################

## Not private
brazil.3<-subset(brazil,subset=(brazil$education==2))
vote.3.lc<-mean(brazil.3$voteintent[brazil.3$lesscred==1],na.rm=T)
vote.3.ac<-mean(brazil.3$voteintent[brazil.3$cred==1],na.rm=T)
vote.3.lc-vote.3.ac
t.test(brazil.3$voteintent[brazil.3$lesscred==1],brazil.3$voteintent[brazil.3$cred==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.3$voteintent[brazil.3$cred_vs_less==0],
                        range=range(brazil.3$voteintent[brazil.3$cred_vs_less==0],na.rm=T))
  results9[i]<-release$release
  vote.3.lc.DP<-median(results9)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.3$voteintent[brazil.3$cred_vs_less==1],
                        range=range(brazil.3$voteintent[brazil.3$cred_vs_less==1],na.rm=T))
  results10[i]<-release$release
  vote.3.ca.DP<-median(results10)
}
vote.3.lc.DP-vote.3.ca.DP

##################################################################################################
## Column 4: Complete secondary [education]
##################################################################################################

## Not private
brazil.4<-subset(brazil,subset=(brazil$education==3))
vote.4.lc<-mean(brazil.4$voteintent[brazil.4$lesscred==1],na.rm=T)
vote.4.ac<-mean(brazil.4$voteintent[brazil.4$cred==1],na.rm=T)
vote.4.lc-vote.4.ac
t.test(brazil.4$voteintent[brazil.4$lesscred==1],brazil.4$voteintent[brazil.4$cred==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.4$voteintent[brazil.4$cred_vs_less==0],
                        range=range(brazil.4$voteintent[brazil.4$cred_vs_less==0],na.rm=T))
  results11[i]<-release$release
  vote.4.lc.DP<-median(results11)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.4$voteintent[brazil.4$cred_vs_less==1],
                        range=range(brazil.4$voteintent[brazil.4$cred_vs_less==1],na.rm=T))
  results12[i]<-release$release
  vote.4.ca.DP<-median(results12)
}
vote.4.lc.DP-vote.4.ca.DP

##################################################################################################
## Column 5: At least some tertiary [education]
##################################################################################################

## Not private
brazil.5<-subset(brazil,subset=(brazil$education==4))
vote.5.lc<-mean(brazil.5$voteintent[brazil.5$lesscred==1],na.rm=T)
vote.5.ac<-mean(brazil.5$voteintent[brazil.5$cred==1],na.rm=T)
vote.5.lc-vote.5.ac
t.test(brazil.5$voteintent[brazil.5$lesscred==1],brazil.5$voteintent[brazil.5$cred==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.5$voteintent[brazil.5$cred_vs_less==0],
                        range=range(brazil.5$voteintent[brazil.5$cred_vs_less==0],na.rm=T))
  results12[i]<-release$release
  vote.5.lc.DP<-median(results12)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.5$voteintent[brazil.5$cred_vs_less==1],
                        range=range(brazil.5$voteintent[brazil.5$cred_vs_less==1],na.rm=T))
  results13[i]<-release$release
  vote.5.ca.DP<-median(results13)
}
vote.5.lc.DP-vote.5.ca.DP

##############
## Table 3a ##
##############

##################################################################################################
## Row 1: Less credible accusations
##################################################################################################

## Not private
brazil.lc<-subset(brazil,subset=(brazil$lesscred==1))
vote.01.lc<-mean(brazil.lc$voteintent[brazil.lc$education<=1],na.rm=T)
vote.234.lc<-mean(brazil.lc$voteintent[brazil.lc$education>=2],na.rm=T)
vote.234.lc-vote.01.lc
t.test(brazil.lc$voteintent[brazil.lc$education<=1],brazil.lc$voteintent[brazil.lc$education>=2])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.lc$voteintent[brazil.lc$education<=1],
                        range=range(brazil.lc$voteintent[brazil.lc$education<=1],na.rm=T))
  results13[i]<-release$release
  vote.01.lc.DP<-median(results13)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.lc$voteintent[brazil.lc$education>=2],
                        range=range(brazil.lc$voteintent[brazil.lc$education>=2],na.rm=T))
  results14[i]<-release$release
  vote.234.lc.DP<-median(results14)
}
vote.234.lc.DP-vote.01.lc.DP

##################################################################################################
## Row 2: More credible accusations
##################################################################################################

## Not private
brazil.ca<-subset(brazil,subset=(brazil$cred==1))
vote.01.ca<-mean(brazil.ca$voteintent[brazil.ca$education<=1],na.rm=T)
vote.234.ca<-mean(brazil.ca$voteintent[brazil.ca$education>=2],na.rm=T)
vote.234.ca-vote.01.ca
t.test(brazil.ca$voteintent[brazil.ca$education<=1],brazil.ca$voteintent[brazil.ca$education>=2])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.ca$voteintent[brazil.ca$education<=1],
                        range=range(brazil.ca$voteintent[brazil.ca$education<=1],na.rm=T))
  results15[i]<-release$release
  vote.01.ca.DP<-median(results15)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.ca$voteintent[brazil.ca$education>=2],
                        range=range(brazil.ca$voteintent[brazil.ca$education>=2],na.rm=T))
  results16[i]<-release$release
  vote.234.ca.DP<-median(results16)
}
vote.234.ca.DP-vote.01.ca.DP

#############
## Table 4 ##
#############

##################################################################################################
## Column 1: No questions right
##################################################################################################

## Not private
brazil.no<-subset(brazil,subset=(brazil$polknowledge==0))
vote.no.lc<-mean(brazil.no$voteintent[brazil.no$cred_vs_less==0],na.rm=T)
vote.no.ca<-mean(brazil.no$voteintent[brazil.no$cred_vs_less==1],na.rm=T)
vote.no.lc-vote.no.ca
t.test(brazil.no$voteintent[brazil.no$cred_vs_less==0],brazil.no$voteintent[brazil.no$cred_vs_less==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.no$voteintent[brazil.no$cred_vs_less==0],
                        range=range(brazil.no$voteintent[brazil.no$cred_vs_less==0],na.rm=T))
  results17[i]<-release$release
  vote.no.lc.DP<-median(results17)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.no$voteintent[brazil.no$cred_vs_less==1],
                        range=range(brazil.no$voteintent[brazil.no$cred_vs_less==1],na.rm=T))
  results18[i]<-release$release
  vote.no.ca.DP<-median(results18)
}
vote.no.lc.DP-vote.no.ca.DP

##################################################################################################
## Column 2: One question right
##################################################################################################

## Not private
brazil.one<-subset(brazil,subset=(brazil$polknowledge==1))
vote.one.lc<-mean(brazil.one$voteintent[brazil.one$cred_vs_less==0],na.rm=T)
vote.one.ca<-mean(brazil.one$voteintent[brazil.one$cred_vs_less==1],na.rm=T)
vote.one.lc-vote.one.ca
t.test(brazil.one$voteintent[brazil.one$cred_vs_less==0],brazil.one$voteintent[brazil.one$cred_vs_less==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.one$voteintent[brazil.one$cred_vs_less==0],
                        range=range(brazil.one$voteintent[brazil.one$cred_vs_less==0],na.rm=T))
  results19[i]<-release$release
  vote.one.lc.DP<-median(results19)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.one$voteintent[brazil.one$cred_vs_less==1],
                        range=range(brazil.one$voteintent[brazil.one$cred_vs_less==1],na.rm=T))
  results20[i]<-release$release
  vote.one.ca.DP<-median(results20)
}
vote.one.lc.DP-vote.one.ca.DP

##################################################################################################
## Column 3: Both questions right
##################################################################################################

## Not private
brazil.two<-subset(brazil,subset=(brazil$polknowledge==2))
vote.two.lc<-mean(brazil.two$voteintent[brazil.two$cred_vs_less==0],na.rm=T)
vote.two.ca<-mean(brazil.two$voteintent[brazil.two$cred_vs_less==1],na.rm=T)
vote.two.lc-vote.two.ca
t.test(brazil.two$voteintent[brazil.two$cred_vs_less==0],brazil.two$voteintent[brazil.two$cred_vs_less==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.two$voteintent[brazil.two$cred_vs_less==0],
                        range=range(brazil.two$voteintent[brazil.two$cred_vs_less==0],na.rm=T))
  results21[i]<-release$release
  vote.two.lc.DP<-median(results21)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=brazil.two$voteintent[brazil.two$cred_vs_less==1],
                        range=range(brazil.two$voteintent[brazil.two$cred_vs_less==1],na.rm=T))
  results22[i]<-release$release
  vote.two.ca.DP<-median(results22)
}
vote.two.lc.DP-vote.two.ca.DP

##################################################################################################
## Column 4/5: Difference between Lowest and Highest Knowledge
##################################################################################################

## Not private
vote.two.lc-vote.no.lc
t.test(brazil.two$voteintent[brazil.two$cred_vs_less==0],brazil.no$voteintent[brazil.no$cred_vs_less==0])
vote.two.ca-vote.no.ca
t.test(brazil.two$voteintent[brazil.two$cred_vs_less==1],brazil.no$voteintent[brazil.no$cred_vs_less==1])

## Differentially private
vote.two.lc.DP-vote.no.lc.DP
vote.two.ca.DP-vote.no.ca.DP

#############
## Plots   ##
#############

##################################################################################################
## Histogram of 1,000 DP Estimates
##################################################################################################

par(mfrow=c(1,2))

h<-hist(results-results4, breaks = 40, plot=FALSE)
h$counts=(h$counts/sum(h$counts))*100
plot(h,col="white",border="black",ylim=c(0,17),
     main="Distribution of DP\nDiff. for Credible Accusations",
     xlab="DP Differential Estimate",ylab="Frequency of Estimate")
abline(v=vote.ca-vote.pc,col="purple",lwd=3)
text(-1.5,14,"True \n Value",col="purple")

g<-hist(results2-results4, breaks = 40, plot=FALSE)
g$counts=(g$counts/sum(g$counts))*100
plot(g,col="white",border="black",ylim=c(0,17),
     main="Distribution of DP\nDiff. for Less Credible Accusations",
     xlab="DP Differential Estimate",ylab="Frequency of Estimate")
abline(v=vote.lc-vote.pc,col="purple",lwd=3)
text(-1.2,14,"True \n Value",col="purple")

##################################################################################################
## Error over 1,000 Bootstraps
##################################################################################################

mybootstrap<-function(data){
  n<-nrow(data)
  index<-1:n
  bootindex<-sample(x=index,size=n,replace=TRUE)
  return(data[bootindex,])
}

bra.zil<-subset(brazil,select=c(voteintent,cred_vs_less,nosource_vs_pure))

x<-matrix(nrow=2002,ncol=3)
bootstraps1DP<-bootstraps1NP<-bootstraps2DP<-bootstraps2NP<-rep(NA,1000)
bootstraps3DP<-bootstraps3NP<-bootstraps4DP<-bootstraps4NP<-rep(NA,1000)

for (i in 1:1000) {
  x<-mybootstrap(bra.zil)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$voteintent[x$cred_vs_less==1],
                        range=range(x$voteintent[x$cred_vs_less==1],na.rm=T))
  bootstraps1DP[i]<-release$release
  bootstraps1NP[i]<-mean(x$voteintent[x$cred_vs_less==1],na.rm=T)
}

for (i in 1:1000) {
  x<-mybootstrap(bra.zil)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$voteintent[x$cred_vs_less==0],
                        range=range(x$voteintent[x$cred_vs_less==0],na.rm=T))
  bootstraps2DP[i]<-release$release
  bootstraps2NP[i]<-mean(x$voteintent[x$cred_vs_less==0],na.rm=T)
}

error.cred<-(bootstraps1DP - bootstraps2DP) - (bootstraps1NP - bootstraps2NP)

for (i in 1:1000) {
  x<-mybootstrap(bra.zil)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$voteintent[x$nosource_vs_pure==0],
                        range=range(x$voteintent[x$nosource_vs_pure==0],na.rm=T))
  bootstraps4DP[i]<-release$release
  bootstraps4NP[i]<-mean(x$voteintent[x$nosource_vs_pure==0],na.rm=T)
}

error.lesscred<-(bootstraps1DP - bootstraps4DP) - (bootstraps1NP - bootstraps4NP)

par(mfrow=c(1,2))

plot(error.cred,main="DP Diff for Credible\nAcc. in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error",ylim=c(-0.6,0.62))
abline(h=mean(error.cred),col="purple",lwd=3,lty=1)
text(180,0.035,"Mean",col="white",cex=1.5)

plot(error.lesscred,main="DP Diff for Less Credible\nAcc. in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error",ylim=c(-0.6,0.62))
abline(h=mean(error.lesscred),col="purple",lwd=3,lty=1)
text(180,0.035,"Mean",col="white",cex=1.5)

##################################################################################################
## RMSE Across Epsilon Values
##################################################################################################

rmse <- function (orig_value, dp_value) {
  return(sqrt(mean((orig_value-dp_value)^2)))
}

eps<-seq(from=0.01,to=1,by=0.01)

x<-rep(NA,500)
list1<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list2<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list4<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=brazil$voteintent[brazil$cred_vs_less==1],
                          range=range(brazil$voteintent[brazil$cred_vs_less==1],na.rm=T))
    list1[[i]][j]<-release$release
  } 
}
for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=brazil$voteintent[brazil$cred_vs_less==0],
                          range=range(brazil$voteintent[brazil$cred_vs_less==0],na.rm=T))
    list2[[i]][j]<-release$release
  } 
}
for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=brazil$voteintent[brazil$nosource_vs_pure==0],
                          range=range(brazil$voteintent[brazil$nosource_vs_pure==0],na.rm=T))
    list4[[i]][j]<-release$release
  } 
}

np_cred<-vote.ca-vote.pc

error<-rep(NA,100)
for (i in 1:100) {
  c_cred<-list1[[i]]
  p_cred<-list4[[i]]
  dp_cred<-c_cred-p_cred
  error[i]<-rmse(np_cred,dp_cred)
}

np_lesscred<-vote.lc-vote.pc

error2<-rep(NA,100)
for (i in 1:100) {
  c_lesscred<-list2[[i]]
  p_lesscred<-list4[[i]]
  dp_lc<-c_lesscred-p_lesscred
  error2[i]<-rmse(np_lesscred,dp_lc)
}

par(mfrow=c(1,2))

plot(x=eps,y=error,main="RMSE for Credible \n Accusations Across Epsilon",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error,col="purple",lwd=4)
points(x=eps[10],y=error[10],cex=2,pch=19,col="black")
text(0.29,0.12,"Typical\nEpsilon",col="black",cex=0.9)

plot(x=eps,y=error2,main="RMSE for Less Credible \n Accusations Across Epsilon",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error2,col="purple",lwd=4)
points(x=eps[10],y=error2[10],cex=2,pch=19,col="black")
text(0.29,0.12,"Typical\nEpsilon",col="black",cex=0.9)