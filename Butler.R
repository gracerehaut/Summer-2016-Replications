## Code for non-privacy protecting and differentially private replications of Butler & Broockman (2011)
## by Grace Rehaut (grehaut@princeton.edu)
## August 6, 2016

setwd("~/Desktop/Harvard/Final Paper")
source("./PrivateZelig-replication2/dpModules/Jack/DP_Means.R") 

##################################################################################################
## Loading in, preparing data
##################################################################################################

set.seed(1234)
sims<-1000
results<-results1<-results2<-results3<-results4<-results5<-results6<-results7<-results8<-results9<-rep(NA,1000)
results10<-results11<-results12<-results13<-results14<-results15<-results16<-results17<-results18<-rep(NA,1000)
results19<-results20<-results21<-results22<-results23<-results24<-results25<-results26<-results27<-rep(NA,1000)
results28<-results29<-results30<-results31<-results32<-results33<-results34<-results35<-results36<-results37<-rep(NA,1000)

setwd("~/Desktop/Harvard/Butler")
butler<-read.csv("Butler.csv")

#############
## Table 1 ##
#############

##################################################################################################
## No partisanship signal
##################################################################################################

butler.no<-subset(butler,subset=(butler$treat_noprimary==1))

## Not private
d.no<-mean(butler.no$reply_atall[butler.no$treat_deshawn==1])
j.no<-mean(butler.no$reply_atall[butler.no$treat_jake==1])
d.no-j.no
t.test(butler.no$reply_atall[butler.no$treat_deshawn==1],butler.no$reply_atall[butler.no$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.no$reply_atall[butler.no$treat_deshawn==1],
                       range=range(butler.no$reply_atall[butler.no$treat_deshawn==1]))
  results[i]<-release$release
  d.no.DP<-median(results)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.no$reply_atall[butler.no$treat_jake==1],
                        range=range(butler.no$reply_atall[butler.no$treat_jake==1]))
  results1[i]<-release$release
  j.no.DP<-median(results1)
}
d.no.DP-j.no.DP

##################################################################################################
## Republican  signal
##################################################################################################

butler.rep<-subset(butler,subset=(butler$treat_repprimary==1))

## Not private
d.rep<-mean(butler.rep$reply_atall[butler.rep$treat_deshawn==1])
j.rep<-mean(butler.rep$reply_atall[butler.rep$treat_jake==1])
d.rep-j.rep
t.test(butler.rep$reply_atall[butler.rep$treat_deshawn==1],butler.rep$reply_atall[butler.rep$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.rep$reply_atall[butler.rep$treat_deshawn==1],
                        range=range(butler.rep$reply_atall[butler.rep$treat_deshawn==1]))
  results2[i]<-release$release
  d.rep.DP<-median(results2)
}
j.rep.DP.sim<-rep(NA,1000)
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.rep$reply_atall[butler.rep$treat_jake==1],
                        range=range(butler.rep$reply_atall[butler.rep$treat_jake==1]))
  results3[i]<-release$release
  j.rep.DP<-median(results3)
}
d.rep.DP-j.rep.DP

##################################################################################################
## Democratic signal
##################################################################################################

butler.dem<-subset(butler,subset=(butler$treat_demprimary==1))

## Not private
d.dem<-mean(butler.dem$reply_atall[butler.dem$treat_deshawn==1])
j.dem<-mean(butler.dem$reply_atall[butler.dem$treat_jake==1])
d.dem-j.dem
t.test(butler.dem$reply_atall[butler.dem$treat_deshawn==1],butler.dem$reply_atall[butler.dem$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.dem$reply_atall[butler.dem$treat_deshawn==1],
                        range=range(butler.dem$reply_atall[butler.dem$treat_deshawn==1]))
  results4[i]<-release$release
  d.dem.DP<-median(results4)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.dem$reply_atall[butler.dem$treat_jake==1],
                        range=range(butler.dem$reply_atall[butler.dem$treat_jake==1]))
  results5[i]<-release$release
  j.dem.DP<-median(results5)
}
d.dem.DP-j.dem.DP

##################################################################################################
## Party differential
##################################################################################################

## Not private
d.rep-d.dem
t.test(butler$reply_atall[butler$treat_deshawn==1 & butler$treat_repprimary==1],butler$reply_atall[butler$treat_deshawn==1 & butler$treat_demprimary==1])
j.rep-j.dem
t.test(butler$reply_atall[butler$treat_jake==1 & butler$treat_repprimary==1],butler$reply_atall[butler$treat_jake==1 & butler$treat_demprimary==1])
      # This above result (0.64) is not what the study reports, but I think it is their mistake and not mine.
mean(butler$reply_atall[butler$treat_repprimary==1]) - mean(butler$reply_atall[butler$treat_demprimary==1])
t.test(butler$reply_atall[butler$treat_repprimary==1],butler$reply_atall[butler$treat_demprimary==1])

## Differentially private
d.rep.DP-d.dem.DP
j.rep.DP-j.dem.DP

for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.rep$reply_atall,range=range(butler.rep$reply_atall))
  results6[i]<-release$release
  overall.rep.DP<-median(results6)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.dem$reply_atall,range=range(butler.dem$reply_atall))
  results7[i]<-release$release
  overall.dem.DP<-median(results7)
}
overall.rep.DP-overall.dem.DP

##################################################################################################
## Combined effect
##################################################################################################

butler.yes<-subset(butler,subset=(butler$treat_noprimary==0))

## Not private
d.yes<-mean(butler.yes$reply_atall[butler.yes$treat_deshawn==1])
j.yes<-mean(butler.yes$reply_atall[butler.yes$treat_jake==1])
d.yes-j.yes
t.test(butler.yes$reply_atall[butler.yes$treat_deshawn==1],butler.yes$reply_atall[butler.yes$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.yes$reply_atall[butler.yes$treat_deshawn==1],
                        range=range(butler.yes$reply_atall[butler.yes$treat_deshawn==1]))
  results8[i]<-release$release
  d.yes.DP<-median(results8)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.yes$reply_atall[butler.yes$treat_jake==1],
                        range=range(butler.yes$reply_atall[butler.yes$treat_jake==1]))
  results9[i]<-release$release
  j.yes.DP<-median(results9)
}
d.yes.DP-j.yes.DP

##############
## Table 2a ##
##############

butler.r<-subset(butler,subset=(butler$leg_party=="R"))

##################################################################################################
## No partisanship signal
##################################################################################################

butler.r.no<-subset(butler.r,subset=(butler.r$treat_noprimary==1))

## Not private
d.r.no<-mean(butler.r.no$reply_atall[butler.r.no$treat_deshawn==1])
j.r.no<-mean(butler.r.no$reply_atall[butler.r.no$treat_jake==1])
d.r.no-j.r.no
t.test(butler.r.no$reply_atall[butler.r.no$treat_deshawn==1],butler.r.no$reply_atall[butler.r.no$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.no$reply_atall[butler.r.no$treat_deshawn==1],
                        range=range(butler.r.no$reply_atall[butler.r.no$treat_deshawn==1]))
  results10[i]<-release$release
  d.r.no.DP<-median(results10)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.no$reply_atall[butler.r.no$treat_jake==1],
                        range=range(butler.r.no$reply_atall[butler.r.no$treat_jake==1]))
  results11[i]<-release$release
  j.r.no.DP<-median(results11)
}
d.r.no.DP-j.r.no.DP

##################################################################################################
## Republican signal
##################################################################################################

butler.r.rep<-subset(butler.r,subset=(butler.r$treat_repprimary==1))

## Not private
d.r.rep<-mean(butler.r.rep$reply_atall[butler.r.rep$treat_deshawn==1])
j.r.rep<-mean(butler.r.rep$reply_atall[butler.r.rep$treat_jake==1])
d.r.rep-j.r.rep
t.test(butler.r.rep$reply_atall[butler.r.rep$treat_deshawn==1],butler.r.rep$reply_atall[butler.r.rep$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.rep$reply_atall[butler.r.rep$treat_deshawn==1],
                        range=range(butler.r.rep$reply_atall[butler.r.rep$treat_deshawn==1]))
  results12[i]<-release$release
  d.r.rep.DP<-median(results12)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.rep$reply_atall[butler.r.rep$treat_jake==1],
                        range=range(butler.r.rep$reply_atall[butler.r.rep$treat_jake==1]))
  results13[i]<-release$release
  j.r.rep.DP<-median(results13)
}
d.r.rep.DP-j.r.rep.DP

##################################################################################################
## Democratic signal
##################################################################################################

butler.r.dem<-subset(butler.r,subset=(butler.r$treat_demprimary==1))

## Not private
d.r.dem<-mean(butler.r.dem$reply_atall[butler.r.dem$treat_deshawn==1])
j.r.dem<-mean(butler.r.dem$reply_atall[butler.r.dem$treat_jake==1])
d.r.dem-j.r.dem
t.test(butler.r.dem$reply_atall[butler.r.dem$treat_deshawn==1],butler.r.dem$reply_atall[butler.r.dem$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.dem$reply_atall[butler.r.dem$treat_deshawn==1],
                        range=range(butler.r.dem$reply_atall[butler.r.dem$treat_deshawn==1]))
  results14[i]<-release$release
  d.r.dem.DP<-median(results14)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.dem$reply_atall[butler.r.dem$treat_jake==1],
                        range=range(butler.r.dem$reply_atall[butler.r.dem$treat_jake==1]))
  results15[i]<-release$release
  j.r.dem.DP<-median(results15)
}
d.r.dem.DP-j.r.dem.DP

##################################################################################################
## Party differential
##################################################################################################

## Not private
d.r.rep-d.r.dem
t.test(butler.r.rep$reply_atall[butler.r.rep$treat_deshawn==1],butler.r.dem$reply_atall[butler.r.dem$treat_deshawn==1])
j.r.rep-j.r.dem
t.test(butler.r.rep$reply_atall[butler.r.rep$treat_jake==1],butler.r.dem$reply_atall[butler.r.dem$treat_jake==1])
mean(butler.r$reply_atall[butler.r$treat_repprimary==1]) - mean(butler.r$reply_atall[butler.r$treat_demprimary==1])
t.test(butler.r$reply_atall[butler.r$treat_repprimary==1],butler.r$reply_atall[butler.r$treat_demprimary==1])

## Differentially private
d.r.rep.DP-d.r.dem.DP
j.r.rep.DP-j.r.dem.DP

for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.rep$reply_atall,range=range(butler.r.rep$reply_atall))
  results16[i]<-release$release
  overall.r.rep.DP<-median(results16)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.dem$reply_atall,range=range(butler.r.dem$reply_atall))
  results17[i]<-release$release
  overall.r.dem.DP<-median(results17)
}
overall.r.rep.DP-overall.r.dem.DP

##################################################################################################
## Combined effect
##################################################################################################

butler.r.yes<-subset(butler.r,subset=(butler.r$treat_noprimary==0))

## Not private
d.r.yes<-mean(butler.r.yes$reply_atall[butler.r.yes$treat_deshawn==1])
j.r.yes<-mean(butler.r.yes$reply_atall[butler.r.yes$treat_jake==1])
d.r.yes-j.r.yes
t.test(butler.r.yes$reply_atall[butler.r.yes$treat_deshawn==1],butler.r.yes$reply_atall[butler.r.yes$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.yes$reply_atall[butler.r.yes$treat_deshawn==1],
                        range=range(butler.r.yes$reply_atall[butler.r.yes$treat_deshawn==1]))
  results18[i]<-release$release
  d.r.yes.DP<-median(results18)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.yes$reply_atall[butler.r.yes$treat_jake==1],
                        range=range(butler.r.yes$reply_atall[butler.r.yes$treat_jake==1]))
  results19[i]<-release$release
  j.r.yes.DP<-median(results19)
}
d.r.yes.DP-j.r.yes.DP

##############
## Table 2b ##
##############

butler.d<-subset(butler,subset=(butler$leg_party=="D"))

##################################################################################################
## No partisanship signal
##################################################################################################

butler.d.no<-subset(butler.d,subset=(butler.d$treat_noprimary==1))

## Not private
d.d.no<-mean(butler.d.no$reply_atall[butler.d.no$treat_deshawn==1])
j.d.no<-mean(butler.d.no$reply_atall[butler.d.no$treat_jake==1])
d.d.no-j.d.no
t.test(butler.d.no$reply_atall[butler.d.no$treat_deshawn==1],butler.d.no$reply_atall[butler.d.no$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.no$reply_atall[butler.d.no$treat_deshawn==1],
                        range=range(butler.d.no$reply_atall[butler.d.no$treat_deshawn==1]))
  results20[i]<-release$release
  d.d.no.DP<-median(results20)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.no$reply_atall[butler.d.no$treat_jake==1],
                        range=range(butler.d.no$reply_atall[butler.d.no$treat_jake==1]))
  results21[i]<-release$release
  j.d.no.DP<-median(results21)
}
d.d.no.DP-j.d.no.DP

##################################################################################################
## Republican signal
##################################################################################################

butler.d.rep<-subset(butler.d,subset=(butler.d$treat_repprimary==1))

## Not private
d.d.rep<-mean(butler.d.rep$reply_atall[butler.d.rep$treat_deshawn==1])
j.d.rep<-mean(butler.d.rep$reply_atall[butler.d.rep$treat_jake==1])
d.d.rep-j.d.rep
t.test(butler.d.rep$reply_atall[butler.d.rep$treat_deshawn==1],butler.d.rep$reply_atall[butler.d.rep$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.rep$reply_atall[butler.d.rep$treat_deshawn==1],
                        range=range(butler.d.rep$reply_atall[butler.d.rep$treat_deshawn==1]))
  results22[i]<-release$release
  d.d.rep.DP<-median(results22)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.rep$reply_atall[butler.d.rep$treat_jake==1],
                        range=range(butler.d.rep$reply_atall[butler.d.rep$treat_jake==1]))
  results23[i]<-release$release
  j.d.rep.DP<-median(results23)
}
d.d.rep.DP-j.d.rep.DP

##################################################################################################
## Democratic signal
##################################################################################################

butler.d.dem<-subset(butler.d,subset=(butler.d$treat_demprimary==1))

## Not private
d.d.dem<-mean(butler.d.dem$reply_atall[butler.d.dem$treat_deshawn==1])
j.d.dem<-mean(butler.d.dem$reply_atall[butler.d.dem$treat_jake==1])
d.d.dem-j.d.dem
t.test(butler.d.dem$reply_atall[butler.d.dem$treat_deshawn==1],butler.d.dem$reply_atall[butler.d.dem$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.dem$reply_atall[butler.d.dem$treat_deshawn==1],
                        range=range(butler.d.dem$reply_atall[butler.d.dem$treat_deshawn==1]))
  results24[i]<-release$release
  d.d.dem.DP<-median(results24)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.dem$reply_atall[butler.d.dem$treat_jake==1],
                        range=range(butler.d.dem$reply_atall[butler.d.dem$treat_jake==1]))
  results25[i]<-release$release
  j.d.dem.DP<-median(results25)
}
d.d.dem.DP-j.d.dem.DP

##################################################################################################
## Party differential
##################################################################################################

## Not private
d.d.rep-d.d.dem
t.test(butler.d.rep$reply_atall[butler.d.rep$treat_deshawn==1],butler.d.dem$reply_atall[butler.d.dem$treat_deshawn==1])
j.d.rep-j.d.dem
t.test(butler.d.rep$reply_atall[butler.d.rep$treat_jake==1],butler.d.dem$reply_atall[butler.d.dem$treat_jake==1])
mean(butler.d$reply_atall[butler.d$treat_repprimary==1]) - mean(butler.d$reply_atall[butler.d$treat_demprimary==1])
t.test(butler.d$reply_atall[butler.d$treat_repprimary==1],butler.d$reply_atall[butler.d$treat_demprimary==1])

## Differentially private
d.d.rep.DP-d.d.dem.DP
j.d.rep.DP-j.d.dem.DP

for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.rep$reply_atall,range=range(butler.d.rep$reply_atall))
  results26[i]<-release$release
  overall.d.rep.DP<-median(results26)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.dem$reply_atall,range=range(butler.d.dem$reply_atall))
  results27[i]<-release$release
  overall.d.dem.DP<-median(results27)
}
overall.d.rep.DP-overall.d.dem.DP

##################################################################################################
## Combined effect
##################################################################################################

butler.d.yes<-subset(butler.d,subset=(butler.d$treat_noprimary==0))

## Not private
d.d.yes<-mean(butler.d.yes$reply_atall[butler.d.yes$treat_deshawn==1])
j.d.yes<-mean(butler.d.yes$reply_atall[butler.d.yes$treat_jake==1])
d.d.yes-j.d.yes
t.test(butler.d.yes$reply_atall[butler.d.yes$treat_deshawn==1],butler.d.yes$reply_atall[butler.d.yes$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.yes$reply_atall[butler.d.yes$treat_deshawn==1],
                        range=range(butler.d.yes$reply_atall[butler.d.yes$treat_deshawn==1]))
  results28[i]<-release$release
  d.d.yes.DP<-median(results28)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.yes$reply_atall[butler.d.yes$treat_jake==1],
                        range=range(butler.d.yes$reply_atall[butler.d.yes$treat_jake==1]))
  results29[i]<-release$release
  j.d.yes.DP<-median(results29)
}
d.d.yes.DP-j.d.yes.DP

#############
## Table 3 #
##############

butler.d.w<-subset(butler.d,subset=(butler.d$leg_white==1 & butler.d$treat_noprimary==1))
butler.d.m<-subset(butler.d,subset=(butler.d$leg_white==0 & butler.d$treat_noprimary==1))
butler.r.w<-subset(butler.r,subset=(butler.r$leg_white==1 & butler.r$treat_noprimary==1))
butler.r.m<-subset(butler.r,subset=(butler.r$leg_white==0 & butler.r$treat_noprimary==1))

##################################################################################################
## White Democratic Legislators
##################################################################################################

## Not private
D.w.d<-mean(butler.d.w$reply_atall[butler.d.w$treat_deshawn==1])
D.w.j<-mean(butler.d.w$reply_atall[butler.d.w$treat_jake==1])
D.w.d-D.w.j
t.test(butler.d.w$reply_atall[butler.d.w$treat_deshawn==1],butler.d.w$reply_atall[butler.d.w$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.w$reply_atall[butler.d.w$treat_deshawn==1],
                        range=range(butler.d.w$reply_atall[butler.d.w$treat_deshawn==1]))
  results30[i]<-release$release
  D.w.d.DP<-median(results30)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.w$reply_atall[butler.d.w$treat_jake==1],
                        range=range(butler.d.w$reply_atall[butler.d.w$treat_jake==1]))
  results31[i]<-release$release
  D.w.j.DP<-median(results31)
}
D.w.d.DP-D.w.j.DP

##################################################################################################
## Minority Democratic Legislators
##################################################################################################

## Not private
D.m.d<-mean(butler.d.m$reply_atall[butler.d.m$treat_deshawn==1])
D.m.j<-mean(butler.d.m$reply_atall[butler.d.m$treat_jake==1])
D.m.d-D.m.j
t.test(butler.d.m$reply_atall[butler.d.m$treat_deshawn==1],butler.d.m$reply_atall[butler.d.m$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.m$reply_atall[butler.d.m$treat_deshawn==1],
                        range=range(butler.d.m$reply_atall[butler.d.m$treat_deshawn==1]))
  results32[i]<-release$release
  D.m.d.DP<-median(results32)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.d.m$reply_atall[butler.d.m$treat_jake==1],
                        range=range(butler.d.m$reply_atall[butler.d.m$treat_jake==1]))
  results33[i]<-release$release
  D.m.j.DP<-median(results33)
}
D.m.d.DP-D.m.j.DP

##################################################################################################
## White Republican Legislators
##################################################################################################

## Not private
R.w.d<-mean(butler.r.w$reply_atall[butler.r.w$treat_deshawn==1])
R.w.j<-mean(butler.r.w$reply_atall[butler.r.w$treat_jake==1])
R.w.d-R.w.j
t.test(butler.r.w$reply_atall[butler.r.w$treat_deshawn==1],butler.r.w$reply_atall[butler.r.w$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.w$reply_atall[butler.r.w$treat_deshawn==1],
                        range=range(butler.r.w$reply_atall[butler.r.w$treat_deshawn==1]))
  results34[i]<-release$release
  R.w.d.DP<-median(results34)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.w$reply_atall[butler.r.w$treat_jake==1],
                        range=range(butler.r.w$reply_atall[butler.r.w$treat_jake==1]))
  results35[i]<-release$release
  R.w.j.DP<-median(results35)
}
R.w.d.DP-R.w.j.DP

##################################################################################################
## Minority Republican Legislators
##################################################################################################

## Not private
R.m.d<-mean(butler.r.m$reply_atall[butler.r.m$treat_deshawn==1])
R.m.j<-mean(butler.r.m$reply_atall[butler.r.m$treat_jake==1])
R.m.d-R.m.j
t.test(butler.r.m$reply_atall[butler.r.m$treat_deshawn==1],butler.r.m$reply_atall[butler.r.m$treat_jake==1])

## Differentially private
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.m$reply_atall[butler.r.m$treat_deshawn==1],
                        range=range(butler.r.m$reply_atall[butler.r.m$treat_deshawn==1]))
  results36[i]<-release$release
  R.m.d.DP<-median(results36)
}
for (i in 1:sims) {
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=butler.r.m$reply_atall[butler.r.m$treat_jake==1],
                        range=range(butler.r.m$reply_atall[butler.r.m$treat_jake==1]))
  results37[i]<-release$release
  R.m.j.DP<-median(results37)
}
R.m.d.DP-R.m.j.DP

##############
## Figure 1 ##
##############

Figure.1<-c(D.m.d-D.m.j,D.w.d-D.w.j,R.w.d-R.w.j,d.no-j.no)
barplot(Figure.1,horiz=TRUE,col="dodgerblue",border="dodgerblue",xlim=c(-.11,0.21),
        xlab="Rate of reply to black alias minus rate of reply to white alias",
        main="Differential Treatment Legislators Practiced, By Legislators' Characteristics [Not Private]",
        names.arg=c("Minority Democratic \n Legislators","White Democratic \n Legislators","White Republican \n Legislators","Overall"))
abline(v=0,col="black",lwd=1)
abline(v=-.10,col="black",lwd=1)
abline(v=-.05,col="black",lwd=1)
abline(v=.05,col="black",lwd=1)
abline(v=.15,col="black",lwd=1)
abline(v=.10,col="black",lwd=1)
abline(v=.20,col="black",lwd=1)
text(0.075,0.75,"-5.1%",col="white",cex=3)
text(-0.03,1.9,"-6.8%",col="white",cex=3)
text(-0.03,3.1,"-7.6%",col="white",cex=3)
text(-0.03,4.25,"-16.5%",col="white",cex=3)

Figure.1.DP<-c(D.m.d.DP-D.m.j.DP,D.w.d.DP-D.w.j.DP,R.w.d.DP-R.w.j.DP,d.no-j.no.DP)
barplot(Figure.1.DP,horiz=TRUE,col="forestgreen",border="forestgreen",xlim=c(-.11,0.21),
        xlab="Rate of reply to black alias minus rate of reply to white alias",
        main="Differential Treatment Legislators Practiced, By Legislators' Characteristics [Differentially Private]",
        names.arg=c("Minority Democratic \n Legislators","White Democratic \n Legislators","White Republican \n Legislators","Overall"))
abline(v=0,col="black",lwd=1)
abline(v=-.10,col="black",lwd=1)
abline(v=-.05,col="black",lwd=1)
abline(v=.05,col="black",lwd=1)
abline(v=.15,col="black",lwd=1)
abline(v=.10,col="black",lwd=1)
abline(v=.20,col="black",lwd=1)
text(0.075,0.75,"16.5%",col="white",cex=3)
text(-0.03,1.9,"-6.8%",col="white",cex=3)
text(-0.03,3.1,"-7.6%",col="white",cex=3)
text(-0.03,4.25,"-16.5%",col="white",cex=3)

##############
## Plots    ##
##############

##################################################################################################
## Histogram of 1,000 DP Estimates
##################################################################################################

g<-hist(results-results1,breaks=48,plot=FALSE)
g$counts=(g$counts/sum(g$counts))*100
plot(g,col="white",border="black",ylim=c(0,25),xlim=c(-0.0520,-0.0506),
     main="Distribution of DP \n  Estimates of Race Differential",
     xlab="Differentially Private Racial Differential Estimate",ylab="Frequency of Estimate")
abline(v=d.no-j.no,col="violetred",lwd=3)
text(-0.05145,23,"True \n Value",col="violetred")

##################################################################################################
## Error over 1,000 Bootstraps
##################################################################################################

mybootstrap<-function(data){
  n<-nrow(data)
  index<-1:n
  bootindex<-sample(x=index,size=n,replace=TRUE)
  return(data[bootindex,])
}

but.ler<-subset(butler.no,select=c(reply_atall,treat_jake,treat_deshawn))

x<-matrix(nrow=183,ncol=3)
bootstraps1DP<-bootstraps1NP<-bootstraps2DP<-bootstraps2NP<-rep(NA,1000)

for (i in 1:1000) {
  x<-mybootstrap(but.ler)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$reply_atall[x$treat_deshawn==1],
                        range=range(x$reply_atall[x$treat_deshawn==1]))
  results3<-release$release
  bootstraps1DP[i]<-results3
  results4<-mean(x$reply_atall[x$treat_deshawn==1])
  bootstraps1NP[i]<-results4
}

for (i in 1:1000) {
  x<-mybootstrap(but.ler)
  set.seed(i)
  release<-Mean.release(eps=0.1,del=2e-20,data=x$reply_atall[x$treat_jake==1],
                        range=range(x$reply_atall[x$treat_jake==1]))
  results5<-release$release
  bootstraps2DP[i]<-results5
  results6<-mean(x$reply_atall[x$treat_jake==1])
  bootstraps2NP[i]<-results6
}

DP.bs<-bootstraps1DP - bootstraps2DP
NP.bs<-bootstraps1NP - bootstraps2NP
error.bs<-DP.bs-NP.bs

NP.true<-j.no-d.no

plot(error.bs,ylim=c(-0.0042,0.0042),main="Race Differential Error in Bootstrapped Data",
     xlab="Iteration Number",ylab="Size of Error")
abline(h=mean(error.bs),col="violetred",lwd=3,lty=1)
text(50,0.0004,"Mean",col="violetred",cex=1.5)

plot(x=DP.bs,y=NP.bs,main="Bootstrapped DP Estimates vs Boostrapped NP Estimates",
     xlab="Bootstrapped DP Estimates",ylab="Bootstrapped NP Estimates")
abline(0,1,col="violetred",lwd=2)
text(-0.11,-0.10,"45º Line",srt=30,col="violetred")

##################################################################################################
## RMSE Across Epsilon Values
##################################################################################################

eps<-seq(from=0.01,to=1,by=0.01)

x<-rep(NA,500)
list<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
           x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)
list2<-list(x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,
            x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x)

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=butler.no$reply_atall[butler.no$treat_deshawn==1],
                          range=range(butler.no$reply_atall[butler.no$treat_deshawn==1]))
    list[[i]][j]<-release$release
  } 
}

for (i in 1:100) {
  epsilon<-eps[i]
  for (j in 1:500) {
    set.seed(j)
    release<-Mean.release(eps=epsilon,del=2e-20,data=butler.no$reply_atall[butler.no$treat_jake==1],
                          range=range(butler.no$reply_atall[butler.no$treat_jake==1]))
    list2[[i]][j]<-release$release
  } 
}

rmse <- function (orig_value, dp_value) {
  return(sqrt(mean((orig_value-dp_value)^2)))
}

np<-j.no-d.no

error<-rep(NA,100)
for (i in 1:100) {
  d_val<-list[[i]]
  j_val<-list2[[i]]
  dp<-j_val-d_val
  error[i]<-rmse(np,dp)
}

plot(x=eps,y=error,main="RMSE for Race Differential \n Calculation Across Epsilon Values",
     xlab="Epsilon Value",ylab="Root Mean Squared Error",cex=1,col="white",pch=8)
lines(x=eps,y=error,col="violetred",lwd=4)
points(x=eps[10],y=error[10],cex=2,pch=19,col="black")
text(0.19,0.00028,"Typical\nEpsilon",col="black")