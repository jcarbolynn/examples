# examples

#lab 9 oyster counts

setwd("~/semester 5/WIS 4601/wk9/lab 9")

os = read.csv("oystersamples.csv", header = T)
View(os)

#subset data by person and then rock
p1 = os[which(os$person == "1"),]
p2 = os[which(os$person == "2"),]
p3 = os[which(os$person == "3"),]

#subset person 1 (me) by rock
p1rockc = p1[which(p1$rock == "c"),]
p1rocka = p1[which(p1$rock == "a"),]
p1rockf = p1[which(p1$rock == "f"),]
p1rocki = p1[which(p1$rock == "i"),]

#check
p1rockc

plot(as.factor(p1$rock), p1$count, type = "p",
     ylab = "Counts",
     xlab = "Rocks",
     main = "Person 1")


par(mfrow = c(2,2))
boxplot(p1rockc$count,
        main = "Rock C",
        ylab = "Count")
boxplot(p1rocka$count,
        main = "Rock A",
        ylab = "Count")
boxplot(p1rockf$count,
        main = "Rock F",
        ylab = "Count")
boxplot(p1rocki$count,
        main = "Rock I",
        ylab = "Count")

#means of each rock count
p1rcmean = mean(p1rockc$count)
#16.33
p1ramean = mean(p1rocka$count)
#24
p1rfmean = mean(p1rockf$count)
#19.67
p1rimean = mean(p1rocki$count)
#86.67

#variance?
p1rcvar = (p1rockc$count - p1rcmean)^2/(2)
p1ravar = (p1rocka$count - p1ramean)^2/(2)
p1rfvar = (p1rockf$count - p1rfmean)^2/(2)
p1rivar = (p1rocki$count - p1rimean)^2/(2)

sum(c(0.03703704, 0.14814815, 0.03703704)) #/3
sum(c(0.05555556, 0.22222222, 0.05555556)) #/2
var(p1rockc$count)
var(p1rocki$count)

#222222222222222222222222222222222222222222

par(mfrow = c(1,1))
plot(as.factor(p1$rock), p1$count, type = "p",
     ylab = "Counts",
     xlab = "Rocks",
     ylim = c(0, 220),
     main = "Person 1")
points(1, 33, col = "red", pch = 16)
points(2, 40, col = "red", pch = 16)
points(3, 91, col = "red", pch = 16)
points(4, 218, col = "red", pch = 16)

par(mfrow = c(1,1))
boxplot(p1rockc$count,
        main = "Rock C",
        ylab = "Count",
        ylim = c(15, 35))
points( 33, col = "red", pch = 16)

boxplot(p1rocka$count,
        main = "Rock A",
        ylab = "Count",
        ylim = c(23, 40))
points(1, 40, col = "red", pch = 16)

boxplot(p1rockf$count,
        main = "Rock F",
        ylab = "Count")
points(1, 91, col = "red", pch = 16)

boxplot(p1rocki$count,
        main = "Rock I",
        ylab = "Count")
points(1, 218, col = "red", pch = 16)

#3333333333333333333333333333333333333333333333

p1means = c(mean(p1rockc$count), mean(p1rocka$count),
            mean(p1rockf$count), mean(p1rocki$count))
truecount = c(33, 40, 91, 218)
p1prop = p1means/truecount
p1prop


#4444444444444444444444444444444444444444444444444
#number does affect count, low numbers had about 50%
#high numbers had lower proportions counted

#EXTRA

p1mean = data.frame(prop = c(.4949, .6, .2161, .3975),
                    rock = c("c", "a", "f", "i"))
meanoy = barplot(p1mean$prop,
                 main = "Mean Proportion Oyster Counts",
                 ylab = "Proportion",
                 xlab = "Rock")
axis(side = 1, at = meanoy,
     labels = c("c", "a", "f", "i"))


extra = data.frame(rock = c("c", "a", "f", "i"),
                   prop = c(.4949, .6, .2161, .3975))

plot(extra$prop~extra$rock)

#####################################################################################
#multiple people

os
par(mfrow = c(1,1))
plot(as.factor(os$rock), os$count, type = "p",
     ylab = "Counts",
     xlab = "Rock")
#but have to divide it by person
#subset data by person

p1 = os[which(os$person == "1"),]
p2 = os[which(os$person == "2"),]
p3 = os[which(os$person == "3"),]

#people points colors
points(os$rock[os$person == "1"],
       os$count[os$person == "1"], col = "red", pch = 16)
points(os$rock[os$person == "2"],
       os$count[os$person == "2"], col = "green", pch = 16)
points(os$rock[os$person == "3"],
       os$count[os$person == "3"], col = "blue", pch = 16)
legend(x = "topleft", pch = 16, col = c("red", "green", "blue"),
       legend = c("person 1", "person 2", "person 3"))


#different barplots for different people
par(mfrow = c(1,3))
plot(as.factor(p1$rock), p1$count, type = "p",
     ylab = "Counts",
     xlab = "Person 1")
points(os$rock[os$person == "1"],
       os$count[os$person == "1"], col = "red", pch = 16)
plot(as.factor(p2$rock), p2$count, type = "p",
     ylab = "Counts",
     xlab = "Person 2")
points(os$rock[os$person == "2"],
       os$count[os$person == "2"], col = "green", pch = 16)
plot(as.factor(p3$rock), p3$count, type = "p",
     ylab = "Counts",
     xlab = "Person 3")
points(os$rock[os$person == "3"],
       os$count[os$person == "3"], col = "blue", pch = 16)

#5555555555555555555555555555555555555555555

#subset data by rock
ra = os[which(os$rock == "a"),]
rc = os[which(os$rock == "c"),]
rf = os[which(os$rock == "f"),]
ri = os[which(os$rock == "i"),]

par(mfrow = c(2,2))
plot(as.factor(ra$person), ra$count, type = "p",
     ylab = "Counts",
     xlab = "Person",
     main = "Rock A")
plot(as.factor(rc$person), rc$count, type = "p",
     ylab = "Counts",
     xlab = "Person",
     main = "Rock C")
plot(as.factor(rf$person), rf$count, type = "p",
     ylab = "Counts",
     xlab = "Person",
     main = "Rock F")
plot(as.factor(ri$person), ri$count, type = "p",
     ylab = "Counts",
     xlab = "Person",
     main = "Rock I")


##############################
#grouped barplot!            #
#rock c                      #
rc1 = c(16, 17, 16)          #
rc2 = c(16, 16, 16)          #
rc3 = c(16, 19, 15)          #
                             #
temp = cbind(rc1, rc2, rc3)  #
barplot(temp, beside = T)    #
plot(data = temp, beside = T)#
##############################

#no
library(ggplot2)
ggplot = ggplot(data = os, aes(as.factor(rock)))

#how to show different people
library(lattice)
xyplot(count~rock, groups = os$rock, data = os)

#just have many plots
par(mfrow = c(2,2))
rcp = plot(rc$person, rc$count,
           main = "Counts by Person",
           ylab = "Count",
           xlab = "Person")

rap = plot(ra$person, ra$count,
           main = "Counts by Person",
           ylab = "Count",
           xlab = "Person")

rfp = plot(rf$person, rf$count,
           main = "Counts by Person",
           ylab = "Count",
           xlab = "Person")

rip = plot(ri$person, ri$count,
           main = "Counts by Person",
           ylab = "Count",
           xlab = "Person",
           labels = F)
list = (c(1:3))
axis(1, at = 1:3, labels = list)

#precision vary between observers?
#measure precision for each rock
#compare through tukey test?


#666666666666666666666666666666666666
#proportion across rocks, confidence interval

#proportion for all counts, all rocks
tot.means = c(mean(ra$count), mean(rc$count),
          mean(rf$count), mean(ri$count))
truecount = c(33, 40, 91, 218)
tot.props = tot.means/truecount
tot.props
os.prop = mean(tot.props)
#0.4462

#standard deviation
tot.sd = sd(tot.props)
#0.20367

#standard error: sd/sqrt(n)
tot.se = tot.sd/sqrt(36)

#95 confidence error
lwr.ci = os.prop - 1.96*tot.se
upr.ci = os.prop + 1.96*tot.se

###########################
#OOOOORRRRRRRR
#CI by rock and do average
#c a f i

#proprtions by rock
mpc = mean(p1rockc$count/33)
mpa = mean(p1rocka$count/40)
mpf = mean(p1rockf$count/91)
mpi = mean(p1rocki$count/218)

mean(c(mpc, mpa, mpf, mpi))

#sd
sdc = sd(p1rockc$count/33)
sda = sd(p1rocka$count/40)
sdf = sd(p1rockf$count/91)
sdi = sd(p1rocki$count/218)

sdc = sd(os$count[os$rock == "c"])    #1.13
sda = sd(os$count[os$rock == "a"])    #2.06
sdf = sd(os$count[os$rock == "f"])    #2.23
sdi = sd(os$count[os$rock == "i"])    #9.37

#se
sec = sdc/sqrt(9)    #
sea = sda/sqrt(9)    #
sef = sdf/sqrt(9)    #
sei = sdi/sqrt(9)    #

#upper ci
upc = mpc + 1.96*sec
upa = mpa + 1.96*sea
upf = mpf + 1.96*sef
upi = mpi + 1.96*sei

#average
mean(c(upc, upa, upf, upi))

#lower ci
lwc = mpc - 1.96*sec
lwa = mpa - 1.96*sea
lwf = mpf - 1.96*sef
lwi = mpi - 1.96*sei

#average
mean(c(lwc, lwa, lwf, lwi))
