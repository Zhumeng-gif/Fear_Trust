setwd("d:/baidu/Tsinghua/wp/fear/R/data/")
library(haven)
library(foreign)
library(car)
dat <- read.csv("newData.csv")
#tmp1 <- read_spss("TrustData with two new IVs.sav")
#tmp2 <- read_spss("CSS2019.sav")
#tmp3 <- cbind.data.frame(id=tmp2$uid, trtcon=tmp2$"体制深度", trtbin=tmp2$"体制内外")
#tmp4 <- merge(tmp1, tmp3, by = "id", all.x=TRUE)
#tmp4$trt1 <- ifelse(tmp4$year==2019&tmp4$wave==4, tmp4$trtbin, tmp4$"体制内外")
#tmp4$trt2 <- ifelse(tmp4$year==2019&tmp4$wave==4, tmp4$trtcon, tmp4$"体制深度")
#dat <- as.data.frame(tmp4)
##rm(list=paste0("tmp", 1:4))
#dat <- as.data.frame(dat[,-c(5,6)])
#dat$zAge <- arm:::rescale(dat$age)
#dat$educ <- dat$edu
#dat$trt <- tmp4$trtbin
#dat$outcome <- dat$trust_gover_z
#dat$tempo <- dat$time
##dat$trt <- dat$treat
#dat$province <- datall$province

dat$zAge <- arm:::rescale(dat$age)
dat$educ <- dat$edu
dat$trt <- Recode(dat$work, "1=1;0=0;else=NA")#, 0)
dat$trt <- ifelse(dat$trt==1|dat$party==1, 1, 0)
dat$trt2 <- ifelse(dat$party==1&dat$edu>3, 1, 0)
#dat$trt <- ifelse(dat$edu>3&dat$party==1, 1, 0)
dat$outcome <- Recode(dat$trust_goverR, "1=1;0=0")
dat$outcomeMax <- Recode(dat$trust_gov_Maxdummy, "1=1;0=0")
#dat$year[dat$year==2014] <- 2015
dat$tempo <- ifelse(dat$year > 2013, 1, 0)

dat$source <- with(dat,
    ifelse(year==2002, "cbs2002",
    ifelse(year==2006, "css2006",
    ifelse(year==2008&wave==2, "pku2008",
    ifelse(year==2008&wave==1, "cbs2008",
    ifelse(year==2009, "pku2009",
    ifelse(year==2010, "cgss2010",
    ifelse(year==2011, "cbs2011",
    ifelse(year==2014, "cbs2014",
    ifelse(year==2017, "css2017",
    ifelse(year==2019&wave==1, "cbs2019",
    ifelse(year==2019&wave==4, "css2019",
    ifelse(year==2021, "css2021", NA)))))))))))))
dat$race[dat$source=="css2006"] <- -999
dat$trust_gover[dat$source=="cbs2019"] <- ifelse(is.na(dat$trust_gover[dat$source=="cbs2019"]), 9, dat$trust_gover[dat$source=="cbs2019"])
dat$trust_gover[dat$source=="cgss2010"] <- ifelse(is.na(dat$trust_gover[dat$source=="cgss2010"]), 9, dat$trust_gover[dat$source=="cgss2010"])
dat$trust_gover[dat$source=="css2017"] <- ifelse(is.na(dat$trust_gover[dat$source=="css2017"]), 9, dat$trust_gover[dat$source=="css2017"])



sourceNames <- unique(dat$source)
sourceNames <- sourceNames[-c(7,8)]
kSource <- length(sourceNames)
dat2 <- with(dat, cbind.data.frame(outcome, outcomeMax, year, trt, trt2, age, zAge, female, party, tempo, edu, hukouRural, marriage, source, race))
dat2 <- subset(dat2, subset=(dat2$source %in% sourceNames))
dat3 <- na.omit(dat2)



trendY1 <- tapply(dat3$outcome[dat3$trt==1], dat3$year[dat3$trt==1], FUN=mean, na.rm=TRUE)
trendY0 <- tapply(dat3$outcome[dat3$trt==0], dat3$year[dat3$trt==0], FUN=mean, na.rm=TRUE)
trendYM1 <- tapply(dat3$outcomeMax[dat3$trt==1], dat3$year[dat3$trt==1], FUN=mean, na.rm=TRUE)
trendYM0 <- tapply(dat3$outcomeMax[dat3$trt==0], dat3$year[dat3$trt==0], FUN=mean, na.rm=TRUE)

trendy1 <- tapply(dat3$outcome[dat3$trt2==1], dat3$year[dat3$trt2==1], FUN=mean, na.rm=TRUE)
trendy0 <- tapply(dat3$outcome[dat3$trt2==0], dat3$year[dat3$trt2==0], FUN=mean, na.rm=TRUE)

nyear <- length(unique(dat3$year))



par(mfrow=c(1,2), mar=c(2,2,3,1), tcl=-0.2, mgp=c(2,0.2,0), oma=c(2,2,0,0))
plot(x=1:nyear, y=trendY1, type="l", axes=FALSE, frame.plot=TRUE,
    xlab="Year", ylab="Proportion to the sample", main="Outcome (Trust vs Distrust)",
    ylim=c(0.75,1), xaxs="i", yaxs="i")
axis(1, 1:nyear, sort(unique(dat3$year)))
axis(2, at=c(0.8,0.9,1))
lines(x=1:nyear, y=trendY0, col="gray50")
abline(v=6, lty=3)
text(x=8, y=trendY1[8], labels="treated", adj=1, xpd=NA)
text(x=8, y=trendY0[8]-0.005, labels="control", adj=0, xpd=NA)

plot(x=1:nyear, y=trendYM1, type="l", axes=FALSE, frame.plot=TRUE,
    xlab="Year", ylab="Proportion to the sample", main="Outcome (Completely Trust vs Others)",
    ylim=c(0.3,1), xaxs="i", yaxs="i")
axis(1, 1:nyear, sort(unique(dat3$year)))
axis(2, at=c(0.4,0.6,0.8,1))
lines(x=1:nyear, y=trendYM0, col="gray75")
abline(v=6, lty=3)
text(x=4, y=trendYM1[4], labels="treated", adj=1, xpd=NA)
text(x=3, y=trendYM0[3], labels="control", adj=0, xpd=NA)

mtext("Survey years", side=1, outer=TRUE, line=0, cex=1.2)
mtext("Proportion to the sample", side=2, outer=TRUE, line=0, cex=1.2)


plot(x=1:nyear, y=trendy1, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.8,1), xaxs="i", yaxs="i")
axis(1, 1:nyear, sort(unique(dat3$year)))
axis(2, at=c(0,0.8,1))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                2222                                                        2                                                                                   222)
lines(x=1:nyear, y=trendy0, col="gray75")
abline(v=6, lty=3)



#write_dta(dat, "mergeData.dta")


M1 <- glm(outcome ~ trt + tempo + trt:tempo + age + female + marriage +  hukouRural + factor(edu),
    data=dat3, family=binomial)
summary(M1)
M2 <- glm(outcomeMax ~ trt + tempo + trt:tempo + age + female + marriage +  hukouRural + factor(edu),
    data=dat3, family=binomial)
summary(M2)

M3 <- glm(outcome ~ trt2 + tempo + trt2:tempo + age + female + marriage +  hukouRural,
    data=dat3, family=binomial)
summary(M3)
M4 <- glm(outcomeMax ~ trt2 + tempo + trt2:tempo + age + female + marriage +  hukouRural,
    data=dat3, family=binomial)
summary(M4)


library(MatchIt)

matchR <- function(data, ratio=1){
    require(MatchIt)
    fDataR <- ps <- NULL
    form <- as.formula(trt ~ age + female + marriage + factor(edu) + hukouRural + race)
    form2 <- as.formula(trt ~ age + female + marriage + hukouRural + factor(edu))
    for(k in 1:kSource){
        datTmp <- subset(data, subset=data$source == sourceNames[k])
        if(sourceNames[k]=="css2006"){
            pfit <- matchit(form2, , family = binomial, data = datTmp, method="nearest", estimand = "ATT", ratio=ratio, replace=FALSE)
        }else{
            pfit <- matchit(form, family = binomial, data = datTmp, method="nearest", estimand = "ATT", ratio=ratio, replace=FALSE)
        }
        mdata <- match.data(pfit)
        ps <- c(ps, pfit$distance)
        fDataR <- rbind.data.frame(fDataR, mdata)
    }
    return(list(pscores = ps, data=fDataR))
}

matchE <- function(data, ratio=1){
    require(MatchIt)
    fDataR <- ps <- NULL
    form <- as.formula(trt2 ~ age + female + marriage + hukouRural + race)
    form2 <- as.formula(trt2 ~ age + female + marriage + hukouRural)
    for(k in 1:kSource){
        datTmp <- subset(data, subset=data$source == sourceNames[k])
        if(sourceNames[k]=="css2006"){
            pfit <- matchit(form2, , family = binomial, data = datTmp, method="nearest", estimand = "ATT", ratio=ratio, replace=FALSE)
        }else{
            pfit <- matchit(form, family = binomial, data = datTmp, method="nearest", estimand = "ATT", ratio=ratio, replace=FALSE)
        }
        mdata <- match.data(pfit)
        ps <- c(ps, pfit$distance)
        fDataR <- rbind.data.frame(fDataR, mdata)
    }
    return(list(pscores = ps, data=fDataR))
}




MR1 <- matchR(dat3, ratio=1)
MR3 <- matchR(dat3, ratio=3)
fData1 <- MR1$data
fData3 <- MR3$data
didreg1 <- glm(outcome ~ trt + tempo + trt * tempo + age + female + marriage + hukouRural + factor(edu), family = quasibinomial, data = fData1, weights=weights)
summary(didreg1)
didregMax1 <- glm(outcomeMax ~ trt + tempo + trt * tempo + age + female + marriage + hukouRural + factor(edu), family = quasibinomial, data = fData1, weights=weights)
summary(didregMax1)

didreg3 <- glm(outcome ~ trt + tempo + trt * tempo + age + female + marriage + hukouRural + factor(edu), family = quasibinomial, data = fData3, weights=weights)
summary(didreg3)
didregMax3 <- glm(outcomeMax ~ trt + tempo + trt * tempo + age + female + marriage + hukouRural + factor(edu), family = quasibinomial, data = fData3, weights=weights)
summary(didregMax3)




yrNames <- sort(unique(dat3$year))
yt11 <- yc11 <- yt13 <- yc13 <- NULL
for(i in 1:length(yrNames)){
    tmp <- with(fData1, weighted.mean(outcome[trt==1&year==yrNames[i]], w=weights[trt==1&year==yrNames[i]], na.rm=TRUE))
    yt11 <- c(yt11, tmp)
    tmp <- with(fData1, weighted.mean(outcome[trt==0&year==yrNames[i]], w=weights[trt==0&year==yrNames[i]], na.rm=TRUE))
    yc11 <- c(yc11, tmp)
    tmp <- with(fData3, weighted.mean(outcome[trt==1&year==yrNames[i]], w=weights[trt==1&year==yrNames[i]], na.rm=TRUE))
    yt13 <- c(yt13, tmp)
    tmp <- with(fData3, weighted.mean(outcome[trt==0&year==yrNames[i]], w=weights[trt==0&year==yrNames[i]], na.rm=TRUE))
    yc13 <- c(yc13, tmp)
}

ytMax11 <- ycMax11 <- ytMax13 <- ycMax13 <- NULL
for(i in 1:length(yrNames)){
    tmp <- with(fData1, weighted.mean(outcomeMax[trt==1&year==yrNames[i]], w=weights[trt==1&year==yrNames[i]], na.rm=TRUE))
    ytMax11 <- c(ytMax11, tmp)
    tmp <- with(fData1, weighted.mean(outcomeMax[trt==0&year==yrNames[i]], w=weights[trt==0&year==yrNames[i]], na.rm=TRUE))
    ycMax11 <- c(ycMax11, tmp)
    tmp <- with(fData3, weighted.mean(outcomeMax[trt==1&year==yrNames[i]], w=weights[trt==1&year==yrNames[i]], na.rm=TRUE))
    ytMax13 <- c(ytMax13, tmp)
    tmp <- with(fData3, weighted.mean(outcomeMax[trt==0&year==yrNames[i]], w=weights[trt==0&year==yrNames[i]], na.rm=TRUE))
    ycMax13 <- c(ycMax13, tmp)
}


xYrs <- sort(unique(dat3$year))
pdf("parallelTrend.pdf", width=8, height=6)
par(mfrow=c(2,3), mar=c(4,1,3,0.5), mgp=c(2,0.2,0), tcl=-0.2, oma=c(1,3,3,1))
plot(x=xYrs, y=trendY1, type="l", axes=FALSE, frame.plot=TRUE,
    xlab="", ylab="", main="Raw Data", cex.main=1.2, font.main=1,
    ylim=c(0.75,1), xaxs="i", yaxs="i")
axis(1, xYrs, labels=FALSE)
axis(2, at=c(0.8,0.9,1))
lines(x=xYrs, y=trendY0, col="gray70")
abline(v=xYrs[6]-1, lty=2)
text(x=xYrs[8], y=trendY1[8], labels="treated", adj=1, xpd=NA)
text(x=xYrs[7], y=trendY0[7], labels="control", adj=0, xpd=NA)

plot(x=xYrs, y=yt11, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.75,1),
    yaxs="i", xaxs="i", main="Matched data (1 vs 1 Matching)", cex.main=1.2, font.main=1)
axis(1, xYrs, labels=FALSE)
axis(2, at=c(0.8,0.9,1), labels=FALSE)
lines(x=xYrs, y=yc11, col="gray70")
abline(v=xYrs[6]-1, lty=2)

plot(x=xYrs, y=yt13, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.75,1),
    yaxs="i", xaxs="i", main="Matched data (1 vs 3 Matching)", cex.main=1.2, font.main=1)
axis(1, xYrs, labels=FALSE)
axis(2, at=c(0.8,0.9,1), labels=FALSE)
lines(x=xYrs, y=yc13, col="gray70")
abline(v=xYrs[6]-1, lty=2)

plot(x=xYrs, y=trendYM1, type="l", axes=FALSE, frame.plot=TRUE,
    xlab="", ylab="", main="Raw Data", cex.main=1.2, font.main=1,
    ylim=c(0.3,1), xaxs="i", yaxs="i")
axis(1, xYrs, cex.axis=0.8)
axis(2, at=c(0.4,0.6,0.8,1))
lines(x=xYrs, y=trendYM0, col="gray70")
abline(v=xYrs[6]-1, lty=2)
#text(x=8, y=trendYM1[8], labels="treated", adj=1, xpd=NA)
#text(x=7, y=trendYM0[7], labels="control", adj=0, xpd=NA)

plot(x=xYrs, y=ytMax11, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.3,1),
    yaxs="i", xaxs="i", main="Matched data (1 vs 1 Matching)", cex.main=1.2, font.main=1)
axis(1, xYrs, cex.axis=0.8)
axis(2, at=c(0.4,0.6,0.8,1), labels=FALSE)
lines(x=xYrs, y=ycMax11, col="gray70")
abline(v=xYrs[6]-1, lty=2)

plot(x=xYrs, y=ytMax13, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.3,1),
    yaxs="i", xaxs="i", main="Matched data (1 vs 3 Matching)", cex.main=1.2, font.main=1)
axis(1, xYrs, cex.axis=0.8)
axis(2, at=c(0.4,0.6,0.8,1), labels=FALSE)
lines(x=xYrs, y=ycMax13, col="gray70")
abline(v=xYrs[6]-1, lty=2)

mtext("Survey years", side=1, outer=TRUE, line=-1, cex=1.2)
mtext("Proportion to the sample", side=2, outer=TRUE, line=1, cex=1.2)
mtext("Outcome (Trust vs Distrust)", side=3, outer=TRUE, line=0, cex=1.2, font=2)
mtext("Outcome (Completely Trust vs Others)", side=3, outer=TRUE, line=-21, cex=1.2, font=2)
dev.off()



ME1 <- matchE(dat3, ratio=1)
ME3 <- matchE(dat3, ratio=3)
fData1a <- ME1$data
fData3a <- ME3$data
didreg1a <- glm(outcome ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fData1a, weights=weights)
summary(didreg1a)
didregMax1a <- glm(outcomeMax ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fData1a, weights=weights)
summary(didregMax1a)

didreg3a <- glm(outcome ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fData3a, weights=weights)
summary(didreg3a)
didregMax3a <- glm(outcomeMax ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fData3a, weights=weights)
summary(didregMax3a)





yrNames <- sort(unique(dat3$year))
yt11a <- yc11a <- NULL
for(i in 1:length(yrNames)){
    tmp <- with(fData1a, weighted.mean(outcome[trt2==1&year==yrNames[i]], w=weights[trt2==1&year==yrNames[i]], na.rm=TRUE))
    yt11a <- c(yt11a, tmp)
    tmp <- with(fData1a, weighted.mean(outcome[trt2==0&year==yrNames[i]], w=weights[trt2==0&year==yrNames[i]], na.rm=TRUE))
    yc11a <- c(yc11a, tmp)
}

plot(x=1:length(yt11a), y=yt11a, type="l", axes=FALSE, frame.plot=TRUE, xlab="", ylab="", ylim=c(0.5,1))
axis(1, 1:length(yt11a), sort(unique(dat3$year)))
axis(2)
lines(x=1:length(yt11a), y=yc11a, col=2)


source("D:/Baidu/Tsinghua/wp/fear/R/balance.R")

tmpFit <- arm:::bayesglm(trt ~ age + female + race + marriage + hukouRural + factor(edu), data=dat3, x=TRUE, y=TRUE)
rawDat <- arm:::model.matrixBayes(tmpFit)
rtrt <- tmpFit$y
tmpFit1 <- arm:::bayesglm(trt ~ age + female + race + marriage + hukouRural + factor(edu), data=fData1, x=TRUE, y=TRUE)
mDat1 <- arm:::model.matrixBayes(tmpFit1)
wts1 <- fData1$weights
mtrt1 <- tmpFit1$y
tmpFit3 <- arm:::bayesglm(trt ~ age + female + race + marriage + hukouRural + factor(edu), data=fData3, x=TRUE, y=TRUE)
mDat3 <- arm:::model.matrixBayes(tmpFit3)
wts3 <- fData3$weights
mtrt3 <- tmpFit3$y
bStats1 <- bstat(rawDat, mDat1, rtrt, mtrt1, wts1)
bStats3 <- bstat(rawDat, mDat3, rtrt, mtrt3, wts3)
longcovnames <- c("Age", "Female", "Race: Han","Married","Rural residency", "Edu: Elementary",  "        Junior High", "        Senior High", "        College", "        Graduate")
pts3 <- plotBalance(bStats3, plot=FALSE)$matched



pdf("balance.pdf", width=6, height=3)
layout(mat=matrix(c(1,2,3), nrow=1, ncol=3), width=c(1,4,4), height=c(1,1,1))
par(mgp=c(1.5,0.1,0),tcl=-0.2, oma=c(0,0,1,0))
plotBalance(bStats1, longcovnames = "", v.axis=FALSE, mar=c(1,0,4,0), cex.main=1.1, main="", xlim=c(0,1), plot=FALSE)
#axis(3, c(0,0.3,0.6,0.9,1.2))
text(0,10:1, longcovnames, adj=0, xpd=NA, cex=1)

plotBalance(bStats1, longcovnames = "", v.axis=FALSE, mar=c(1,1,4,2), cex.main=1.1, main="")
axis(3, c(0,0.3,0.6,0.9,1.2))
#text(-0.6,10:1, longcovnames, adj=0, xpd=NA, cex=0.8)
text(0.65,11.5, "1 vs 1 Matching", adj=0.5, xpd=NA, cex=1.2, font=1)
#legend("bottomright", legend=c("All", "Matched"), pch=c(1,19), xpd=NA, cex=0.8)

plotBalance(bStats3, longcovnames = "", v.axis=FALSE, mar=c(1,1,4,2), cex.main=1.1, main="")
axis(3, c(0,0.3,0.6,0.9,1.2))
#text(-0.6,10:1, longcovnames, adj=0, xpd=NA, cex=0.8)
text(0.65,11.5, "1 vs 3 Matching", adj=0.5, xpd=NA, cex=1.2, font=1)
legend("bottomright", legend=c("All", "Matched"), pch=c(1,19), xpd=NA, cex=1)
mtext("Absolute Standardized Difference in Means", side=3, outer=TRUE, font=2, line=-1, cex=1)
dev.off()



ps1 <- MR1$ps
ps3 <- MR3$ps


ps11 <- cut(ps1[fData1$trt==1], breaks = seq(0,1,by=0.1), labels=1:10)
ps10 <- cut(ps1[fData1$trt==0], breaks = seq(0,1,by=0.1), labels=1:10)
ps31 <- cut(ps1[fData3$trt==1], breaks = seq(0,1,by=0.1), labels=1:10)
ps30 <- cut(ps1[fData3$trt==0], breaks = seq(0,1,by=0.1), labels=1:10)


pdf("commonSupport.pdf", width=6, height=3.5)
par(mfrow=c(1,2), mar=c(3,1,2,0.5), mgp=c(2,0.2,0), tcl=-0.2, oma=c(0,1,2,0))
plot(x=0, y=0, xlim=c(0,10), ylim=c(-25000,22000), type="n", axes=FALSE,
    xlab = "", ylab="",
    main="1 vs 1 Matching", font.main=1, cex.main=1)
abline(h=0, lwd=0.5)
rect(xleft=seq(0,9,by=1), xright=seq(1,10,by=1), ybottom=0, ytop=table(ps11), col="gray", lwd=0.5)
rect(xleft=seq(0,9,by=1), xright=seq(1,10,by=1), ybottom=0, ytop=table(ps10)*-1, lwd=0.5)
axis(2, c(-20000,-10000,0,10000,20000), abs(c(-20000,-10000,0,10000,20000)), cex.axis=0.7)
axis(1, seq(0,10,by=2), seq(0,1,by=.2), cex.axis=0.8)
box()

plot(x=0, y=0, xlim=c(0,10), ylim=c(-25000,22000), type="n", axes=FALSE,
    xlab = "", ylab="",
    main="1 vs 3 Matching", font.main=1, cex.main=1)
abline(h=0, lwd=0.5)
rect(xleft=seq(0,9,by=1), xright=seq(1,10,by=1), ybottom=0, ytop=table(ps31), col="gray", lwd=0.5)
rect(xleft=seq(0,9,by=1), xright=seq(1,10,by=1), ybottom=0, ytop=table(ps30)*-1, lwd=0.5)
axis(2, c(-20000, -10000, 0,10000,20000), abs(c(-20000,-10000,0,10000,12000)), cex.axis=0.8, labels=FALSE)
axis(1, seq(0,10,by=2), seq(0,1,by=.2), cex.axis=0.8)
box()

legend("bottomright", legend=c("treated", "untreated"), col=c("gray", "black"), pch=c(15,22), cex=0.7)
mtext("Frequencies", side=2, font=1, outer=TRUE, line=0)
mtext("Propensity score values", side=1, font=1, outer=TRUE, line=-1.5)
mtext("Distribution of propensity scores", side=3, font=2, outer=TRUE, cex=1.1)
dev.off()



permute <- function(data, ratio=1){
    datNew <- NULL
    for(k in 1:kSource){
        datTmp <- subset(data, subset=data$source == sourceNames[k])
        idx <- sample(1:nrow(datTmp), nrow(datTmp), replace=FALSE)
        datTmp$trt <- datTmp$trt[idx]
        datNew <- rbind.data.frame(datNew, datTmp)
    }
    MR <- matchR(datNew, ratio=ratio)
    mData <- MR$data
    fit <- glm(outcome ~ trt + tempo + trt:tempo + age + female + marriage + factor(edu) + hukouRural, family=quasibinomial, data=mData, weights=weights)
    return(coef(fit)["trt:tempo"])
}


Perm1 <- function() replicate(1000, permute(dat3, ratio=1))
Perm3 <- function() replicate(1000, permute(dat3, ratio=3))

library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3", "sourceNames", "kSource", "permute", "matchR")) # export object to each thread
tryCatch(resp1 <- clusterCall(cl=cl, fun = Perm1), finally = stopCluster(cl))
library(abind)
resp1 <- abind(resp1)

cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3", "sourceNames", "kSource", "permute", "matchR")) # export object to each thread
tryCatch(resp3 <- clusterCall(cl=cl, fun = Perm3), finally = stopCluster(cl))
library(abind)
resp3 <- abind(resp3)




FOO <- function(data=dat3, ratio=1){
    betas <- NULL
    n <- dim(data)[1]
    idx <- sample(1:n, size=n, replace=TRUE)
    tmp <- as.data.frame(data[idx,])
    fDataR <- matchR(tmp, ratio=ratio)$data
    didreg <- glm(outcome ~ trt + tempo + trt * tempo + age + female + marriage + factor(edu) + hukouRural, family = quasibinomial, data = fDataR, weight=weights)
    betas <- rbind(betas, coef(didreg))
    return(betas)
}


FOO2 <- function(data=dat3, ratio=1){
    betas <- NULL
    n <- dim(data)[1]
    idx <- sample(1:n, size=n, replace=TRUE)
    tmp <- as.data.frame(data[idx,])
    fDataR <- matchR(tmp, ratio=ratio)$data
    didreg <- glm(outcomeMax ~ trt + tempo + trt * tempo + age + female + marriage + hukouRural + factor(edu), family = quasibinomial, data = fDataR, weight=weights)
    betas <- rbind(betas, coef(didreg))
    return(betas)
}

FOOa <- function(data=dat3, ratio=1){
    betas <- NULL
    n <- dim(data)[1]
    idx <- sample(1:n, size=n, replace=TRUE)
    tmp <- as.data.frame(data[idx,])
    fDataR <- matchE(tmp, ratio=ratio)$data
    didreg <- glm(outcome ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fDataR, weight=weights)
    betas <- rbind(betas, coef(didreg))
    return(betas)
}

FOO2a <- function(data=dat3, ratio=1){
    betas <- NULL
    n <- dim(data)[1]
    idx <- sample(1:n, size=n, replace=TRUE)
    tmp <- as.data.frame(data[idx,])
    fDataR <- matchE(tmp, ratio=ratio)$data
    didreg <- glm(outcomeMax ~ trt2 + tempo + trt2 * tempo + age + female + marriage + hukouRural, family = quasibinomial, data = fDataR, weight=weights)
    betas <- rbind(betas, coef(didreg))
    return(betas)
}



set.seed(123)
BOOT1 <- function() replicate(1000, FOO(data=dat3, ratio=1))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO", "matchR", "kSource", "sourceNames")) # export object to each thread
tryCatch(res1 <- clusterCall(cl=cl, fun = BOOT1), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
res1 <- abind(lapply(res1, print), along=3)
out1 <- aperm(res1)



set.seed(123)
BOOT3 <- function() replicate(1000, FOO(data=dat3, ratio=3))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO", "matchR", "kSource", "sourceNames")) # export object to each thread
tryCatch(res3 <- clusterCall(cl=cl, fun = BOOT3), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
res3 <- abind(lapply(res3, print), along=3)
out3 <- aperm(res3)



set.seed(123)
BOOTMax1 <- function() replicate(1000, FOO2(data=dat3, ratio=1))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO2", "matchR", "kSource", "sourceNames")) # export object to each thread
tryCatch(resMax1 <- clusterCall(cl=cl, fun = BOOTMax1), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
resMax1 <- abind(lapply(resMax1, print), along=3)
outMax1 <- aperm(resMax1)
save(outMax1, file="outMax1.Rdata")


set.seed(123)
BOOTMax3 <- function() replicate(1000, FOO2(data=dat3, ratio=3))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO2", "matchR", "kSource", "sourceNames")) # export object to each thread
tryCatch(resMax3 <- clusterCall(cl=cl, fun = BOOTMax3), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
resMax3 <- abind(lapply(resMax3, print), along=3)
outMax3 <- aperm(resMax3)
save(outMax3, file="outMax3.Rdata")






set.seed(123)
BOOT1a <- function() replicate(1000, FOOa(data=dat3, ratio=1))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOOa", "matchE", "kSource", "sourceNames")) # export object to each thread
tryCatch(res1a <- clusterCall(cl=cl, fun = BOOT1a), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
res1a <- abind(lapply(res1a, print), along=3)
out1a <- aperm(res1a)
save(out1a, file="out1a.Rdata")



set.seed(123)
BOOT3a <- function() replicate(1000, FOOa(data=dat3, ratio=3))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOOa", "matchE", "kSource", "sourceNames")) # export object to each thread
tryCatch(res3 <- clusterCall(cl=cl, fun = BOOT3a), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
res3a <- abind(lapply(res3a, print), along=3)
out3a <- aperm(res3a)
save(out3a, file="out3a.Rdata")



set.seed(123)
BOOTMax1a <- function() replicate(1000, FOO2a(data=dat3, ratio=1))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO2a", "matchE", "kSource", "sourceNames")) # export object to each thread
tryCatch(resMax1a <- clusterCall(cl=cl, fun = BOOTMax1a), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
resMax1a <- abind(lapply(resMax1a, print), along=3)
outMax1a <- aperm(resMax1a)
save(outMax1a, file="outMax1a.Rdata")


set.seed(123)
BOOTMax3a <- function() replicate(1000, FOO2a(data=dat3, ratio=3))
library(parallel)
cl <- makeCluster(parallel::detectCores())
clusterExport(cl = cl, c("dat3","FOO2a", "matchE", "kSource", "sourceNames")) # export object to each thread
tryCatch(resMax3a <- clusterCall(cl=cl, fun = BOOTMax3a), finally = stopCluster(cl))
library(abind)
library(R2WinBUGS)
resMax3a <- abind(lapply(resMax3a, print), along=3)
outMax3a <- aperm(resMax3a)
save(outMax3a, file="outMax3a.Rdata")





library(readxl)
cases <- read_excel("figure1.xlsx")

yrs <- cases$year
case <- cases$cases

pdf("corruptionCases.pdf", width=5, height=4)
par(mar=c(3,3,1,1), mgp=c(1.5,0.15,0), tcl=-0.2)
plot(x=yrs, y=case, type="n", xaxs="i", yaxs="i", xlim=c(2003,2023), ylim=c(0, 700000), axes=FALSE, frame.plot=TRUE,
    xlab="Year", ylab="Number of Investigation Cases")
rect(xright=yrs-0.25, xleft=yrs+0.25, ybottom=0, ytop=case, col="gray", border=NA)
lines(x=yrs, y=case, lwd=1)
axis(1, seq(2004,2022, by=2), cex.axis=0.7)
axis(2, at=c(0, 200000, 400000, 600000), labels=c("0", "200,000", "400,000", "600,000"), cex.axis=0.7)
abline(v=2015, lty=2)
box()
dev.off()




# contrust trust proportion
yTab <- t(table(dat$trust_gover, dat$source))[1:10,]

rowLab <- rownames(yTab)
yList <- vector("list", length(rowLab))
for(i in 1:length(rowLab)){
    n <- sum(yTab[i,])
    if(i<6){
        nMiss <- sum(yTab[i,c("-1", "0","7", "8", "9")])/n
        nDisTrust <- sum(yTab[i,c("1","2","3")])/n
        nTrust <- yTab[i,c("4","5","6")]/n
    }else if(i>6){
        nMiss <- sum(yTab[i,c("-1", "0","5","7", "8", "9")])/n
        nDisTrust <- sum(yTab[i,c("1","2")])/n
        nTrust <- yTab[i,c("3","4")]/n
    }else{
        nMiss <- sum(yTab[i,c("-1", "0","7", "8", "9")])/n
        nDisTrust <- sum(yTab[i,c("1","2")])/n
        nTrust <- yTab[i,c("3","4","5")]/n
    }
    yList[[i]] <- c(nMiss, nDisTrust, nTrust)
}


pdf("outcomeStacked.pdf", width=5, height=4)
par(mar=c(2,5,2,1), mgp=c(0.5,0.2,0), tcl=-0.2)
plot(0,0, type="n", ylab="", xlab="Propotion to the sample", frame.plot=TRUE, xlim=c(0,1),
    ylim=c(11.5,0.5), axes=FALSE, xaxs="i", yaxs="i")
for(i in 1:10){
    rect(xright=0, xleft=yList[[i]][1], ytop=i+0.15, ybottom=i-0.15, col="black")
    rect(xright=yList[[i]][1], xleft=sum(yList[[i]][1:2]), ytop=i+0.15, ybottom=i-0.15)
    if(i < 7){
        rect(xright=sum(yList[[i]][1:2]), xleft=sum(yList[[i]][1:3]), ytop=i+0.15, ybottom=i-0.15, col="gray90")
        rect(xright=sum(yList[[i]][1:3]), xleft=sum(yList[[i]][1:4]), ytop=i+0.15, ybottom=i-0.15, col="gray70")
        rect(xright=sum(yList[[i]][1:4]), xleft=sum(yList[[i]][1:5]), ytop=i+0.15, ybottom=i-0.15, col="gray50")
    }else{
        rect(xright=sum(yList[[i]][1:2]), xleft=sum(yList[[i]][1:3]), ytop=i+0.15, ybottom=i-0.15, col="gray70")
        rect(xright=sum(yList[[i]][1:3]), xleft=sum(yList[[i]][1:4]), ytop=i+0.15, ybottom=i-0.15, col="gray50")
    }
}
axis(3)
axis(2, at=1:10, toupper(rowLab), las=2, tck=FALSE, adj=0)
rect(xright=0.01, xleft=0.03, ytop=11, ybottom=11.3, col="black", xpd=NA)
text(x=0.035,y=11.15,"% Missing (DK,NA)", xpd=NA, adj=0, cex=0.8)
rect(xright=0.4, xleft=0.42, ytop=11, ybottom=11.3, col="white", xpd=NA)
text(x=0.425,y=11.15,"% Distrust", xpd=NA, adj=0, cex=0.8)
rect(xright=c(0.7,0.72,0.74), xleft=c(0.72,0.74,0.76), ytop=11, ybottom=11.3, col=c("gray90", "gray70", "gray50"), xpd=NA)
text(x=0.765,y=11.15,"% Trust", xpd=NA, adj=0, cex=0.8)
dev.off()


pMissing <- tapply(is.na(dat2$outcome), dat2$year, mean)
pTrust <- tapply(dat2$outcome, dat2$year, mean, na.rm=TRUE)
pTrustMax <- tapply(dat2$outcomeMax, dat2$year, mean, na.rm=TRUE)
xYrs <- names(pTrust)

pdf("outcomeTrend.pdf", width=5, height=4)
par(mar=c(3,3,1,1), mgp=c(1.5,0.2,0), tcl=-0.2)
plot(x=xYrs, y=pTrust, type="l", ylim=c(0,1),ylab="Proportion to the sample", xlab="Year",
    axes=FALSE, frame.plot=TRUE, xaxs="i", yaxs="i")
axis(2, at=seq(0,1,by=0.2), cex.axis=0.9)
axis(1, at=sort(unique(dat2$year)), cex.axis=0.8)
lines(x=xYrs, y=pTrustMax, col="gray70")
lines(x=xYrs, y=pMissing, lty=3, lwd=1.5)
abline(v=2013, lty=2)
text(x=2008, y=pMissing["2008"], "Prop. missing", adj=0, cex=0.8)
text(x=2008, y=pTrustMax["2008"], "Prop. completely trust vs all others", adj=0, cex=0.8)
text(x=2008, y=pTrust["2008"]-0.05, "Prop. trust vs distrust", adj=0, cex=0.8)
dev.off()




dHist <- function(x, ylim=c(0,4500), main="", xlab="", ylab="", resLab=c("No","Yes")){
    y <- x
    plot(0,0, type="n", xlim=c(-0.5,1.5), ylim=ylim, main=main, ylab=ylab, xlab="",
    frame.plot=TRUE, axes=FALSE, yaxs="i", xpd=NA)
    rect(xright=c(-0.1,0.9), xleft=c(0.1,1.1), ybottom=c(0,0), ytop=c(y[1], y[2]))
    axis(1, c(0,1), resLab, tck=FALSE)
}


trtTab <- t(table(dat$trt, dat$source))[1:10,]

pdf("trt.pdf", width=8, height=4)
par(mfrow=c(2,5), mar=c(2,0.5,1.5,0.5), mgp=c(1.5,0.2,0), tcl=-0.2, oma=c(0,2,3,0))
dHist(trtTab[1,],ylab="count", main=toupper(rownames(trtTab)[1]))
axis(2, c(0, 1000, 2000, 3000, 4000), xpd=NA, cex.axis=0.8)
dHist(trtTab[2,],ylab="", main=toupper(rownames(trtTab)[2]))
axis(2, c(0, 1000, 2000, 3000, 4000), labels=FALSE)
dHist(trtTab[3,],ylab="", main=toupper(rownames(trtTab)[3]))
axis(2, c(0, 1000, 2000, 3000, 4000), labels=FALSE)
dHist(trtTab[4,],ylab="", main=toupper(rownames(trtTab)[4]))
axis(2, c(0, 1000, 2000, 3000, 4000), labels=FALSE)
dHist(trtTab[5,],ylab="", main=toupper(rownames(trtTab)[5]))
axis(2, c(0, 1000, 2000, 3000, 4000), labels=FALSE)
dHist(trtTab[6,],ylab="count", main=toupper(rownames(trtTab)[6]), ylim=c(0,12000))
axis(2, c(0, 3000, 6000, 9000, 12000), xpd=NA, cex.axis=0.8)
dHist(trtTab[7,],ylab="", main=toupper(rownames(trtTab)[7]), ylim=c(0,12000))
axis(2, c(0, 3000, 6000, 9000, 12000), xpd=NA, labels=FALSE)
dHist(trtTab[8,],ylab="", main=toupper(rownames(trtTab)[8]), ylim=c(0,12000))
axis(2, c(0, 3000, 6000, 9000, 12000), xpd=NA, labels=FALSE)
dHist(trtTab[9,],ylab="", main=toupper(rownames(trtTab)[9]), ylim=c(0,12000))
axis(2, c(0, 3000, 6000, 9000, 12000), xpd=NA, labels=FALSE)
dHist(trtTab[10,],ylab="", main=toupper(rownames(trtTab)[10]), ylim=c(0,12000))
axis(2, c(0, 3000, 6000, 9000, 12000), xpd=NA, labels=FALSE)
mtext("Is the respondent in the treated group? ", side=3, cex=1.2, line=1 ,outer=TRUE, font=2)
dev.off()


dHist2 <- function(x, main="", ylim=c(0,80000), xlab=NULL){
    N <- length(x)
    y <- table(x)
    nY <- length(y)
    n <- sum(y)
    rateMis <- (N-n)/N
    minX <- min(x, na.rm=TRUE)
    maxX <- max(x, na.rm=TRUE)
    xlim=c(minX-0.3, maxX+0.3)
    if(is.null(xlab)){
        xlab = rep("", nY)
    }
    chk <- ifelse(nY <6, 1, 0)
    if(chk){
    plot(0,0, xlab="", ylab="count", main=main, xlim=xlim, ylim=ylim,yaxs="i", axes=FALSE, type="n")
    if(nY == 2){
        rect(seq(minX,maxX)+0.05, rep(0,nY), seq(minX,maxX)-0.05, y)
    }else{
        rect(seq(minX,maxX)+0.2, rep(0,nY), seq(minX,maxX)-0.2, y)
    }
    #text(-2.3, 2400, bquote(N == .(N)), xpd=NA, adj=0)
    axis(1, seq(minX,maxX), labels=xlab, tck=FALSE, pos=1)
    axis(2)
    box()
    }else{
        hist(x, ylim=ylim, freq=TRUE, yaxs="i", xlab="", ylab="count", main=main, col="white")
    }
}

han <- ifelse(dat$race==-999, NA, dat$race)

pdf("summ.pdf", width=8, height=4)
layout(mat=matrix(c(1,2,3,4,5,6,6,0), nrow=2, ncol=4, byrow=TRUE), heights=c(1,1), widths=c(1,1,1,1))
par(mgp=c(1.5,0.2,0), mar=c(2,3,3,0.5),tcl=-0.2)
dHist2(dat$female, xlab=c("No", "Yes"), main="female")
dHist2(dat$hukouRural, xlab=c("No", "Yes"), main="Rural Household")
dHist2(dat$marriage,  ylim=c(0,80000), xlab=c("No", "Yes"), main="Married")
dHist2(han, ylim=c(0,80000), xlab=c("No", "Yes"), main="Race: Han")
dHist2(dat$age, ylim=c(0,10000), main="Age")
box()
dHist2(dat$edu, ylim=c(0,35000), xlab=rep("", 5), main="Education")
text(x=1:5, y=-1, labels=c("Elementry\nEdu", "Junior\nHigh Edu", "Senior\nHigh Edu", "College Edu", "Graudate\nLevel Edu"), pos=1, xpd=NA)
dev.off()


load("out1.Rdata")
load("out3.Rdata")
load("outMax1.Rdata")
load("outMax3.Rdata")
load("resp1.Rdata")
load("resp3.Rdata")


idx <- names(coef(M1))[c(2,3,12,4:11)]
oPts1 <- coef(M1)[idx]
sPts1 <- arm:::se.coef(M1)[idx]
m1Pts1 <- coef(didreg1)[idx]
m3Pts1 <- coef(didreg3)[idx]
ci1 <- apply(out1, 2, quantile, probs=c(0.025, 0.975))[,idx]
ci3 <- apply(out3, 2, quantile, probs=c(0.025, 0.975))[,idx]

oPtsMax1 <- coef(M2)[idx]
sPtsMax1 <- arm:::se.coef(M2)[idx]
m1PtsMax1 <- coef(didregMax1)[idx]
m3PtsMax1 <- coef(didregMax3)[idx]
ciMax1 <- apply(outMax1, 2, quantile, probs=c(0.025, 0.975))[,idx]
ciMax3 <- apply(outMax3, 2, quantile, probs=c(0.025, 0.975))[,idx]



coefNames <- c("Treatment", "Time", "Treatment:Time",
    c("Age", "Female", "Married","Rural residency", "Edu: Junior High", "        Senior High", "        College", "        Graduate"))

pdf("regression1.pdf", width=8, height=5)
layout(mat = matrix(c(1,2,3), nrow=1, ncol=3), width=c(2,4,4), height=c(1,1,1))
par(mar=c(1,1,4,0.5), mgp=c(2,0.15,0), tcl=-0.2)
plot(0,0,type="n",axes=FALSE, xlim=c(0,1), ylim=c(11.5,0.5),
    xlab="", ylab="", main="")
text(x=0.2, y=(1:11)-0.1, labels=coefNames, cex=1.2, adj=0, xpd=NA)

plot(0,0,type="n",axes=FALSE,frame.plot=TRUE,
    xlim=c(-1,1.2), ylim=c(11.5,0.5),
    xlab="", ylab="", main="Outcome (Trust vs Distrust)")
points(x=oPts1, y=1:11)
segments(x0=(oPts1 + 1.96*sPts1), y0=1:11, x1= (oPts1 - 1.96*sPts1), y1=1:11)
points(x=m1Pts1, y=(1:11)-0.1, pch=19)
segments(x0=ci1[1,], y0=(1:11)-0.1, x1=ci1[2,], y1=(1:11)-0.1)
points(x=m3Pts1, y=(1:11)-0.2, pch=8)
segments(x0=ci3[1,], y0=(1:11)-0.2, x1=ci3[2,], y1=(1:11)-0.2)
abline(v=0, lty=2)
axis(3)
text(x=0.5, y=10, bquote(N["raw"]*" = "*.(nrow(dat3))), adj=0, cex=1.1)
text(x=0.5, y=10.5, bquote(N["1 vs 1"]*" = "*.(nrow(fData1))), adj=0, cex=1.1)
text(x=0.5, y=11, bquote(N["1 vs 3"]*" = "*.(nrow(fData3))), adj=0, cex=1.1)

plot(0,0,type="n",axes=FALSE,frame.plot=TRUE,
    xlim=c(-1,1.2), ylim=c(11.5,0.5),
    xlab="", ylab="", main="Outcome (Completely Trust Others)")
points(x=oPtsMax1, y=1:11)
segments(x0=(oPtsMax1 + 1.96*sPtsMax1), y0=1:11, x1= (oPtsMax1 - 1.96*sPtsMax1), y1=1:11)
points(x=m1PtsMax1, y=(1:11)-0.1, pch=19)
segments(x0=ciMax1[1,], y0=(1:11)-0.1, x1=ciMax1[2,], y1=(1:11)-0.1)
points(x=m3PtsMax1, y=(1:11)-0.2, pch=8)
segments(x0=ciMax3[1,], y0=(1:11)-0.2, x1=ciMax3[2,], y1=(1:11)-0.2)
abline(v=0, lty=2)
axis(3)
legend("bottomright", legend=c("Raw", "1 vs 1 Matching", "1 vs 3 Matching"), pch=c(1,19,8), xpd=NA, cex=1.1)
dev.off()



load("out1a.Rdata")
load("out3a.Rdata")
load("outMax1a.Rdata")
load("outMax3a.Rdata")

idx <- names(coef(M3))[c(2,3,8,4:7)]
oPts1 <- coef(M3)[idx]
sPts1 <- arm:::se.coef(M3)[idx]
m1Pts1 <- coef(didreg1a)[idx]
m3Pts1 <- coef(didreg3a)[idx]
ci1 <- apply(out1a, 2, quantile, probs=c(0.025, 0.975))[,idx]
ci3 <- apply(out3a, 2, quantile, probs=c(0.025, 0.975))[,idx]

oPtsMax1 <- coef(M4)[idx]
sPtsMax1 <- arm:::se.coef(M4)[idx]
m1PtsMax1 <- coef(didregMax1a)[idx]
m3PtsMax1 <- coef(didregMax3a)[idx]
ciMax1 <- apply(outMax1a, 2, quantile, probs=c(0.025, 0.975))[,idx]
ciMax3 <- apply(outMax3a, 2, quantile, probs=c(0.025, 0.975))[,idx]



coefNames <- c("Treatment", "Time", "Treatment:Time",
    c("Age", "Female", "Married","Rural residency"))



pdf("regression2.pdf", width=7, height=3)
layout(mat = matrix(c(1,2,3), nrow=1, ncol=3), width=c(2,4,4), height=c(1,1,1))
par(mar=c(1,1,4,0.5), mgp=c(2,0.15,0), tcl=-0.2)
plot(0,0,type="n",axes=FALSE, xlim=c(0,1), ylim=c(7.5,0.5),
    xlab="", ylab="", main="")
text(x=0.2, y=(1:7)-0.1, labels=coefNames, cex=1.2, adj=0, xpd=NA)

plot(0,0,type="n",axes=FALSE,frame.plot=TRUE,
    xlim=c(-1,1.2), ylim=c(7.5,0.5),
    xlab="", ylab="", main="Outcome (Trust vs Distrust)")
points(x=oPts1, y=1:7)
segments(x0=(oPts1 + 1.96*sPts1), y0=1:7, x1= (oPts1 - 1.96*sPts1), y1=1:7)
points(x=m1Pts1, y=(1:7)-0.1, pch=19)
segments(x0=ci1[1,], y0=(1:7)-0.1, x1=ci1[2,], y1=(1:7)-0.1)
points(x=m3Pts1, y=(1:7)-0.2, pch=8)
segments(x0=ci3[1,], y0=(1:7)-0.2, x1=ci3[2,], y1=(1:7)-0.2)
abline(v=0, lty=2)
axis(3)
text(x=0.5, y=5.5, bquote(N["raw"]*" = "*.(nrow(dat3))), adj=0, cex=1)
text(x=0.5, y=6, bquote(N["1 vs 1"]*" = "*.(nrow(fData1))), adj=0, cex=1)
text(x=0.5, y=6.5, bquote(N["1 vs 3"]*" = "*.(nrow(fData3))), adj=0, cex=1)

plot(0,0,type="n",axes=FALSE,frame.plot=TRUE,
    xlim=c(-1,1.2), ylim=c(7.5,0.5),
    xlab="", ylab="", main="Outcome (Completely Trust Others)")
points(x=oPtsMax1, y=1:7)
segments(x0=(oPtsMax1 + 1.96*sPtsMax1), y0=1:7, x1= (oPtsMax1 - 1.96*sPtsMax1), y1=1:7)
points(x=m1PtsMax1, y=(1:7)-0.1, pch=19)
segments(x0=ciMax1[1,], y0=(1:7)-0.1, x1=ciMax1[2,], y1=(1:7)-0.1)
points(x=m3PtsMax1, y=(1:7)-0.2, pch=8)
segments(x0=ciMax3[1,], y0=(1:7)-0.2, x1=ciMax3[2,], y1=(1:7)-0.2)
abline(v=0, lty=2)
axis(3)
legend("bottomright", legend=c("Raw", "1 vs 1 Matching", "1 vs 3 Matching"), pch=c(1,19,8), xpd=NA, cex=0.9)
dev.off()







invlogit <- arm:::invlogit


logPlot <- function(fit, main="", com=c(1,2,3,4),...){
    betas <- coef(fit)
    plot(x=0, y=0, xlim=c(20,95), ylim=c(0.88,1), main=main, type="n", xlab="age", ylab="Pr(Y = 1)",axes=FALSE,frame.plot=TRUE,xaxs="i",yaxs="i",...)
    fx11 <- function(x) invlogit(betas[1] + betas[2]*1 + betas[3]*1 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*1*1)
    fx01 <- function(x) invlogit(betas[1] + betas[2]*0 + betas[3]*1 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*0*1)
    fx10 <- function(x) invlogit(betas[1] + betas[2]*1 + betas[3]*0 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*1*0)
    fx00 <- function(x) invlogit(betas[1] + betas[2]*0 + betas[3]*0 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*0*0)
    if(com==1){
        curve(fx00(x), from=20, to=95, add=TRUE, lwd=1, lty=2, col="gray65")
    }
    if(com==2){
        curve(fx00(x), from=20, to=95, add=TRUE, lwd=1, lty=2, col="gray65")
         curve(fx01(x), from=20, to=95, add=TRUE, lwd=1, lty=2, col=1)
    }
    if(com==3){
        curve(fx10(x), from=20, to=95, add=TRUE, lwd=1, lty=1, col="gray65")
        curve(fx00(x), from=20, to=95, add=TRUE, lwd=1, lty=2, col="gray65")
    }
    if(com==4){
        curve(fx11(x), from=20, to=95, add=TRUE, lwd=1, lty=1, col=1)
        curve(fx10(x), from=20, to=95, add=TRUE, lwd=1, lty=1, col="gray65")
    }
}

betas <- coef(didreg1)

fx11 <- function(x) invlogit(betas[1] + betas[2]*1 + betas[3]*1 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*1*1)
fx01 <- function(x) invlogit(betas[1] + betas[2]*0 + betas[3]*1 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*0*1)
fx10 <- function(x) invlogit(betas[1] + betas[2]*1 + betas[3]*0 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*1*0)
fx00 <- function(x) invlogit(betas[1] + betas[2]*0 + betas[3]*0 + betas[4]*x + betas[5]*1 + betas[6]*1 + betas[7]*1 + betas[10]*1 + betas[12]*0*0)


pdf("didcurves.pdf", width=8.5,height=2.5)
par(mfrow=c(1,3), mar=c(1.5,1,3,0),mgp=c(1.5,0.2,0),tcl=-0.2, oma=c(2,2,0,1))

#logPlot(M1, main="Baseline average", com=1)
#text(x=0, y=fx00(x=0)+0.05, labels="control, before 2013", adj=0, cex=1)
#axis(1, at=c(-1,-0.5,0,0.5,1,1.5), labels=FALSE)
#axis(2, seq(0,1,by=0.2), labels=seq(0,1,by=0.2), tick=TRUE)

logPlot(didreg1, main="Time trend in control group", com=2)
text(x=50, y=fx01(x=50), labels="control, after 2013", adj=0, cex=1)
text(x=50, y=fx00(x=50), labels="control, before 2013", adj=1, cex=1)
axis(1, at=c(20,35,50,65,80,95), labels=c(20,35,50,65,80,95))
axis(2, seq(0.9,1,by=0.05), labels=seq(0.9,1,by=0.05), tick=TRUE)

logPlot(didreg1, main="Difference between\ntwo groups before 2013", com=3)
text(x=50, y=fx10(x=50), labels="treated, before 2013", adj=1, cex=1)
text(x=50, y=fx00(x=50), labels="control, before 2013", adj=0, cex=1)
axis(1, at=c(20,35,50,65,80,95), labels=c(20,35,50,65,80,95))
axis(2, seq(0.9,1,by=0.05), labels=FALSE, tick=TRUE)

logPlot(M1, main="Time trend in treatment group", com=4)
text(x=50, y=fx10(x=50), labels="treated, before 2013", adj=0, cex=1)
text(x=50, y=fx11(x=50), labels="treated, after 2013", adj=1, cex=1)
axis(1, at=c(20,35,50,65,80,95), labels=c(20,35,50,65,80,95))
axis(2, seq(0.9,1,by=0.05), labels=FALSE, tick=TRUE)


mtext("age", side=1, font=1, cex=1.1, outer=TRUE)
mtext("Pr(Trust in central government)", side=2, font=1, cex=0.8, outer=TRUE, line=0.5)
dev.off()


attach(fData1)

getYhat <- function(fit, female=1, marriage=1, edu=4, hukouRural=1){
    dat11 <- data.frame("trt"=1, "tempo"=1, "age"=seq(18,95,length=1000), "female"=female, "edu"=edu, "marriage"=marriage, "hukouRural"=hukouRural)
    dat10 <- data.frame("trt"=1, "tempo"=0, "age"=seq(18,95,length=1000), "female"=female, "edu"=edu, "marriage"=marriage, "hukouRural"=hukouRural)
    dat01 <- data.frame("trt"=0, "tempo"=1, "age"=seq(18,95,length=1000), "female"=female, "edu"=edu, "marriage"=marriage, "hukouRural"=hukouRural)
    dat00 <- data.frame("trt"=0, "tempo"=0, "age"=seq(18,95,length=1000), "female"=female, "edu"=edu, "marriage"=marriage, "hukouRural"=hukouRural)
    yhat11 <- predict.glm(fit, newdata=dat11, type="response")
    yhat10 <- predict.glm(fit, newdata=dat10, type="response")
    yhat01 <- predict.glm(fit, newdata=dat01, type="response")
    yhat00 <- predict.glm(fit, newdata=dat00, type="response")
    yhat <- (yhat11-yhat10) - (yhat01-yhat00)
    return(yhat)
}

intplot <- function(fit, y1=getYhat(fit=fit), y2=getYhat(fit=fit),...){
    xs <- seq(18,95,length=1000)
    plot(x=0, y=0, xlim=c(20,95), ylim=c(0,0.03), type="n", xlab="age", ylab="Pr(Y = 1)",
    axes=FALSE, frame.plot=TRUE,xaxs="i", yaxs="i",...)
    lines(x=xs, y=y1, lty=2)
    lines(x=xs, y=y2)
    #axis(1)
    #axis(2, c(-0.2,-0.1,0))
}



pdf("beta.pdf", width=6.5, height=4)
par(mfrow=c(2,2), mar=c(1,1,1,0.5), mgp=c(1.5,0.2,0), tcl=-0.2, oma=c(3,3,0,0.5))

intplot(fit=didreg1, y1=getYhat(fit=didreg1, female=0), y2=getYhat(fit=didreg1, female=1))
axis(1,at=c(20,35,50,65,80,95),labels=FALSE)
axis(2, c(0,0.01,0.02,0.03), c("0","0.01","0.02","0.03"), xpd=NA)
legend("topright", legend=c("Male", "Female"), lty=c(2,1), cex=0.8)

intplot(fit=didreg1, y1=getYhat(fit=didreg1, marriage=0), y2=getYhat(fit=didreg1, marriage=1))
axis(1,at=c(20,35,50,65,80,95),labels=FALSE)
axis(2, c(0,0.01,0.02,0.03),labels=FALSE)
legend("topright", legend=c("Not married", "Married"), lty=c(2,1), cex=0.8)

intplot(fit=didreg1, y1=getYhat(fit=didreg1, edu=1), y2=getYhat(fit=didreg1,edu=4))
axis(1,at=c(20,35,50,65,80,95),labels=c(20,35,50,65,80,95))
axis(2, c(0,0.01,0.02,0.03), c("0","0.01","0.02","0.03"), xpd=NA)
legend("topright", legend=c("Elementary Edu.", "College Edu."), lty=c(2,1), cex=0.8)

intplot(fit=didreg1, y1=getYhat(fit=didreg1, hukouRural=0), y2=getYhat(fit=didreg1, hukouRural=1))
axis(1, at=c(20,35,50,65,80,95), labels=c(20,35,50,65,80,95), xpd=NA)
axis(2, c(0,0.01,0.02,0.03), labels=FALSE)
legend("topright", legend=c("Nonrural residency", "Rurual residency"), lty=c(2,1), cex=0.8)

mtext("age", side=1, font=1, cex=1.1, outer=TRUE, line=1)
mtext("Difference in changes over times", side=2, font=1, cex=1.1, outer=TRUE, line=1)
dev.off()









pMissing1 <- tapply(is.na(dat$trust_goverR[dat$trt==1]), dat$year[dat$trt==1], mean)[-4]
pMissing0 <- tapply(is.na(dat$trust_goverR[dat$trt==0]), dat$year[dat$trt==0], mean)[-4]
xYrs <- names(pMissing1)

pMissC <- tapply(is.na(dat$trust_courtR), dat$year, mean)[xYrs]


pdf("NA.pdf", height=3.5, width=8)
par(mfrow=c(1,2), mar=c(2,2,2,0.5), mgp=c(2,0.2,0), tcl=-0.2, oma=c(2,1,0,1))
plot(x=xYrs, y=pMissing1, ylim=c(0,0.21), type="l", axes=FALSE, frame.plot=TRUE,
    xaxs="i", yaxs="i", xlab="", ylab="", main="Trust in central government")
lines(x=xYrs, y=pMissing0, lty=2)
axis(1, xYrs)
axis(2, seq(0,0.2,by=0.05), labels=c("0", "0.05", "0.10", "0.15", "0.20"))
text(x=2014, y=pMissing0["2014"], labels="Untreated", adj=1, xpd=NA, cex=0.8)
text(x=2014, y=pMissing1["2014"], labels="Treated", adj=0.5, xpd=NA, cex=0.8)

plot(x=xYrs, y=pMissing, ylim=c(0,0.21), type="l", axes=FALSE, frame.plot=TRUE,
    xaxs="i", yaxs="i", xlab="", ylab="", main="Sensitive vs Nonsensitive Qs")
lines(x=xYrs, y=pMissC, col="gray75")
axis(2, seq(0,0.2,by=0.05), labels=FALSE)
axis(1, xYrs)
text(x=2010, y=pMissC["2010"], labels="Trust in courts", adj=0, xpd=NA, cex=0.8)
text(x=2011, y=pMissing["2011"], labels="Trust in central gov't", adj=0, xpd=NA, cex=0.8)

mtext("Nonresponse rate", side=2, outer=TRUE, cex=1.1)
mtext("Survey years", side=1, outer=TRUE, line=0, cex=1.1)
dev.off()







load("resp1.Rdata")
load("resp3.Rdata")


pdf("placebo.pdf", width=7, height=3.5)
par(mfrow=c(1,2), mar=c(2,1.5,2,0.2), mgp=c(1.5,0.2,0), tcl=-0.2, oma=c(0.5,1,0,0.5))
beta01 <- coef(didreg1)[12]
beta03 <- coef(didreg3)[12]
hist(resp1, freq=FALSE,
    xaxs="i", yaxs="i", ylim=c(0,4), xlim=c(-0.83,0.83), main="1 vs 1 Matching", col="lightgray", border="white",
    nclass=15, axes=FALSE, ylab="", xlab="")
abline(v=beta01, col=1, lty=2)
axis(1)
axis(2)
lines(density(resp1))
box()

hist(resp3, freq=FALSE,
    xaxs="i", yaxs="i", ylim=c(0,4), xlim=c(-0.83,0.83), main="1 vs 3 Matching", col="lightgray", border="white",
    nclass=15, axes=FALSE, ylab="", xlab="")
abline(v=beta03, col=1, lty=2)
axis(1)
axis(2, labels=FALSE)
lines(density(resp3))
box()
mtext("The DID estimates", side=1, outer=TRUE, cex=1.1, line=-1)
mtext("Density", side=2, outer=TRUE, cex=1.1)
dev.off()
