#' Calculating measures for calibration for benefit for a prediction model
#'
#' This function calculates a series of measures to assess decision accuracy, discrimination for benefit, and calibration for benefit of a prediction model.
#' @param repeats The number of repetitions for the algorithm.
#' @param Ngroups The number of groups to split the data.
#' @param X A dataframe with patient covariates.
#' @param Y The observed outcome. For binary outcomes this should be 0 or 1
#' @param treat A vector with the treatment assignment. This must be 0 (for control treatment)
#' or 1 (for active treatment).
#' @param predicted.treat.0 A vector with the model predictions for each patient, under the control treatment.
#' For the case of a binary outcome this should be probabilities of an event.
#' @param predicted.treat.1 A vector with the model predictions for each patient, under the active treatment.
#' For the case of a binary outcome this should be probabilities of an event.
#' @param type The type of the outcome, "binary" or "continuous".
#' @param bootstraps The number of bootstrap samples to be used for calculating confidence intervals.
#' @return A table with all estimated measures of performance.
#' @importFrom Matching Match
#' @importFrom Hmisc rcorr.cens
#' @importFrom stats coef confint binomial complete.cases complete.cases glm lm median predict quantile sd var kmeans
#' @examples
#'  # continuous outcome
#'  dat0=simcont(500)$dat
#'  head(dat0)
#'  # Randomly shuffle the data
#'  dat<-dat0[sample(nrow(dat0)),]
#'  # Create random folds
#'  dat$folds <- cut(seq(1,nrow(dat)),breaks=10,labels=FALSE)
#'
#'  # Obtain out-of-sample predictions
#'  dat.out.CV<-list()
#'  for (i in 1:10){
#'    dat.in.CV=dat[dat$folds!=i,]
#'    dat.out.CV[[i]]=dat[dat$folds==i,]
#'    dat1<-dat.out.CV[[i]]; dat1$t=1
#'    dat0<-dat.out.CV[[i]]; dat0$t=0
#'    m1=lm(data=dat.in.CV, y.observed~x1*t+x2*t)
#'    dat.out.CV[[i]]$predict.treat.1=predict(newdata=dat1, m1)# predictions in treatment
#'    dat.out.CV[[i]]$predict.treat.0=predict(newdata=dat0, m1)# predicions in control
#'  }
#'
#'  dat.CV=dat.out.CV[[1]]
#'  for (i in 2:10){  dat.CV=rbind(dat.CV,dat.out.CV[[i]])}
#'
#'  # assess model performance
#'  predieval(repeats=20, Ngroups=c(5:10),
#'              X=dat.CV[,c("x1", "x2","x3")],
#'              Y=dat.CV$y.observed,
#'              predicted.treat.1 = dat.CV$predict.treat.1,
#'              predicted.treat.0 = dat.CV$predict.treat.0,
#'              treat=dat.CV$t, type="continuous")
#'
#'
#'  # binary outcome
#'  dat0=simbinary(500)$dat
#'  head(dat0)
#'
#'  # Randomly shuffle the data
#'  dat<-dat0[sample(nrow(dat0)),]
#'  # Create random folds
#'  dat$folds <- cut(seq(1,nrow(dat)),breaks=10,labels=FALSE)
#'
#'  dat.out.CV<-list()
#'  for (i in 1:10){
#'    dat.in.CV=dat[dat$folds!=i,]
#'    dat.out.CV[[i]]=dat[dat$folds==i,]
#'    dat1<-dat.out.CV[[i]]; dat1$t=1
#'    dat0<-dat.out.CV[[i]]; dat0$t=0
#'    glm1=glm(y.observed~(x1+x2+x3)*t, data=dat.in.CV, family = binomial(link = "logit"))
#'    dat.out.CV[[i]]$predict.treat.1=predict(newdata=dat1, glm1)# predictions in treatment
#'    dat.out.CV[[i]]$predict.treat.0=predict(newdata=dat0, glm1)# predicions in control
#'  }
#'
#'  dat.CV=dat.out.CV[[1]]
#'  for (i in 2:10){  dat.CV=rbind(dat.CV,dat.out.CV[[i]])}
#'
#'
#'  predieval(repeats=20, Ngroups=c(5:10), X=dat.CV[,c("x1", "x2","x3")],
#'              Y=dat.CV$y.observed,
#'              predicted.treat.1 = expit(dat.CV$predict.treat.1),
#'              predicted.treat.0 = expit(dat.CV$predict.treat.0),
#'              treat=dat.CV$t, type="binary",bootstraps = 50)

#'
#' @export
predieval<-function(repeats=50,
                     Ngroups=10,
                     X,
                     treat,
                     Y,
                     predicted.treat.1,

                     predicted.treat.0,
                     type="continuous",
                     bootstraps=500
){

  Ngroups.length=length(Ngroups)
  ################ continuous data --------------
  if(type=="continuous"){
    cat(" Type of outcome: continuous","\n",
        "Repeats: ", repeats, "\n")

    dat1<-cbind(X,"Y"=c(Y),"t"=treat,"benefit"=predicted.treat.1-predicted.treat.0)

    # calibration - mean bias
    mean.bias<-mean(dat1$Y[dat1$t==1])-mean(dat1$Y[dat1$t==0])-mean(dat1$benefit)

    ## calibration - group by benefit
    rmse.reg<- c(); a0.reg<- c(); a1.reg<- c(); r2.reg<- c()
    for(group in 1:Ngroups.length){
      quantiles1 <- quantile(dat1$benefit, probs <-  seq(0, 1, 1/Ngroups[[group]]))
      g1 <- list(); predicted.reg <-  c();observed.reg <-  c();
      for (i in 1:Ngroups[[group]]) {g1[[i]] <- dat1[dat1$benefit >=  quantiles1[i] &
                                                       dat1$benefit < quantiles1[i + 1], ]
      predicted.reg <-  c(predicted.reg, mean(g1[[i]]$benefit))
      observed.reg <-  c(observed.reg, mean(g1[[i]]$Y[g1[[i]]$t == 1]) -
                           mean(g1[[i]]$Y[g1[[i]]$t == 0]))    }
      compare<-data.frame(predicted.reg, observed.reg)
      compare<-compare[complete.cases(compare),]
      rmse.reg<-  c(rmse.reg, sqrt(mean((compare$predicted.reg-compare$observed.reg)^2)))
      lm1r<- summary(lm(compare$observed.reg~compare$predicted.reg))
      a0.reg<- c(a0.reg, coef(lm1r)[1,1])
      a1.reg<- c(a1.reg, coef(lm1r)[2,1])
      r2.reg<- c(r2.reg, lm1r$r.squared)
    }

    results.regr=data.frame(Method=c( paste("group by benefit N=",Ngroups[1], sep="")),
                            RMSE=rmse.reg[1]
                            ,a0=a0.reg[1]
                            ,a1=a1.reg[1]
                            ,R2=r2.reg[1])

    if(Ngroups.length>1){
      for (n in 2:Ngroups.length){
        d1<-data.frame(Method=c(paste("group by benefit N=",Ngroups[n], sep="")),
                       RMSE=rmse.reg[n]
                       ,a0=a0.reg[n]
                       ,a1=a1.reg[n]
                       ,R2=r2.reg[n])
        results.regr=rbind(results.regr, d1)
      }
    }

    # Decision accuracy PB
    g11<-dat1[dat1$benefit>0& dat1$t==1,]; n11<-length(g11$benefit)
    g12<-dat1[dat1$benefit>0& dat1$t==0,]; n12<-length(g12$benefit)
    g13<-dat1[dat1$benefit<0& dat1$t==1,]; n13<-length(g13$benefit)
    g14<-dat1[dat1$benefit<0& dat1$t==0,]; n14<-length(g14$benefit)
    dat1$agree1<-( sign(dat1$benefit) == sign(2*dat1$t-1))
    dat.disc<-cbind("Y"=dat1$Y, "agree1"=dat1$agree1, X)
    s1<-summary(lm(Y~., data=dat.disc))$coef[2,]
    discr.1<-(paste(round(s1[1], digits=2), " [",round(s1[1]-1.96*s1[2], digits=2), "; ",
                   round(s1[1]+1.96*s1[2], digits=2), "]", sep=""))

    PB0<-sum(g11$Y)/(n11+n14)-sum(g12$Y)/(n12+n14)+sum(g14$Y)*(1/(n11+n14)-1/(n12+n14))
    var.PB0<-n11*var(g11$Y)/(n11+n14)^2+n12*var(g12$Y)/(n12+n14)^2+n14*var(g14$Y)*(1/(n11+n14)-1/(n12+n14))^2
    discr.2<-paste(round(PB0, digits=2), " [",round(PB0-1.96*sqrt(var.PB0), digits=2), "; ",
                  round(PB0+1.96*sqrt(var.PB0), digits=2), "]", sep="")

    PB1<-sum(g14$Y)/(n11+n14)-sum(g13$Y)/(n11+n13)+sum(g11$Y)*(1/(n11+n14)-1/(n11+n13))
    var.PB1<-n14*var(g14$Y)/(n11+n14)^2+n13*var(g13$Y)/(n11+n13)^2+n11*var(g11$Y)*(1/(n11+n14)-1/(n11+n13))^2
    discr.3<-paste(round(PB1, digits=2), " [",round(PB1-1.96*sqrt(var.PB1), digits=2), "; ",
                  round(PB1+1.96*sqrt(var.PB1), digits=2), "]", sep="")



    # k-means clustering
    rmse.m1<-c()
    coeff.m11<-c()
    coeff.m12<-c()
    R.2.m1<-c()
    results.kmeans<-list()
    percent1.s2<-list()
    for(n in 1:Ngroups.length){

      percent.s2<-c()
      for (k in 1:repeats)
      {
        groups.kmean<-kmeans(x=dat1[,colnames(X)], centers = Ngroups[n], iter.max = 50)
        dat1$group<-groups.kmean$cluster
        ngroups<-as.vector(table(dat1$group))
        observed.groups1<-c()
        observed.groups0<-c()
        estimated.groups.m1<-c()

        dall<-NULL
        for( i in 1:Ngroups[n]){
          # observed
          observed.groups1<-c(observed.groups1,(mean(dat1$Y[dat1$group==i & dat1$t==1])))
          observed.groups0<-c(observed.groups0,(mean(dat1$Y[dat1$group==i & dat1$t==0])))
          observed.sign<-(observed.groups1-observed.groups0)>0
          #estimated
          estimated.groups.m1<-c(estimated.groups.m1,mean(dat1$benefit[dat1$group==i]))
          estimated.sign<-(estimated.groups.m1>0)
        }
        # collate resuts
        dall1<-(cbind(observed.sign, estimated.sign))
        dall1<-dall1[complete.cases(dall1),]
        percent.s2<-c(percent.s2, sum(dall1[,1]==dall1[,2] )/length(dall1[,1]))

        dall<-data.frame(observed.groups0, observed.groups1, estimated.groups.m1,ngroups)
        dall<-dall[complete.cases(dall),]
        dall$observed.groups=dall$observed.groups1-dall$observed.groups0
        reg.m1<-summary(lm(dall$observed.groups~dall$estimated.groups.m1, weights= dall$ngroups))
        coeff.m11<-c( coeff.m11, coef(reg.m1)[1])
        coeff.m12<-c( coeff.m12,coef(reg.m1)[2])
        R.2.m1<-c(R.2.m1, reg.m1$r.squared)
        rmse.m1<-c(rmse.m1, sqrt(mean((dall$observed.groups-dall$estimated.groups.m1)^2))) }
      # summary
      percent1.s2[[n]]<-round(mean(percent.s2), digits=2)

      results.kmeans[[n]]<-data.frame(rmse= round(median(rmse.m1),digits=2),
                                      a0= round(median(coeff.m11),digits=2),
                                      a1= round(median(coeff.m12),digits=2),
                                      R2= round(median(R.2.m1),digits=2)  )
    }

    # match by X ---
    rmse.m1<-c()
    m1.a0<-c()
    m1.a1<-c()
    m1.R2<-c()
    percentX.s22<-c()
    for(k in 1:repeats){
      g1<-dat1[dat1$t==1,]
      g0<-dat1[dat1$t==0,]
      n1<-min(length(g1$t) ,length(g0$t))
      g1<-g1[sample(length(g1$t),n1),]
      g0<-g0[sample(length(g0$t),n1),]
      dataall<-rbind(g1,g0)
      covariates<-dataall[,colnames(X)]

      matched<-Match(Tr=dataall$t, X=covariates)

      data.compare<-data.frame(tr.obs=dataall$Y[matched$index.treated], ctr.obs=dataall$Y[matched$index.control],
                              benefit.m1=(dataall$benefit[matched$index.treated]+dataall$benefit[matched$index.control])/2)

      data.compare$obs.benefit<-with(data.compare, tr.obs-ctr.obs)
      percentX.s22<-c(percentX.s22, mean(sign(data.compare$obs.benefit)==sign(data.compare$benefit.m1)))
      rmse.m1<-c(rmse.m1, sqrt(mean((data.compare$obs.benefit-data.compare$benefit.m1)^2)))
      reg.m1<-lm(data.compare$obs.benefit~data.compare$benefit.m1)
      m1.a0<-c(m1.a0, coef(reg.m1)[1]); m1.a1=c(m1.a1, coef(reg.m1)[2]);  m1.R2=c(m1.R2, summary(reg.m1)$r.squared)
    }

    # summary
    results.after.matching<-data.frame(rmse=round(median(rmse.m1),digits=2),
                                      a0= round(median(m1.a0),digits=2),
                                      a1= round(median(m1.a1),digits=2),
                                      R2= round(median(m1.R2),digits=2)                      )

    ### match by benefit----
    dataall<-g0<-g1<-n1<-covariates<-data.compare<-matched<-reg.m1<-NULL
    rmse.m1<-m1.a0<-m1.a1<-m1.R2<-c()
    percentB.s2=c()
    for(k in 1:repeats){
      g1<-dat1[dat1$t==1,]
      g0<-dat1[dat1$t==0,]
      n1<-min(length(g1$t) ,length(g0$t))
      g1<-g1[sample(length(g1$t),n1),]
      g0<-g0[sample(length(g0$t),n1),]
      dataall<-rbind(g1,g0)
      matched1<-Match(Tr=dataall$t, X=dataall$benefit)

      data.compare<-data.frame(tr.obs=dataall$Y[matched1$index.treated], ctr.obs=dataall$Y[matched1$index.control],
                              benefit.m1=(dataall$benefit[matched1$index.treated]+dataall$benefit[matched1$index.control])/2)

      data.compare$obs.benefit<-with(data.compare, tr.obs-ctr.obs)
      percentB.s2<-c(percentB.s2, mean(sign(data.compare$obs.benefit)==sign(data.compare$benefit.m1)))
      rmse.m1<-c(rmse.m1, sqrt(mean((data.compare$obs.benefit-data.compare$benefit.m1)^2)))
      reg.m1<-lm(data.compare$obs.benefit~data.compare$benefit.m1)
      m1.a0<-c(m1.a0, coef(reg.m1)[1]); m1.a1=c(m1.a1, coef(reg.m1)[2]);  m1.R2=c(m1.R2, summary(reg.m1)$r.squared)
    }
    ## summary
    results.d<-data.frame("Estimand, estimation method"=c("Population-level benefit (PB), adjusted",
                                                         "Population-level benefit vs t=0 (PB0)",
                                                         "Population-level benefit vs t=1 (PB1)",
                                                         paste("Benefit accuracy (BA), k-means N=",Ngroups[1], sep="")),
                         "Estimate"=c(discr.1,discr.2, discr.3,  round(percent1.s2[[1]], digits=2)), check.names = F)
    results.after.matching.benefit=data.frame(rmse=round(median(rmse.m1),digits=2),
                                              a0= round(median(m1.a0),digits=2),
                                              a1= round(median(m1.a1),digits=2),
                                              R2= round(median(m1.R2),digits=2)                      )

    results<-data.frame(Method=c( paste("kmeans N=",Ngroups[1], sep="")),
                       RMSE=c( results.kmeans[[1]]$rmse)
                       ,a0=c( results.kmeans[[1]]$a0)
                       ,a1=c( results.kmeans[[1]]$a1)
                       ,R2=c( results.kmeans[[1]]$R2) )

    if(Ngroups.length>1){


      for (n in 2:Ngroups.length){
        d1<-data.frame(Method=c(paste("kmeans N=",Ngroups[n], sep="")),
                       RMSE=c( results.kmeans[[n]]$rmse)
                       ,a0=c(  results.kmeans[[n]]$a0)
                       ,a1=c(results.kmeans[[n]]$a1)
                       ,R2=c( results.kmeans[[n]]$R2))
        results=rbind(results, d1)

        d2<-data.frame("Estimand, estimation method"=c(paste("Benefit accuracy (BA), k-means N=",Ngroups[n], sep="")),
                       "Estimate"=as.character(c(round(percent1.s2[[n]], digits=2))), check.names = F)
        results.d=rbind(results.d, d2)
      }
    }
    results.d<-rbind(results.d, data.frame("Estimand, estimation method"="Benefit accuracy (BA), match by covariates", "Estimate"=as.character(round(mean(percentX.s22), digits=2)), check.names = F))
    results.d<-rbind(results.d, data.frame("Estimand, estimation method"="Benefit accuracy (BA), match by benefit", "Estimate"=as.character(round(mean(percentB.s2), digits=2)), check.names = F))

    results<-rbind(results.regr, results)

    results<-rbind(results, data.frame(Method=c("match by covariates", "match by benefit"),
                                      RMSE=c( results.after.matching$rmse ,results.after.matching.benefit$rmse)
                                      ,a0=c( results.after.matching$a0 ,results.after.matching.benefit$a0)
                                      ,a1=c( results.after.matching$a1 ,results.after.matching.benefit$a1)
                                      ,R2=c( results.after.matching$R2 ,results.after.matching.benefit$R2) ))





    dat1$predicted.treat.0<-predicted.treat.0
    lm1<-lm(dat1$Y~dat1$predicted.treat.0+dat1$t:dat1$benefit)
    sum.lm1<-summary(lm1)

    ci1<-confint(lm1)
    c2<-paste(round(summary(lm1)$coef[3],digits=2)," [",round( ci1[3,1],digits=2),"; " ,round(ci1[3,2], digits=2),"]", sep="")


    results.reg1<-data.frame("Calibration slope"=c2)
    rownames(results.reg1)<-c("Estimate")
    results[2:5]<-round(results[2:5], digits=3)
    mean.bias<-round(mean.bias, 2)
    results.all<-list("outcome.type"=type,"decision.accuracy"=results.d, "calibration.for.benefit"=results,
                     "regression.for.benefit"=results.reg1, "mean.bias"=mean.bias)



  }


  ################ binary data  --------------

  if(type=="binary"){


    cat(" Type of outcome: binary","\n",
        "Repeats: ", repeats, "\n",
        "Number of bootstraps for caclulating confidence intervals: ", bootstraps,"\n" )


    dat1<-cbind(X,"Y"=Y,"t"=treat, "predicted.treat.1"=predicted.treat.1,
                "predicted.treat.0"=predicted.treat.0, "benefit"=predicted.treat.1-predicted.treat.0)


    ### decision accuracy PB
    g11=dat1[dat1$benefit>0& dat1$t==1,]; n11=length(g11$benefit)
    g12=dat1[dat1$benefit>0& dat1$t==0,]; n12=length(g12$benefit)
    g13=dat1[dat1$benefit<0& dat1$t==1,]; n13=length(g13$benefit)
    g14=dat1[dat1$benefit<0& dat1$t==0,]; n14=length(g14$benefit)
    dat1$agree=1*( sign(dat1$benefit) == sign(2*dat1$t-1))
    dat.disc=cbind("Y"=dat1$Y, "agree"=dat1$agree, X)
    magree=glm(Y~., data=dat.disc, family = "binomial")
    d.test0=dat.disc; d.test0$agree=0
    p0=predict(magree, newdata=d.test0, type = "response")
    d.test1=dat.disc; d.test1$agree=1
    p1=predict(magree, newdata=d.test1, type = "response")
    S1mean=mean(p1)-mean(p0)
    bootdif <- function(dd) {
      boot=sample(nrow(dd), replace = T)
      db=dd[boot,]
      glmfit <- glm(Y~., data=db, family = "binomial")
      newdata <- db;newdata$agree=0
      p0 <- mean(predict(glmfit, newdata, type = "response"))
      newdata <- db;newdata$agree=1
      p1 <- mean(predict(glmfit, newdata, type = "response"))
      return(p1 - p0) }
    bootest <- unlist(lapply(1:bootstraps, function(x) bootdif(dat.disc)))
    da.1=paste(round(S1mean, digits=3),
               " [", round(quantile(bootest, c(0.025, 0.975))[1], digits=3), "; ",
               round(quantile(bootest, c(0.025, 0.975))[2], digits=3),"]", sep="")


    PB0=sum(g11$Y)/(n11+n14)-sum(g12$Y)/(n12+n14)+sum(g14$Y)*(1/(n11+n14)-1/(n12+n14))
    var.PB0=n11*var(g11$Y)/(n11+n14)^2+n12*var(g12$Y)/(n12+n14)^2+n14*var(g14$Y)*(1/(n11+n14)-1/(n12+n14))^2
    da.2=paste(round(PB0, digits=3), " [",round(PB0-1.96*sqrt(var.PB0), digits=3), "; ",
               round(PB0+1.96*sqrt(var.PB0), digits=3), "]", sep="")

    PB1=sum(g14$Y)/(n11+n14)-sum(g13$Y)/(n11+n13)+sum(g11$Y)*(1/(n11+n14)-1/(n11+n13))
    var.PB1=n14*var(g14$Y)/(n11+n14)^2+n13*var(g13$Y)/(n11+n13)^2+n11*var(g11$Y)*(1/(n11+n14)-1/(n11+n13))^2
    da.3=paste(round(PB1, digits=3), " [",round(PB1-1.96*sqrt(var.PB1), digits=3), "; ",
               round(PB1+1.96*sqrt(var.PB1), digits=3), "]", sep="")


    ### BA and c for benefit --------
    percent.1on1.ben=c()
    c.by.benefit=c(); se.c.by.benefit=c()
    c.by.covariates=c(); se.c.by.covariates=c()
    n.0 <- sum(dat1$t==0)
    n.1 <- sum(dat1$t==1)
    n.10=min(n.0, n.1)
    for (i in 1:repeats){
      rows.in<-c(which(dat1$t==1)[sample(n.1, n.10)], which(dat1$t==0)[sample(n.0, n.10)])
      dat2=dat1[rows.in[order(rows.in)],]
      X2=X[rows.in[order(rows.in)],]
      ## match by benefit
      ind.0 <- which(dat2$t==0)
      order.0 <- order(dat2$benefit[ind.0])
      ind.0 <- ind.0[order.0]

      ind.1 <- which(dat2$t==1)
      order.1 <- order(dat2$benefit[ind.1])
      ind.1 <- ind.1[order.1]

      pred.ben.0 <- dat2$benefit[ind.0]
      pred.ben.1 <- dat2$benefit[ind.1]
      pred.ben.avg <- (pred.ben.1+pred.ben.0)/2

      obs.out.0 <- dat2$Y[ind.0]
      obs.out.1<- dat2$Y[ind.1]
      obs.ben <- obs.out.1-obs.out.0

      pred.ben.avg2=pred.ben.avg[obs.ben!=0]
      obs.ben2=obs.ben[obs.ben!=0]

      percent.1on1.ben=c(percent.1on1.ben, mean(sign(obs.ben2)==sign(pred.ben.avg2)))

      cindex <- rcorr.cens(pred.ben.avg, obs.ben)
      c.by.benefit <- c(c.by.benefit, cindex["C Index"][[1]])
      se.c.by.benefit<-c(se.c.by.benefit, cindex["S.D."][[1]]/2	)

      ## match by covariates
      percent.1on1.X=c()
      rr1 <- Match(Tr=dat2$t, X=X2, M=1,ties=F,replace=FALSE)
      ind.0 <- rr1$index.control
      ind.1 <- rr1$index.treated
      ### Calculation of predicted and observed benefit in matched pairs
      pred.ben.0 <- dat2$benefit[ind.0]
      pred.ben.1 <- dat2$benefit[ind.1]
      pred.ben.avg <- (pred.ben.1+pred.ben.0)/2
      obs.out.0 <- dat2$Y[ind.0]
      obs.out.1 <- dat2$Y[ind.1]
      obs.ben <- obs.out.1-obs.out.0

      pred.ben.avg2=pred.ben.avg[obs.ben!=0]
      obs.ben2=obs.ben[obs.ben!=0]

      percent.1on1.X=c(percent.1on1.X, mean(sign(obs.ben2)==sign(pred.ben.avg2)))
      # Benefit c-statistic
      cindex2 <- rcorr.cens(pred.ben.avg, obs.ben)
      c.by.covariates <- c(c.by.covariates, cindex2["C Index"][[1]])
      se.c.by.covariates<-c(se.c.by.covariates, cindex2["S.D."][[1]]/2	)

    }

    mean.percent.1on1.ben=mean(percent.1on1.ben)
    mean.percent.1on1.X=mean(percent.1on1.X)
    results.c.f.b=c(
      paste(round(mean(c.by.covariates), digits=2), "[", round(mean(c.by.covariates)-1.96*mean(se.c.by.covariates), digits=2), "; ",
            round(mean(c.by.covariates)+1.96*mean(se.c.by.covariates), digits = 2), "]", sep=""),
      paste(round(mean(c.by.benefit), digits=2), "[", round(mean(c.by.benefit)-1.96*mean(se.c.by.benefit), digits=2),
            "; ", round(mean(c.by.benefit)+1.96*mean(se.c.by.benefit), digits = 2), "]", sep=""))


    # calibration - mean bias
    ben.observed=mean(dat1$Y[dat1$t==1])-mean(dat1$Y[dat1$t==0])
    ben.model=mean((dat1$predicted.treat.1))-mean((dat1$predicted.treat.0))
    mean.bias=ben.observed-ben.model
    ## calibration - group by benefit
    rmse.reg<- c(); a0.reg<- c(); a1.reg<- c(); r2.reg<- c()
    for(group in 1:Ngroups.length){
      quantiles1 <- quantile(dat1$benefit, probs <-  seq(0, 1, 1/Ngroups[[group]]))
      g1 <- list(); predicted.reg <-  c();observed.reg <-  c();
      for (i in 1:Ngroups[[group]]) {g1[[i]] <- dat1[dat1$benefit >=  quantiles1[i] &
                                                       dat1$benefit < quantiles1[i + 1], ]
      predicted.reg <-  c(predicted.reg, mean(g1[[i]]$benefit))
      observed.reg <-  c(observed.reg, mean(g1[[i]]$Y[g1[[i]]$t == 1]) -
                           mean(g1[[i]]$Y[g1[[i]]$t == 0]))    }
      compare=data.frame(predicted.reg, observed.reg)
      compare=compare[complete.cases(compare),]
      rmse.reg<-  c(rmse.reg, sqrt(mean((compare$predicted.reg-compare$observed.reg)^2)))
      lm1r<- summary(lm(compare$observed.reg~compare$predicted.reg))
      a0.reg<- c(a0.reg, coef(lm1r)[1,1])
      a1.reg<- c(a1.reg, coef(lm1r)[2,1])
      r2.reg<- c(r2.reg, lm1r$r.squared)
    }

    results.regr=data.frame(Method=c( paste("group by benefit N=",Ngroups[1], sep="")),
                            RMSE=rmse.reg[1]
                            ,a0=a0.reg[1]
                            ,a1=a1.reg[1]
                            ,R2=r2.reg[1])

    if(Ngroups.length>1){
      for (n in 2:Ngroups.length){
        d1<-data.frame(Method=c(paste("group by benefit N=",Ngroups[n], sep="")),
                       RMSE=rmse.reg[n]
                       ,a0=a0.reg[n]
                       ,a1=a1.reg[n]
                       ,R2=r2.reg[n])
        results.regr=rbind(results.regr, d1)
      }
    }


    # match via kmeans
    rmse.m<-coeff.1<-coeff.2<-R.2.m<-c()
    results.kmeans.bin<-list()
    percent1.s2<-list()
    for(n in 1:Ngroups.length){
      percent.s2<-c()
      for (k in 1:repeats)
      {
        groups.kmean<-kmeans(x=dat1[,colnames(X)], centers = Ngroups[n], iter.max = 50)
        dat1$group<-groups.kmean$cluster
        ngroups<-as.vector(table(dat1$group))
        observed.groups1<-c()
        observed.groups0<-c()
        estimated.groups<-c()
        dall=NULL
        dall1=NULL
        for( i in 1:Ngroups[n]){
          # observed
          observed.groups1=c(observed.groups1,
                             (mean(dat1$Y[dat1$group==i & dat1$t==1])))

          observed.groups0=c(observed.groups0,
                             (mean(dat1$Y[dat1$group==i & dat1$t==0])))
          observed.sign=(observed.groups1-observed.groups0)>0
          # estimated
          estimated.groups=c(estimated.groups,mean(dat1$benefit[dat1$group==i]))
          estimated.sign=(estimated.groups>0)
        }
        # collate resuts
        dall1=(cbind(observed.sign, estimated.sign))
        dall1=dall1[complete.cases(dall1),]
        percent.s2=c(percent.s2, sum(dall1[,1]==dall1[,2] )/length(dall1[,1]))
        dall=data.frame(observed.groups0, observed.groups1, estimated.groups)
        is.na(dall$observed.groups0[is.infinite(dall$observed.groups0)])=TRUE
        is.na(dall$observed.groups1[is.infinite(dall$observed.groups1)])=TRUE
        dall=dall[complete.cases(dall),]
        dall$observed.groups=dall$observed.groups1-dall$observed.groups0
        reg=summary(lm(dall$observed.groups~dall$estimated.groups))
        coeff.1=c( coeff.1, coef(reg)[1])
        coeff.2=c( coeff.2,coef(reg)[2])
        R.2.m=c(R.2.m, reg$r.squared)
        rmse.m=c(rmse.m, sqrt(mean((dall$observed.groups-dall$estimated.groups)^2)))
      }
      # summary
      percent1.s2[[n]]=round(mean(percent.s2), digits=2)
      results.kmeans.bin[[n]]<-data.frame(rmse= round(median(rmse.m),digits=3),
                                          a0= round(median(coeff.1),digits=2),
                                          a1= round(median(coeff.2),digits=2),
                                          R2= round(median(R.2.m),digits=2)  )
    }

    results=data.frame(Method=c(paste("kmeans N=",Ngroups[1], sep="")),
                       RMSE=c(results.kmeans.bin[[1]]$rmse)
                       ,a0=c(  results.kmeans.bin[[1]]$a0)
                       ,a1=c( results.kmeans.bin[[1]]$a1)
                       ,R2=c( results.kmeans.bin[[1]]$R2))

    if(Ngroups.length>1){
      for (n in 2:Ngroups.length){
        d1<-data.frame(Method=c(paste("kmeans N=",Ngroups[n], sep="")),
                       RMSE=c( results.kmeans.bin[[n]]$rmse)
                       ,a0=c(  results.kmeans.bin[[n]]$a0)
                       ,a1=c(results.kmeans.bin[[n]]$a1)
                       ,R2=c( results.kmeans.bin[[n]]$R2))
        results=rbind(results, d1)
      }      }
    results=rbind(results.regr, results)


    dat1$log.pred.t1<-logit(predicted.treat.1)
    dat1$log.pred.t0<-logit(predicted.treat.0)
    dat1$benefit.lm<-dat1$log.pred.t1-dat1$log.pred.t0
    glm1=glm(dat1$Y~dat1$log.pred.t0+dat1$t:dat1$benefit.lm, family=binomial)
    ci1=confint(glm1)
    slope.ben=paste(round(summary(glm1)$coef[3],digits=2)," [",round( ci1[3,1],digits=2),"; " ,round(ci1[3,2], digits=2),"]", sep="")


    results.reg=data.frame(slope.ben)
    rownames(results.reg)=c("Estimate")


    results.DA=data.frame("Estimand, estimation method"=c("Population-level benefit (PB), adjusted",
                                                          "Population-level benefit vs t=0 (PB0)",
                                                          "Population-level benefit vs t=1 (PB1)",
                                                          paste("Benefit accuracy (BA), k-means N=",Ngroups[1], sep="")),
                          "Estimate"=c(da.1,da.2, da.3,  round(percent1.s2[[1]], digits=2)), check.names = F)




    if(Ngroups.length>1){
      for (n in 2:Ngroups.length){
        d1<-data.frame("Estimand, estimation method"=paste("Benefit accuracy (BA), k-means N=",Ngroups[n], sep=""),
                       "Estimate"=as.character(round(percent1.s2[[n]], digits=2)), check.names = F)
        results.DA=rbind(results.DA, d1)      }    }

    results.DA=rbind(results.DA,
                     data.frame("Estimand, estimation method"=c("Benefit accuracy (BA), match by covariates",
                                                                "Benefit accuracy (BA), match by benefit"),
                                "Estimate"=c(as.character(round(mean.percent.1on1.X, digits=2)),
                                             as.character(round(mean.percent.1on1.ben, digits=2))), check.names = F ))

    results.discrimination=data.frame("method"=c("c-for-benefit, match by covariates","c-for-benefit, match by benefit"),
                                      "Estimates"=c(results.c.f.b[1], results.c.f.b[2]))
    results[2:5]=round(results[2:5], digits=3)
    results.bias=round(mean.bias, digits=4)
    results.all=list("outcome.type"=type, "decision.accuracy"=results.DA,
                     "calibration.via.kmeans"=results, "mean.bias"=results.bias,
                     "regression.for.benefit"=results.reg, "discrimination.for.benefit"=results.discrimination)


  }






  return(results.all)
}











