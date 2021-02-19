## ----setup, include=FALSE, cache=FALSE--------------------
# set global chunk options
library('knitr')
opts_chunk$set(fig.path="caperpy-",
               fig.align="center",
               fig.show="hold",
               echo=TRUE,
               results="markup",
               fig.width=10,
               fig.height=10, out.width='\\linewidth',
               out.height='\\linewidth',
               cache=FALSE,
               dev='png',
               concordance=TRUE,
               error=FALSE)
opts_knit$set(aliases = c(h = 'fig.height',
              w = 'fig.width',
              wo='out.width',
              ho='out.height'))
options(replace.assign=TRUE,width=60)
set.seed(9567)

## ----eval=FALSE-------------------------------------------
#  ## If devtools is not yet installed, type
#  install.packages("devtools")
#  
#  ## Install the package caperpyogm
#  devtools::install_github("ClementCalenge/caperpyogm")

## ----results="asis", echo=FALSE---------------------------

dataf <- data.frame(
    Notation=c("$y_{i,t,k}$",
               "$b(t)$",
               "$N_{i,b(t)}$",
               "$p_{i,t,k}$",
               "$\\alpha_d$",
               "$\\beta_d$",
               "$o_{i,t,k}$",
               "$e_{g(i),t}$",
               "$\\sigma_e$",
               "$\\lambda_{i,b(t)}$",
               "$\\kappa_{g(i),\\ell(i),b(t)}$",
               "$\\nu_{u(i)}$",
               "$\\eta_i$",
               "$\\epsilon_{i,b(t)}$",
               "$\\mu_{g(i),\\ell(i)}$",
               "$\\sigma_{\\kappa}$",
               "$\\sigma_{\\nu}$",
               "$\\sigma_{\\eta}$",
               "$\\sigma_{\\epsilon}$",
               "$z_q$",
               "$f_q$",
               "$s_q$",
               "$b_q$",
               "$r_q$",
               "$h_q$",
               "$\\pi_q$",
               "$a_{g(q)}$",
               "$b_{g(q)}$",
               "$d$",               
               "$\\beta$",
               "$\\alpha$",
               "$S_c$",
               "$D_c$",
               "$N_c$",
               "$\\delta$",
               "$e(c)$",
               "$\\zeta_{e(c)}$",
               "$S_{g\\ell}$",
               "$Q$",
               "$n_{g\\ell b}$",
               "$n_b$"),
    Type=c("D",
           "N",
           "P",
           "P",
           "P",
           "P",
           "D",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "D",
           "D",
           "D",
           "P",
           "D",
           "D",
           "P",
           "P",
           "P",
           "P",
           "P",
           "P",
           "D",
           "D",
           "D",
           "P",
           "N",
           "P",
           "D/P",
           "D",
           "P",
           "P"),
    Description=c("Number of detected males during count occasions $k$ of the year $t$ on lek $i$",
                  "Index of the two-year period containing the year $t$",
                  "Number of males actually present on lek $i$ during the two year-period $b(t)$",
                  "Probability of detection of a male present on lek $i$ during count occasion $k$ of year $t$",
                  "Intercept of the model for the process of detection of males during counts",
                  "Slope of the number of observers in the model for the detection process of males during counts",
                  "Number of observers participating to the count occasion $k$ of the year $t$ on lek $i$",
                  "Random effect of the year $t$ on the detectability of males in geographic region $g$",
                  "Standard deviation of the year random effect on the detectability of males during counts",
                  "Expectation of the Poisson distribution of the number of males on lek $i$ during period $b(t)$",
                  "Parameter describing the average number of males during period $b(t)$ on a lek of type $\\ell(i)$ in region $g(i)$",
                  "Random effect of the natural unit $u(i)$",
                  "Random effect of the lek $i$",
                  "Overdispersion residual",
                  "Mean of the average numbers of males during period $b(t)$ on leks of type $\\ell(i)$ in region $g(i)$",
                  "Standard deviation of the average numbers of males during period $b(t)$ on leks of type $\\ell(i)$ in region $g(i)$",
                  "Standard deviation of the natural units random effects",
                  "Standard deviation of the lek random effects",
                  "Standard deviation of the overdispersion residuals",
                  "Whether the cell $q$ contains an unknown lek detected during a search of our program",
                  "Whether the cell $q$ was sampled in 2018--2019",
                  "Whether the cell $q$ contains an unknown lek detected before the search of our program",
                  "Whether the cell $q$ actually contains an unknown lek",
                  "Proportion of the cell $q$ covered by the presence area of the capercaillie",
                  "Whether the cell $q$ already contains a known lek",
                  "Probability that the cell $q$ contains an unknown lek",
                  "Intercept of the model predicting $\\pi_q$",
                  "Slope of $r_q$ in the model predicting $\\pi_q$",
                  "Slope of $h_q$ in the model predicting $\\pi_q$",
                  "Probability of detection of an unknown lek during the search of a grid cell",
                  "Probability that an unknown lek present in the cell was discovered prior to its sampling",
                  "Number of the known leks present in grid cell $c$ included in the search sector",
                  "Number of the known leks present in grid cell $c$ detected by the observer",
                  "Number of the known leks present in grid cell $c$",
                  "Probability to detect a lek during a search when the lek is in the search sector",
                  "Level of experience of the observers searching the cell $c$",
                  "Probability that the search sector defines by the observer with experience $e(c)$ includes a lek present in a cell",
                  "Number of leks of type $\\ell$ in the region $g$ (to be estimated for UL",
                  "Number of grid cells in the frame",
                  "Number of males on leks of type $\\ell$ during the period $b$ in region $g$",
                  "Number of males on all leks of the mountain range during period $b$"
                  ))
library(xtable)

xt <- xtable(dataf, align="lllp{12cm}")
print(xt, include.rownames=FALSE,
      sanitize.text.function = function(x) {x}, floating=FALSE)

## ----load-library-----------------------------------------
library(caperpyogm)

## ----description-lekcounts--------------------------------
head(lekcounts)

## ----JAGS-model-paper-------------------------------------
cat(modelCountDetectBinREY)

## ----preparation-model-count------------------------------
dataList <- dataCount2jags(lekcounts$lek, lekcounts$period,
                           lekcounts$nbobs, lekcounts$nbmales,
                           lekcounts$gr, as.numeric(factor(lekcounts$type)),
                           lekcounts$natun, lekcounts$year)

## ----fit-model-count, eval=FALSE--------------------------
#  coefModelCountDetectBinREY <- fitModelCount(dataList, "modelCountDetectBinREY")

## ----traceplot-kappa--------------------------------------
library(bayesplot)
library(MCMCvis)
cm <- MCMCchains(coefModelCountDetectBinREY,
                 params=c("kappa","sigmakappa","sigmaepsilon","sigmaeta",
                          "sigmanu", "interceptpd","pente1pd","sigmaREY"),
                 mcmc.list=TRUE)
for (i in 1:length(cm)) {
    for (j in c("sigmakappa","sigmaepsilon","sigmaeta[1]",
                "sigmaeta[2]","sigmaeta[3]","sigmanu","sigmaREY"))
        cm[[i]][,j] <- 1/cm[[i]][,j]
}
mcmc_trace(cm, regex_pars="kappa")

## ----traceplot-sigma--------------------------------------
mcmc_trace(cm, regex_pars=c("sigmakappa","sigmaepsilon","sigmaeta",
                            "sigmanu"))

## ----traceplot-detection----------------------------------
mcmc_trace(coefModelCountDetectBinREY,
           regex_pars=c("REY","interceptpd","pente1pd","sigmaREY"))

## ----gelman-diagnostic, eval=FALSE------------------------
#  gelman.diag(cm)

## ----simulate-datasets, eval=FALSE------------------------
#  simBinREY <- simulateModelCount(coefModelCountDetectBinREY, dataList)

## ----residuals-prediction---------------------------------
plot(predict(simBinREY), residuals(simBinREY),
     xlab="Predicted number of males",
     ylab="Standardized residuals")

## ----residuals-prediction-lek-----------------------------
plot(predict(simBinREY, groupingFactor="lek"),
     residuals(simBinREY, groupingFactor="lek"),
     xlab="predictions",
     ylab="residuals")

## ----calc-chi2--------------------------------------------
od <- simBinREY$origData
sim <- simBinREY$sim
ry <- od$nbmales

## Chi2 obs
chi2obs <- (sum(((ry-apply(sim,1,mean))^2)/apply(sim,1,mean)))

## distribution of simulated Chi2
chisq <- ((sim-apply(sim,1,mean))^2)/apply(sim,1,mean)
ch <- (colSums(chisq))

## proportion of chi-square
mean(ch>=chi2obs)

## ----crit-nb-tot------------------------------------------
mean(colSums(sim)>=sum(ry))

## ----crit-per-lek-count-----------------------------------
q1 <- apply(sim,1,quantile,0.1)
q2 <- apply(sim,1,quantile,0.9)
mean((ry>=q1)&(ry<=q2))

## ----comparef---------------------------------------------
comparef <- function(na)
{
    ta <- tapply(ry,na,sum)
    app <- apply(sim,2,function(x) tapply(x,na,sum))
    sapply(1:nrow(app), function(i) mean(ta[i]>app[i,]))
}

## ----comparef-gr------------------------------------------
comparef(od$gr)

## ----comparef-combination---------------------------------
comparef(paste0(od$gr,"-",od$type,"-",od$period))

## ----description-gridSearch-------------------------------
head(gridSearch)

## ----presentation-DetectionExpe---------------------------
DetectionExpe

## ----JAGS-model-paper-ul----------------------------------
cat(modelULPresence)

## ---------------------------------------------------------
dataListQ <- datagrid2jags(gridSearch, DetectionExpe)

## ----fit-model-grid, eval=FALSE---------------------------
#  coefModelULPresence <- fitModelGrid(dataListQ, "modelULPresence")

## ----traceplot-gcs----------------------------------------
mcmc_trace(coefModelULPresence)

## ----gelman-diagnostic-gcs, eval=FALSE--------------------
#  gelman.diag(coefModelULPresence)

## ----simulate-datasets-grid-------------------------------
sg <- simulateModelGrid(coefModelULPresence, dataListQ)

## ----crit-tot---------------------------------------------
obs <- sum(dataListQ$newUL)
sim <- colSums(sg$sim)
mean(sim<obs)

## ---------------------------------------------------------
obs <- tapply(dataListQ$newUL, dataListQ$gr, sum)
sim <- apply(sg$sim,2,function(x) tapply(x, dataListQ$gr, sum))

## Region 1
mean(sim[1,]<obs[1])

## Region 2
mean(sim[2,]<obs[2])


## ---------------------------------------------------------
NKAL <- c(14L, 64L, 76L, 48L, 46L)
NKIL <- c(6L, 80L, 102L, 32L, 96L)

## ---------------------------------------------------------
head(gridFrame)

## ---------------------------------------------------------
nm <- estimateNmales(coefModelCountDetectBinREY, coefModelULPresence,
                     gridFrame, NKAL, NKIL)

## ---------------------------------------------------------
summary(nm)

## ---------------------------------------------------------
distritime(getTotal(nm))

## ---------------------------------------------------------
showChangeRate(nm)

## ---------------------------------------------------------
summary(nm,1)

## ---------------------------------------------------------
set.seed(980)
ooo <- sample(c(rep(1:10,each=33)))

## ----eval=FALSE-------------------------------------------
#  listCoefsCVBinREY <- kfoldCVModelCount(ooo, dataList, "modelCountDetectBinREY")

## ----eval=FALSE-------------------------------------------
#  llcBinREY <- LLCount(dataList, listCoefsCVBinREY, ooo)

## ---------------------------------------------------------
elpdBinREY <- elpdLeks(llcBinREY)

## ----eval=FALSE-------------------------------------------
#  ## K-fold CV for different models
#  listCoefsCVBin <- kfoldCVModelCount(ooo, dataList, "modelCountDetectBin")
#  listCoefsCVBinREYObs2 <- kfoldCVModelCount(ooo, dataList, "modelCountDetectBinREYObs2")
#  listCoefsCVBetaBinREY <- kfoldCVModelCount(ooo, dataList, "modelCountDetectBetaBinREY")
#  
#  ## The list
#  llcBin <- LLCount(dataList, listCoefsCVBinREY, ooo)
#  llcBinREYObs2 <- LLCount(dataList, listCoefsCVBinREY, ooo)
#  llcBetaBinREY <- LLCount(dataList, listCoefsCVBinREY, ooo)

## ---------------------------------------------------------
dataList2 <- dataCount2jags(lekcounts$lek, lekcounts$year,
                            lekcounts$nbobs, lekcounts$nbmales,
                            lekcounts$gr, as.numeric(factor(lekcounts$type)),
                            lekcounts$natun, lekcounts$year)

## ----eval=FALSE-------------------------------------------
#  listCoefsCVBinPerYear <- kfoldCVModelCount(ooo, dataList2, "modelCountDetectBin")
#  llcBinPerYear <- LLCount(dataList2, listCoefsCVBinPerYear, ooo)

## ---------------------------------------------------------
elpdBin <- elpdLeks(llcBin)
elpdBinREYObs2 <- elpdLeks(llcBinREYObs2)
elpdBetaBinREY <- elpdLeks(llcBetaBinREY)
elpdBinPerYear <- elpdLeks(llcBinPerYear)

## ---------------------------------------------------------
## Number of occasions for each lek
cidl <- tapply(dataList$repetition, dataList$lek, sum)
cidl2 <- tapply(dataList2$repetition, dataList2$lek, sum)

## corrected contribution
elpdcBinREY <- elpdBinREY/cidl
elpdcBin <- elpdBin/cidl
elpdcBinREYObs2 <- elpdBinREYObs2/cidl
elpdcBetaBinREY <- elpdBetaBinREY/cidl
elpdcBinPerYear <- elpdBinPerYear/cidl

## ---------------------------------------------------------
## ELPD 
## Randomization test
library(ade4)
pv <- sapply(list(elpdBin, elpdBinREY, elpdBinREYObs2, elpdBetaBinREY,
            elpdBinPerYear), function(x) {
    
    sim <- sapply(1:1000, function(i)
        sum(sample(c(-1,1), length(elpdBinREY),
                   replace=TRUE)*(elpdBinREY-x)))
    obs <- sum(elpdBinREY-x)
    as.randtest(sim,obs)$pvalue
})

## criterion elpd
elpds <- c(sum(elpdBin),sum(elpdBinREY),
                  sum(elpdBinREYObs2),sum(elpdBetaBinREY),
           sum(elpdBinPerYear))

## SD difference with the best model
elpdDiffsd <- sqrt(length(elpdBin))*c(sd(elpdBin-elpdBinREY),
                                  0, sd(elpdBinREYObs2-elpdBinREY),
                                  sd(elpdBetaBinREY-elpdBinREY),
                                  sd(elpdBinPerYear-elpdBinREY))


## "Corrected" ELPD 
## Randomization test
pvc <- sapply(list(elpdcBin, elpdcBinREY, elpdcBinREYObs2, elpdcBetaBinREY,
                  elpdcBinPerYear), function(x) {
    
    sim <- sapply(1:1000, function(i)
        sum(sample(c(-1,1), length(elpdBinREY),
                   replace=TRUE)*(elpdcBinREY-x)))
    obs <- sum(elpdcBinREY-x)
    as.randtest(sim,obs)$pvalue
})

## criterion
elpdcs <- c(sum(elpdcBin),sum(elpdcBinREY),
            sum(elpdcBinREYObs2),sum(elpdcBetaBinREY),
            sum(elpdcBinPerYear))

## SD Difference with the best model
elpdcDiffsd <- sqrt(length(elpdcBin))*c(sd(elpdcBin-elpdcBinREY),
                                        0, sd(elpdcBinREYObs2-elpdcBinREY),
                                        sd(elpdcBetaBinREY-elpdcBinREY),
                                        sd(elpdcBinPerYear-elpdcBinREY))


## Data.frame containing the result for elpd
dfelpd <- data.frame(Model=c("modelCountDetectBin","modelCountDetectBinREY",
                             "modelCountDetectBinREYObs2",
                             "modelCountDetectBetaBinREY",
                             "modelCountDetectBin per year"),
                     elpd=elpds,
                     SDDiff=elpdDiffsd,
                     Pvalue=round(pv,3))


## for elpdc
dfelpdc <- data.frame(Model=c("modelCountDetectBin","modelCountDetectBinREY",
                              "modelCountDetectBinREYObs2",
                              "modelCountDetectBetaBinREY",
                              "modelCountDetectBin per year"),
                      elpdc=elpdcs,
                      SDDiffc=elpdcDiffsd,
                      Pvalue=round(pvc,3))


## ---------------------------------------------------------
dfelpd

## ---------------------------------------------------------
dfelpdc

## ---------------------------------------------------------
plot(c(1:length(elpdcBinREY))/length(elpdcBinREY),
     sort(elpdcBinREY), xlab="Quantile",
     ylab="Contributions to elpdc")
text(0.05, -8.71, "Lek 281")

## ----eval=FALSE-------------------------------------------
#  lcb <- lekcounts %>% filter(lek!=281)
#  lcb$lek <- as.numeric(factor(lcb$lek))
#  dataListwo281 <- dataCount2jags(lcb$lek, lcb$period,
#                                  lcb$nbobs, lcb$nbmales,
#                                  lcb$gr, as.numeric(factor(lcb$type)),
#                                  lcb$natun, lcb$year)
#  
#  coefModelm281 <- fitModelCount(dataListwo281, "modelCountDetectBinREY")

## ---------------------------------------------------------
nmwo281 <- estimateNmales(coefModelm281, coefModelULPresence,
                          gridFrame, NKAL, NKIL)
summary(nmwo281)

## ---------------------------------------------------------
summary(nm)

## ---------------------------------------------------------
summary(nm,1)

## ---------------------------------------------------------
summary(nmwo281,1)

## ----fig.width=10, fig.height=5, out.width='\\linewidth',out.height='0.5\\linewidth'----
library(matrixStats)
kap <- MCMCchains(coefModelm281, "kappa")
kapb <- MCMCchains(coefModelCountDetectBinREY, "kappa")
par(mfrow=c(1,2))
cmbk <- colMeans(kapb)
cmbv <- colVars(kapb)
cmk <- colMeans(kap)
cmv <- colVars(kap)

## Remove the "virtual levels" included in the model to have
## a square matrix region x period x types of leks
## (we included these levels because we do not have the
## same number of regions for all types of lek)
cmbk <- cmbk[cmv<8]
cmbv <- cmbv[cmv<8]
cmk <- cmk[cmv<8]
cmv <- cmv[cmv<8]


plot(cmbk, cmk, xlab="Point estimate of kappa with 281",
     ylab="Point estimate of kappa without 281")
plot(cmbv, cmv, xlab="SD of kappa distribution with 281",
     ylab="SD of kappa distribution without 281") 

## ---------------------------------------------------------
set <- 1/MCMCchains(coefModelm281, "sigmaeta")
setb <- 1/MCMCchains(coefModelCountDetectBinREY, "sigmaeta")
colnames(setb) <- c("KAL","KIL","UL")
colnames(set) <- paste0(c("KAL","KIL","UL"),"/wo281")
setg <- cbind(setb,set)[,c(1,4,2,5,3,6)]
boxplot(setg, cex.axis=0.9, ylab="Posterior distr. on variance of the lek random effects")

## ---------------------------------------------------------
delta2b <- c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.3)

## ----eval=FALSE-------------------------------------------
#  libtt <- list()
#  
#  for (j in 1:10) {
#      cat("###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n### Iteration", j,"\n\n\n")
#      liresuBeta <- list()
#      for (i in 1:7) {
#          cat("###########################\n### Model", i,"\n\n\n")
#          if (i!=1) {
#              sdl <- simulateDataList(coefModelCountDetectBinREY,
#                                      dataList, betaBinDelta=delta2b[i])
#          } else {
#              sdl <- simulateDataList(coefModelCountDetectBinREY, dataList)
#          }
#          fm <- fitModelCount(sdl, "modelCountDetectBinREY", n.iter=30000, thin=30)
#          liresuBeta[[i]] <- list(dataList=sdl, coefs=fm)
#          saveRDS(liresuBeta, file="liresuBeta.Rds")
#      }
#      libtt[[j]] <- liresuBeta
#      saveRDS(libtt, file="libtt.Rds")
#  }
#  
#  
#  listNmalesBB <- lapply(libtt, function(liresuBeta)
#      lapply(liresuBeta, function(x)
#          estimateNmales(x$coefs, coefModelULPresence, gridFrame, NKAL, NKIL)))
#  
#  medianNmalesBB <- sapply(listNmalesBB, function(z)
#      sapply(z, function(x) median(getTotal(x)[,1])))

## ---------------------------------------------------------
## coefficient of variation
coefvar <- c(0,sapply(delta2b[-1], function(delta2) {
    alpha <- 0.6*(1-delta2)/delta2
    beta <- 0.4*(1-delta2)/delta2
    sqrt(alpha*beta/((alpha+beta)^2 * (alpha+beta+1)))/0.6
}))


plot(rep(coefvar, 10),as.vector(medianNmalesBB),
     xlab="% of unaccounted variation in detection proba",
     ylab="Estimated number of males in 2010-2011")
xy <- data.frame(x=rep(coefvar, 10), y=as.vector(medianNmalesBB))
xv <- seq(0,0.5,length=200)
lines(xv, predict(lm(y~x+I(x^2), data=xy), newdata=data.frame(x=xv)), col="red")

## ---------------------------------------------------------
set.seed(1)
simn <- simulateN(coefModelCountDetectBinREY,dataList)

## ----tentative-n-fixe, eval=FALSE-------------------------
#  delta2b <- c(0, 0.004, 0.015, 0.05, 0.1, 0.2)
#  libtt2 <- list()
#  for (j in 1:10) {
#      cat("###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n### Iteration", j,"\n\n\n")
#      liresuBeta <- list()
#      for (i in 1:6) {
#          cat("###########################\n### Model", i,"\n\n\n")
#          if (i!=1) {
#              sdl <- simulateDataList2(coefModelCountDetectBinREY,dataList,
#                                       simn, betaBinDelta=delta2b[i])
#          } else {
#              sdl <- simulateDataList2(coefModelCountDetectBinREY,
#                                       dataList, simn)
#          }
#          fm <- fitModelCount(sdl, "modelCountDetectBinREY", n.iter=10000, thin=10)
#          liresuBeta[[i]] <- list(dataList=sdl, coefs=fm)
#      }
#      libtt2[[j]] <- liresuBeta
#      saveRDS(libtt2, file="libtt2.Rds")
#  }
#  
#  
#  listNmalesBB2 <- lapply(libtt2, function(liresuBeta)
#      lapply(liresuBeta, function(x)
#          estimateNmales(x$coefs, coefModelULPresence,
#                         gridFrame, NKAL, NKIL)))
#  
#  medianNmalesBB2 <- sapply(listNmalesBB2, function(z)
#      sapply(z, function(x) mean(getTotal(x)[,1])))
#  save(medianNmalesBB2, file="medianNmalesBB2.rda")
#  

## ---------------------------------------------------------
delta2b <- c(0, 0.004, 0.015, 0.05, 0.1, 0.2)

## coefficient of variation
coefvar <- c(0,sapply(delta2b[-1], function(delta2) {
    alpha <- 0.6*(1-delta2)/delta2
    beta <- 0.4*(1-delta2)/delta2
    sqrt(alpha*beta/((alpha+beta)^2 * (alpha+beta+1)))/0.6
}))


plot(rep(coefvar, 10),as.vector(medianNmalesBB2),
     xlab="% of unaccounted variation in detection proba",
     ylab="Estimated number of males in 2010-2011")
xy <- data.frame(x=rep(coefvar, 10), y=as.vector(medianNmalesBB2))
xv <- seq(0,0.5,length=200)
lines(xv, predict(lm(y~x+I(x^2), data=xy), newdata=data.frame(x=xv)), col="red")

## ---------------------------------------------------------
doubleCountspb <- c(0.05, 0.1, 0.2, 0.3, 0.5)

## ----eval=FALSE-------------------------------------------
#  lidtt <- list()
#  for (j in 1:10) {
#      cat("###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n### Iteration", j,"\n\n\n")
#      liresudc <- list()
#      for (i in 1:5) {
#          cat("###########################\n### Model", i,"\n\n\n")
#          sdl <- simulateDataList(coefModelCountDetectBinREY, dataList,
#                                  doubleCountsp=doubleCountspb[i])
#          fm <- fitModelCount(sdl, "modelCountDetectBinREY", n.iter=30000, thin=30)
#          liresudc[[i]] <- list(dataList=sdl, coefs=fm)
#          saveRDS(liresudc, file="liresudc.Rds")
#      }
#      lidtt[[j]] <- liresudc
#      saveRDS(lidtt, file="lidtt.Rds")
#  }
#  
#  listNmalesDC <- lapply(lidtt, function(liresudc)
#      lapply(liresudc, function(x)
#          estimateNmales(x$coefs, coefModelULPresence, gridFrame, NKAL, NKIL)))
#  
#  medianNmalesDC <- sapply(listNmalesDC,
#                           function(z)
#      sapply(z, function(x) median(getTotal(x)[,1])))

## ---------------------------------------------------------
## Add the case where h=0 (simulated in the previous section)
plot(rep(c(0,doubleCountspb), 10),
     as.vector(rbind(medianNmalesBB[1,], medianNmalesDC)),
     xlab="Proportion of detected bird counted twice",
     ylab="Estimated number of males")
xy <- data.frame(x=rep(c(0,doubleCountspb), 10),
                 y=as.vector(rbind(medianNmalesBB[1,], medianNmalesDC)))
xv <- seq(0,0.5,length=200)
lines(xv, predict(lm(y~x, data=xy), newdata=data.frame(x=xv)), col="red")


## ----eval=FALSE-------------------------------------------
#  doubleCountspb <- c(0.05, 0.1, 0.2, 0.3, 0.5)
#  
#  lidtt2 <- list()
#  for (j in 1:10) {
#      cat("###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n###########################",
#          "\n### Iteration", j,"\n\n\n")
#      liresudc <- list()
#      for (i in 1:5) {
#          cat("###########################\n### Model", i,"\n\n\n")
#          sdl <- simulateDataList2(coefModelCountDetectBinREY,dataList, simn,
#                                  doubleCountsp=doubleCountspb[i])
#          fm <- fitModelCount(sdl, "modelCountDetectBinREY", n.iter=10000, thin=10)
#          liresudc[[i]] <- list(dataList=sdl, coefs=fm)
#          saveRDS(liresudc, file="liresudc.Rds")
#      }
#      lidtt2[[j]] <- liresudc
#      saveRDS(lidtt2, file="lidtt2.Rds")
#  }
#  
#  
#  listNmalesDC2 <- lapply(lidtt2, function(liresuBeta)
#      lapply(liresuBeta, function(x)
#          estimateNmales(x$coefs, coefModelULPresence, gridFrame, NKAL, NKIL)))
#  
#  medianNmalesDC2 <- sapply(listNmalesDC2, function(z)
#      sapply(z, function(x) mean(getTotal(x)[,1])))

## ---------------------------------------------------------
doubleCountspb <- c(0.05, 0.1, 0.2, 0.3, 0.5)

## Note that we add the case where h=0 (simulated in the previous section)
plot(rep(c(0,doubleCountspb), 10),
     as.vector(rbind(medianNmalesBB2[1,], medianNmalesDC2)),
     xlab="Proportion of detected bird counted twice",
     ylab="Estimated number of males")
xy <- data.frame(x=rep(c(0,doubleCountspb), 10),
                 y=as.vector(rbind(medianNmalesBB2[1,], medianNmalesDC2)))
xv <- seq(0,0.5,length=200)
lines(xv, predict(lm(y~x, data=xy), newdata=data.frame(x=xv)), col="red")

