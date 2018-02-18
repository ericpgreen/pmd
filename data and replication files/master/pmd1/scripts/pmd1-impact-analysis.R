# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PMD 1 Impact Analysis
# main
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Setup
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# load
# =============================================================================
# packages
# -----------------------------------------------------------------------------
# library(xlsx) loaded later
  library(arm)
  library(ggplot2)
  library(extrafont)
  # font_import() # run once
  loadfonts()
  library(gridExtra)
  library(gtable)
  library(grid)
  library(lavaan)
  library(reshape2)
  
# data
# -----------------------------------------------------------------------------
  base.end <- read.csv("data and replication files/master/pmd1/input/pmd1.csv")
  
  
# paths
# =============================================================================
  outtables <- "data and replication files/master/pmd1/output/tables/"
  outfigures <- "data and replication files/master/pmd1/output/figures/"
  outfolder <- "data and replication files/master/pmd1/output/"

  
# missing data analysis
# =============================================================================
# create function
# http://gettinggeneticsdone.blogspot.com/2011/02/summarize-missing-data-for-all.html
  propmiss <- function(dataframe) {
    m <- sapply(dataframe, function(x) {
      data.frame(
        nmiss=sum(is.na(x)), 
        n=length(x), 
        propmiss=sum(is.na(x))/length(x)
      )
    })
    d <- data.frame(t(m))
    d <- sapply(d, unlist)
    d <- as.data.frame(d)
    d$variable <- row.names(d)
    row.names(d) <- NULL
    d <- cbind(d[ncol(d)],d[-ncol(d)])
    return(d[order(d$propmiss), ])
  }
  miss <- propmiss(base.end)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Participants 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# create objects to receive results 
  part.c <- as.data.frame(NULL)
  part.k <- as.data.frame(NULL)
  
# kids
# =============================================================================  

# list variables
  demo.k <- c("b.care.tc.age", "b.care.tc.female", "b.care.sdq.conductprob")
# indicate whether SDs should be calculated
  demo.k.m <- c(1,0,1)
# give variables new labels
  demo.k.lab <- c("Mean age (SD)", 
                  "Female",
                  "Mean SDQ conduct (SD)")
# loop
  for (i in 1:length(demo.k)) {
    temp <- lm(get(demo.k[i]) ~ b.treatment, data=base.end)
    temp2 <- summary(temp)
    part.k[i,1] <- paste0("\\hspace{0.25cm}", demo.k.lab[i])
    if (demo.k.m[i]==1) { # skip sd if percentage
    # control
      part.k[i,2] <- paste0(sprintf("%.2f", 
                                    round(mean(base.end[base.end$b.treatment==0,     
                                               demo.k[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$b.treatment==0, 
                                             demo.k[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    # treatment
      part.k[i,3] <- paste0(sprintf("%.2f",
                                    round(mean(base.end[base.end$b.treatment==1,     
                                               demo.k[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$b.treatment==1, 
                                             demo.k[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    } else {  # skip SD for percentages
    # control
      part.k[i,2] <- sprintf("%.2f",
                             round(mean(base.end[base.end$b.treatment==0,     
                                         demo.k[i]], 
                           na.rm=TRUE), 2))
    # treatment
      part.k[i,3] <- sprintf("%.2f",
                             round(mean(base.end[base.end$b.treatment==1,     
                                        demo.k[i]], 
                                        na.rm=TRUE), 2))
    }
    part.k[i,4] <- sprintf("%.3f",
                           round(temp2$coefficients[2,4], 3))
    remove(temp)
    remove(temp2)
  }
  
  
# caregivers
# =============================================================================  
  
# list variables
  demo.c <- c("b.care.age", "b.care.female", "b.care.marital.mar.inrel.cohab",
              "b.care.religion.christian", "b.care.work.inc.mo.all.99",
              "b.care.work.hoursinweek", "b.care.num.fam.inhh",
              "b.care.num.dep.u18", "b.care.tc.relation.bioparent")
# indicate if SD should be calculated
  demo.c.m <- c(1,0,0,0,1,1,1,1,0)
# give new labels
  demo.c.lab <- c("Mean age (SD)", 
                  "Female", 
                  "Married or cohabiting",
                  "Christian", 
                  "Mean household income last 4 weeks (SD)$\\dagger$", 
                  "Mean hours worked in typical week (SD)", 
                  "Mean household size (SD)",
                  "Mean number of dependents under 18 (SD)",
                  "Biological caregiver of target child")
# convert Liberian dollars to USD
  # already done in baseline file
  #base.end$b.care.work.inc.mo.all.99 <- base.end$b.care.work.inc.mo.all.99/xr
# loop
  for (i in 1:length(demo.c)) {
    temp <- lm(get(demo.c[i]) ~ b.treatment, data=base.end)
    temp2 <- summary(temp)
    part.c[i,1] <- paste0("\\hspace{0.25cm}", demo.c.lab[i])
    if (demo.c.m[i]==1) { # skip sd if percentage
    # control
      part.c[i,2] <- paste0(sprintf("%.2f", 
                                    round(mean(base.end[base.end$b.treatment==0,     
                                               demo.c[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$b.treatment==0, 
                                             demo.c[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    # treatment
      part.c[i,3] <- paste0(sprintf("%.2f",
                                    round(mean(base.end[base.end$b.treatment==1,     
                                               demo.c[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$b.treatment==1, 
                                             demo.c[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    } else {  # skip SD for percentages
    # control
      part.c[i,2] <- sprintf("%.2f",
                             round(mean(base.end[base.end$b.treatment==0,     
                                         demo.c[i]], 
                           na.rm=TRUE), 2))
    # treatment
      part.c[i,3] <- sprintf("%.2f",
                             round(mean(base.end[base.end$b.treatment==1,     
                                        demo.c[i]], 
                                        na.rm=TRUE), 2))
    }
    part.c[i,4] <- sprintf("%.3f",
                           round(temp2$coefficients[2,4], 3))
    remove(temp)
    remove(temp2)
  }

# create table 
# =============================================================================  
  library(xtable)
  for (i in c("part.k", "part.c")) {
    t <- xtable(get(i))
    f <- paste0(outtables, i, ".tex")
    print(t,
          only.contents=TRUE,
          include.rownames=FALSE,
          include.colnames=FALSE,
          hline.after=NULL,
          type="latex",
          file=f,
          sanitize.text.function=identity
          )  
  }
  detach("package:xtable", unload=TRUE)
# n's
  n.k.t <- sum(base.end[base.end$b.treatment==1,]$child.comp.b, 
                        na.rm=TRUE)
  n.k.c <- sum(base.end[base.end$b.treatment==0,]$child.comp.b, 
                        na.rm=TRUE)
  n.c.t <- sum(base.end[base.end$b.treatment==1,]$care.comp.b, 
                        na.rm=TRUE)
  n.c.c <- sum(base.end[base.end$b.treatment==0,]$care.comp.b, 
                        na.rm=TRUE)
# stats for paper
  n.child <- n.k.t+n.k.c
  n.care <- n.c.t+n.c.c
  female.p.k <- round(mean(base.end$b.care.tc.female, na.rm=TRUE)*100, 1)
  age.m.k <- round(mean(base.end$b.care.tc.age, na.rm=TRUE), 1)
  female.p.c <- round(mean(base.end$b.care.female, na.rm=TRUE)*100, 1)
  age.m.c <- round(mean(base.end$b.care.age, na.rm=TRUE), 1)
  marcoh.p.c <- round(mean(base.end$b.care.marital.mar.inrel.cohab, 
                           na.rm=TRUE)*100, 1)
  chr.p.c <- round(mean(base.end$b.care.religion.christian, na.rm=TRUE)*100, 1)
  hh.size <- round(mean(base.end$b.care.num.fam.inhh, na.rm=TRUE), 1)
  dep <- round(mean(base.end$b.care.num.dep.u18, na.rm=TRUE), 1)
  biopar.n <- round((1-mean(base.end$b.care.tc.relation.bioparent, 
                            na.rm=TRUE))*100, 1) 
  hh.inc <- round(mean(base.end$b.care.work.inc.mo.all.99, na.rm=TRUE), 2)
  work <- round(mean(base.end$b.care.work.hoursinweek, na.rm=TRUE), 1)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Attrition 
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# setup 
# =============================================================================  
# define attriters as not present at endline
  base.end$att.c <- ifelse(base.end$care.comp.e==0, 1, 0)
  base.end$att.k <- ifelse(base.end$child.comp.e==0, 1, 0)
  att.1.c <- sum(base.end$att.c, na.rm=TRUE)                # caregiver att
  att.1.k <- sum(base.end$att.c, na.rm=TRUE)                # child att
  att.0.c <- sum(base.end$care.comp.b, na.rm=TRUE)-att.1.c  # caregiver found
  att.0.k <- sum(base.end$child.comp.b, na.rm=TRUE)-att.1.k # child found

# compare baseline values 
# =============================================================================  
# create objects to receive results
# -----------------------------------------------------------------------------
  attr.c <- as.data.frame(NULL)
  attr.k <- as.data.frame(NULL)

# kids 
# -----------------------------------------------------------------------------
# list variables
  demo.kt <- c("b.treatment", demo.k)
# indicate whether SDs should be calculated
  demo.k.m <- c(0, demo.k.m)
# give variables new labels
  demo.k.lab <- c("Assigned treatment", demo.k.lab)
# loop
  for (i in 1:length(demo.kt)) {
    temp <- lm(get(demo.kt[i]) ~ att.k, data=base.end)
    temp2 <- summary(temp)
    attr.k[i,1] <- paste0("\\hspace{0.25cm}", demo.k.lab[i])
    if (demo.k.m[i]==1) { # skip sd if percentage
    # found
      attr.k[i,2] <- paste0(sprintf("%.2f", 
                                    round(mean(base.end[base.end$att.k==0,     
                                               demo.kt[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$att.k==0, 
                                             demo.kt[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    # not found
      attr.k[i,3] <- paste0(sprintf("%.2f",
                                    round(mean(base.end[base.end$att.k==1,     
                                               demo.kt[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$att.k==1, 
                                             demo.kt[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    } else {  # skip SD for percentages
    # found
      attr.k[i,2] <- sprintf("%.2f",
                             round(mean(base.end[base.end$att.k==0,     
                                         demo.kt[i]], 
                           na.rm=TRUE), 2))
    # not found
      attr.k[i,3] <- sprintf("%.2f",
                             round(mean(base.end[base.end$att.k==1,     
                                        demo.kt[i]], 
                                        na.rm=TRUE), 2))
    }
    attr.k[i,4] <- sprintf("%.3f",
                           round(temp2$coefficients[2,4], 3))
    remove(temp)
    remove(temp2)
  }
  
# caregivers 
# -----------------------------------------------------------------------------
# list variables
  demo.ct <- c("b.treatment", demo.c)
# indicate if SD should be calculated
  demo.c.m <- c(0, demo.c.m)
# give new labels
  demo.c.lab <- c("Assigned treatment", demo.c.lab)
# loop
  for (i in 1:length(demo.ct)) {
    temp <- lm(get(demo.ct[i]) ~ att.c, data=base.end)
    temp2 <- summary(temp)
    attr.c[i,1] <- paste0("\\hspace{0.25cm}", demo.c.lab[i])
    if (demo.c.m[i]==1) { # skip sd if percentage
    # found
      attr.c[i,2] <- paste0(sprintf("%.2f", 
                                    round(mean(base.end[base.end$att.c==0,     
                                               demo.ct[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$att.c==0, 
                                             demo.ct[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    # not found
      attr.c[i,3] <- paste0(sprintf("%.2f",
                                    round(mean(base.end[base.end$att.c==1,     
                                               demo.ct[i]], 
                                          na.rm=TRUE), 2)),
                            " (",
                            sprintf("%.2f",
                                    round(sd(base.end[base.end$att.c==1, 
                                             demo.ct[i]],
                                             na.rm=TRUE), 2)),
                            ")")
    } else {  # skip SD for percentages
    # found
      attr.c[i,2] <- sprintf("%.2f",
                             round(mean(base.end[base.end$att.c==0,     
                                         demo.ct[i]], 
                           na.rm=TRUE), 2))
    # not found
      attr.c[i,3] <- sprintf("%.2f",
                             round(mean(base.end[base.end$att.c==1,     
                                        demo.ct[i]], 
                                        na.rm=TRUE), 2))
    }
    attr.c[i,4] <- sprintf("%.3f",
                           round(temp2$coefficients[2,4], 3))
    remove(temp)
    remove(temp2)
  }
  
# create table 
# -----------------------------------------------------------------------------
  library(xtable)
  for (i in c("attr.k", "attr.c")) {
    t <- xtable(get(i))
    f <- paste0(outtables, i, ".tex")
    print(t,
          only.contents=TRUE,
          include.rownames=FALSE,
          include.colnames=FALSE,
          hline.after=NULL,
          type="latex",
          file=f,
          sanitize.text.function=identity
          )  
  }
  detach("package:xtable", unload=TRUE)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CONSORT
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  assessed.c <- n.c.t+n.c.c
  excluded.c <- 0
  excluded.c.i <- 0
  excluded.c.e <- 0
  randomized.c <- n.c.t+n.c.c
  allocated.c.t <- n.c.t
  allocated.c.c <- n.c.c
  perprotocol.c.t <- n.c.t
  perprotocol.c.c <- n.c.c
  notperprotocol.c.t <- 0
  notperprotocol.c.c <- 0
  lostfu.c.t <- sum(base.end$att.c[base.end$b.treatment==1], na.rm=TRUE)
  lostfu.c.c <- sum(base.end$att.c[base.end$b.treatment==0], na.rm=TRUE)
  analyzed.c.t <- allocated.c.t
  analyzed.c.c <- allocated.c.c


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Treatment effects
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# create ATE table 
# =============================================================================

# setup
# -----------------------------------------------------------------------------
# create a sig function 
  asterisk <- function(y) symnum(y, c(0, .001, .01, .05, .1, 1),
                                 c("***", "**", "*", "\\cdot", " "))
# create objects to receive results
  ate <- NULL
  ate <- as.data.frame(ate)
  ate.mb <- as.data.frame(NULL)
  ate.nc <- as.data.frame(NULL)
  
# define vectors of predictors and subset 
# -----------------------------------------------------------------------------
# a priori individual-level covariates
  # demo.c and demo.k defined earlier
# convert to factors
  fact <- c("community.id", "b.treatment", 
            "b.care.female", "b.care.marital.mar.inrel.cohab",
            "b.care.religion.christian", "b.care.tc.relation.bioparent",
            "b.care.tc.female", "b.care.tc.orphan.single")
  base.end[fact] <- lapply(base.end[fact], as.factor)

# define outcomes 
# -----------------------------------------------------------------------------
  harsh <- c("p.harshdiscipline.a",
             "whip.ever",
             "whiphand.ever",
             "whipobj.ever",
             "slapbutt.ever",
             "beat.ever",
             "pb11.harshverb.ever")
  pbm <- c("p.pbm.a",
           "pb10.timeout",
           "pb12.teachrules",
           "askstop",
           "pb1.praise") 
  ratio <- c("hp2pbm")
  open.coding <- c("pb9.1",
                   "pb9.2",
                   "pb9.3",
                   "pb9.4",
                   "pb9.5",
                   "pb9.12",
                   "pb9.7",
                   "pb9.9",
                   "pb9.11",
                   "pb9.13",
                   "pb9.14",
                   "pb9.15")
  beliefs <- c("pbe1.praise",
               "pbe2.harshphysical")
  efficacy <- c("lad.control",
                "ps3.bringupwell")
  pos.int.p <- c("p.posint.a",
                 "lad.spendtime",
                 "lad.plays",
                 "lad.talks",
                 "lad.praises")
  pos.int.c <- c("c.posint.a",
                 "cr.time.y",
                 "cr.play.y",
                 "cr.talk.y",
                 "cr.praise.y")
  comm <- c("lad.understand",
            "lad.verbal",        
            "care.verb",
            "care.praises.n",
            "NTA.n",
            "child.verb",
            "totalVerb.childPer")
  ability <- c("c.language",
               "c.receptive.vocab",
               "c.expressive.vocab",
               "c.verbal.comp",
               "c.verbal.fluency",
               "c.numeracy.count")
  sdq <- c("p.sdq.hyperactivity",
           "p.sdq.emotional",
           "p.sdq.conduct") 
  malaria <- c("bn1.net.own",
               "bn2.net.use.any",
               "bn4.net.use.tc",
               "bednet.wenttobed",
               "bn9.nethang")
  outcomes <- c(harsh, pbm, ratio, open.coding, beliefs, efficacy,
                pos.int.p, pos.int.c, comm, ability, sdq, malaria) 
# duplicate the data frame for testing bounds of imputation
  base.end.mb <- base.end
# impute median for all missing
  base.end[outcomes] <- apply(base.end[outcomes], 2, 
                             function(x) ifelse(is.na(x),
                                                median(x, na.rm=T), x))
# impute manski bounds
  out.up <- c("p.pbm.a",
              "p.posint.a",
              "c.posint.a",
              "care.praises.n",
              "child.verb",
              "totalVerb.childPer",
              "c.language",
              "c.numeracy.count",
              "bn1.net.own",
              "bn2.net.use.any",
              "bn4.net.use.tc")
  out.dn <- c("p.harshdiscipline.a",
              "p.sdq.hyperactivity",
              "p.sdq.emotional",
              "p.sdq.conduct")
  base.end.mb[out.up] <- apply(base.end.mb[out.up], 2,
                         function(x) ifelse(is.na(x) & 
                                            base.end.mb$b.treatment==1,
                                            quantile(x, .1, na.rm=T), 
                                     ifelse(is.na(x) & 
                                            base.end.mb$b.treatment==0,
                                            quantile(x, .9, na.rm=T),
                                            x)))
  base.end.mb[out.dn] <- apply(base.end.mb[out.dn], 2,
                         function(x) ifelse(is.na(x) & 
                                            base.end.mb$b.treatment==1,
                                            quantile(x, .9, na.rm=T),
                                     ifelse(is.na(x) & 
                                            base.end.mb$b.treatment==0,
                                            quantile(x, .1, na.rm=T),
                                            x)))

# loop 
# -----------------------------------------------------------------------------
  for (i in 1:length(outcomes)) {
    m <- paste(outcomes[i], 
               paste("b.treatment", "community.id",
                     paste(demo.k, collapse=" + "),
                     paste(demo.c, collapse=" + "),
                     sep=" + "),
               sep=" ~ ")
    r <- lm(m, data=base.end)
    rs <- summary(r)
  # extract results
    ate[i,1] <- outcomes[i]                              # variable name
    ate[i,2] <- display(r)$n                             # observations
    control.m <- aggregate(base.end[outcomes[i]],        # unadj control m
                           by=list(base.end$b.treatment), 
                           FUN=mean, 
                           na.rm=TRUE)[1,2]
    ate[i,3] <- control.m
    control.sd <- aggregate(base.end[outcomes[i]],       # control sd
                            by=list(base.end$b.treatment), 
                            FUN=sd, 
                            na.rm=TRUE)[1,2]
    ate[i,4] <- control.sd
    treat.m <- aggregate(base.end[outcomes[i]],          # unadj treat m
                         by=list(base.end$b.treatment), 
                         FUN=mean, 
                         na.rm=TRUE)[2,2]
    ate[i,5] <- treat.m
    treat.sd <- aggregate(base.end[outcomes[i]],         # treat sd
                          by=list(base.end$b.treatment), 
                          FUN=sd, 
                          na.rm=TRUE)[2,2]
    ate[i,6] <- treat.sd
    ate[i,7] <- rs$coefficients[2,1]                     # ATE
    ate[i,8] <- rs$coefficients[2,2]                     # std err
    ate[i,9] <- rs$coefficients[2,4]                     # p-value
    ate[i,10] <- asterisk(rs$coefficients[2,4])          # asterisk
    ate[i,11] <- rs$coefficients[2,1]/control.sd         # glass delta
    ate[i,12] <- confint(r)[2,1]                         # L95CI
    ate[i,13] <- confint(r)[2,2]                         # U95CI
    ate[i,14] <- (rs$coefficients[2,1]/control.m)*100    # ATE p of unadj
    ate[i,15] <- (confint(r)[2,1]/control.m)*100         # L95CI%
    ate[i,16] <- (confint(r)[2,2]/control.m)*100         # U95CI%
  }
  
# loop manski bounds 
# -----------------------------------------------------------------------------
  for (i in 1:length(outcomes)) {
    m <- paste(outcomes[i], 
               paste("b.treatment", "community.id",
                     paste(demo.k, collapse=" + "),
                     paste(demo.c, collapse=" + "),
                     sep=" + "),
               sep=" ~ ")
    r <- lm(m, data=base.end.mb)
    rs <- summary(r)
    # extract results
    ate.mb[i,1] <- outcomes[i]                              # variable name
    ate.mb[i,2] <- display(r)$n                             # observations
    control.m <- aggregate(base.end.mb[outcomes[i]],        # unadj control m
                           by=list(base.end.mb$b.treatment), 
                           FUN=mean, 
                           na.rm=TRUE)[1,2]
    ate.mb[i,3] <- control.m
    control.sd <- aggregate(base.end.mb[outcomes[i]],       # control sd
                            by=list(base.end.mb$b.treatment), 
                            FUN=sd, 
                            na.rm=TRUE)[1,2]
    ate.mb[i,4] <- control.sd
    treat.m <- aggregate(base.end.mb[outcomes[i]],          # unadj treat m
                         by=list(base.end.mb$b.treatment), 
                         FUN=mean, 
                         na.rm=TRUE)[2,2]
    ate.mb[i,5] <- treat.m
    treat.sd <- aggregate(base.end.mb[outcomes[i]],         # treat sd
                          by=list(base.end.mb$b.treatment), 
                          FUN=sd, 
                          na.rm=TRUE)[2,2]
    ate.mb[i,6] <- treat.sd
    ate.mb[i,7] <- rs$coefficients[2,1]                     # ATE
    ate.mb[i,8] <- rs$coefficients[2,2]                     # std err
    ate.mb[i,9] <- rs$coefficients[2,4]                     # p-value
    ate.mb[i,10] <- asterisk(rs$coefficients[2,4])          # asterisk
    ate.mb[i,11] <- rs$coefficients[2,1]/control.sd         # glass delta
    ate.mb[i,12] <- confint(r)[2,1]                         # L95CI
    ate.mb[i,13] <- confint(r)[2,2]                         # U95CI
    ate.mb[i,14] <- (rs$coefficients[2,1]/control.m)*100    # ATE p of unadj
    ate.mb[i,15] <- (confint(r)[2,1]/control.m)*100         # L95CI%
    ate.mb[i,16] <- (confint(r)[2,2]/control.m)*100         # U95CI%
  }
  
# loop no controls
# -----------------------------------------------------------------------------
  for (i in 1:length(outcomes)) {
    m <- paste(outcomes[i], 
               paste("b.treatment", "community.id",
                     sep=" + "),
               sep=" ~ ")
    r <- lm(m, data=base.end)
    rs <- summary(r)
    # extract results
    ate.nc[i,1] <- outcomes[i]                              # variable name
    ate.nc[i,2] <- display(r)$n                             # observations
    control.m <- aggregate(base.end[outcomes[i]],           # unadj control m
                           by=list(base.end$b.treatment), 
                           FUN=mean, 
                           na.rm=TRUE)[1,2]
    ate.nc[i,3] <- control.m
    control.sd <- aggregate(base.end[outcomes[i]],          # control sd
                            by=list(base.end$b.treatment), 
                            FUN=sd, 
                            na.rm=TRUE)[1,2]
    ate.nc[i,4] <- control.sd
    treat.m <- aggregate(base.end[outcomes[i]],             # unadj treat m
                         by=list(base.end$b.treatment), 
                         FUN=mean, 
                         na.rm=TRUE)[2,2]
    ate.nc[i,5] <- treat.m
    treat.sd <- aggregate(base.end[outcomes[i]],            # treat sd
                          by=list(base.end$b.treatment), 
                          FUN=sd, 
                          na.rm=TRUE)[2,2]
    ate.nc[i,6] <- treat.sd
    ate.nc[i,7] <- rs$coefficients[2,1]                     # ATE
    ate.nc[i,8] <- rs$coefficients[2,2]                     # std err
    ate.nc[i,9] <- rs$coefficients[2,4]                     # p-value
    ate.nc[i,10] <- asterisk(rs$coefficients[2,4])          # asterisk
    ate.nc[i,11] <- rs$coefficients[2,1]/control.sd         # glass delta
    ate.nc[i,12] <- confint(r)[2,1]                         # L95CI
    ate.nc[i,13] <- confint(r)[2,2]                         # U95CI
    ate.nc[i,14] <- (rs$coefficients[2,1]/control.m)*100    # ATE p of unadj
    ate.nc[i,15] <- (confint(r)[2,1]/control.m)*100         # L95CI%
    ate.nc[i,16] <- (confint(r)[2,2]/control.m)*100         # U95CI%
  }
  
# add column names 
# -----------------------------------------------------------------------------
  names(ate) <- c("variable", "obs", 
                  "control.mean", "control.sd",
                  "treat.mean", "treat.sd",
                  "ate", "ate.se", "pvalue", "sig", "glass", 
                  "l95ci", "u95ci", "ate.p", "l95ci.p", "u95ci.p")
  names(ate.mb) <- c("variable", "obs", 
                     "control.mean", "control.sd", 
                     "treat.mean", "treat.sd",
                     "ate", "ate.se", "pvalue", "sig", "glass", 
                     "l95ci", "u95ci", "ate.p", "l95ci.p", "u95ci.p")
  names(ate.nc) <- c("variable", "obs", 
                     "control.mean", "control.sd", 
                     "treat.mean", "treat.sd",
                     "ate", "ate.se", "pvalue", "sig", "glass", 
                     "l95ci", "u95ci", "ate.p", "l95ci.p", "u95ci.p")
  
# add expected direction 
# -----------------------------------------------------------------------------
  source("data and replication files/master/pmd1/scripts/pmd1-impact-analysis-dir.R")
  ate$expdir.yes <- ifelse((ate$ate>0 &
                              ate$expdir=="+") |
                             (ate$ate<0 &
                                ate$expdir=="-"),1,0)
# highlight rows in expected direction and sig 
  row.i.d <- which(ate$expdir.yes==1 & ate$pvalue >= 0.05)
  row.i.ds <- which(ate$expdir.yes==1 & ate$pvalue < 0.05)
  
# add scale
# -----------------------------------------------------------------------------
  source("data and replication files/master/pmd1/scripts/pmd1-impact-analysis-scale.R")

  
# create tables and figures 
# =============================================================================

# dataframes and labels 
# -----------------------------------------------------------------------------
# parenting
  out.par <- c(harsh, pbm, beliefs, efficacy)
  ate.par <- ate[ate$variable %in% out.par,]
  ate.par <- ate.par[match(out.par, ate.par$variable),] # re-order
  ate.par.p <- ate.par
  # table labels
    ate.par$variable <- c(# harsh discipline
                            "Harsh discipline composite$\\dagger$",
                            "\\hspace{0.25cm}Whipped child last 4 weeks",
                            "\\hspace{0.25cm}Whipped child with hand last 4 weeks",
                            "\\hspace{0.25cm}Whipped child with object last 4 weeks",
                            "\\hspace{0.25cm}Slapped child on butt with hand last 4 weeks",
                            "\\hspace{0.25cm}Beat child last 4 weeks",
                            "\\hspace{0.25cm}Shouted at child last 4 weeks",
                          # positive behavior management
                            "Positive behavior management composite",
                            "\\hspace{0.25cm}Used time out last 4 weeks",
                            "\\hspace{0.25cm}Taught rules about behavior last 4 weeks",
                            "\\hspace{0.25cm}Asked child to stop behavior in last 4 weeks",
                            "\\hspace{0.25cm}Praised child last 4 weeks",
                          # beliefs
                            "Praise is bad for children",
                            "Sometimes harsh punishment is the only option",
                          # efficacy
                            "Ladder of perceived ability to control child behavior",
                            "I am bringing up my child well")
  # figure labels
    ate.par.p$variable <- c(# harsh discipline
                              "Harsh discipline composite",
                              "Whipped child last 4 weeks",
                              "Whipped child with hand last 4 weeks",
                              "Whipped child with object last 4 weeks",
                              "Slapped child on butt with hand last 4 weeks",
                              "Beat child last 4 weeks",
                              "Shouted at child last 4 weeks",
                            # positive behavior management
                              "Positive behavior management composite",
                              "Used time out last 4 weeks",
                              "Taught rules about behavior last 4 weeks",
                              "Asked child to stop behavior in last 4 weeks",
                              "Praised child last 4 weeks",
                            # beliefs
                              "Praise is bad for children",
                              "Sometimes harsh punishment is the only option",
                            # efficacy
                              "Ladder of perceived ability to control child behavior",
                              "I am bringing up my child well")
# open coding parenting
  out.ocp <- open.coding
  ate.ocp <- ate[ate$variable %in% out.ocp,]
  ate.ocp <- ate.ocp[match(out.ocp, ate.ocp$variable),] # re-order
  ate.ocp.p <- ate.ocp
  # table labels
    ate.ocp$variable <- c("Beat body",
                          "Slapped on the face", 
                          "Shouted or yelled",
                          "Denied food",
                          "Locked out of the house",
                          "Pump tire",
                          "Asked to stop behavior",
                          "Time out",
                          "Took away privledge/activity",
                          "Advised",
                          "Put to bed",
                          "Other"
                          )
  # figure labels
    ate.ocp.p$variable <- c("Beat body",
                          "Slapped on the face", 
                          "Shouted or yelled",
                          "Denied food",
                          "Locked out of the house",
                          "Pump tire",
                          "Asked to stop behavior",
                          "Time out",
                          "Took away privledge/activity",
                          "Advised",
                          "Put to bed",
                          "Other"
    )
# positive interaction
  out.pos <- c(pos.int.p, pos.int.c)
  ate.pos <- ate[ate$variable %in% out.pos,]
  ate.pos <- ate.pos[match(out.pos, ate.pos$variable),] # re-order
  ate.pos.p <- ate.pos
  # table labels
    ate.pos$variable <- c(# caregivers
                            "Positive interaction composite, caregivers",
                            "\\hspace{0.25cm}Ladder: time spent with child past week",
                            "\\hspace{0.25cm}Ladder: played with child past week",
                            "\\hspace{0.25cm}Ladder: talked with child past week",
                            "\\hspace{0.25cm}Ladder: praised child past week",
                          # kids
                            "Positive interaction composite, children",
                            "\\hspace{0.25cm}Spent time with caregiver past week",
                            "\\hspace{0.25cm}Played with caregiver at home past week",
                            "\\hspace{0.25cm}Talked with caregiver past week",
                            "\\hspace{0.25cm}Praised by caregiver past week"
                          )
  # figure labels
    ate.pos.p$variable <- c(# caregivers
                            "Positive interaction composite (care)",
                              "Ladder: time spent with child past week (care)",
                              "Ladder: played with child past week (care)",
                              "Ladder: talked with child past week (care)",
                              "Ladder: praised child past week (care)",
                            # kids
                            "Positive interaction composite (child)",
                              "Spent time with caregiver past week (child)",
                              "Played with caregiver at home past week (child)",
                              "Talked with caregiver past week (child)",
                              "Praised by caregiver past week (child)"
                          )
# communication
  out.com <- comm
  ate.com <- ate[ate$variable %in% out.com,]
  ate.com <- ate.com[match(out.com, ate.com$variable),] # re-order
  ate.com.p <- ate.com
  # table labels
    ate.com$variable <- c("Ladder: Able to understand child\\'s speech",
                          "Ladder: Frequency of child\\'s verbalizations",
                          "Number of caregiver verbalizations",
                          "Caregiver praises normalized by number of verbalizations",
                          "Caregiver negative talk normalized by number of verbalizations",
                          "Number of child verbalizations",
                          "Child verbalizations as percentage of total verbalizations")
  # figure labels
    ate.com.p$variable <- c("Ladder: Able to understand child's speech",
                            "Ladder: Frequency of child's verbalizations",
                            "Number of caregiver verbalizations",
                            "Caregiver praises normalized by number of verbalizations",
                            "Caregiver negative talk normalized by number of verbalizations",
                            "Number of child verbalizations",
                            "Child verbalizations as percentage of total verbalizations")
# ability and sdq
  out.aas <- c(ability, sdq)
  ate.aas <- ate[ate$variable %in% out.aas,]
  ate.aas <- ate.aas[match(out.aas, ate.aas$variable),] # re-order
  ate.aas.p <- ate.aas
  # table labels
    ate.aas$variable <- c("Child language ability",
                          "\\hspace{0.25cm}Receptive vocabulary",
                          "\\hspace{0.25cm}Expressive vocabulary",
                          "\\hspace{0.25cm}Story comprehension",
                          "\\hspace{0.25cm}Verbal fluency",
                          "Child numeracy and counting",
                          "SDQ: hyperactivity (care)",
                          "SDQ: emotional (care)",
                          "SDQ: conduct (care)")
  # figure labels
    ate.aas.p$variable <- c("Child language ability",
                            "Receptive vocabulary",
                            "Expressive vocabulary",
                            "Story comprehension",
                            "Verbal fluency",
                            "Child numeracy and counting",
                            "SDQ: hyperactivity (care)",
                            "SDQ: emotional (care)",
                            "SDQ: conduct (care)")
# malaria
  out.mal <- malaria
  ate.mal <- ate[ate$variable %in% out.mal,]
  ate.mal <- ate.mal[match(out.mal, ate.mal$variable),] # re-order
  ate.mal.p <- ate.mal
  # table labels
    ate.mal$variable <- c("Household owns bed net",
                          "Someone slept under the bednet last night",
                          "Child slept under the bednet last night",
                          "Used bednet when child went to bed",
                          "Enumerator observed net hanging")
  # figure labels
    ate.mal.p$variable <- c("Household owns bed net",
                            "Someone slept under the bednet last night",
                            "Child slept under the bednet last night",
                            "Used bednet when child went to bed",
                            "Enumerator observed net hanging")

# create tables 
# -----------------------------------------------------------------------------
  out <- c("ate.par", "ate.ocp", "ate.pos", "ate.com", "ate.aas", "ate.mal")
  library(xtable)
  for (i in out) {
    temp <- get(i)
    temp$ci95 <- paste0("(", round(temp$l95ci,2), 
                        " to ", 
                        round(temp$u95ci,2), 
                        ")")
    temp <- temp[,c("variable", "scale", "expdir",
                    "control.mean", "control.sd", "ate", "ate.se",
                    "sig", "ci95", "glass")]
    temp$scale <- paste0(temp$scale, " (", temp$expdir, ")")
    temp$expdir <- NULL
    t <- xtable(temp)
    digits(t) <- c(0,0,0,2,2,2,2,0,0,2)
    f <- paste0(outtables, i, ".tex")
    print(t,
          only.contents=TRUE,
          include.rownames=FALSE,
          include.colnames=FALSE,
          hline.after=NULL,
          type="latex",
          file=f,
          sanitize.text.function=identity
    )
    remove(temp)
  }
  detach("package:xtable", unload=TRUE)

# create plots 
# -----------------------------------------------------------------------------
  for (p in out) {
    pp <- paste(p, "p", sep=".")
    ggplot(get(pp), aes(x=reorder(variable, ate.p), y=ate.p)) +
      geom_pointrange(aes(ymin=l95ci.p,
                          ymax=u95ci.p),
                      linetype="dashed") +
      geom_point(size=3, aes(colour=factor(expdir.yes))) +
      ylab("ATE in percentage change") +
      geom_hline(yintercept=0, color="darkgrey") +
      ylim(-100,100) +
      scale_colour_manual(values=c("0"="grey", "1"="black"),
                          name="Expected Direction",
                          breaks=c("0", "1"),
                          labels=c("No", "Yes")) +
      theme_bw() +
      theme(legend.position="none") +
      theme(panel.border = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(family="Times New Roman")) +
      theme(axis.text.x = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.text.y = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.line = element_line(color = 'black')) +
      coord_flip()
    #dev.off()
    f <- paste0(outfigures, 
                gsub(".", "-", p, fixed=TRUE), ".pdf")
    if (nrow(get(pp))>=10) {
      ggsave(filename=f, height=(4/18)*nrow(get(pp)))
    } else {
      ggsave(filename=f, height=.33*nrow(get(pp)))
    }
  }

# main outcomes table and plot 
# =============================================================================
  priout <- c("p.harshdiscipline.a", "p.pbm.a", "p.posint.a",
              "c.posint.a", "care.praises.n", "child.verb", 
              "totalVerb.childPer", 
              "c.language",
              "c.numeracy.count",
              "p.sdq.hyperactivity", "p.sdq.emotional", "p.sdq.conduct",      
              "bn1.net.own", "bn4.net.use.tc")
  priout.tbl <- ate[ate$variable %in% priout,]
  priout.tbl$gl95 <- priout.tbl$l95ci/priout.tbl$control.sd
  priout.tbl$gu95 <- priout.tbl$u95ci/priout.tbl$control.sd
  priout.tbl$variable2 <- c("Harsh discipline composite, caregiver report",
                           "Positive behavior management composite, caregiver report",
                           "Positive interaction composite, caregiver report",
                           "Positive interaction composite, child report",
                           "Caregiver praises normalized by number of verbalizations",
                           "Number of child verbalizations",
                           "Child verbalizations as percentage of total verbalizations",
                           "Child language ability",
                           "Child numeracy and counting",
                           "SDQ: hyperactivity, caregiver report",
                           "SDQ: emotional, caregiver report",
                           "SDQ: conduct, caregiver report",
                           "Household owns bed net, caregiver report",
                           "Child slept under the bednet last night, caregiver report")
  priout.tbl$variable3 <- priout.tbl$variable2
  priout.tbl$variable2 <- paste0("\\hspace{0.25cm}", priout.tbl$variable2)
  par <- c("p.harshdiscipline.a", "p.pbm.a")
  int <- c("p.posint.a", "c.posint.a")
  ver <- c("care.praises.n", "child.verb", "totalVerb.childPer")
  abi <- c("c.language", "c.numeracy.count")
  emo <- c("p.sdq.hyperactivity", "p.sdq.emotional", "p.sdq.conduct",      
           "c.sdq.hyperactivity", "c.sdq.emotional", "c.sdq.conduct")
  mal <- c("bn1.net.own", "bn4.net.use.tc")
  par.tbl <- priout.tbl[priout.tbl$variable %in% par,]
  int.tbl <- priout.tbl[priout.tbl$variable %in% int,]
  ver.tbl <- priout.tbl[priout.tbl$variable %in% ver,]
  abi.tbl <- priout.tbl[priout.tbl$variable %in% abi,]
  emo.tbl <- priout.tbl[priout.tbl$variable %in% emo,]
  mal.tbl <- priout.tbl[priout.tbl$variable %in% mal,]
  tbl <- c("par.tbl", "int.tbl", "ver.tbl", "abi.tbl", "emo.tbl", "mal.tbl")

  library(xtable)
  for (i in tbl) {
    temp <- get(i)
    temp$ci95 <- paste0("(", round(temp$l95ci,2), 
                        " to ", 
                        round(temp$u95ci,2), 
                        ")")
    temp <- temp[,c("variable2", "scale", "expdir",
                    "control.mean", "control.sd", "ate", "ate.se",
                    "sig", "ci95", "glass")]
    temp$scale <- paste0(temp$scale, " (", temp$expdir, ")")
    temp$expdir <- NULL
    t <- xtable(temp)
    digits(t) <- c(0,0,0,2,2,2,2,0,0,2)
    f <- paste0(outtables, i, ".tex")
    print(t,
          only.contents=TRUE,
          include.rownames=FALSE,
          include.colnames=FALSE,
          hline.after=NULL,
          type="latex",
          file=f,
          sanitize.text.function=identity
    )
    remove(temp)
  }
  detach("package:xtable", unload=TRUE)

# create plots 
# -----------------------------------------------------------------------------
  labs <- c("Panel A: Parenting Behaviors",
            "Panel B: Caregiver-Child Interactions",
            "Panel C: Communication During Observational Task",
            "Panel D: Child Cognitive Abilities",
            "Panel E: Child Wellbeing",
            "Panel F: Malaria Prevention")
  ct <- 1
  for (i in tbl) {
    plot <- ggplot(get(i), aes(x=reorder(as.character(variable3), 
                                  glass), y=glass)) +
      geom_pointrange(aes(ymin=gl95,
                          ymax=gu95),
                      linetype="dashed") +
      geom_point(size=2.5, aes(colour=factor(expdir.yes))) +
      ylab("Standardized Effect Size (dotted 95% CI)") +
      geom_hline(yintercept=0, color="darkgrey") +
      ylim(-1,1) +
      scale_colour_manual(values=c("0"="grey", "1"="black"),
                          name="Expected Direction",
                          breaks=c("0", "1"),
                          labels=c("No", "Yes")) +
      theme_bw() +
      theme(legend.position="none") +
      labs(title = labs[ct]) +
      theme(plot.title = element_text(family="Times New Roman",
                                      size=12)) +
      theme(panel.border = element_blank()) +
      theme(axis.title.y = element_blank()) +
      theme(axis.title.x = element_text(family="Times New Roman",
                                        size=10)) +
      theme(axis.text.x = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.text.y = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.line = element_line(color = 'black')) +
      coord_fixed(xlim = 5) +
      coord_flip() 
      assign(paste(i, "plot", sep="."), plot)
      
      ct <- ct+1
  }
# combine plots
  gb1 <- ggplot_build(par.tbl.plot)
  gb2 <- ggplot_build(int.tbl.plot)
  gb3 <- ggplot_build(ver.tbl.plot)
  gb4 <- ggplot_build(abi.tbl.plot)
  gb5 <- ggplot_build(emo.tbl.plot)
  gb6 <- ggplot_build(mal.tbl.plot)

# work out how many y breaks for each plot
  n1 <- length(gb1$data)
  n2 <- length(gb2$data)
  n3 <- length(gb3$data)
  n4 <- length(gb4$data)
  n5 <- length(gb5$data)
  n6 <- length(gb6$data)

  gA <- ggplot_gtable(gb1)
  gB <- ggplot_gtable(gb2)
  gC <- ggplot_gtable(gb3)
  gD <- ggplot_gtable(gb4)
  gE <- ggplot_gtable(gb5)
  gF <- ggplot_gtable(gb6)

# combine both plots (last should really be "pmax", it's an unfortunate bug)
  g1 <- gtable:::rbind_gtable(gA, gB, "last")
  g2 <- gtable:::rbind_gtable(g1, gC, "last")
  g3 <- gtable:::rbind_gtable(g2, gD, "last")
  g4 <- gtable:::rbind_gtable(g3, gE, "last")
  g <- gtable:::rbind_gtable(g4, gF, "last")

# locate the panels in the gtable layout
  panels <- g$layout$t[grep("panel", g$layout$name)]
# assign new (relative) heights to the panels, based on the number of breaks
  g$heights[panels] <- unit(c(n1, n2, n3, n4, n5, n6), "null")
  
# combine
  grid.newpage()
  f <- paste0(outfigures, "main.pdf")
  #f <- paste0(outfigures, "main.tif")
#   tiff(f, width=9, height=9, units="in", res=300,
#        compression="lzw", type = "cairo")
  cairo_pdf(f, width=7, height=9)
  grid.draw(g)
  dev.off()


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Sensitivity
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
# missing
  missing.sen <- miss[miss$variable %in% priout,]
  missing.sen <- missing.sen[match(priout, missing.sen$variable),] # re-order
  missing.sen$propmiss <- round(missing.sen$propmiss*100, 1)
  missing.sen <- missing.sen[,c(3,2,4)]
# main specification results
  priout.tbl.m1 <- priout.tbl
# bounded
  priout.tbl.mb <- ate.mb[ate.mb$variable %in% priout,]
  priout.tbl.mb$gl95 <- priout.tbl.mb$l95ci/priout.tbl.mb$control.sd
  priout.tbl.mb$gu95 <- priout.tbl.mb$u95ci/priout.tbl.mb$control.sd
  vars <- c("Harsh discipline composite, caregiver report",
            "Positive behavior management composite, caregiver report",
            "Positive interaction composite, caregiver report",
            "Positive interaction composite, child report",
            "Caregiver praises normalized by number of verbalizations",
            "Number of child verbalizations",
            "Child verbalizations as percentage of total verbalizations",
            "Child language ability",
            "Child numeracy and counting",
            "SDQ: hyperactivity, caregiver report",
            "SDQ: emotional, caregiver report",
            "SDQ: conduct, caregiver report",
            "Household owns bed net, caregiver report",
            "Child slept under the bednet last night, caregiver report")
  priout.tbl.mb$variable3 <- vars
# no covariates
  priout.tbl.nc <- ate.nc[ate.nc$variable %in% priout,]
  priout.tbl.nc$gl95 <- priout.tbl.nc$l95ci/priout.tbl.nc$control.sd
  priout.tbl.nc$gu95 <- priout.tbl.nc$u95ci/priout.tbl.nc$control.sd
  priout.tbl.nc$variable3 <- vars
# remove columns
  priout.tbl.m1 <- priout.tbl.m1[,c("variable3", 
                                    "ate", "ate.se", "sig")]
  priout.tbl.mb <- priout.tbl.mb[,c("variable3", 
                                    "ate", "ate.se", "sig")]
  priout.tbl.nc <- priout.tbl.nc[,c("variable3", 
                                    "ate", "ate.se", "sig")]
  names(priout.tbl.mb) <- c("variable3", 
                            "ate.mb", "ate.se.mb", "sig.mb")
  names(priout.tbl.nc) <- c("variable3", 
                            "ate.nc", "ate.se.nc", "sig.nc")
  sen <- merge(priout.tbl.m1, priout.tbl.nc, by="variable3")
  sen <- merge(sen, priout.tbl.mb, by="variable3")
  sen <- sen[match(vars, sen$variable3),] # re-order
# add missing to table
  sen <- cbind(sen, missing.sen)
  sen <- sen[,c(1,11,12,13,2:10)]
# xtable
  library(xtable)
  t <- xtable(sen)
  digits(t) <- c(0,0,0,0,1,2,2,0,2,2,0,2,2,0)
  f <- paste0(outtables, "sen.tex")
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  detach("package:xtable", unload=TRUE)


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Treatment heterogeneity
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  source("data and replication files/master/pmd1/scripts/pmd1-impact-analysis-het.R")
  source("data and replication files/master/pmd1/scripts/pmd1-impact-analysis-quantile.R")


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Mediation
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  
  source("data and replication files/master/pmd1/scripts/pmd1-impact-analysis-mediation.R")
