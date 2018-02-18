# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PMD 1 Impact Analysis
# quantile regression
# runs from scripts/pmd1-impact-analysis.R
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


# set to 1 if wanting to run in Stata
# =============================================================================
# change in/out paths in stataqr.do file first
  run <- 0
  if (run==1) {
    # path to stata executable for quantile regression
    #   instructions for mac: http://www.stata.com/support/faqs/mac/advanced-topics/
    #   change path and suffix as needed (StataSE, StataMP, smStata)
    stata <- "/Applications/Stata/StataMP.app/Contents/MacOS/StataMP"
    # export for stata 
    library(foreign)
    keep <- c("b.treatment", "community.id", 
              "p.harshdiscipline.a", "p.pbm.a", "c.posint.a", "p.sdq.conduct",
              demo.c, demo.k)
    # alphabetical is important
    out <- c("c.posint.a", "p.harshdiscipline.a", "p.pbm.a", "p.sdq.conduct")
    title <- c("Panel A: Child-reported positive interactions",
               "Panel B: Caregiver-reported harsh discipline",
               "Panel C: Caregiver-reported positive behavior management",
               "Quantile treatment effects on caregiver-reported child conduct problems")
    dat.qr <- base.end[keep]
    write.dta(dat.qr, "data and replication files/master/pmd1/output/stata/stata.dta")
    # run in stata
    system(paste(stata, "-q -e scripts/stataqr", sep=" "))
  } else {
    out <- c("c.posint.a", "p.harshdiscipline.a", "p.pbm.a", "p.sdq.conduct")
    title <- c("Panel A: Child-reported positive interactions",
               "Panel B: Caregiver-reported harsh discipline",
               "Panel C: Caregiver-reported positive behavior management",
               "Quantile treatment effects on caregiver-reported child conduct problems")
  }
  

# process results
# =============================================================================
  dir <- list.files("data and replication files/master/pmd1/output/stata/output")
# process
  for (w in 1:length(dir)) {
    stataout <- read.csv(paste0("data and replication files/master/pmd1/output/stata/output/", 
                                dir[w]), 
                         stringsAsFactors=F)
    qs <- seq(from=1, to=9, by=1)
    quants <- paste0("q", qs)
    labels <- c("b", "se", "t", "p", "ci")
    quant <- outer(qs, labels, paste, sep="")
    quant <- as.vector(t(quant)) 
    names(stataout) <- c("covariates", quant)
    names(stataout) <- gsub("([0-9]+)([a-z]+)", "\\2\\.\\1", names(stataout))
    #stataout <- stataout[-c(1:2, nrow(stataout)),]
    stataout <- stataout[-c(1, nrow(stataout)),]
    g <- grep("ci", names(stataout))
    ct <- 1
    for (i in g) {
      temp <- colsplit(stataout[,i], ",", c(paste0("cl.", ct),
                                            paste0("cu.", ct)))
      stataout <- cbind(stataout, temp)
      remove(temp)
      ct <- ct+1
    }
    labels <- c(labels, "cl", "cu")
    labelsc <- rep(labels, 9)
    qs <- c(rep(1,7),
            rep(2,7),
            rep(3,7),
            rep(4,7),
            rep(5,7),
            rep(6,7),
            rep(7,7),
            rep(8,7),
            rep(9,7))
    colorder <- paste(labelsc, qs, sep=".")
    stataout <- stataout[,c("covariates", colorder)]
    s <- 2
    e <- 8
    b <- 7
    stataout.l <- reshape(stataout, 
                          direction="long", 
                          varying=c(2:length(stataout)),
                          v.names="value", 
                          timevar="par.q",
                          times=paste(labels, qs))
    stataout.l$covariates <- gsub("=", "", stataout.l$covariates)
    stataout.l$value <- gsub("=", "", stataout.l$value)
    stataout.l$value <- as.numeric(stataout.l$value)
    stataout.l.t <- subset(stataout.l, stataout.l$covariates=="b_treatment")
    stataout.l.t.b <- subset(stataout.l.t, 
                             substr(stataout.l.t$par.q, 1, 1)=="b")
    stataout.l.t.b$quant <- seq(from=.1, to=.9, by=.1)
    stataout.l.t.l <- subset(stataout.l.t, 
                             substr(stataout.l.t$par.q, 1, 2)=="cl")
    stataout.l.t.l$quant <- seq(from=.1, to=.9, by=.1)
    stataout.l.t.u <- subset(stataout.l.t, 
                             substr(stataout.l.t$par.q, 1, 2)=="cu")
    stataout.l.t.u$quant <- seq(from=.1, to=.9, by=.1)
    stataout.l.t.ci <- data.frame(cbind(stataout.l.t.l[,3], stataout.l.t.u[,3]))
    names(stataout.l.t.ci) <- c("lower", "upper")
    stataout.l.t.b <- data.frame(stataout.l.t.b, stataout.l.t.ci)
  
  # standardize mean regression
    m <- paste(paste0("scale(", out[w],")"), 
               paste("b.treatment", "community.id",
                     paste(demo.k, collapse=" + "),
                     paste(demo.c, collapse=" + "),
                     sep=" + "),
               sep=" ~ ")
    r <- lm(m, data=base.end)
    rs <- summary(r)
    hd.m <- rs$coefficients[2,1]                     
    hd.l <- confint(r)[2,1]                       
    hd.u <- confint(r)[2,2]
  # plot
    p <- ggplot(stataout.l.t.b, aes(x=quant, y=value)) +
      stat_smooth(se=F, colour="black", size=1) +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1) +
      ylim(-2,2) +
      geom_hline(yintercept=0, colour="grey", size=1) +
      geom_hline(yintercept=hd.m, linetype="longdash") +
      geom_hline(yintercept=hd.l, linetype="dotted") +
      geom_hline(yintercept=hd.u, linetype="dotted") +
      theme_bw() +
      theme(legend.position="none") +
      theme(panel.border = element_blank()) +
      theme(axis.title.y = element_text(family="Times New Roman")) +
      theme(axis.text.y = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.title.x = element_text(family="Times New Roman")) +
      theme(axis.text.x = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.text.y = element_text(colour="black",
                                       family="Times New Roman")) +
      theme(axis.line = element_line(color = 'black')) +
      ylab("Estimated standardized treatment effect") +
      xlab("Quantiles of the outcome variable") +
      scale_x_continuous(breaks=c(seq(from=.1, to=.9, by=.1))) +
      labs(title = title[w]) +
      theme(plot.title = element_text(family="Times New Roman",
                                      size=12))
      #f <- paste0(outfigures, "hdquant", w, ".pdf")
      #ggsave(filename=f)
      assign(paste0("p", w), p)
      remove(p)
  # effects at tails
    hd.90 <- round(stataout.l.t.b$value[stataout.l.t.b$par.q=="b 9"], 2)
    hd.10 <- round(stataout.l.t.b$value[stataout.l.t.b$par.q=="b 1"], 2)
    assign(paste("hd.m", out[w], sep="."), hd.m)
    assign(paste("hd.90", out[w], sep="."), hd.90)
    assign(paste("hd.10", out[w], sep="."), hd.10)
    remove(hd.90)
    remove(hd.10)
  }
# example
  # remove text size to print correctly on device
  ex <- stataout.l.t.b
  ex$value <- c(-0.4, -0.4, -0.2, 0, 0.7, 0.8, 0.8, 0.8, 0.9)
  ex$lower <- c(-0.6, -0.6, -0.4, -0.2, 0.3, 0.5, 0.5, 0.3, 0)
  ex$upper <- c(-0.2, -0.2, 0, 0.4, 0.9, 1.2, 1.2, 1.3, 1.4)
  p0 <- ggplot(ex, aes(x=quant, y=value)) +
    stat_smooth(se=F, colour="black", size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.1) +
    ylim(-2,2) +
    geom_hline(yintercept=0, colour="grey", size=1) +
    geom_hline(yintercept=.7, linetype="longdash") +
    geom_hline(yintercept=0.5, linetype="dotted") +
    geom_hline(yintercept=0.9, linetype="dotted") +
    annotate("text", x = .15, y = .1, 
             label = "Line of no effect", family= "Times New Roman",
             size=3) +
    annotate("text", x = .26, y = 1.1, 
             label = "OLS estimate (dashed) and 95%CI (dotted)", 
             family= "Times New Roman",
             size=3) +
    annotate("text", x = .5, y = -1.6, 
             label = "< 0 == negative effect", family= "Times New Roman",
             size=3) +
    annotate("text", x = .5, y = 1.6, 
             label = "> 0 == positive effect", family= "Times New Roman",
             size=3) +
    annotate("text", x = .5, y = -0.8, 
             label = "Treatment effect estimate at quantile (thick black line) and 95%CI (grey band)", 
             family= "Times New Roman",
             size=3) +
    theme_bw() +
    theme(legend.position="none") +
    theme(panel.border = element_blank()) +
    theme(axis.title.y = element_text(family="Times New Roman")) +
    theme(axis.text.y = element_text(colour="black",
                                     family="Times New Roman")) +
    theme(axis.title.x = element_text(family="Times New Roman")) +
    theme(axis.text.x = element_text(colour="black",
                                     family="Times New Roman")) +
    theme(axis.text.y = element_text(colour="black",
                                     family="Times New Roman")) +
    theme(axis.line = element_line(color = 'black')) +
    ylab("Estimated standardized treatment effect") +
    xlab("Quantiles of the outcome variable") +
    scale_x_continuous(breaks=c(seq(from=.1, to=.9, by=.1))) +
    labs(title = "How to interpret quantile regression plots") +
    theme(plot.title = element_text(family="Times New Roman",
                                    size=12)) 
# combine
  f <- paste0(outfigures, "hdquant.pdf")
  pdf(f, width=12)
  grid.arrange(p0, p1, p2, p3, nrow=2)
  dev.off()

# glass delta on harsh parenting
  hp.gd <- round(ate.par$glass[ate.par$variable=="Harsh discipline composite$\\dagger$"], 2)
