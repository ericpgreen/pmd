# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PMD 1 Impact Analysis
# mediation
# runs from scripts/pmd1-impact-analysis.R
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

  
# rename dataframe
  dat <- base.end
  
# 1 mediator models 
# =============================================================================
# put in format: (long name, short label, x, y, m)
  mods <- list(m1=c("Caregiver reported child conduct problems mediated by caregiver reported use of harsh discipline",
                    "Conduct BY Harsh",
                    "b.treatment", "p.sdq.conduct", "p.harshdiscipline.a"),
               m2=c("Child receptive vocabulary mediated by child reported positive interactions",
                    "Receptive Vocab BY Positive Interactions",
                    "b.treatment", "c.receptive.vocab", "c.posint.a"),
               m3=c("Caregiver reported use of harsh discipline mediated by child reported positive interactions",
                    "Harsh BY Positive Interactions",
                    "b.treatment", "p.harshdiscipline.a", "c.posint.a"),
               m4=c("Child reported positive interactions mediated by caregiver reported use of harsh discipline",
                    "Positive Interactions BY Harsh",
                    "b.treatment", "c.posint.a", "p.harshdiscipline.a"),
               m5=c("Caregiver reported child emotional problems mediated by child reported positive interactions",
                    "Emotional BY Positive Interactions",
                    "b.treatment", "c.sdq.emotional", "c.posint.a")
              )
# create object to store results
  temp <- data.frame(matrix(NA,
                            nrow = (length(mods)), 
                            ncol = 14))
# loop
  for (i in 1:length(mods)) {
    # define variables
      x <- mods[[i]][3]
      y <- mods[[i]][4]
      m1 <- mods[[i]][5]
      dat$x <- dat[,x]
      # standardize, but first check to make sure not already standardized
      dat$y <- dat[,y]
      if ((abs(mean(dat$y, na.rm=T))-0) > 0.01) {
        dat$y <- scale(dat$y)
      }
      dat$m1 <- dat[,m1]
      if ((abs(mean(dat$m1, na.rm=T))-0) > 0.01) {
        dat$m1 <- scale(dat$m1)
      }
    # define model
      model <-  '
                # outcome model 
                  y ~ cp*x + b1*m1
                
                # mediator model(s)
                  m1 ~ a1*x
                
                # mediated (indirect) effects
                  IDEm1 := a1*b1

                # total effect
                  c := cp + (a1*b1)
              '
    # estimate
      fit <- sem(model, data=dat, se="bootstrap")
      boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")
    # construct mediation percentage
      med <- boot.fit$est[boot.fit$label=="IDEm1"]
    # store estimates
      temp[i,1] <- i                                      # model number
      temp[i,2] <- mods[[i]][2]                           # short label
      temp[i,3] <- mods[[i]][1]                           # long label
      temp[i,4] <- boot.fit$est[boot.fit$label=="a1"]     # a coeff (x on m)
      temp[i,5] <- boot.fit$se[boot.fit$label=="a1"]      # a se    
      temp[i,6] <- boot.fit$est[boot.fit$label=="b1"]     # b coeff (m on y)
      temp[i,7] <- boot.fit$se[boot.fit$label=="b1"]      # b se
      temp[i,8] <- boot.fit$est[boot.fit$label=="c"]      # c coeff (x on y)
      temp[i,9] <- boot.fit$se[boot.fit$label=="c"]       # c se
      temp[i,10] <- boot.fit$est[boot.fit$label=="cp"]    # c' coeff (x on y|m)
      temp[i,11] <- boot.fit$se[boot.fit$label=="cp"]     # c' se
      temp[i,12] <- boot.fit$est[boot.fit$label=="IDEm1"] # mediation 
      temp[i,13] <- boot.fit$ci.lower[boot.fit$label=="IDEm1"] # ci05 boot
      temp[i,14] <- boot.fit$ci.upper[boot.fit$label=="IDEm1"] # ci95 boot
  }
# rename
  assign("med1", temp)
  remove(temp)
  names(med1) <- c("model", "short", "long", 
                   "a.est", "a.se", "b.est", "b.se", "c.est", "c.se",
                   "cp.est", "cp.se", "med", "ci05", "ci95")
# plot 
  ggplot(med1, aes(x=reorder(short, med), y=med)) +
    geom_pointrange(aes(ymin=ci05,
                        ymax=ci95),
                    linetype="dashed") +
    geom_point(size=3, aes(colour="black")) +
    ylab("Standardized mediation effect (dashed 95%CI)") +
    geom_hline(yintercept=0, color="darkgrey") +
    ylim(-1,1) +
    theme_bw() +
    labs(title = "Standardized mediation effects") +
    theme(plot.title = element_text(family="Times New Roman",
                                    size=12)) +
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
# save plot clean up
  f <- paste0(outfigures, "med1.pdf")
  if (nrow(med1)>=10) {
    ggsave(filename=f, height=(4/18)*nrow(med1))
  } else {
    ggsave(filename=f, height=.33*nrow(med1))
  }
  

# 2 mediator models 
# =============================================================================
# put in format: (long name, short label, x, y, m1, m2, m1name, m2name)
  mods <- list(m1=c("Panel A: Child emotional problems mediated by harsh discipline and positive interactions",
                    "Emotional BY (Harsh AND Positive Interaction)",
                    "b.treatment", "p.sdq.emotional", 
                    "p.harshdiscipline.a", "c.posint.a",
                    "Harsh Discipline", "Positive Interaction"),
               m2=c("Panel B: Child language mediated by harsh discipline and positive interactions",
                    "Language BY (Harsh AND Positive Interaction)",
                    "b.treatment", "c.language", 
                    "p.harshdiscipline.a", "c.posint.a",
                    "Harsh Discipline", "Positive Interaction")
              )
# create object to store results
  temp <- data.frame(matrix(NA,
                            nrow = (length(mods)), 
                            ncol = 27))
# loop
  for (i in 1:length(mods)) {
    # define variables
      x <- mods[[i]][3]
      y <- mods[[i]][4]
      m1 <- mods[[i]][5]
      m2 <- mods[[i]][6]
      dat$x <- dat[,x]
      # standardize, but first check to make sure not already standardized
      dat$y <- dat[,y]
      if ((abs(mean(dat$y, na.rm=T))-0) > 0.01) {
        dat$y <- scale(dat$y)
      }
      dat$m1 <- dat[,m1]
      if ((abs(mean(dat$m1, na.rm=T))-0) > 0.01) {
        dat$m1 <- scale(dat$m1)
      }
      dat$m2 <- dat[,m2]
      if ((abs(mean(dat$m2, na.rm=T))-0) > 0.01) {
        dat$m2 <- scale(dat$m2)
      }
    # define model
      model <-  '
                # outcome model 
                  y ~ cp*x + b1*m1 + b2*m2
                
                # mediator model(s)
                  m1 ~ a1*x
                  m2 ~ a2*x
                
                # mediated (indirect) effects
                  IDEm1 := a1*b1
                  IDEm2 := a2*b2
                  IDEtot := (a1*b1) + (a2*b2)
                
                # total effect
                  c := cp + (a1*b1) + (a2*b2)
                '
    # estimate
      fit <- sem(model, data=dat, se="bootstrap")
      boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")
    # construct mediation percentage
      medm1 <- boot.fit$est[boot.fit$label=="IDEm1"]
      medm2 <- boot.fit$est[boot.fit$label=="IDEm2"]
      medtot <- boot.fit$est[boot.fit$label=="IDEtot"]
    # store estimates
      temp[i,1] <- i                                      # model number
      temp[i,2] <- mods[[i]][2]                           # short label
      temp[i,3] <- mods[[i]][1]                           # long label
      temp[i,4] <- "Total Indirect Effect"
      temp[i,5] <- mods[[i]][7]                           # name m1
      temp[i,6] <- mods[[i]][8]                           # name m2
      temp[i,7] <- boot.fit$est[boot.fit$label=="a1"]     # a1 coeff (x on m1)
      temp[i,8] <- boot.fit$se[boot.fit$label=="a1"]      # a1 se
      temp[i,9] <- boot.fit$est[boot.fit$label=="a2"]     # a2 coeff (x on m2)
      temp[i,10] <- boot.fit$se[boot.fit$label=="a2"]     # a2 se
      temp[i,11] <- boot.fit$est[boot.fit$label=="b1"]    # b1 coeff (m1 on y)
      temp[i,12] <- boot.fit$se[boot.fit$label=="b1"]     # b1 se
      temp[i,13] <- boot.fit$est[boot.fit$label=="b2"]    # b2 coeff (m2 on y)
      temp[i,14] <- boot.fit$se[boot.fit$label=="b2"]     # b2 se
      temp[i,15] <- boot.fit$est[boot.fit$label=="c"]     # c coeff (x on y)
      temp[i,16] <- boot.fit$se[boot.fit$label=="c"]      # c se
      temp[i,17] <- boot.fit$est[boot.fit$label=="cp"]    # c' (x on y|m1+m2)
      temp[i,18] <- boot.fit$se[boot.fit$label=="cp"]     # c' se
      temp[i,19] <- boot.fit$est[boot.fit$label=="IDEm1"]       # mediation m1
      temp[i,20] <- boot.fit$est[boot.fit$label=="IDEm2"]       # mediation m2
      temp[i,21] <- boot.fit$est[boot.fit$label=="IDEtot"]      # total med
      temp[i,22] <- boot.fit$ci.lower[boot.fit$label=="IDEm1"]  # ci05 boot m1
      temp[i,23] <- boot.fit$ci.upper[boot.fit$label=="IDEm1"]  # ci95 boot m1
      temp[i,24] <- boot.fit$ci.lower[boot.fit$label=="IDEm2"]  # ci05 boot m2
      temp[i,25] <- boot.fit$ci.upper[boot.fit$label=="IDEm2"]  # ci95 boot m2
      temp[i,26] <- boot.fit$ci.lower[boot.fit$label=="IDEtot"] # ci05 boot tot
      temp[i,27] <- boot.fit$ci.upper[boot.fit$label=="IDEtot"] # ci95 boot tot
  }
# rename
  assign("med2", temp)
  remove(temp)
  names(med2) <- c("model", "short", "long", "totname", "m1name", "m2name",
                   "a1.est", "a1.se", "a2.est", "a2.se",
                   "b1.est", "b1.se", "b2.est", "b2.se",
                   "c.est", "c.se", "cp.est", "cp.se", 
                   "m1med", "m2med", "totmed",
                   "m1ci05", "m1ci95", 
                   "m2ci05", "m2ci95", 
                   "totci05", "totci95")
# separate, rearrange, and plot
  for (i in 1:nrow(med2)) {
    temp <- med2[i,c("long",
                     "totname", "totmed", "totci05", "totci95",
                     "m1name", "m1med", "m1ci05", "m1ci95",
                     "m2name", "m2med", "m2ci05", "m2ci95")]
    names(temp) <- c("long",
                     "n.1", "p.1", "l.1", "u.1",
                     "n.2", "p.2", "l.2", "u.2",
                     "n.3", "p.3", "l.3", "u.3")
    temp <- reshape(temp, 
                    direction="long", 
                    varying=2:13,
                    timevar="model",
                    v.names=c("l", "n", "p", "u"),
                    times=c("1","2","3"))
    names(temp) <- c("long", "model", "name", "est", "l95", "u95", "id")
  # rename
    assign(paste("df", names(mods[i]), sep="."), temp)
    remove(temp)
  }

# plot
  for (i in 1:length(mods)) {
    d <- get(paste("df", names(mods[i]), sep="."))
    plot <- ggplot(d, aes(x=reorder(name, abs(est)), y=est)) +
      geom_point(size=3, colour="black") +
      geom_pointrange(aes(ymin=l95,
                          ymax=u95),
                      linetype="dashed") +
      ylab("Standardized mediation effect (dashed 95%CI)") +
      geom_hline(yintercept=0, color="darkgrey") +
      ylim(-.5,.5) +
      theme_bw() +
      labs(title = d$long[1]) +
      theme(plot.title = element_text(family="Times New Roman",
                                      size=12)) +
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
    assign(paste("plot", i, sep="."), plot)
  }

# combine plots
  for (i in 1:length(mods)) {
    gb <- ggplot_build(get(paste("plot", i, sep=".")))
    n <- length(gb$data)
    g <- ggplot_gtable(gb)
    assign(paste0("g",i), g)
    remove(g)
    assign(paste0("n",i), n)
    remove(n)
  }

# combine both plots (last should really be "pmax", it's an unfortunate bug)
  p <- gtable:::rbind_gtable(g1, g2, "last")
  #p <- gtable:::rbind_gtable(p, g3, "last")
  # manual!
  
# locate the panels in the gtable layout
  panels <- p$layout$t[grep("panel", p$layout$name)]
# assign new (relative) heights to the panels, based on the number of breaks
  p$heights[panels] <- unit(c(n1, n2), "null")
  
# combine
  grid.newpage()
  f <- paste0(outfigures, "med2.pdf")
  cairo_pdf(f, width=7, height=4)
  grid.draw(p)
  dev.off()

#semPaths(fit, layout="spring")


# 3 mediator models 
# =============================================================================
# put in format: (long name, short label, x, y, m1, m2, m3, m1name, m2name, m3name)
  mods <- list(m1=c("Child emotional problems mediated by harsh discipline and positive interactions",
                    "Emotional BY (Harsh AND Positive Behavior AND Positive Interaction)",
                    "b.treatment", "p.sdq.emotional", 
                    "p.harshdiscipline.a", "p.pbm.a", "c.posint.a",
                    "Harsh Discipline", "Positive Behavior Management", "Positive Interaction"),
               m2=c("Child language mediated by harsh discipline and positive interactions",
                    "Receptive Vocab BY (Harsh AND Positive Behavior AND Positive Interaction)",
                    "b.treatment", "c.language", 
                    "p.harshdiscipline.a", "p.pbm.a", "c.posint.a",
                    "Harsh Discipline", "Positive Behavior Management", "Positive Interaction")
  )
# create object to store results
  temp <- data.frame(matrix(NA,
                            nrow = (length(mods)), 
                            ncol = 35))
# loop
  for (i in 1:length(mods)) {
    # define variables
    x <- mods[[i]][3]
    y <- mods[[i]][4]
    m1 <- mods[[i]][5]
    m2 <- mods[[i]][6]
    m3 <- mods[[i]][7]
    dat$x <- dat[,x]
    # standardize, but first check to make sure not already standardized
    dat$y <- dat[,y]
    if ((abs(mean(dat$y, na.rm=T))-0) > 0.01) {
      dat$y <- scale(dat$y)
    }
    dat$m1 <- dat[,m1]
    if ((abs(mean(dat$m1, na.rm=T))-0) > 0.01) {
      dat$m1 <- scale(dat$m1)
    }
    dat$m2 <- dat[,m2]
    if ((abs(mean(dat$m2, na.rm=T))-0) > 0.01) {
      dat$m2 <- scale(dat$m2)
    }
    dat$m3 <- dat[,m3]
    if ((abs(mean(dat$m3, na.rm=T))-0) > 0.01) {
      dat$m3 <- scale(dat$m3)
    }    
    # define model
      model <-  '
                # outcome model 
                  y ~ cp*x + b1*m1 + b2*m2 + b3*m3
                
                # mediator model(s)
                  m1 ~ a1*x
                  m2 ~ a2*x
                  m3 ~ a3*x
                  
                # mediated (indirect) effects
                  IDEm1 := a1*b1
                  IDEm2 := a2*b2
                  IDEm3 := a3*b3
                  IDEtot := (a1*b1) + (a2*b2) + (a3*b3)
                  
                # total effect
                  c := cp + (a1*b1) + (a2*b2) + (a3*b3)
                '
    # estimate
      fit <- sem(model, data=dat, se="bootstrap")
      boot.fit <- parameterEstimates(fit, boot.ci.type="bca.simple")
    # construct mediation percentage
      medm1 <- boot.fit$est[boot.fit$label=="IDEm1"]
      medm2 <- boot.fit$est[boot.fit$label=="IDEm2"]
      medm3 <- boot.fit$est[boot.fit$label=="IDEm3"]
      medtot <- boot.fit$est[boot.fit$label=="IDEtot"]
  # store estimates
    temp[i,1] <- i                                      # model number
    temp[i,2] <- mods[[i]][2]                           # short label
    temp[i,3] <- mods[[i]][1]                           # long label
    temp[i,4] <- "Total Indirect Effect"
    temp[i,5] <- mods[[i]][8]                           # name m1
    temp[i,6] <- mods[[i]][9]                           # name m2
    temp[i,7] <- boot.fit$est[boot.fit$label=="a1"]     # a1 coeff (x on m1)
    temp[i,8] <- boot.fit$se[boot.fit$label=="a1"]      # a1 se
    temp[i,9] <- boot.fit$est[boot.fit$label=="a2"]     # a2 coeff (x on m2)
    temp[i,10] <- boot.fit$se[boot.fit$label=="a2"]     # a2 se
    temp[i,11] <- boot.fit$est[boot.fit$label=="b1"]    # b1 coeff (m1 on y)
    temp[i,12] <- boot.fit$se[boot.fit$label=="b1"]     # b1 se
    temp[i,13] <- boot.fit$est[boot.fit$label=="b2"]    # b2 coeff (m2 on y)
    temp[i,14] <- boot.fit$se[boot.fit$label=="b2"]     # b2 se
    temp[i,15] <- boot.fit$est[boot.fit$label=="c"]     # c coeff (x on y)
    temp[i,16] <- boot.fit$se[boot.fit$label=="c"]      # c se
    temp[i,17] <- boot.fit$est[boot.fit$label=="cp"]    # c' (x on y|m1+m2)
    temp[i,18] <- boot.fit$se[boot.fit$label=="cp"]     # c' se
    temp[i,19] <- boot.fit$est[boot.fit$label=="IDEm1"]       # mediation m1
    temp[i,20] <- boot.fit$est[boot.fit$label=="IDEm2"]       # mediation m2
    temp[i,21] <- boot.fit$est[boot.fit$label=="IDEtot"]      # total med
    temp[i,22] <- boot.fit$ci.lower[boot.fit$label=="IDEm1"]  # ci05 boot m1
    temp[i,23] <- boot.fit$ci.upper[boot.fit$label=="IDEm1"]  # ci95 boot m1
    temp[i,24] <- boot.fit$ci.lower[boot.fit$label=="IDEm2"]  # ci05 boot m2
    temp[i,25] <- boot.fit$ci.upper[boot.fit$label=="IDEm2"]  # ci95 boot m2
    temp[i,26] <- boot.fit$ci.lower[boot.fit$label=="IDEtot"] # ci05 boot tot
    temp[i,27] <- boot.fit$ci.upper[boot.fit$label=="IDEtot"] # ci95 boot tot
    temp[i,28] <- mods[[i]][10]                         # name m2
    temp[i,29] <- boot.fit$est[boot.fit$label=="a3"]    # a3 coeff (x on m3)
    temp[i,30] <- boot.fit$se[boot.fit$label=="a3"]     # a3 se
    temp[i,31] <- boot.fit$est[boot.fit$label=="b3"]    # b3 coeff (m3 on y)
    temp[i,32] <- boot.fit$se[boot.fit$label=="b3"]     # b3 se
    temp[i,33] <- boot.fit$est[boot.fit$label=="IDEm3"]       # mediation m3
    temp[i,34] <- boot.fit$ci.lower[boot.fit$label=="IDEm3"]  # ci05 boot m3
    temp[i,35] <- boot.fit$ci.upper[boot.fit$label=="IDEm3"]  # ci95 boot m3
  }
# rename
  assign("med3", temp)
  remove(temp)
  names(med3) <- c("model", "short", "long", "totname", "m1name", "m2name",
                   "a1.est", "a1.se", "a2.est", "a2.se",
                   "b1.est", "b1.se", "b2.est", "b2.se",
                   "c.est", "c.se", "cp.est", "cp.se", 
                   "m1med", "m2med", "totmed",
                   "m1ci05", "m1ci95", 
                   "m2ci05", "m2ci95", 
                   "totci05", "totci95",
                   "m3name", "a3.est", "a3.se", "b3.est", "b3.se",
                   "m3med", "m3ci05", "m3ci95")
  # separate, rearrange, and plot
  for (i in 1:nrow(med3)) {
    temp <- med3[i,c("long",
                     "totname", "totmed", "totci05", "totci95",
                     "m1name", "m1med", "m1ci05", "m1ci95",
                     "m2name", "m2med", "m2ci05", "m2ci95",
                     "m3name", "m3med", "m3ci05", "m3ci95")]
    names(temp) <- c("long",
                     "n.1", "p.1", "l.1", "u.1",
                     "n.2", "p.2", "l.2", "u.2",
                     "n.3", "p.3", "l.3", "u.3",
                     "n.4", "p.4", "l.4", "u.4")
    temp <- reshape(temp, 
                    direction="long", 
                    varying=2:17,
                    timevar="model",
                    v.names=c("l", "n", "p", "u"),
                    times=c("1","2","3", "4"))
    names(temp) <- c("long", "model", "name", "est", "l95", "u95", "id")
    # rename
      assign(paste("df", names(mods[i]), sep="."), temp)
      remove(temp)
  }

# plot
  for (i in 1:length(mods)) {
    d <- get(paste("df", names(mods[i]), sep="."))
    plot <- ggplot(d, aes(x=reorder(name, abs(est)), y=est)) +
      geom_point(size=3, colour="black") +
      geom_pointrange(aes(ymin=l95,
                          ymax=u95),
                      linetype="dashed") +
      ylab("Standardized mediation effect (dashed 95%CI)") +
      geom_hline(yintercept=0, color="darkgrey") +
      ylim(-.5,.5) +
      theme_bw() +
      labs(title = d$long[1]) +
      theme(plot.title = element_text(family="Times New Roman",
                                      size=12)) +
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
    assign(paste("plot", i, sep="."), plot)
  }

# combine plots
  for (i in 1:length(mods)) {
    gb <- ggplot_build(get(paste("plot", i, sep=".")))
    n <- length(gb$data)
    g <- ggplot_gtable(gb)
    assign(paste0("g",i), g)
    remove(g)
    assign(paste0("n",i), n)
    remove(n)
  }

# combine both plots (last should really be "pmax", it's an unfortunate bug)
  p <- gtable:::rbind_gtable(g1, g2, "last")
#p <- gtable:::rbind_gtable(p, g3, "last")
# manual!

# locate the panels in the gtable layout
  panels <- p$layout$t[grep("panel", p$layout$name)]
# assign new (relative) heights to the panels, based on the number of breaks
  p$heights[panels] <- unit(c(n1, n2), "null")
  
# combine
  grid.newpage()
  f <- paste0(outfigures, "med3.pdf")
  cairo_pdf(f, width=7, height=4)
  grid.draw(p)
  dev.off()

# paper stats
  emo.tot <- round(med2$totmed[med2$short=="Emotional BY (Harsh AND Positive Interaction)"], 2)
  lang.tot <- round(med2$totmed[med2$short=="Language BY (Harsh AND Positive Interaction)"], 2)


