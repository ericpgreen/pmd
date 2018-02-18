# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# PMD 1 Impact Analysis
# treatment heterogeneity
# runs from scripts/pmd1-impact-analysis.R
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# run models
# =============================================================================
# define dvs
# -----------------------------------------------------------------------------
  dv <- c("p.harshdiscipline.a", "p.pbm.a", "c.posint.a")
# specify subgroup variables
  het <- c("b.care.female", "b.care.tc.female", 
           "b.care.tc.age", "b.care.sdq.overall")
  het.lab <- c("Assigned to treatment",
               "Female caregiver",
               "Female caregiver x Assignment",
               "Assigned to treatment",
               "Female child", 
               "Female child x Assignment",
               "Assigned to treatment",
               "Child age",
               "Child age x Assignment",
               "Assigned to treatment",
               "Child conduct",
               "Child conduct x Assignment")
  
# loop
# -----------------------------------------------------------------------------
  for (i in 1:length(dv)) {
    # model 2 with each interaction separately
      for (h in het) {
        # remove covariate if == het
          if (h %in% demo.k) {
            g <- grep(h, demo.k)
            demo.k.h <- demo.k[-g]
          } else {
            demo.k.h <- demo.k
          }
          if (h %in% demo.c) {
            g <- grep(h, demo.c)
            demo.c.h <- demo.c[-g]
          } else {
            demo.c.h <- demo.c
          }
      # run         
        m2 <- paste(dv[i], 
                   paste(paste("b.treatment", h, sep="*"), 
                               "community.id",
                               paste(demo.k.h, collapse=" + "),
                               paste(demo.c.h, collapse=" + "),
                         sep=" + "),
                   sep=" ~ ")
        r.m2 <- lm(m2, data=base.end)
        rs.m2 <- summary(r.m2)
        assign(paste("r.m2", dv[i], h, sep="."), r.m2)
        assign(paste("rs.m2", dv[i], h, sep="."), rs.m2)
        remove(r.m2)
        remove(rs.m2)
      }
  }

  
# create tables 
# =============================================================================
# decimal
  rndw0 <- function(x, k) format(round(x, k), nsmall=k)
# loop  
  for (i in 1:length(dv)) {
    # create objects to receive results
      hate <- data.frame(matrix(NA, 
                                nrow = (length(het)*3), 
                                ncol = 3))
      hates <- data.frame(matrix(NA, 
                                 nrow = 2, 
                                 ncol = 3))
      hates[1,1] <- "Obs"
      hates[2,1] <- "Control Mean"
    # extract results for m2
      rs <- 1
      for (h in 1:length(het)) {
        # treatment
        hate[rs,1] <- "Assigned to treatment"
        hate[rs,2] <- paste(rndw0(coefficients(get(paste("rs.m2",
                                                         dv[i],
                                                         het[h],
                                                         sep=".")))[2,1], 
                                  2),
                            " ",
                            paste0("[",
                                   rndw0(coefficients(get(paste("rs.m2",
                                                                dv[i],
                                                                het[h],
                                                                sep=".")))[2,2], 
                                         2),
                                   "]"))
        hate[rs,3] <- asterisk(coefficients(get(paste("rs.m2",
                                                      dv[i],
                                                      het[h],
                                                      sep=".")))[2,4])
        # covariate
        hate[rs+1,1] <- het[h]
        hate[rs+1,2] <- paste(rndw0(coefficients(get(paste("rs.m2",
                                                           dv[i],
                                                           het[h],
                                                           sep=".")))[3,1], 
                                    2),
                              " ",
                              paste0("[",
                                     rndw0(coefficients(get(paste("rs.m2",
                                                                  dv[i],
                                                                  het[h],
                                                                  sep=".")))[3,2], 
                                           2),
                                     "]"))
        hate[rs+1,3] <- asterisk(coefficients(get(paste("rs.m2",
                                                        dv[i],
                                                        het[h],
                                                        sep=".")))[3,4])
        # interaction
        i.row <- nrow(coefficients(get(paste("rs.m2", 
                                        dv[i], 
                                        het[h],
                                        sep="."))))
        hate[rs+2,1] <- paste(het[h], "Assigned", sep="x")
        hate[rs+2,2] <- paste(rndw0(coefficients(get(paste("rs.m2",
                                                           dv[i],
                                                           het[h],
                                                           sep=".")))[i.row,1], 
                                    2),
                              " ",
                              paste0("[",
                                     rndw0(coefficients(get(paste("rs.m2",
                                                                  dv[i],
                                                                  het[h],
                                                                  sep=".")))[i.row,2], 
                                           2),
                                     "]"))
        hate[rs+2,3] <- asterisk(coefficients(get(paste("rs.m2",
                                                        dv[i],
                                                        het[h],
                                                        sep=".")))[i.row,4])
        rs <- rs+3
      }
    # summary
      hates[1,2] <- rndw0(display(get(paste("r.m2", 
                                            dv[i],
                                            het[h],
                                            sep=".")))$n, 0)
      hates[2,2] <- rndw0(aggregate(base.end[,dv[i]],         
                                    by=list(base.end$b.treatment), 
                                    FUN=mean, 
                                    na.rm=TRUE)[1,2], 2)
    # remove NA
      hate <- apply(hate, 2, function(x) ifelse(is.na(x), "", x))
      hates <- apply(hates, 2, function(x) ifelse(is.na(x), "", x))
      hate[,1] <- het.lab
    # rename
      assign(paste("het", dv[i], sep="."), hate)
      assign(paste("hets", dv[i], sep="."), hates)
    # remove 
      remove(hate)
      remove(hates)
  }
# combine
  het.est <- cbind(het.p.harshdiscipline.a, 
                   het.p.pbm.a[,-1])
  het.est <- cbind(het.est, het.c.posint.a[,-1])
  het.sum <- cbind(hets.p.harshdiscipline.a, 
                   hets.p.pbm.a[,-1])
  het.sum <- cbind(het.sum, hets.c.posint.a[,-1])
# xtable
  library(xtable)
  s <- 1
  for (i in 1:length(het)) {
    t <- xtable(het.est[s:(s+2),])
    f <- paste0(outtables, "het.est.", het[i], ".tex")
    print(t,
          only.contents=TRUE,
          include.rownames=FALSE,
          include.colnames=FALSE,
          hline.after=NULL,
          type="latex",
          file=f,
          sanitize.text.function=identity)
    s <- s+3
  }
  t <- xtable(het.sum)
  f <- paste0(outtables, "het.est.sum.tex")
  print(t,
        only.contents=TRUE,
        include.rownames=FALSE,
        include.colnames=FALSE,
        hline.after=NULL,
        type="latex",
        file=f,
        sanitize.text.function=identity)
  detach("package:xtable", unload=TRUE)
