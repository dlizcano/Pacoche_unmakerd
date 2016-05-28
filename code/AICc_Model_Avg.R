


data(mallard)
mallardUMF <- unmarkedFramePCount(mallard.y, siteCovs = mallard.site,
                                  obsCovs = mallard.obs)
##set up models so that each variable on abundance appears twice
fm.mall.one <- pcount(~ ivel + date ~ length + forest, mallardUMF,
                      K = 30)
fm.mall.two <- pcount(~ ivel + date ~ elev + forest, mallardUMF,
                      K = 30)
fm.mall.three <- pcount(~ ivel + date ~ length + elev, mallardUMF,
                        K = 30)
fm.mall.four <- pcount(~ ivel + date ~ 1, mallardUMF, K = 30)

##model list
Cands <- list(fm.mall.one, fm.mall.two, fm.mall.three, fm.mall.four)
Modnames <- c("length + forest", "elev + forest", "length + elev",
              "null")
##compute model-averaged predictions of abundance for values of elev
output <- modavgPred(cand.set = Cands, modnames = Modnames, newdata =
                       data.frame(elev = seq(from = -1.4, to = 2.4, by = 0.1),
                                  length = 0, forest = 0), parm.type = "lambda",
                     type = "response")

plot(output$mod.avg.pred, seq(from = -1.4, to = 2.4, by = 0.1), type = "l")
lines(output$upper.CL, seq(from = -1.4, to = 2.4, by = 0.1), type = "l", col = "red", lty = 2)
plot(output$lower.CL, seq(from = -1.4, to = 2.4, by = 0.1), type = "l")

output$lower.CL[1]
output$lower.CL[2]

str(output)


# there is a bug in storing the $lower.CL sub element. All elements (model-averaged predictions, unconditional SE's, lower.CL, upper.CL) are stored in the $matrix.output matrix and this is where the print method fetches it to display results (as a matrix). 
# This is also why I didn't see the bug you're reporting while testing : ( I'll fix it in the next version of the package, but in the meantime, \
# you can use $matrix.output[, c("mod.avg.pred", "uncond.se", "lower.CL", "upper.CL")]

output$matrix.output[, c("mod.avg.pred", "uncond.se", "lower.CL", "upper.CL")]

