install.packages("pwr")
library(pwr)

#> for a one-way ANOVA comparing 5 groups, calculate the 
#> sample size needed in each group to obtain a power of
#> 0.80, when the effect size is moderate (0.25) and a 
#> significance level of 0.05 is employed
pwr.anova.test(k = 5, f = .25, sig.level = .10, power = .8)


# Plot sample size curves for detecting correlations of various sizes

# range of correlations
r <- seq(.1, .5, .01)
nr <- length(r)

# power values
p <- seq(.4, .9, .1)
np <- length(p)

# obtain sample sizes
samsize <- array(numeric(nr*np), dim = c(nr, np))
for (i in 1:np) {
  for (j in 1:nr) {
    result <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(result$n)
  }
}

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type = "n",
     xlab = "Correlation Coefficient (r)",
     ylab = "Sample Size (n)")

# add power curves
for (i in 1:np) {
  lines(r, samsize[,i], type = "l", lwd = 2, col = colors[i])
}

# add annotation (grid lines, title, legend)
abline(v = 0, h = seq(0, yrange[2], 50), lty = 2, col = "grey89")
abline(h = 0, v = seq(xrange[1], xrange[2], .02), lty = 2,
       col = "grey89")
title("Sample Size Estimation for Correlation Studies\n
      Sig = 0.05 (Two-tailed)")
legend("topright", title = "Power", as.character(p),
       fill = colors)
