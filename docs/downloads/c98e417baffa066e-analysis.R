# LAB-03. tips, exported from reshape2 data/tips.rda, as used by textbook chapter 6.
# Offline CSV avoids installing reshape2 for this exercise; analysis uses base/recommended R only.
options(digits = 7)
d <- read.csv("data/tips.csv", stringsAsFactors = FALSE)
stopifnot(nrow(d) == 244, !anyNA(d), all(d$total_bill > 0), all(d$tip >= 0), all(d$size >= 1))
d$day <- factor(d$day, levels = c("Sun", "Thur", "Fri", "Sat"))
stopifnot(!anyNA(d$day))
dir.create("output", showWarnings = FALSE)
simple <- lm(tip ~ total_bill, data = d)
adjusted <- lm(tip ~ total_bill + size + day, data = d)
coefficient_table <- function(fit) {
  coefficients <- coef(summary(fit)); ci <- confint(fit)
  data.frame(term = rownames(coefficients), estimate = coefficients[,1], se = coefficients[,2],
             t = coefficients[,3], p = coefficients[,4], low = ci[,1], high = ci[,2], row.names = NULL)
}
write.csv(coefficient_table(simple), "output/simple-coefficients.csv", row.names = FALSE)
write.csv(coefficient_table(adjusted), "output/adjusted-coefficients.csv", row.names = FALSE)
# HC3 standard errors: illustrates sensitivity to nonconstant residual variance.
X <- model.matrix(adjusted)
bread <- solve(crossprod(X))
w <- residuals(adjusted)^2/(1-hatvalues(adjusted))^2
hc3 <- bread %*% crossprod(X, X * w) %*% bread
write.csv(data.frame(term = names(coef(adjusted)), classical_se = coef(summary(adjusted))[,2],
                     hc3_se = sqrt(diag(hc3))), "output/se-comparison.csv", row.names = FALSE)
write.csv(data.frame(model = c("simple", "adjusted"), r_squared = c(summary(simple)$r.squared, summary(adjusted)$r.squared),
                     residual_sd = c(summary(simple)$sigma, summary(adjusted)$sigma)),
          "output/model-metrics.csv", row.names = FALSE)
png("output/bill-and-tip.png", width = 1200, height = 750, res = 140)
plot(d$total_bill, d$tip, pch = 16, col = "#24654e88", xlab = "Total bill (USD)", ylab = "Tip (USD)",
     main = "A positive association is not a causal effect")
abline(simple, col = "#a04c27", lwd = 2)
legend("topleft", "Simple linear fit", col = "#a04c27", lty = 1, lwd = 2, bty = "n")
dev.off()
png("output/diagnostics.png", width = 1400, height = 1000, res = 130)
par(mfrow = c(2, 2)); plot(adjusted); dev.off()
capture.output({cat("LAB-03: 244 restaurant bills from reshape2::tips\n"); print(summary(simple)); print(summary(adjusted));
  cat("\nClassical and HC3 standard errors:\n"); print(read.csv("output/se-comparison.csv"));
  cat("\nMaximum Cook distance:", max(cooks.distance(adjusted)), "\n");
  cat("Within-sample associations; no random assignment, no causal or universal restaurant claim.\n")},
  file = "output/results.txt")
capture.output(sessionInfo(), file = "output/session-info.txt")
cat(readLines("output/results.txt"), sep = "\n")
