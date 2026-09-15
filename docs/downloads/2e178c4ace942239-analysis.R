# LAB-01. Original textbook's fictional teaching data; not observed store results.
# Run in this package directory: Rscript --vanilla analysis.R
options(digits = 7)
d <- read.csv("data/ch3_data1.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
stopifnot(identical(names(d), c("region", "score")), nrow(d) == 500,
          !anyNA(d), all(d$score >= 1 & d$score <= 5))
d$region <- factor(d$region, levels = c("Seoul", "Gyeonggi", "Busan", "Daejeon"))
stopifnot(!anyNA(d$region))
dir.create("output", showWarnings = FALSE)
summary_table <- do.call(rbind, lapply(levels(d$region), function(region) {
  x <- d$score[d$region == region]
  data.frame(region = region, n = length(x), mean = mean(x), median = median(x),
             sd = sd(x), q1 = unname(quantile(x, .25)), q3 = unname(quantile(x, .75)),
             min = min(x), max = max(x), share_at_5 = mean(x == 5))
}))
write.csv(summary_table, "output/summary.csv", row.names = FALSE)
png("output/distributions.png", width = 1400, height = 650, res = 140)
par(mfrow = c(1, 2), mar = c(5, 4, 3, 1))
boxplot(score ~ region, d, ylim = c(1, 5), col = c("#8bb89c", "#e1ca83", "#98b7c6", "#c8a8b3"),
        xlab = "Region", ylab = "Satisfaction score", main = "Distribution, not just a mean")
set.seed(20260915)
stripchart(score ~ region, d, vertical = TRUE, method = "jitter", add = TRUE,
           pch = 16, cex = .3, col = "#243b3250")
plot(ecdf(d$score[d$region == levels(d$region)[1]]), xlim = c(1, 5),
     main = "Empirical cumulative distribution", xlab = "Score", ylab = "Share at or below score",
     col = "#24654e", verticals = TRUE, do.points = FALSE)
colors <- c("#24654e", "#946300", "#2c6985", "#954574")
for (i in 2:4) lines(ecdf(d$score[d$region == levels(d$region)[i]]), col = colors[i], do.points = FALSE)
legend("topleft", levels(d$region), col = colors, lty = 1, bty = "n", cex = .8)
dev.off()
capture.output({cat("LAB-01: textbook fictional teaching data\n"); print(summary_table);
  cat("\nMissing cells:", sum(is.na(d)), "\nExact duplicate rows:", sum(duplicated(d)),
      "\nDuplicate scores are not duplicate people: there is no respondent identifier.\n")},
  file = "output/results.txt")
capture.output(sessionInfo(), file = "output/session-info.txt")
cat(readLines("output/results.txt"), sep = "\n")
