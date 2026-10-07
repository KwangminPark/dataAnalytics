# LAB-02. Textbook teaching data. Repeated store-day observations are not independent stores.
# Run in this package directory: Rscript --vanilla analysis.R
options(digits = 7)
raw <- read.csv("data/ch4_data.csv", fileEncoding = "UTF-8", check.names = FALSE)
expected <- c("날짜", "브랜드", "일매출", "방문객수", "구매고객수", "평균체류시간_분")
stopifnot(identical(names(raw), expected), nrow(raw) == 90, !anyNA(raw))
names(raw) <- c("date", "brand", "sales", "visitors", "buyers", "minutes")
raw$date <- as.Date(raw$date)
brand_names <- c("스타벅스", "컴포즈커피", "메가커피")
raw$brand <- factor(raw$brand, levels = brand_names, labels = c("Starbucks", "Compose", "Mega"))
stopifnot(!anyNA(raw), !anyDuplicated(raw[c("date", "brand")]),
          all(raw$sales > 0), all(raw$buyers <= raw$visitors), all(table(raw$brand) == 30))
dir.create("output", showWarnings = FALSE)
summary_table <- do.call(rbind, lapply(levels(raw$brand), function(b) {
  x <- raw$sales[raw$brand == b]
  data.frame(brand = b, n_days = length(x), mean_won = mean(x), median_won = median(x), sd_won = sd(x))
}))
# This one-way model is a teaching comparison conditional on independence assumptions.
fit <- aov(sales ~ brand, data = raw)
tukey <- TukeyHSD(fit, "brand")$brand
welch <- oneway.test(sales ~ brand, data = raw, var.equal = FALSE)
# Same-date blocks address shared dates, but do not remove within-store serial correlation.
blocked <- lm(sales ~ factor(date) + brand, data = raw)
null_blocked <- lm(sales ~ factor(date), data = raw)
block_comparison <- anova(null_blocked, blocked)
serial <- sapply(split(raw, raw$brand), function(group) {
  group <- group[order(group$date), ]
  cor(head(group$sales, -1), tail(group$sales, -1))
})
write.csv(summary_table, "output/summary.csv", row.names = FALSE)
write.csv(data.frame(comparison = rownames(tukey), tukey, row.names = NULL), "output/tukey.csv", row.names = FALSE)
write.csv(data.frame(test = c("one_way_F", "one_way_p", "welch_F", "welch_p", "date_block_F", "date_block_p"),
                     value = c(summary(fit)[[1]][1,"F value"], summary(fit)[[1]][1,"Pr(>F)"],
                               unname(welch$statistic), welch$p.value, block_comparison[2,"F"], block_comparison[2,"Pr(>F)"])),
          "output/model-metrics.csv", row.names = FALSE)
png("output/sales-comparison.png", width = 1400, height = 680, res = 140)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))
boxplot(sales/1000000 ~ brand, raw, col = c("#8bb89c", "#e1ca83", "#98b7c6"),
        xlab = "Textbook brand label", ylab = "Daily sales (million KRW)", main = "30 days per label, not 30 stores")
wide <- reshape(raw[c("date", "brand", "sales")], idvar = "date", timevar = "brand", direction = "wide")
wide <- wide[order(wide$date), ]
matplot(wide$date, as.matrix(wide[-1])/1000000, type = "l", lty = 1,
        col = c("#24654e", "#946300", "#2c6985"), xlab = "Date", ylab = "Sales (million KRW)", main = "Repeated daily measurements")
legend("topright", levels(raw$brand), col = c("#24654e", "#946300", "#2c6985"), lty = 1, bty = "n", cex = .75)
dev.off()
png("output/diagnostics.png", width = 1200, height = 600, res = 130)
par(mfrow = c(1, 2)); plot(fit, which = 1); plot(fit, which = 2); dev.off()
capture.output({cat("LAB-02: textbook teaching data, provenance does not establish real brand performance\n");
  print(summary_table); print(summary(fit)); print(tukey); print(welch); print(block_comparison);
  cat("\nFligner variance check (does not verify independence):\n"); print(fligner.test(sales ~ brand, raw));
  cat("\nWithin-label lag-1 sample correlations:\n"); print(serial);
  cat("\nNo store identifier or sampling design: do not generalize to brand populations.\n")}, file = "output/results.txt")
capture.output(sessionInfo(), file = "output/session-info.txt")
cat(readLines("output/results.txt"), sep = "\n")
