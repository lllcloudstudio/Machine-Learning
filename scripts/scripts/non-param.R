library(DBI)
library(RMySQL)                                   # same driver your Plumber API uses

setwd("C:/Users/aflac/Documents/GitHub/Machine-Learning/scripts/scripts")   # folder with config.yml
db_config <- config::get()                        # reads db_host, db_port, db_user, db_password

conn <- dbConnect(MySQL(),
                  host     = db_config$db_host,
                  port     = db_config$db_port,
                  username = db_config$db_user,
                  password = db_config$db_password,
                  dbname   = "REFERENCE")
df <- dbGetQuery(conn, "SELECT * FROM kolachalama_data")
cl <- dbGetQuery(conn, "SELECT id, cluster FROM kolachalama_kmeans_silhouette WHERE k = 3") ### CHU2 After first df
df3 <- merge(df, cl, by = "id") ### CHU2 After first df 
dbDisconnect(conn)

cols <- c("bMI (Kg/m2)", "SBP", "Hb", "WBC", "Platelet", "BUN", "Creatinine",
          "egfr (MDRD)", "HCO3", "ph", "HbA1c", "LDL", "Tsat", "Ferritin", "DBP", "HDL", "TG")
d <- df[complete.cases(df[, cols]), cols]

pairs <- t(combn(cols, 2))
res <- data.frame(a = pairs[, 1], b = pairs[, 2]) ### columns only 1 2 
tests <- lapply(seq_len(nrow(res)), function(i) ### at row # of df to add to res df
  cor.test(d[[res$a[i]]], d[[res$b[i]]], method = "spearman", exact = FALSE)) ### spearman correlation
res$r     <- sapply(tests, function(t) unname(t$estimate)) ### add to new df
res$p     <- sapply(tests, function(t) t$p.value)
res$p_adj <- p.adjust(res$p, "BH")
res[res$p_adj < 0.05, ][order(-abs(res$r[res$p_adj < 0.05])), ] ### modify to new df 

sig <- res[res$p_adj < 0.05, ]
sig[order(-abs(sig$r)), ]
cor.test(d$SBP, d$DBP, method = "spearman", exact = FALSE)$estimate ### distance effect considered compared `rho`
cor.test(scale(d$SBP), scale(d$DBP), method = "spearman", exact = FALSE)$estimate ### scaled paire DBP SBP 3#

#######

#######

compare_groups <- function(df, group, labs) {
  g_all <- factor(df[[group]])
  rows <- lapply(labs, function(v) tryCatch({
    ok <- !is.na(df[[v]]) & !is.na(g_all)
    x  <- df[[v]][ok]
    g  <- droplevels(g_all[ok])
    k  <- kruskal.test(x ~ g)
    row <- data.frame(lab = v, n = length(x), H = unname(k$statistic), p_kruskal = k$p.value)
    if (nlevels(g) == 2) {                                   # rank-sum test only for 2 groups
      w <- wilcox.test(x ~ g, exact = FALSE) ### exact = FALSE param
      row$W <- unname(w$statistic)
      row$p_wilcox <- w$p.value
    }
    med <- tapply(x, g, median);   names(med) <- paste0("median_",  names(med))
    rs  <- tapply(rank(x), g, sum); names(rs)  <- paste0("ranksum_", names(rs))
    cbind(row, as.data.frame(as.list(med)), as.data.frame(as.list(rs)))
  }, error = function(e) NULL))
  out <- do.call(rbind, Filter(Negate(is.null), rows))
  out$p_kruskal_adj <- p.adjust(out$p_kruskal, "BH")
  if ("p_wilcox" %in% names(out)) out$p_wilcox_adj <- p.adjust(out$p_wilcox, "BH")
  out[order(out$p_kruskal), ]
}

res_sex <- compare_groups(df, "Sex_grp", cols)      # Wilcoxon rank-sum + Kruskal-Wallis
print(res_sex, digits = 3)


res_cluster <- compare_groups(df3, "cluster", cols) ### CHU2 
print(res_cluster, digits = 3) ### CHU2
###################
o <- res_sex[order(res_sex$p_wilcox_adj), ] ### CHU4
barplot(rev(-log10(o$p_wilcox_adj)), horiz = TRUE, names.arg = rev(o$lab), ### CHU3
        las = 1, xlab = "-log10(adjusted p)") ### CHU4
abline(v = -log10(0.05), lty = 2)                 ### CHU4 # bars past this line are significant

top <- head(res_sex$lab, 6) ### CHU4
op <- par(mfrow = c(2, 3)) ### CHU4                       # 2 x 3 grid, one page
for (v in top) boxplot(df[[v]] ~ factor(df$Sex_grp), main = v, xlab = "", ylab = "") ### CHU4
par(op)

