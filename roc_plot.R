library(pROC)
setwd("Documents/Imperial/Papers/carriage_age/analysis")

nl_self_predict <- read.delim("nl_self_predict.txt")
nl.nonmissing <- read.delim("nl.nonmissing.pheno")
nl_merge <- merge(nl.nonmissing, nl_self_predict, by.x = "Lane", by.y = "Sample")

maela_self_predict <- read.delim("maela_self_predict.txt")
maela_pheno <- read.delim("maela.pheno")
maela_merge <- merge(maela_pheno, maela_self_predict, by.x = "samples", by.y = "Sample")

roc(nl_merge, "Age_month_dichot", "Link", plot = TRUE, col = '#56B4E9', 
    smooth = TRUE, 
    print.auc = TRUE, print.auc.x = 0.45, print.auc.y = 0.03, print.auc.cex = 0.8,
    print.auc.pattern = "Dutch cohort (AUC: %0.2f)")
roc(maela_merge, "mother", "Link", plot = TRUE, add = TRUE, col = "#E69F00", 
    smooth = TRUE, 
    print.auc = TRUE, print.auc.x = 0.45, print.auc.y = 0.1, print.auc.cex = 0.8,
    print.auc.pattern = "Maela cohort (AUC: %0.2f)")
