suppressWarnings(suppressMessages({ library(dplyr); library(tidyr) }))
source("R/veg_helpers.R")
b <- readRDS("data/sites/HARV.rds"); tr <- b$trees; pl <- b$plots
cat("trees rows", nrow(tr), "| plots", nrow(pl), "| years", paste(b$meta$years, collapse=","), "\n")
snap <- tree_snapshot(tr); one <- one_per_tree(live_only(snap))
cat("snapshot rows", nrow(snap), "| live one-per-tree", nrow(one), "\n")
cat("\nstand_site:\n"); print(stand_site(snap, pl))
cat("\nsize_class:\n"); print(size_class(snap))
cat("\nspecies_structure top 5:\n"); print(head(species_structure(snap, pl), 5))
g <- tree_growth(tr); cat("\ntree_growth: n", nrow(g), "median cm/yr", round(median(g$growth_cm_yr[g$growth_cm_yr<=5], na.rm=TRUE),3), "neg%", round(100*mean(g$growth_cm_yr< -0.1)), "\n")
cat("\nstatus_summary:\n"); print(status_summary(snap))
cat("\nplot_summary_veg top 3:\n"); print(head(plot_summary_veg(snap, pl)[,c("plotID","ba_ha","density_ha","n_species","tallest","biggest")],3))
both <- one[is.finite(one$stemDiameter)&one$stemDiameter>0&is.finite(one$height)&one$height>0,]
donly <- sum(is.finite(one$stemDiameter)&one$stemDiameter>0 & !(is.finite(one$height)&one$height>0))
cat("\nSize Lab dots (both dia+ht):", nrow(both), "| diameter-only:", donly, "\n")
id <- g$individualID[which.max(g$growth_cm_yr[g$growth_cm_yr<=5])]
cat("\nsample tree history (", short_tree(id), "):\n"); print(tree_history(tr, id))
cat("\nqc flags:\n"); print(tree_qc_flags(tree_history(tr, id)))
cat("OK\n")
