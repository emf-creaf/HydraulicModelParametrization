
trait_table<- readr::read_delim("Data/TestTableForCorrespondaceMaxtrix2.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
trait_pa <- !is.na(trait_table)

traits <- names(trait_table)[-1]

comb_vec <- character(0)
nspp_vec <- integer(0)
for(m in 1:length(traits)) {
  cm <- combn(traits, m)
  for(k in 1:ncol(cm)) {
    trait_comb <- cm[,k, drop = FALSE]
    comb_vec <- c(comb_vec, paste0(trait_comb, collapse = "/"))
    nspp_vec <- c(nspp_vec, sum(apply(trait_pa[,trait_comb, drop = FALSE], 1, all)))
  }
}
df <- data.frame(trait_comb = comb_vec, nspp = nspp_vec)

# check
sum(!is.na(trait_table$TLP) & !is.na(trait_table$P50_VC))
df[6,]
hist(trait_table$TLP)
