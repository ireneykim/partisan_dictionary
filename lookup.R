
nyt_comment <- read.csv("raw/lemma_n1000_gun.csv")

nyt_comment %>% .$comment -> gun_comment
gun_comment0 <- gun_comment[1:7000] # make it smaller for testing

gun_comment_label <- as.character(gun_comment0)

classified_gun_rep <- cbind(gun_comment0, "wf_rep" = rowSums(sapply(unlist(dict_R), grepl, x = gun_comment0)))

classified_gun_dem <- cbind(gun_comment0, "wf_dem" = rowSums(sapply(unlist(dict_D), grepl, x = gun_comment0)))

gun_rep <- as.data.frame(classified_gun_rep)
gun_dem <- as.data.frame(classified_gun_dem)

gun_dict_lookup <- cbind(gun_comment_label,gun_rep,gun_dem)
gun_dict_lookup <- gun_dict_lookup[,c("gun_comment_label","wf_rep","wf_dem")]
gun_dict_lookup$wf_rep <- as.numeric(as.character(gun_dict_lookup$wf_rep))
gun_dict_lookup$wf_dem <- as.numeric(as.character(gun_dict_lookup$wf_dem))
