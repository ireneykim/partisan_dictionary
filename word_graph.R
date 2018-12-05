#' make a word graph to compare two corpuses  
#'
#' @param df A bigram dataframe
#' @param word A work of interest
#' @param n number of words to present
#' @return A combined two geom_bar() with word pharases containing the word of interest under two conditions a) significantly different (Pearson's Chi-square statistic p < .05) and b) the top n words of lowest p-value
#' 
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "filter"
#' @examples
#' word_graph(df, "tax", 30)
#' @export

word_graph <- function(df, word=NULL, n){
  library(ggplot2)
  library(tidyverse)
  df_word <- df[grepl(word, df[,1]),]
  df_word_long <- df_word %>% gather(key = "party", value = "appearance_ratio", R_ratio:D_ratio) 
  # significantly different & top 50 p-value words only
  df_word_long <- df_word_long %>% dplyr::filter(sig==1) %>% dplyr::arrange(desc(ps_pvalue)) %>% dplyr::top_n(n*2)
  out <- ggplot2::ggplot(df_word_long,
                         aes(x=reorder(bigram,appearance_ratio),
                             fill=party,
                             y = ifelse(test = party == "R_ratio",
                                        yes = appearance_ratio, 
                                        no = -appearance_ratio))) +
    ggplot2::geom_bar(stat="identity") +
    scale_fill_manual(values=c("blue", "red")) + 
    xlab("Bigrams") +
    ylab("Ratio of Appearance") +
    theme_minimal()+
    coord_flip()
  return(out)
}

# questions -- if there's no matching word, how to send an error message -- there's no matching word? 