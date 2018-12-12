#' make a word graph to compare two corpuses  
#'
#' @param df A bigram dataframe
#' @param word A work of interest
#' @param n number of words to present
#' @param pvalue significance level e.g., 0.05
#' @return A combined two geom_bar() with word pharases containing the word of interest under two conditions a) that passed the assigned significance level (Pearson's Chi-square statistic) and b) that present the top n words of lowest p-value
#' 
#' @importFrom magrittr "%>%"
#' @importFrom dplyr "filter"
#' @examples
#' word_graph(df 0.05)
#' @export

word_graph <- function(df, word=NULL, n, pvalue){
  library(ggplot2)
  library(tidyverse)
  df_word <- df[grepl(word, df[,1]),]
  df_word$sig_level <- ifelse(df_word$ps_pvalue < pvalue, 1, 0)
  df_word_long <- df_word %>% gather(key = "party", value = "appearance_ratio", R_ratio:D_ratio) 
  # significantly different & top 50 p-value words only
  df_word_long <- df_word_long %>% dplyr::filter(sig_level==1) %>% dplyr::arrange(desc(ps_pvalue)) %>% dplyr::top_n(n*2)
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
