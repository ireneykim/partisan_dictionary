#' separately grouping 2 by 2 contingency table by each bigram (from longformat)
#'
#' @param df A bigram dataframe
#' @return The dataframe with column M added
#' @examples
#' #separate_table(df)
#' @export

separate_table <- function(data=df){
  library(tidyverse)
  df %>%
    group_by(bigram) %>%
    nest() %>%
    mutate(M = map(data, function(dat){
      dat2 <- dat %>% spread(type,count)
      M <- as.matrix(dat2[, -1])
      row.names(M) <- dat2$party
      return(M)
    }))
  }
