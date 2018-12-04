library(dplyr)
library(tidyverse)
library(data.table)
library(tm)
library(tidytext)


DY2010 <- fread("raw/clean_DY2011.csv", sep=",", header=TRUE, fill=FALSE, blank.lines.skip=TRUE)
RY2010 <- fread("raw/clean_RY2011.csv", sep=",", header=TRUE, fill=FALSE, blank.lines.skip=TRUE)

DY2010$clean_cont <- gsub("[^A-Za-z ]", " ", DY2010$content)
RY2010$clean_cont <- gsub("[^A-Za-z ]", " ", RY2010$content)

#custom stopwords from https://github.com/6/stopwords-json
custom_stop_words1 <- tibble(word = c("a","a's","able","about","above","according","accordingly","across","actually","after","afterwards","again","against","ain't","all","allow","allows","almost","alone","along","already","also","although","always","am","among","amongst","an","and","another","any","anybody","anyhow","anyone","anything","anyway","anyways","anywhere","apart","appear","appreciate","appropriate","are","aren't","around","as","aside","ask","asking","associated","at","available","away","awfully","b","be","became","because","become","becomes","becoming","been","before","beforehand","behind","being","believe","below","beside","besides","best","better","between","beyond","both","brief","but","by","c","c'mon","c's","came","can","can't","cannot","cant","cause","causes","certain","certainly","changes","clearly","co","com","come","comes","concerning","consequently","consider","considering","contain","containing","contains","corresponding","could","couldn't","course","currently","d","definitely","described","despite","did","didn't","different","do","does","doesn't","doesnt","doing","dont","don't","done","down","downwards","during","e","each","edu","eg","eight","either","else","elsewhere","enough","entirely","especially","et","etc","even","ever","every","everybody","everyone","everything","everywhere","ex","exactly","example","except","f","far","few","fifth","first","five","followed","following","follows","for","former","formerly","forth","four","from","further","furthermore","g","get","gets","getting","given","gives","go","goes","going","gone","got","gotten","greetings","h","had","hadn't","happens","hardly","has","hasn't","have","haven't","having","he","he's","hello","help","hence","her","here","here's","hereafter","hereby","herein","hereupon","hers","herself","hi","him","himself","his","hither","hopefully","how","howbeit","however","i","i'd","i'll","i'm","i've","ie","if","ignored","immediate","in","inasmuch","inc","indeed","indicate","indicated","indicates","inner","insofar","instead","into","inward","is","isn't","it","it'd","it'll","it's","its","itself","j","just","k","keep","keeps","kept","know","known","knows","l","last","lately","later","latter","latterly","least","less","lest","let","let's","like","liked","likely","little","look","looking","looks","ltd","m","mainly","many","may","maybe","me","mean","meanwhile","merely","might","more","moreover","most","mostly","much","must","my","myself","n","name","namely","nd","near","nearly","necessary","need","needs","neither","never","nevertheless","new","next","nine","no","nobody","non","none","noone","nor","normally","not","nothing","novel","now","nowhere","o","obviously","of","off","often","oh","ok","okay","old","on","once","one","ones","only","onto","or","other","others","otherwise","ought","our","ours","ourselves","out","outside","over","overall","own","p","particular","particularly","per","perhaps","placed","please","plus","possible","presumably","probably","provides","q","que","quite","qv","r","rather","rd","re","really","reasonably","regarding","regardless","regards","relatively","respectively","right","s","said","same","saw","say","saying","says","second","secondly","see","seeing","seem","seemed","seeming","seems","seen","self","selves","sensible","sent","serious","seriously","seven","several","shall","she","should","shouldn't","since","six","so","some","somebody","somehow","someone","something","sometime","sometimes","somewhat","somewhere","soon","sorry","specified","specify","specifying","still","sub","such","sup","sure","t","t's","take","taken","tell","tends","th","than","thank","thanks","thanx","that","that's","thats","the","their","theirs","them","themselves","then","thence","there","there's","thereafter","thereby","therefore","therein","theres","thereupon","these","they","they'd","they'll","they're","they've","think","third","this","thorough","thoroughly","those","though","three","through","throughout","thru","thus","to","together","too","took","toward","towards","tried","tries","truly","try","trying","twice","two","u","un","under", "unfortunately"))

custom_stop_words2 <- tibble(word = c("unless","unlikely","until","unto","up","upon","us","use","used","useful","uses","using","usually","uucp","v","value","various","very","via","viz","vs","w","want","wants","was","wasn't","way","we","we'd","we'll","we're","we've","welcome","well","went","were","weren't","what","what's","whatever","when","whence","whenever","where","where's","whereafter","whereas","whereby","wherein","whereupon","wherever","whether","which","while","whither","who","who's","whoever","whole","whom","whose","why","will","willing","wish","with","within","without","won't","wonder","would","wouldn't","x","y","yes","yet","you","you'd","you'll","you're","you've","your","yours","yourself","yourselves","z","zero"))

custom_stop_words <- rbind(custom_stop_words1,custom_stop_words2)
rm(custom_stop_words1,custom_stop_words2)

data(stop_words) #from tm

DY2010_clean <- DY2010 %>% 
  select(c("clean_cont")) %>%
  mutate(lower = tolower(clean_cont)) %>%
  mutate(clean = removeWords(lower, stop_words$word)) %>%
  mutate(final = removeWords(clean, custom_stop_words$word)) %>%
  select(c("final")) %>%
  filter(str_count(final,'\\w+') > 0)


RY2010_clean <- RY2010 %>% 
  select(c("clean_cont")) %>%
  mutate(lower = tolower(clean_cont)) %>%
  mutate(clean = removeWords(lower, stop_words$word)) %>%
  mutate(final = removeWords(clean, custom_stop_words$word)) %>%
  select(c("final")) %>%
  filter(str_count(final,'\\w+') > 0)


R_bigrams_count <- RY2010_clean %>%
  unnest_tokens(bigram, final, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  rename(rep_n = n)

D_bigrams_count <- DY2010_clean %>%
  unnest_tokens(bigram, final, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  rename(dem_n = n)

total_bigrams <- merge(R_bigrams_count,D_bigrams_count, by="bigram", all = TRUE)
total_bigrams[is.na(total_bigrams)] <- 0

# remove too small number of bigrams for fast processing
total_bigrams <- total_bigrams %>% filter(rep_n > 20 | dem_n > 20)

total_bigrams <- total_bigrams %>% 
  mutate(rep_not = sum(rep_n)-rep_n) %>%
  mutate(dem_not = sum(dem_n)-dem_n)

### make long format
chisq_long_r <- total_bigrams %>% select(bigram, rep_n) %>% gather('rep_n', key = "party", value = "count") 
chisq_long_r$party <- c("REP")
chisq_long_r$type <- c("frequency")

chisq_long_d <- total_bigrams %>% select(bigram,dem_n) %>% gather('dem_n', key = "party", value = "count") 
chisq_long_d$party <- c("DEM")
chisq_long_d$type <- c("frequency")

chisq_long_r_not <- total_bigrams %>% select(bigram,rep_not) %>% gather('rep_not', key = "party", value = "count") 
chisq_long_r_not$party <- c("REP")
chisq_long_r_not$type <- c("rest")

chisq_long_d_not <- total_bigrams %>% select(bigram,dem_not) %>% gather('dem_not', key = "party", value = "count") 
chisq_long_d_not$party <- c("DEM")
chisq_long_d_not$type <- c("rest")

chisq_long <- rbind(chisq_long_r,chisq_long_d,chisq_long_r_not,chisq_long_d_not)

## grouping for chisq test
total_bigrams_chisq <- chisq_long %>%
  group_by(bigram) %>%
  nest() %>%
  mutate(M = map(data, function(dat){
    dat2 <- dat %>% spread(type,count)
    M <- as.matrix(dat2[, -1])
    row.names(M) <- dat2$party
    return(M)
  }))

## test chisq statistic
bigrams_chisq <- total_bigrams_chisq %>%
  mutate(ps_pvalue = map_dbl(M, ~chisq.test(.x)$p.value)) %>%
  select(-data, -M) %>%
  ungroup()


## assign bigrams to their own party
total_bigrams <- merge(total_bigrams, bigrams_chisq, by="bigram")
total_bigrams$compare <- ((total_bigrams$rep_n)/(total_bigrams$rep_n+total_bigrams$rep_not)) - ((total_bigrams$dem_n)/(total_bigrams$dem_n+total_bigrams$dem_not))
total_bigrams$which <- ifelse(total_bigrams$compare > 0, "R", ifelse(total_bigrams$compare == 0, NA,"D"))

## extract bigrams p value less than .05
total_bigrams$sig <- ifelse(total_bigrams$ps_pvalue < 0.05, 1, 0)
table(total_bigrams$sig,total_bigrams$which)

dict_R <- total_bigrams %>% filter(sig == 1 & which =="R") %>% select(bigram)
dict_D <- total_bigrams %>% filter(sig == 1 & which =="D") %>% select(bigram)


