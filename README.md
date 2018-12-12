# partisan_dictionary

# ngrams.R 
R script to produce n-grams 

1) pre-processing the text data (removing punctuations, special characters, stop words) and lowering the capital words.
2) producing n-grams (e.g., bigrams)
3) removing words that have the small number counts for fast processing
4) making the dataset as a long format
5) reorganizing the dataset for Pearson's chi-square test (fun: separate_table) and conducting the test to see which party uses each phrase more frequently compared to the rest word counts
6) assigning bigrams to their own party
7) extracting bigrams p-value less than .05
8) calculating the appearance ratio relative to the total frequency of bigrams (for visualization)

# separate_table.R
A function to produce a dataset for conducting Pearson's chi-square test with a long format

# lookup.R
R script to look up the partisan dictionary and count the number of word appearance in a new dataset (that should be pre-processed with the same way the dictionary made. Here, the datasets for the dictionary and the comment are applied by WordNet Lemmatizer with Penn Treebank tags for POS tagging in Python)

# word_graph.R
A function to visualize a word graph that compares two corpses 

e.g.,

total_bigram <- read.csv("raw/total_bigram.csv")

word_graph(total_bigrams, "tax", 10, 0.05)