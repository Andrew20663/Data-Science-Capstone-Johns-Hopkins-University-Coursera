Johns Hopkins University Data Science Specialization Capstone
========================================================
author: Andrew Delos Santos
date: 01/08/2020
autosize: true

Introduction
========================================================

For this particular application, the goal was to create a predictive text app 
from my own predictive text algorithm. The project touches upon natural language
processing and uses both N-gram models and Back-Off models. Also, from the given
dataset from SwiftKey and Johns Hopkins University, the real dataset was provided 
by a web crawler and the final model goes up to a 4-gram prediction model. To 
learn more about NLP and the models used please see these:

- <https://en.wikipedia.org/wiki/N-gram>
- <https://en.wikipedia.org/wiki/Katz%27s_back-off_model>

Exploratory Data Analysis
========================================================
In order to analyze the top 1-gram words, I first started with the Twitter file. 
I cleaned the data, split the strings and created a data frame table to see both 
the word and the frequency in which it occurred. My hypothesis was that the word
"the" would be the top word and as shown below it was. For more reference on the 
code, please see my milestone report on provided here: 
<https://rpubs.com/Andrew20663/563541> 


```r
cleaned_twitter <- readRDS("cleaned_twitter.RDS")
one_gram <- unlist(strsplit(cleaned_twitter, "\\s+"))
one_gram.frequency <- table(one_gram)
one_gram_df <- cbind.data.frame(names(one_gram.frequency), as.integer(one_gram.frequency))
names(one_gram_df) <- c("word", "frequency")
row.names(one_gram_df) <- one_gram_df[,1]
one_gram_df <- one_gram_df[order(-one_gram_df[["frequency"]]), ]
head(one_gram_df, 1)
```

```
    word frequency
the  the    929181
```

Predictive Text Algorithm 
========================================================
For the overall predictive text algorithm, I used a 1-gram, 2-gram, 3-gram, and 
4-gram model to predict the text. I also sampled .05% of the size of the Twitter, 
News, and Blogs text files. I know this size may be questionable or not enough,
but due to the limitations of RAM on my computer and the size of the file, this 
was the choice I had due to time constraints. I created a combined corpora of all 
three sampled text files which I then used to test. I also created a more efficient 
way of looking up words by creating a dictionary to look up the index of the word 
in the corpus and then converted it back to what the real word was based on the 
1-gram dictionary. The link to my app is here: 

<https://andrew20663.shinyapps.io/final_app/?fbclid=IwAR2U12fQfqL_YImsPjgViBtiQGWXpTI7oGRNGJCEMweGuMpTZwVsgStDLEs>

Conclusion
=======================================================
Thank you for viewing my presentation! The milestone report, shiny application 
and my Github repo can all be found below. The source code for this project is in 
the Github link and in the milestone report for the exploratory analysis part. 
Even though the app is finished, I still hope to improve on it and will make 
improvements to it. Thank you again.

- <https://github.com/Andrew20663/Data-Science-Capstone-Johns-Hopkins-University-Coursera>
- <https://rpubs.com/Andrew20663/560786>
- <https://andrew20663.shinyapps.io/final_app/?fbclid=IwAR2U12fQfqL_YImsPjgViBtiQGWXpTI7oGRNGJCEMweGuMpTZwVsgStDLEs>
