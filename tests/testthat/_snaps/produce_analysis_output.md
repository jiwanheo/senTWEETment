# produce_analysis_output works

    Code
      outputs$sentiment_by_tweet
    Output
      # A tibble: 1 x 2
        ID                                                                   Sentiment
        <chr>                                                                    <dbl>
      1 "<a href=\"https://twitter.com/jiwanheo/status/1481810657817374720\~        -3

---

    Code
      outputs$all_words_scored
    Output
                                                                                                                                                                                Picture
      1 <a href="https://twitter.com/jiwanheo" target=_blank><img class="profile-table-img" src=https://pbs.twimg.com/profile_images/1387996217959985156/1N4AcdFH_normal.jpg></img></a>
                                                                                               ID
      1 <a href="https://twitter.com/jiwanheo/status/1481810657817374720">1481810657817374720</a>
        Word Sentiment
      1  bad        -3

---

    Code
      outputs$all_tweets_scored
    Output
                                                                                                                                                                                Picture
      1 <a href="https://twitter.com/jiwanheo" target=_blank><img class="profile-table-img" src=https://pbs.twimg.com/profile_images/1387996217959985156/1N4AcdFH_normal.jpg></img></a>
             User                    Date
      1 @jiwanheo 2022-01-14 02:09:26 UTC
                                                                                               ID
      1 <a href="https://twitter.com/jiwanheo/status/1481810657817374720">1481810657817374720</a>
                                                                                                 Text
      1 Testing not that bad sentiment analysis. @dontshowup https://thisshouldntshow #thisshouldshow
        Sentiment
      1        -3

---

    Code
      outputs$overall_scores
    Output
      $sentiment_n
      [1] 1
      
      $sentiment_sum
      [1] -3
      
      $sentiment_avg
      [1] -3
      

