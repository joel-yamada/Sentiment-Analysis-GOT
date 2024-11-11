# Sentiment Analysis GOT
Sentiment Analysis and Word Trend on Game of Thrones (Seasons 1-8) using Shiny app in R

To run in R:
runGitHub(repo = "Sentiment-Analysis-GOT", username = "joel-yamada")

Web App Link:
https://joel-yamada.shinyapps.io/sentiment-analysis-got-main/

### Description
I used Shiny to create two different analyses on seasons 1-8 of Game of Thrones. 

The first is a sentiment analysis using the afinn lexicons, which segment words into "negative" or "positive" with a weighted sentiment score. This score is totaled based on section size, and output in a bar graph by season.

The second is a word trend analysis in the second tab. You can input a word and choose the size of the set to see an overlayed density plot on a histogram. A possivle use case for this is to see the prominence and reoccurence of side characters throughout the series, or the amount of deaths throughout the show each season.
