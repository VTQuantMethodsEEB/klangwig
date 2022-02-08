#creating git credentials

#this website:
#https://happygitwithr.com/credential-caching.html#credential-caching

#you will need to then follow these directions:
#https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token

#VERY IMPORTANT#
#make sure you save your personal access token!!
#if you navigate away from this, you will never see it again!

install.packages("gitcreds")
library(gitcreds)
gitcreds_set()

gitcreds_get()
