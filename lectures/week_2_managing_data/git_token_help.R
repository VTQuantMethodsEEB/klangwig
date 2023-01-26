#creating git credentials

#Instructions from this website:
#https://happygitwithr.com/credential-caching.html#credential-caching

#you will need to then follow these directions:
#https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token

#1 Create your personal access token
usethis::create_github_token()
#Look over the scopes; I highly recommend selecting 
#“repo”, “user”, and “workflow”. 
#Click “Generate token”.

#Copy the generated PAT to your clipboard! paste it someplace (notes, a word doc)
#you will never be able to see it again so this is important
#VERY IMPORTANT#
#make sure you save your personal access token!!
#if you navigate away from this, you will never see it again!

#2. Run the following lines
install.packages("gitcreds")
library(gitcreds)
gitcreds_set()
#paste your personal access token
gitcreds_get()
