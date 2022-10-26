# Install 'r4np' from GitHub
devtools::install_github("ddauber/r4np")

# Create the template structure
r4np::create_project_folder()

# clone github repository
usethis::create_from_github("https://github.com/HelenBehn/EPPN-Data-Analysis.git", destdir = "C:/Users/Helen Behn/Documents/R/Data Analysis EPPN")

gh_token_help()

gitcreds::gitcreds_set()

getwd()

"C:/Users/Helen Behn/Documents/R/Data Analysis EPPN" git clone https://github.com/HelenBehn/EPPN-Data-Analysis.git

# clone github repository
usethis::create_from_github("https://github.com/HelenBehn/EPPN-Data-Analysis.git", destdir = "C:/Users/Helen Behn/Documents/R/Data Analysis EPPN")
use_github("https://github.com/HelenBehn/EPPN-Data-Analysis.git")

# install.packages("devtools")
devtools::install_github("r-lib/usethis")
install.packages("usethis")
install.packages("gh")
library(gh)
library(usethis)

gh_token_help("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")

gitcreds::gitcreds_get()

gh::gh(GET https://github.com/HelenBehn/EPPN-Data-Analysis.git, token = "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")

3


gh_token_help()
gitcreds::gitcreds_set()

## set personal access token ## DAS HAT ENDLICH FUNKTIONIERT!!! 30 Tage gültig
credentials::set_github_pat("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")

#new project, github first
usethis::create_from_github(
  "https://github.com/HelenBehn/Data_Analysis_EPPN.git",
  destdir = "C:/Users/Helen Behn/Documents/R/Data Analysis EPPN"
)

here::dr_here("C:/Users/Helen Behn/Documents/R/Data Analysis EPPN")

#new project, github first
usethis::create_from_github(
  "https://github.com/HelenBehn/Data_Analysis_EPPN.git",
  destdir = "C:/Users/Helen Behn/Documents/R"
)


setwd("C:/Users/Helen Behn/Documents/R/Data_Analysis_EPPN")
git_add("C:/Users/Helen Behn/Documents/R/Data_Analysis_EPPN")
