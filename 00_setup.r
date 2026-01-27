# # Only need to run these once (uncomment if needed)
# install.packages("foreign")  
# install.packages("devtools")
# install.packages("tidyverse")
# install.packages("readr")
# install.packages("readxl")
# install.packages("haven")
# install.packages("survey")

# # Run these every time you restart R:
library(foreign)
library(devtools)
library(tidyverse)
library(readr)
library(readxl)
library(haven)
library(survey)

install_github("sshrestha274/meps_r_pkg/MEPS")

library(MEPS)