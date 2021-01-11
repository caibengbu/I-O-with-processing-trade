library(reshape2)
library(dplyr)
library(readxl)
setwd("~/Downloads/MRIO")
M_D <- read_excel("MRIO_M-D.xlsx",col_names = FALSE)
M_N <- read_excel("MRIO_M-N.xlsx",col_names = FALSE)
M_P <- read_excel("MRIO_M-P.xlsx",col_names = FALSE)
load("/Volumes/Transcend/CSTMR/CSTMR/imp2012_42.RData")
aggregate_to_42 <- function(x)
{
  library(readxl)
  concordance2 <- read_excel("concordance1.xlsx", 
                             sheet = 2, col_names = FALSE)
  ref = concordance2$X__2
  ref = ifelse(nchar(ref) == 1, paste("0",ref,sep = ""),ref)
  x = cbind(ref,x)
  colnames(x)[1] = "ori"
  x = melt(x, id.vars = "ori", variable.name = "des")
  x <- x %>%
    group_by(ori,des) %>%
    summarise(value = sum(value)) %>%
    mutate(des49 = ref) %>%
    group_by(ori,des49) %>%
    summarise(value = sum(value))
  y <- matrix(data = x$value,nrow = 42,ncol = 42,byrow = TRUE)
  return(y)
}
M_D <- aggregate_to_42(M_D)
M_N <- aggregate_to_42(M_N)
M_P <- aggregate_to_42(M_P)
save(list = c("M_D","M_N","M_P"), file = "M-DPN.RData")
