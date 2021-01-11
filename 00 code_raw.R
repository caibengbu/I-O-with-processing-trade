# Loading
library("sas7bdat")
library("dplyr")
library("hash")
library("readxl")
setwd("/Volumes/Transcend/CSTMR/CSTMR")
##year = 2012
##emclass = "EX"
##data <- read.sas7bdat("exp2012.sas7bdat")
##filename <- c("exp2012","imp2012","exp2007","imp2007")
##filenamesas <- paste(filename,".sas7bdat",sep = "")
##filenamer <- paste(filename,".RData",sep = "")

# Ori_Region Hash
region_code2 <- c(11,12,13,14,15,21,22,23,31,32,33,34,35,36,37,41,42,43,44,45,46,50,51,52,53,54,61,62,63,64,65)
region_name <- c("Beijing","Tianjin","Hebei","Shanxi","Inner Mongolia","Liaoning","Jilin","Heilongjiang","Shanghai","Jiangsu","Zhejiang","Anhui","Fujian","Jiangxi","Shandong","Henan","Hubei","Hunan","Guangdong","Guangxi","Hainan","Chongqing","Sichuan","Guizhou","Yunnan","Tibet","Shaanxi","Gansu","Qinghai","Ningxia","Xinjiang")
region_ref <- hash(keys = region_code2, values = region_name)

# Des_Country Hash
load("country_code.RData")
country_ref <- hash(keys = country_code$ctyo, values = country_code$wiot_abbr)

# PT Hash
P_ref <- hash(keys = c(14,15), values = c("PA","PI"))

# BEC Hash
load("OECD_USITC_BEC_July_2017.RData")
OECD_USITC_BEC_July_2017 <- OECD_USITC_BEC_July_2017[!is.na(OECD_USITC_BEC_July_2017$HS6),]
OECD_USITC_BEC_July_2017$i4_2 <- substr(OECD_USITC_BEC_July_2017$i4,1,2)
index = is.na(OECD_USITC_BEC_July_2017$INT)
OECD_USITC_BEC_July_2017$INT = ifelse(index,0.33,OECD_USITC_BEC_July_2017$INT)
OECD_USITC_BEC_July_2017$CONS = ifelse(index,0.33,OECD_USITC_BEC_July_2017$CONS)
OECD_USITC_BEC_July_2017$CAP = ifelse(index,0.34,OECD_USITC_BEC_July_2017$CAP)
### "specialist review" is divided evenly by the three BEC categories ###
load("Complementary_OECD.RData")
load("WIOD_ISIC4.RData")
INT_ref <- hash(keys = OECD_USITC_BEC_July_2017$HS6, values = OECD_USITC_BEC_July_2017$INT)
CONS_ref <- hash(keys = OECD_USITC_BEC_July_2017$HS6, values = OECD_USITC_BEC_July_2017$CONS)
CAP_ref <- hash(keys = OECD_USITC_BEC_July_2017$HS6, values = OECD_USITC_BEC_July_2017$CAP)
.set(INT_ref,keys = Complementary_OECD$HS6, values = Complementary_OECD$INT)
.set(CONS_ref,keys = Complementary_OECD$HS6, values = Complementary_OECD$CONS)
.set(CAP_ref,keys = Complementary_OECD$HS6, values = Complementary_OECD$CAPS)

# Sector Hash
concordance1 <- read_excel("/Volumes/Transcend/CSTMR/CSTMR/concordance1.xlsx", 
                           col_names = FALSE)
concordance2 <- read_excel("/Volumes/Transcend/CSTMR/CSTMR/concordance1.xlsx", 
                           sheet = "工作表2",col_names = FALSE)
index = nchar(concordance1$X__2)>5
concordance1_c <- concordance1[index,]
concordance1_s <- concordance1[!index,]
a <- strsplit(concordance1_c$X__2,"/")
a <- unlist(a)
a <- matrix(a,ncol = 2,byrow = TRUE)
concordance1_c <- cbind(concordance1_c[,1],a)
concordance1_s[,1] <- apply(as.matrix(concordance1_s[,1]),1,substr,start = 1,stop = 6)
concordance1_c[,1] <- apply(as.matrix(concordance1_c[,1]),1,substr,start = 1,stop = 6)
concordance1_s <- unique(concordance1_s)
concordance1_c <- unique(concordance1_c)
sector_hash_s <- hash(keys = concordance1_s$X__1, values = concordance1_s$X__2)
sector_hash_c1 <- hash(keys = concordance1_c$X__1, values = concordance1_c$`1`)
sector_hash_c2 <- hash(keys = concordance1_c$X__1, values = concordance1_c$`2`)
from139to42_hash <- hash(keys = concordance2$X__1, values = concordance2$X__2)
# Look-up Function
lookup_main <- function(ref_key, ref_hash, unexpected_val = NULL)
{
  ref_value = ref_hash[[ref_key]]
  if(length(ref_value)==0)
  {
    ref_value = unexpected_val
  }
  return(ref_value)
}

# Labelling
data$stdhs <- substr(data$HS8,1,6)
data$ctyd <- as.character(data$ctyd)
data$custreg <- as.character(data$custreg)
## Origion
data$region_code<-substr(as.character(data$loctrad),1,2)
data$region_name<-apply(as.matrix(data$region_code),1,lookup_main,ref_hash = region_ref)
## Destination
data$ctyd_name <- apply(as.matrix(data$ctyd),1,lookup_main,
                        ref_hash = country_ref,unexpected_val = "ROW")
## BEC(CPK)
data$INT <- apply(as.matrix(data$stdhs),1,lookup_main,
                  ref_hash = INT_ref,unexpected_val = "other")
data$CONS <- apply(as.matrix(data$stdhs),1,lookup_main,
                   ref_hash = CONS_ref,unexpected_val = "other")
data$CAP <- apply(as.matrix(data$stdhs),1,lookup_main,
                  ref_hash = CAP_ref,unexpected_val = "other")
## PT/OT
data$custreg2 <- apply(as.matrix(data$custreg),1,lookup_main,
                       ref_hash = P_ref, unexpected_val = "OT")
## Sector
data$stdhs_index = ifelse(data$stdhs %in% concordance1_s$X__1, "s",
                       ifelse(data$stdhs %in% concordance1_c$X__1, "c", "x"))
data_s <- data[data$stdhs_index == "s",]
data_c <- data[data$stdhs_index == "c",]
data_x <- data[data$stdhs_index == "x",]
data_s$sec <- apply(as.matrix(data_s$stdhs),1,lookup_main,
                  ref_hash = sector_hash_s,unexpected_val = "other")
data_c$sec1 <- apply(as.matrix(data_c$stdhs),1,lookup_main,
                    ref_hash = sector_hash_c1,unexpected_val = "other")
data_c$sec2 <- apply(as.matrix(data_c$stdhs),1,lookup_main,
                     ref_hash = sector_hash_c2,unexpected_val = "other")
data_x$sec = "unclassified"
data_c1 = data_c[,-27]
data_c1$value = data_c1$value/2
data_c2 = data_c[,-26]
data_c2$value = data_c2$value/2
colnames(data_c1)[dim(data_c1)[2]] = "sec"
colnames(data_c2)[dim(data_c2)[2]] = "sec"
data <- rbind(data_s, data_c1, data_c2, data_x)
data <- data %>%
  mutate(valueINT = value*INT) %>%
  mutate(valueCONS = value*CONS) %>%
  mutate(valueCAP = value*CAP)
dataINT <- data %>%
  group_by(region_name,sec,ctyd_name,custreg2) %>%
  summarize(value = sum(valueINT)) %>%
  mutate(year = year, emclass = emclass, bec = "INT")
dataCONS <- data %>%
  group_by(region_name,sec,ctyd_name,custreg2) %>%
  summarize(value = sum(valueCONS)) %>%
  mutate(year = year, emclass = emclass, bec = "CONS")
dataCAP <- data %>%
  group_by(region_name,sec,ctyd_name,custreg2) %>%
  summarize(value = sum(valueCAP)) %>%
  mutate(year = year, emclass = emclass, bec = "CAP")
data <- rbind(dataINT,dataCONS,dataCAP)
save(data,file = "imp2012_139.RData")
data$sec_ <- apply(as.matrix(data$sec),1,lookup_main,
                   ref_hash = from139to42_hash,unexpected_val = "other")
data <- data %>%
  group_by(region_name,sec_,ctyd_name,custreg2,year,emclass,bec) %>%
  summarize(value = sum(value))

data <- data.frame(data)
data1 <- reshape(data, timevar = "bec",idvar = c("region_name","sec_","ctyd_name","custreg2","year","emclass"),
                direction = "wide")
save(data1,file = "imp2012_49.RData")
write.csv(data1, file = "imp2012_49.csv")






