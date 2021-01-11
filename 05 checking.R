library(readr)
library(dplyr)
#setwd("~/Downloads/MRIO/")
prov = c("Beijing",
         "Tianjin",
         "Hebei",
         "Shanxi",
         "InnerMong",
         "Liaoning",
         "Jilin",
         "Heilongjiang",
         "Shanghai",
         "Jiangsu",
         "Zhejiang",
         "Anhui",
         "Fujian",
         "Jiangxi",
         "Shandong",
         "Henan",
         "Hubei",
         "Hunan",
         "Guangdong",
         "Guangxi",
         "Hainan",
         "Chongqing",
         "Sichuan",
         "Guizhou",
         "Yunnan",
         "Tibet",
         "Shaanxi",
         "Gansu",
         "Qinghai",
         "Ningxia",
         "Xinjiang")
load("decom_IM.RData")
load("MRIO_data.RData")
decom$region_name = as.character(decom$region_name)
decom$region_name[decom$region_name == "Inner Mongolia"] = "InnerMong"
dataname_P <- paste(prov,".P",sep = "")
dataname_NP <- paste(prov,".N",sep = "")
IMname = paste(prov,".IM",sep = "")
load("Decom_N.INT.RData")
load("Decom_P.RData")
for(i in 1:length(prov))
{
  assign("NP",eval(parse(text = dataname_NP[i])))
  assign("P",eval(parse(text = dataname_P[i])))
  sum_NP = apply(as.matrix(NP),1,sum)
  sum_P = apply(as.matrix(P),1,sum)
  index = decom$region_name == prov[i]
  C_M = decom[index,4]
  I_M = decom[index,3]
  sum = sum_NP + sum_P + C_M + I_M
  assign("tmp_IM", eval(parse(text = IMname[i])))
  if(sum(sum-tmp_IM) < 1e-10)
  {
    print(paste("Checking for",prov[i],"completed. Requirement 1 met."))
  }
  if(sum(sum-tmp_IM) >= 1e-10)
  {
    print(paste("Checking for",prov[i],"completed. Requirement 1 NOT met."))
  }
}
load("imp_aggregated_by_country.RData")
data$custreg = ifelse(data$custreg2 == "OT", "N", "P")
data <- data %>%
  group_by(region_name,custreg) %>%
  summarise(value = sum(value.CAP) + sum(value.CONS) + sum(value.INT))
data$region_name[data$region_name == "Inner Mongolia"] = "InnerMong"
data_P <- as.data.frame(data[data$custreg == "P",])
data_N <- as.data.frame(data[data$custreg == "N",])

rate = replicate(length(prov),0)
for(i in 1:length(prov))
{
  assign("N",eval(parse(text = dataname_NP[i])))
  sum_N = sum(N)
  assign("tmp_IM", data_N[data_N$region_name == prov[i],3])
  rate[i] = tmp_IM/sum_N
}
N_rate.table = data.frame(region_name = prov, rate.N = rate)

prov <- prov[-26]
dataname_P <- dataname_P[-26]
rate = replicate(length(prov),0)
for(i in 1:length(prov))
{
  assign("P",eval(parse(text = dataname_P[i])))
  sum_P = sum(P)
  assign("tmp_IM", data_P[data_P$region_name == prov[i],3])
  rate[i] = tmp_IM/sum_P
}
P_rate.table = data.frame(region_name = prov, rate.P = rate)
rate_all = full_join(P_rate.table,N_rate.table, by = "region_name")

library(ggplot2)
library(reshape2)
plot.table <- melt(data = rate_all,id.vars = "region_name")
p <- ggplot(data = plot.table, aes(x = region_name, y=log10(value), fill = variable))
p + geom_bar(stat="identity",position=position_dodge()) + 
  geom_hline(yintercept = log10((1/6.3)*10^8),colour = "grey20") +
  geom_hline(yintercept = log10((1/6.3)*10^9),colour = "red") +
  geom_hline(yintercept = log10((1/6.3)*10^7),colour = "red") +
  theme(axis.text.x  = element_text(angle=90, vjust=0.2, size=9))

















