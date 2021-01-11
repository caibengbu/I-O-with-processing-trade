#setwd("~/Downloads/MRIO/")
#load("M-DNP.RData")
load("decom_IM.Rdata")
M_NP = M_D + M_N
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
prov.res_name.P = paste(prov,"P",sep = ".")
prov.res_name.N = paste(prov,"N",sep = ".")
# Decomposition of P
average_polio = apply(M_P,2,sum)
average_polio = average_polio/sum(average_polio)
sum_MP_reverse = apply(M_P,1,sum)^(-1)
coeff1 = diag(sum_MP_reverse) %*% M_P
index = is.nan(coeff1[,1])
a = length(index[index==TRUE])
average_polio = matrix(average_polio,nrow = a, ncol = 42, byrow = TRUE)
coeff1[index,] = average_polio
for(i in 1:31)
{
  decom_tmp <- decom$P[seq(42*(i-1)+1,42*i)]
  result = diag(decom_tmp) %*% coeff1
  assign(prov.res_name.P[i],result)
}
save(list = prov.res_name.P,file = "Decom_P.RData")
for(i in 1:31)
{
  write.csv(eval(parse(text = prov.res_name.P[i])),file = paste(prov[i],"_P",".csv",sep = ""))
}


# Decomposition of INT
average_polio = apply(M_NP,2,sum)
average_polio = average_polio/sum(average_polio)
sum_MNP_reverse = apply(M_NP,1,sum)^(-1)
coeff2 = diag(sum_MNP_reverse) %*% M_NP
index2 = is.nan(coeff2[,1])
b = length(index2[index2==TRUE])
average_polio = matrix(average_polio,nrow = b, ncol = 42, byrow = TRUE)
coeff2[index2,] = average_polio
for(i in 1:31)
{
  decom_tmp <- decom$N.INT[seq(42*(i-1)+1,42*i)]
  result = diag(decom_tmp) %*% coeff2
  assign(prov.res_name.N[i],result)
}
save(list = prov.res_name.N,file = "Decom_N.INT.RData")
for(i in 1:31)
{
  write.csv(eval(parse(text = prov.res_name.N[i])),file = paste(prov[i],"_NP",".csv",sep = ""))
}

