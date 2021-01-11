#setwd("~/Downloads/MRIO/")
library(readr)
load("decom_IM.Rdata")
decom$region_name = as.character(decom$region_name)
decom$region_name[decom$region_name == "Inner Mongolia"] <- "InnerMong"
load("MRIO_data.RData")
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
save(prov,file = "prov.RData")
f_name = paste(prov,".F",sep = "")
CM3_name = paste(prov,".CM3",sep = "")
P_name = paste(prov,".P",sep = "")
NP_name = paste(prov,".N",sep = "")
load("Decom_N.INT.RData")
load("Decom_P.RData")
MATRIX_name = paste(prov,".MATRIX",sep = "")
z_name = paste(prov,".Z",sep = "")
matrix_fun <- function(int_mat, io_mat.z, io_mat.y)
{
  io_mat.y <- io_mat.y[,-5]
  int_mat.z <- int_mat[1:42,1:42]
  int_mat.y <- int_mat[1:42,43:46]
  a = int_mat.z > io_mat.z
  int_mat.z[a] <- io_mat.z[a]
  b = int_mat.y > io_mat.y
  int_mat.y[b] <- io_mat.y[b]
  int_mat_ = cbind(int_mat.z,int_mat.y)
  TOL_ = apply(int_mat_,1,sum)
  TOL = apply(int_mat,1,sum)
  int_mat_ <- cbind(int_mat_,TOL - TOL_)
  colnames(int_mat_)[47] <- "INV"
  return(int_mat_)
}

for(i in 1:length(prov))
{
  assign("tmp", eval(parse(text = f_name[i])))
  F10123 = tmp[,1:3]
  N.CONS = as.vector(decom$N.CONS[decom$region_name == prov[i]])
  N.CAP = as.vector(decom$N.CAP[decom$region_name == prov[i]])
  sum_F10123 = apply(F10123,1,sum)
  sum_F10123_rev = sum_F10123^(-1)
  index = is.infinite(sum_F10123_rev)
  coeff = diag(sum_F10123_rev) %*% F10123
  average_ratio = apply(as.matrix(F10123),2,sum)/sum(F10123)
  average_ratio = matrix(data = average_ratio, nrow = length(index[index]),ncol = 3,byrow = T)
  coeff[index,] = average_ratio
  decom_CONS = diag(N.CONS) %*% coeff
  assign(CM3_name[i],decom_CONS)
  assign("CM3_tmp",decom_CONS)
  assign("P_tmp", eval(parse(text = P_name[i])))
  assign("NP_tmp", eval(parse(text = NP_name[i])))
  #P_tmp <- read_csv(P_name.csv[i], col_types = cols(X1 = col_skip()))
  #NP_tmp <- read_csv(NP_name.csv[i], col_types = cols(X1 = col_skip()))
  ZM = P_tmp + NP_tmp
  MATRIX = cbind(ZM,CM3_tmp,N.CAP)
  label_s = paste("S",seq(1,42),sep = "")
  collabel = c(label_s,"CMR","CMU","CMG","KM")
  colnames(MATRIX) <- collabel
  rownames(MATRIX) <- label_s
  assign("f_tmp",eval(parse(text = f_name[i])))
  assign("z_tmp",eval(parse(text = z_name[i])))
  MATRIX_final = matrix_fun(MATRIX,z_tmp,f_tmp)
  assign(MATRIX_name[i],MATRIX_final)
  write.csv(MATRIX_final,file = paste(prov[i],"_int_mat.csv",sep = ""))
}
save(list = MATRIX_name, file = "MATRIX.Rdata")
