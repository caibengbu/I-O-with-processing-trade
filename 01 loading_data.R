## Setting total number of regions and sectors ##
setwd("~/Downloads/MRIO/")
library(readxl)
ns = 42
nr = 31
nv = 4
nf = 5
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
provnameZ <- paste(prov,"Z",sep = ".")
provnameV <- paste(prov,"V",sep = ".")
provnameF <- paste(prov,"F",sep = ".")
provnameTIU <- paste(prov,"TIU",sep = ".")
provnameEX <- paste(prov,"EX",sep = ".")
provnamePEX <- paste(prov,"PEX",sep = ".")
provnameIM <- paste(prov,"IM",sep = ".")
provnamePIM <- paste(prov,"PIM",sep = ".")
provnameERR <- paste(prov,"ERR",sep = ".")
provnameX <- paste(prov,"X",sep = ".")
provnameIMmat <- paste(prov,"IMcof.mat",sep = ".")
provnameEXmat <- paste(prov,"EXcof.mat",sep = ".")
## Loading data ##
for(i in 1:length(prov))
{
  df.tmp <- read_excel("MRIOdataChina.xlsx", 
                       sheet = prov[i], col_names = FALSE)
  mat.tmp <- data.matrix(df.tmp)
  assign(provnameZ[i],mat.tmp[2:(ns+1),2:(ns+1)])
  assign(provnameV[i],mat.tmp[(ns+2):(ns+nv+1),2:(ns+1)])
  assign(provnameF[i],mat.tmp[2:(ns+1),(ns+3):(ns+nf+2)])
  assign(provnameTIU[i],mat.tmp[2:(ns+nv+2),ns+2])
  assign(provnameEX[i],mat.tmp[2:(ns+1),ns+nf+3])
  assign(provnamePEX[i],mat.tmp[2:(ns+1),ns+nf+4])
  assign(provnameIM[i],mat.tmp[2:(ns+1),ns+nf+5])
  assign(provnamePIM[i],mat.tmp[2:(ns+1),ns+nf+6])
  assign(provnameERR[i],mat.tmp[2:(ns+1),ns+nf+7])
  assign(provnameX[i],mat.tmp[2:(ns+1),ns+nf+8])
  assign(provnameIMmat[i],mat.tmp[(ns+nv+6):(ns+nv+5+nr),2:(ns+1)])
  assign(provnameEXmat[i],mat.tmp[2:(ns+1),(ns+nf+12):(ns+nf+11+nr)])
  write.csv(mat.tmp[2:(ns+1),ns+nf+7],file = paste(provnameERR[i],"csv",sep = "."))
  write.csv(mat.tmp[2:(ns+1),ns+nf+3],file = paste(provnameEX[i],"csv",sep = "."))
  print(paste("Data loading for",prov[i],"completed!"))
}
save(list = c(provnameZ,
              provnameERR,
              provnameEX,
              provnameEXmat,
              provnameF,
              provnameIM,
              provnameIMmat,
              provnamePEX,
              provnamePIM,
              provnameTIU,
              provnameV,
              provnameX,
              "ns","nr","nv","nf","prov"),
     file = "MRIO_data.RData")
