# setwd("~/Downloads/MRIO/")
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
self.name = paste(prov,".self",sep = "")
f_name = paste(prov,".F",sep = "")
z_name = paste(prov,".Z",sep = "")
v_name = paste(prov,".V",sep = "")
int_mat.csv = paste(prov,"_int_mat.csv",sep = "")
NCT.name = paste(prov,".NCT",sep = "")
PIM.name = paste(prov,".PIM",sep = "")
int.name = paste(prov,".MATRIX",sep = "")
IMcof.name = paste(prov,".IMcof.mat",sep = "")
label <- seq(1,42)
label = ifelse(nchar(label) == 1, paste("0",label,sep = ""), label)
submatrix.name = outer(prov,prov,paste,sep = ".") # First prov as origin, second as destination
label <- paste("S",seq(1,42),sep = "")
col_name <- c(label,"CDR","CDU","CDG","KD","INV")
load("MATRIX.Rdata")
non_competitive_table <- function(int_mat,f,z)
{
  int_mat.z = int_mat[1:42,1:42]
  int_mat.f = int_mat[1:42,43:47]
  int_mat_.z = z - int_mat.z
  int_mat_.f = f - int_mat.f
  int_mat_ = cbind(int_mat_.z,int_mat_.f)
  return(int_mat_)
}
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
  int_mat_$INV = TOL - TOL_
  int_mat_$TOL = TOL
  return(int_mat_)
}
for(i in 1:length(prov))
{
  assign("z_tmp",eval(parse(text = z_name[i])))
  assign("f_tmp",eval(parse(text = f_name[i])))
  assign("int_mat",eval(parse(text = int.name[i])))
  NCT <- non_competitive_table(int_mat,f_tmp,z_tmp)
  rownames(NCT) <- label
  colnames(NCT) <- col_name
  tol <- apply(NCT,1,sum)
  assign("PIM_tmp",eval(parse(text = PIM.name[i])))
  index = tol < PIM_tmp
  diff = PIM_tmp - tol
  NCT[index,47] <- NCT[index,47] + diff[index]
  diff[index] <- 0
  self = -diff
  assign(NCT.name[i],NCT)
  assign(self.name[i],self)
}
save(list = NCT.name, file = "NCT.RData")
for(i in 1:length(prov))
{
  assign("NCT_tmp",eval(parse(text = NCT.name[i])))
  TOTAL <- apply(NCT_tmp,1,sum)
  TOTAL_rev <- TOTAL^(-1)
  coeff = diag(TOTAL_rev) %*% as.matrix(NCT_tmp)
  assign("IMcof_tmp",eval(parse(text = IMcof.name[i])))
  assign("self",eval(parse(text = self.name[i])))
  IMcof_tmp[i,] <- as.vector(self)
  IMcof_tmp <- t(IMcof_tmp)
  for(j in 1:length(prov))
  {
    tmp <- diag(IMcof_tmp[,j]) %*% coeff
    assign(submatrix.name[j,i], tmp)
  }
}
library(abind)
mat_3d <- abind(Beijing.Beijing,Beijing.Tianjin,along = 0) # First prov as origin, second as destination
for(j in 3:length(prov))
{
  assign("attachmat",eval(parse(text = submatrix.name[1,j])))
  mat_3d <- abind(mat_3d,attachmat,along = 1)
}
mat_3d.2 <- abind(Tianjin.Beijing,Tianjin.Tianjin,along = 0)
for(j in 3:length(prov))
{
  assign("attachmat",eval(parse(text = submatrix.name[2,j])))
  mat_3d.2 <- abind(mat_3d.2,attachmat,along = 1)
}
mat_4d <- abind(mat_3d,mat_3d.2,along = 0)
for(i in 3:length(prov))
{
  assign("prov.01",eval(parse(text = submatrix.name[i,1])))
  assign("prov.02",eval(parse(text = submatrix.name[i,2])))
  mat_3d.tmp <- abind(prov.01,prov.02,along = 0)
  for(j in 3:length(prov))
  {
    assign("attachmat",eval(parse(text = submatrix.name[i,j])))
    mat_3d.tmp <- abind(mat_3d.tmp,attachmat,along = 1)
  }
  mat_4d <- abind(mat_4d,mat_3d.tmp,along = 1)
}
mat_4d <- aperm(mat_4d,c(3,1,4,2))
mat_4d.z <- mat_4d[,,1:42,]
mat_4d.y <- mat_4d[,,43:47,]
mat_4dz_sort <- array(mat_4d.z,dim = c(42*31,42*31))
mat_4dy_sort <- array(mat_4d.y,dim = c(42*31,5*31))
concatenate <- cbind(mat_4dz_sort,mat_4dy_sort)
V = cbind(Beijing.V,Tianjin.V)
IMz = cbind(Beijing.MATRIX[1:42,1:42],Tianjin.MATRIX[1:42,1:42])
IMf = cbind(Beijing.MATRIX[1:42,43:47],Tianjin.MATRIX[1:42,43:47])
for(i in 3:length(prov))
{
  assign("tmp",eval(parse(text = v_name[i])))
  assign("tmp_MATRIX",eval(parse(text = int.name[i])))
  tmp_MATRIXz <- tmp_MATRIX[1:42,1:42]
  tmp_MATRIXf <- tmp_MATRIX[1:42,43:47]
  V <- cbind(V,tmp)
  IMz <- cbind(IMz,tmp_MATRIXz)
  IMf <- cbind(IMf,tmp_MATRIXf)
}
fill.na <- matrix(0,nrow = 4, ncol = dim(concatenate)[2]-dim(V)[2])
IM <- cbind(IMz,IMf)
concatenate <- rbind(as.matrix(concatenate),as.matrix(IM))
third_quadrant <- cbind(V,fill.na)
concatenate <- rbind(concatenate,third_quadrant)
rowlabel <- outer(label,prov,paste,sep = ".")
ylabel <- outer(c("CR","CU","CG","K","INV"),prov,paste,sep = ".")
collabel <- c(rowlabel,ylabel)
imlabel <- paste("IM.S",seq(1,42),sep = "")
rowlabel <- c(rowlabel,imlabel,"VA001","VA002","VA003","VA004")
rownames(concatenate)<-rowlabel
colnames(concatenate)<-collabel
save(concatenate,file = "IOmatrix_int.RData")
write.csv(concatenate,file = "IOmatrix_int.csv")
