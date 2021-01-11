setwd("~/Downloads/MRIO/")
load("MRIO_data.RData")
IMcof.name <- paste(prov,"IMcof.mat",sep = ".")
TIU.name <- paste(prov,"TIU",sep = ".")
TIU_.name <- paste(prov,"TIU_",sep = ".")
F.name <- paste(prov,"F",sep = ".")
F_.name <- paste(prov,"F_",sep = ".")
for(i in 1:length(prov))
{
  assign("tmp",eval(parse(text = IMcof.name[i])))
  IMcof.sum = apply(tmp,2,sum)
  assign("tiu",eval(parse(text = TIU.name[i])))
  assign("f",eval(parse(text = F.name[i])))
  tiu_f <- cbind(tiu,f)
  tiu_f.sum.rev <- apply(tiu_f,1,sum)^(-1)
  coef1 <- diag(tiu_f.sum.rev)%*%tiu_f
  decom <- diag(IMcof.sum)%*%coef1
  assign(TIU_.name[i],decom[,1])
  assign(F_.name[i],decom[,2:6])
}
save(list = c(TIU_.name), file = "TIU_.RData")
save(list = c(F_.name), file = "F_.RData")