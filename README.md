# R-smbinning-
library(smbinning)
library(plyr)
library(dplyr)
library(rlist)
library(tcltk)

infile = 'C:/Users/Administrator/Desktop/scorecard/R-scorecard-master/'
df = read.csv(paste0(infile,'german_credit.csv'))
#设置y变量名
df = plyr::rename(df,c("Creditability" = "y"))
#等深分箱（10等分,剔除首尾）
dengshen = function(Data,Name){
  Quantile = quantile(Data[Name],seq(0,1,0.1),na.rm = T)
  smbinning.custom(Data,'y',Name,Quantile[2:length(Quantile) - 1])$ivtable
}
#设置一个空list
data_iv = list()
L = dim(df)[2] - 1
fenxiang = function(Data){
  pb <- tkProgressBar("进度","已完成 %",0,100) 
  for(Name in colnames(Data)){
     if(Name != 'y'){
      if(dim(unique(Data[Name],rm.na = T))[1] <= 10){
        Data[Name] = as.factor(Data[,Name])}else{
        Data[Name] = Data[Name]}
     if(Data[,Name] %>% is.factor){
        eval(parse(text = paste0("data_iv = data_iv %>% 
                                 list.append(.,",Name," = smbinning.factor(Data,'y',Name)$ivtable)")))
        
     }else{
       eval(parse(text = paste0("data_iv = data_iv %>% 
                                list.append(.,",Name,"= tryCatch(smbinning(Data,'y',Name)$ivtable,error = function(x) dengshen(Data,Name)))")))
       }
     }
    info<- sprintf("已完成%d%%",round(which(colnames(Data) == Name) * 100/L))
    setTkProgressBar(pb,
                     which(colnames(Data) == Name) * 100/L,
                     sprintf("进度(%s)", info),info) 
    }
  close(pb)
  return(data_iv %>% ldply())
}

#开始分箱
iv = fenxiang(df) 









