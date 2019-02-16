#这里代码的作用就是将按时间排列的气象数据转化成按气象站点排列，为我们后面的插值做准备
Path="D:\\data_xijiang\\F_F_1989\\forcing_1989\\1_prec"
Filenames<-dir(Path)
FilePath<-paste(Path,Filenames,sep = "\\")
v1=length(FilePath)

for(i in 1:v1){
  V_S_code<-read.table(file=FilePath[i])[,1]
  lati<-read.table(file=FilePath[i])[,2]
  long<-read.table(file=FilePath[i])[,3]
  year<-read.table(file=FilePath[i])[,4]
  month<-read.table(file=FilePath[i])[,5]
  day<-read.table(file=FilePath[i])[,6]
  prec<-read.table(file=FilePath[i])[,7]
  df<-data.frame(V_S_code,lati,long,year,month,day,prec)
  m_df<-as.matrix(df)
  v2<-length(prec)
  
  
  if(month[1] %in% c(1,3,5,7,8,10,12)){
    data<-array(,c(88,7,31))
    r=1
    k=1
    for(j in 1:v2){
      C_S_code=df$V_S_code[j]
      data[k,,r]=m_df[j,]
      r=r+1
      if(j==v2||df$V_S_code[j+1]!=C_S_code){
        r=1
        k=k+1
      }
    }
    for(k in 1:31){
      write.table(data[,,k],paste("D:\\data_xijiang\\data_result\\forcing\\XJ_day_forcing_data\\prec\\day_prec",year[1],month[1],k,".txt",sep = "_"),col.names = F,row.names = F,quote = F)
    }
  }
  else if(month[1] %in% c(4,6,9,11)){
    data<-array(,c(88,7,30))
    r=1
    k=1
    for(j in 1:v2){
      C_S_code=df$V_S_code[j]
      data[k,,r]=m_df[j,]
      r=r+1
      if(j==v2||df$V_S_code[j+1]!=C_S_code){
        r=1
        k=k+1
      }
    }
    for(k in 1:30){
      write.table(data[,,k],paste("D:\\data_xijiang\\data_result\\forcing\\XJ_day_forcing_data\\prec\\day_prec",year[1],month[1],k,".txt",sep = "_"),col.names = F,row.names = F,quote = F)
    }
  }
  else if(month[1]==2){
    if(year[1]%%4==0&&year[1]%%100!=0||year[1]%%400==0){
      data<-array(,c(88,7,29))
      r=1
      k=1
      for(j in 1:v2){
        C_S_code=df$V_S_code[j]
        data[k,,r]=m_df[j,]
        r=r+1
        if(j==v2||df$V_S_code[j+1]!=C_S_code){
          r=1
          k=k+1
        }
      }
      for(k in 1:29){
        write.table(data[,,k],paste("D:\\data_xijiang\\data_result\\forcing\\XJ_day_forcing_data\\prec\\day_prec",year[1],month[1],k,".txt",sep = "_"),col.names = F,row.names = F,quote = F)
      }
    }
    else{
      data<-array(,c(88,7,28))
      r=1
      k=1
      for(j in 1:v2){
        C_S_code=df$V_S_code[j]
        data[k,,r]=m_df[j,]
        r=r+1
        if(j==v2||df$V_S_code[j+1]!=C_S_code){
          r=1
          k=k+1
        }
      }
      for(k in 1:28){
        write.table(data[,,k],paste("D:\\data_xijiang\\data_result\\forcing\\XJ_day_forcing_data\\prec\\day_prec",year[1],month[1],k,".txt",sep = "_"),col.names = F,row.names = F,quote = F)
      }
    }
    
    
    
  }
  
  
}




