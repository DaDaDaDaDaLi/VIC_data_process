Path="D:\\data_xijiang\\data_result\\forcing\\XJ_IDW\\prec"
Filenames <- dir(Path)
FilePath <- paste(Path,Filenames,sep = "\\")

n1<-length(FilePath)

grid_prec<-matrix(,ncol = 4661,nrow = 2861,byrow = T)

for(i in 1:n1){
  V<-read.table(file = FilePath[i])
  V<-as.matrix(V)
  grid_prec[i,]<-V
}

#以上是降水

Path="D:\\data_xijiang\\data_result\\forcing\\XJ_IDW\\TMAX"
Filenames <- dir(Path)
FilePath <- paste(Path,Filenames,sep = "\\")
n1<-length(FilePath)

grid_TMAX<-matrix(,ncol = 4661,nrow = 2861,byrow = T)

for(i in 1:n1){
  V<-read.table(file = FilePath[i])
  V<-as.matrix(V)
  grid_TMAX[i,]<-V
}

#以上是最高气温

Path="D:\\data_xijiang\\data_result\\forcing\\XJ_IDW\\TMIN"
Filenames <- dir(Path)
FilePath <- paste(Path,Filenames,sep = "\\")
n1<-length(FilePath)

grid_TMIN<-matrix(,ncol = 4661,nrow = 2861,byrow = T)

for(i in 1:n1){
  V<-read.table(file = FilePath[i])
  V<-as.matrix(V)
  grid_TMIN[i,]<-V
}

#以上是最低气温

Path="D:\\data_xijiang\\data_result\\forcing\\XJ_IDW\\win"
Filenames <- dir(Path)
FilePath <- paste(Path,Filenames,sep = "\\")

n1<-length(FilePath)

grid_win<-matrix(,ncol = 4661,nrow = 2861,byrow = T)

for(i in 1:n1){
  V<-read.table(file = FilePath[i])
  V<-as.matrix(V)
  grid_win[i,]<-V
}

#以上是风速数据

grid_point_lati<-read.table("D:\\data_xijiang\\data_long_lati.txt",sep = ",")[,5]
grid_point_lati<-round(grid_point_lati,4)
n1<-length(grid_point_lati)
c1<-c()
for(i in 1:n1){
  c1[i]<-as.character(grid_point_lati[i])
}
for(i in 1:n1){
  if(nchar(c1[i])!=7){
    if(nchar(c1[i])==6){
      c1[i]<-paste(c1[i],"0",sep = "")
    }
    else if(nchar(c1[i])==5){
      c1[i]<-paste(c1[i],"00",sep = "")
    }
    else if(nchar(c1[i])==4){
      c1[i]<-paste(c1[i],"000",sep = "")
    }
    else{
      c1[i]<-paste(c1[i],"0000",sep = "")
    }
  }
}
#写这里的代码的作用就是r语言输出是若输出名是32.200他会自动省去00变成32.2，这样vic模型就识别不出来了，这里的解决办法是
#将数字转化为字符，这样就不会出现这样情况

grid_point_long<-read.table("D:\\data_xijiang\\data_long_lati.txt",sep = ",")[,4]
grid_point_long<-round(grid_point_long,4)
grid_point_long<-as.character(grid_point_long)
c2<-c()
for(i in 1:n1){
  c2[i]<-as.character(grid_point_long[i])
}
for(i in 1:n1){
  if(nchar(c2[i])!=8){
    if(nchar(c2[i])==7){
      c2[i]<-paste(c2[i],"0",sep = "")
    }
    else if(nchar(c2[i])==6){
      c2[i]<-paste(c2[i],"00",sep = "")
    }
    else if(nchar(c1[i])==5){
      c2[i]<-paste(c2[i],"000",sep = "")
    }
    else{
      c2[i]<-paste(c2[i],"0000",sep = "")
    }
  }
}
#理由同上
grid_point<-data.frame(c1,c2)



C_data<-array(data=NA,c(2861,4,4661))
for(k in 1:4661){
  C_data[,1,k]<-grid_prec[,k]
  C_data[,2,k]<-grid_TMAX[,k]
  C_data[,3,k]<-grid_TMIN[,k]
  C_data[,4,k]<-grid_win[,k]
}
C_data<-C_data/10
#气象站所测数据都是10倍，这里给她还原
for(k in 1:4661){
  write.table(C_data[,,k],file = paste("D:\\data_xijiang\\data_result\\forcing\\XJ_forcing_data_F1\\data",grid_point$c1[k],grid_point$c2[k],sep = "_"),col.names = F,row.names = F,quote = F)
}
#最终数据已经核对，格网数据和对应的经纬度可以对应的上
if(F){
  c<-"45.0130"
  c2<-"106.0200"
  write.table(c,file=paste("D:\\data_xijiang\\data",c,c2,sep = "_"),row.names = F,col.names = F,quote = F)
  
}


