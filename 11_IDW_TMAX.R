Path="D:\\data_xijiang\\data_result\\forcing\\XJ_day_forcing_data\\TMAX"
Filenames<-dir(Path)
FilePath<-paste(Path,Filenames,sep = "\\")
n1=length(FilePath)
#n1是文件数
grid_point_lati<-read.table("D:\\data_xijiang\\data_long_lati.txt",header = F,sep = ",")[,5]
grid_point_lati<-round(grid_point_lati,2)
grid_point_long<-read.table("D:\\data_xijiang\\data_long_lati.txt",header = F,sep = ",")[,4]
grid_point_long<-round(grid_point_long,2)
grid_point<-data.frame(grid_point_lati,grid_point_long)
n2<-nrow(grid_point)
#n2是流域网格数4661
grid_point<-as.matrix(grid_point)#以上是网格的经纬度坐标



sample_point<-read.table("D:\\data_xijiang\\F_F_1989\\forcing_1989\\2_prec\\day_prec_1988_1_1_.txt")[,c(2,3,7)]
sample_point<-as.matrix(sample_point)
n3<-nrow(sample_point)
#n3是所选择的流域水文站点的数量104
dis<-array(data=NA,c(4661,104))
for(j in 1:n2){
  for(k in 1:n3){
    dis[j,k]<-sqrt((grid_point[j,1]-sample_point[k,1])^2+(grid_point[j,2]-sample_point[k,2])^2)
  }
}
dis<-round(dis,2)
#对于所有样本点和待插值点来说，他们的相对距离是不变的,变的只是待插值点的值

for(i in 1:n1){
  V_S_code<-read.table(file=FilePath[i])[,1]
  lati<-read.table(file=FilePath[i])[,2]
  long<-read.table(file=FilePath[i])[,3]
  year<-read.table(file=FilePath[i])[,4]
  month<-read.table(file=FilePath[i])[,5]
  day<-read.table(file=FilePath[i])[,6]
  if(month<10){
    month<-as.character(month)
    month<-paste("00",month,sep = "")
  }
  else if(month>=10){
    month<-as.character(month)
    month<-paste("0",month,sep = "")
  }
  if(day<10){
    day<-as.character(day)
    day<-paste("00",day,sep = "")
  }
  else if(day>=10){
    day<-as.character(day)
    day<-paste("0",day,sep = "")
  }
  TMAX<-read.table(file=FilePath[i])[,7]
  df<-data.frame(V_S_code,lati,long,year,TMAX)
  
  m_df<-as.matrix(df)
  V_grid=rep(0,4661)
  for(l in 1:n2){
    FM=0
    #分母
    W=c()
    #权重
    for(m in 1:n3){
      FM=FM+(1/dis[l,m])^2
    }
    for(m in 1:n3){
      W[m]=(1/dis[l,m])^2/FM
    }
    #以上相当于把第l个的待插值点对应的各个样本点权重计算出来了
    for(m in 1:n3){
      V_grid[l]=V_grid[l]+W[m]*m_df[m,5]
    }
    #把第l个待插值点的值计算出来
    
  }
  V_grid=round(V_grid,2)#在这里可以设置一个数据框然后利用cbind()函数把经纬度加上，对应点的经纬度一直不变
  
  write.table(V_grid,file=paste("D:\\data_xijiang\\data_result\\forcing\\XJ_IDW\\TMAX\\TMAX",year[1],month[1],day[1],".txt",sep = "_"),col.names = F,row.names=F,quote = F)
}
#输出插值后的网格值时最好把对应网格经纬度也加上，我忘记加了，后面在制作最终的驱动数据时，文件名需要加上对应网格经纬度
  



if(F){
  dis1=sqrt((grid_point[1,1]-sample_point[1,1])^2+(grid_point[1,2]-sample_point[1,2])^2)
  c1<-read.table("dem_long_lati.txt",header = T,sep = ",")[,5]
  c2<-read.table("dem_long_lati.txt",header = T,sep = ",")[,4]
  c<-data.frame(c1,c2)
  c<-round(c,2)
  write.table(c,"dem_long_lati.txt",col.names = F,row.names = F,quote = F)
  
  
  Path="C:\\Users\\Administrator\\Desktop\\vic\\result\\forcing"
  Filenames<-read.table("C:\\Users\\Administrator\\Desktop\\vic\\result\\forcing\\grid_TMAX_filename.txt")
  Filenames<-as.character(Filenames)
  class(Filenames)
  FilePath <- paste(Path,Filenames,sep = "\\")
  n1<-length(FilePath)
  warnings()
  
}