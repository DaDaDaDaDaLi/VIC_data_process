#这段代码的作用是提取研究区的气象站点数据
lati = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,2]
lati = lati/100
long = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,3]
long = long/100
S_code = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,1]
#这里提取的是站点编码
fdata<-data.frame(lati,long,S_code)
write.table(fdata,"D:\\data_xijiang\\data_result\\forcing\\arcgis_input_win_long_lati.txt",row.names=F,col.names=F,quote=F)
#输出全国流域站点经纬度坐标和站点编码,需要用arcgis进行提取
S_station_code <- read.table("D:\\data_xijiang\\data_result\\forcing\\arcgis_output_station_long_lati_XJ.txt",header=T,sep=",")[,4]
f_station_code <- factor(S_station_code)
S_code = levels(f_station_code)
#输出因子水平，转换为数字
S_code=as.numeric(S_code)
v3=length(S_code)
#v3是研究流域所需站点个数
#以上所有代码的作用是输出研究流域所有站点的代码，除了降水，其他气象要素也一样适用 
#上次做到这里


path <- "D:\\data_xijiang\\data_result\\forcing\\win"
#读取文件夹路径
FileNames <- dir(path)
#获取文件名
FilePath <- paste(path,FileNames,sep="\\")
#生成文件路径
v1 = length(FilePath)
#v1数据文件个数



for(i in 1:v1){
v4=1
#v4是变量
V_S <- read.table(file=FilePath[i])[,1]
V_lati <-read.table(file=FilePath[i])[,2]
V_lati <- V_lati/100
V_long <-read.table(file=FilePath[i])[,3]
V_long <- V_long/100
V_year <-read.table(file=FilePath[i])[,5]
V_month <- read.table(file=FilePath[i])[,6]
V_day <- read.table(file=FilePath[i])[,7]
V_win <- read.table(file=FilePath[i])[,8]
v2=length(V_win)
#v2的数据量最大，因为它是该月全国所有站点的日数据
for(j in 1:v2){
if(V_win[j]==32700)
V_win[j]=0
} 
for(j in 1:v2){
  if(V_win[j]==32766){
    if(j<=5){
      V_win[j]=15
    }
    if(j>5){
      V_sum=0
      V_sum=V_win[j-5]+V_win[j-4]+V_win[j-3]+V_win[j-2]+V_win[j-1]
      V_win[j]=V_sum/5
      V_win[j]=round(V_win[j])
    }
  }
}
#这里可能也会出现32766，这里排32766思想是用前五天的数据相加求均值来代替这一天的数据
#排32700
f_data=data.frame(V_S,V_lati,V_long,V_year,V_month,V_day,V_win)
m_f_data=as.matrix(f_data)
if(V_month[1] %in% c(1,3,5,7,8,10,12)){
m_data <- matrix(,nrow=v3*31,ncol=7,byrow=T)
for(k in 1:v2){
for(l in 1:v3){
if(V_S[k]==S_code[l]){
m_data[v4,]=m_f_data[k,]
v4=v4+1
}
}
}
write.table(m_data,file=paste("D:\\data_xijiang\\data_result\\forcing\\XJ_forcing_data\\win\\XJ_win",V_year[1],V_month[1],".txt",sep="_"),row.names=F,col.names=F,quote=F)
}

#大月的计算
else if(V_month[1] %in% c(4,6,9,11)){
m_data <- matrix(,nrow=v3*30,ncol=7,byrow=T)
for(k in 1:v2){
for(l in 1:v3){
if(V_S[k]==S_code[l]){
m_data[v4,]=m_f_data[k,]
v4=v4+1
}
}
}
write.table(m_data,file=paste("D:\\data_xijiang\\data_result\\forcing\\XJ_forcing_data\\win\\XJ_win",V_year[1],V_month[1],".txt",sep="_"),row.names=F,col.names=F,quote=F)
}
#小月的计算
if(F){
  x<-c(1990:2010)
  for(i in 1:21){
    if(x[i]%%4==0&&x[i]%%100!=0||x[i]%%400==0){
      print(x[i])
      
    }
  }
}
#以下代码是判断在2010—2017年的八年中有没有那一年是闰年，结果2012,2016是闰年，其余是平年
else if(V_month[1]==2){
  if(V_year[1]%%4==0&&V_year[1]%%100!=0||V_year[1]%%400==0){
    m_data <- matrix(,nrow=v3*29,ncol=7,byrow=T)
    for(k in 1:v2){
      for(l in 1:v3){
        if(V_S[k]==S_code[l]){
          m_data[v4,]=m_f_data[k,]
          v4=v4+1
        }
      }
    }
    write.table(m_data,file=paste("D:\\data_xijiang\\data_result\\forcing\\XJ_forcing_data\\win\\XJ_win",V_year[1],V_month[1],".txt",sep="_"),row.names=F,col.names=F,quote=F)
  }
  #闰年二月
  else{
    m_data <- matrix(,nrow=v3*28,ncol=7,byrow=T)
    for(k in 1:v2){
      for(l in 1:v3){
        if(V_S[k]==S_code[l]){
          m_data[v4,]=m_f_data[k,]
          v4=v4+1
        }
      }
    }
    write.table(m_data,file=paste("D:\\data_xijiang\\data_result\\forcing\\XJ_forcing_data\\win\\XJ_win",V_year[1],V_month[1],".txt",sep="_"),row.names=F,col.names=F,quote=F)
  }
  #平年二月
  
}
v4=1

}

#以上是降雨数据的提取。其他的要素数据也一样适用
