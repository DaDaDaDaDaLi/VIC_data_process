#��δ������������ȡ�о���������վ������
lati = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,2]
lati = lati/100
long = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,3]
long = long/100
S_code = read.table("D:\\data_xijiang\\data_result\\forcing\\win\\SURF_CLI_CHN_MUL_DAY-PRE-13011-201001.TXT")[,1]
#������ȡ����վ�����
fdata<-data.frame(lati,long,S_code)
write.table(fdata,"D:\\data_xijiang\\data_result\\forcing\\arcgis_input_win_long_lati.txt",row.names=F,col.names=F,quote=F)
#���ȫ������վ�㾭γ�������վ�����,��Ҫ��arcgis������ȡ
S_station_code <- read.table("D:\\data_xijiang\\data_result\\forcing\\arcgis_output_station_long_lati_XJ.txt",header=T,sep=",")[,4]
f_station_code <- factor(S_station_code)
S_code = levels(f_station_code)
#�������ˮƽ��ת��Ϊ����
S_code=as.numeric(S_code)
v3=length(S_code)
#v3���о���������վ�����
#�������д��������������о���������վ��Ĵ��룬���˽�ˮ����������Ҫ��Ҳһ������ 
#�ϴ���������


path <- "D:\\data_xijiang\\data_result\\forcing\\win"
#��ȡ�ļ���·��
FileNames <- dir(path)
#��ȡ�ļ���
FilePath <- paste(path,FileNames,sep="\\")
#�����ļ�·��
v1 = length(FilePath)
#v1�����ļ�����



for(i in 1:v1){
v4=1
#v4�Ǳ���
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
#v2�������������Ϊ���Ǹ���ȫ������վ���������
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
#�������Ҳ�����32766��������32766˼������ǰ���������������ֵ��������һ�������
#��32700
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

#���µļ���
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
#С�µļ���
if(F){
  x<-c(1990:2010)
  for(i in 1:21){
    if(x[i]%%4==0&&x[i]%%100!=0||x[i]%%400==0){
      print(x[i])
      
    }
  }
}
#���´������ж���2010��2017��İ�������û����һ�������꣬���2012,2016�����꣬������ƽ��
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
  #�������
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
  #ƽ�����
  
}
v4=1

}

#�����ǽ������ݵ���ȡ��������Ҫ������Ҳһ������