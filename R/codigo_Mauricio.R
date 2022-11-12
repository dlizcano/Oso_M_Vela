

library(readxl)
library(readr)
# Leer datos
full.data <- read_excel("data/Lineas_BD_PNNCh.xlsx")
oso_data <- read_csv("data/TO_PNNCh.csv") 
  
# View(full.data)

#Leer funcion
source("R/Mauricio_mat.R")

# ejecutar funcion
mat.per.sp<-f.matrix.creator2(data = full.data, year = 2019)
# especies
names(mat.per.sp) 

# matriz de la especie 1
mat.per.sp[[1]]

# matriz de la especie 2
mat.per.sp[[2]]


#code to shrink the matrix to exactly 75(each 7 days) columns
  f.shrink.matrix.to75<-function(matrix){
    nc<-dim(matrix)[2]
    if(!nc%%75){ # of the number of columns is exactly divisible by 75
      newc<-nc%/%75
      old.cols<-seq(1,nc,newc)
      new.matrix<-matrix(NA,nr=nrow(matrix),nc=75)
      for(i in 1:75){
        new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
      }
    } else{
      rem<-nc%%75
      newc<-nc%/%75
      old.cols<-seq(1,nc-rem,newc)
      new.matrix<-matrix(NA,nr=nrow(matrix),nc=75)
      for(i in 1:74)
        new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
      new.matrix[,75]<-apply(matrix[,old.cols[75]:nc],1,max,na.rm=T) 
    }
    new.matrix[new.matrix=="-Inf"]<-NA
    rownames(new.matrix)<-rownames(matrix)
    new.matrix
  }
  
# cada 7 dias
oso_colapsed75<-f.shrink.matrix.to75(matrix=oso_data)
  

  
  #code to shrink the matrix to exactly 37(each 14 days) columns
  f.shrink.matrix.to37<-function(matrix){
    nc<-dim(matrix)[2]
    if(!nc%%37){ # of the number of columns is exactly divisible by 75
      newc<-nc%/%37
      old.cols<-seq(1,nc,newc)
      new.matrix<-matrix(NA,nr=nrow(matrix),nc=37)
      for(i in 1:37){
        new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
      }
    } else{
      rem<-nc%%37
      newc<-nc%/%37
      old.cols<-seq(1,nc-rem,newc)
      new.matrix<-matrix(NA,nr=nrow(matrix),nc=37)
      for(i in 1:36)
        new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
      new.matrix[,37]<-apply(matrix[,old.cols[37]:nc],1,max,na.rm=T) 
    }
    new.matrix[new.matrix=="-Inf"]<-NA
    rownames(new.matrix)<-rownames(matrix)
    new.matrix
  }


  # cada 14 dias
  oso_colapsed37<-f.shrink.matrix.to37(matrix=oso_data)
  