

fastdist<-function(ma,la){
  
  
  len<-length(ma)
  
  point<-cbind(la,ma)
  smat<-apply(point, 1,crossprod)
  mat1<-matrix(smat,nrow=len,ncol=len)
  mat3<-tcrossprod(point)
  mat4<-mat1+t(mat1)-2*mat3
  diag(mat4)<-0
  mat5<-sqrt(mat4)
  
  return(mat5)
  
  
}



