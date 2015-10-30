#Process Big Data File

makde<-density(ma,kernel="gaussian")

all_peak_start<-NULL;
all_peak_end<-NULL;


rec_div<-function(makde,start,gap,all_ps,all_pe){
  
  new_ps<-NULL
  new_pe<-NULL
  
  if(gap<=0.1){
    
    ret<-dfsm(makde)#ret$ps ret$pe
    
    ps<-ret$ps
    pe<-ret$pe
    
    for(i in 1:(length(ps))){
      
      new_ps<-append(new_ps,start+gap/512*ps[i],length(new_ps))
      new_pe<-append(new_pe,start+gap/512*pe[i],length(new_pe))
      
    }
    
  }else{
    
    ret<-dfsm(makde)#ret$ps ret$pe
    
    ps<-ret$ps
    pe<-ret$pe
    
    for(i in 1:(length(ps))){
      
      tmp_end<-gap/512*pe[i]
      tmp_start<-gap/512*ps[i]
      tmp_gap<-tmp_end-tmp_start
      
      sel<-which(a$masses>=(start+tmp_start) & a$masses<(start+tmp_end))
      
      ma<-a$masses[sel]
      
      if(length(ma)<=2){
        ;
      }else{
        if(gap<=0.2){
          kde<-density(ma,kernel="gaussian",bw="SJ",n=512)
        }else{
          kde<-density(ma,kernel="gaussian")
        }
      
      
        tmp<-rec_div(kde$y,(start+tmp_start),tmp_gap,all_ps,all_pe)
      
        if(gap<=0.2){
         new_ps<-append(new_ps,tmp$news,length(new_ps));
          new_pe<-append(new_pe,tmp$newe,length(new_pe));
       }else{
         #
          new_ps<-append(new_ps,tmp$news,length(new_ps));
          new_pe<-append(new_pe,tmp$newe,length(new_pe));
        }
      #all_ps<-tmp$aps
      #all_pe<-tmp$ape
      }
    }
  }
  
  return (list("aps"=all_ps,"ape"=all_pe,"news"=new_ps,"newe"=new_pe))
}


orgs<-301.1
orge<-301.2

gap<-0.1

while(orgs<orge){
  
  
  sel<-which(a$masses>=orgs & a$masses<(orgs+gap))
  
  ma<-a$masses[sel]
  
  len<-length(ma)

  
  if(len<=2){
    orgs<-orgs+gap
    next;
  }
  
  makde<-density(ma,kernel="gaussian")
  
  mm<-rec_div(makde$y,orgs,gap,NULL,NULL)
  
  ret<-showret(mm$news,mm$newe,orgs,orgs+gap)
  
  print("Accelerating Rate:")
  
  print(ret$r)
  
  print("Sampling Filting Rate:")
  
  print(ret$rr)
  
  orgs<-orgs+gap
}

