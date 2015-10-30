#Scan Peak DFSM

#input: kde return val kde



dfsm<-function(makde){

scaled<-scale(makde)

kscaled<-NULL

state<-0

kthres<-0.02

#0 for Start
#1 for Start Climbing
#2 for Climbing
#3 for Going Down
#4 for Stop

peakstart<-NULL
peakend<-NULL


for(i in 1:511){
  
  #assuming substraction of indeces is 1 because always get 512 number 
  
  diff<-scaled[i+1]-scaled[i]
  
  kscaled<-append(kscaled,diff,length(kscaled))
  
  if(state==0){
    if(diff<kthres){
      ;#out of loop
      state<-0
    }else{
      state<-1  
    }
  }else if(state==1){
    peakstart<-append(peakstart,i,length(peakstart))
    peakend<-append(peakend,i,length(peakend))
    if(diff<kthres){
      peakstart<-peakstart[1:(length(peakstart)-1)]
      peakend<-peakend[1:(length(peakend)-1)]
      state<-0
    }else{
      state<-2
    }
  }else if(state==2){
    nowlen<-length(peakend)
    peakend[nowlen]<-i
    if(diff<(-0.02)){
      state<-3
    }else{
      state<-2
    }
  }else if(state==3){
    nowlen<-length(peakend)
    peakend[nowlen]<-i
    if(diff>(-0.01)){
      state<-4
    }else{
      state<-3
    }
  }else if(state==4){
    nowlen<-length(peakend)
    peakend[nowlen]<-i
    state<-0
  }else{
    #Error
  }
}

#length chec

if(length(peakstart)<length(peakend)){
  peakend<-peakend[1:(length(peakend)-1)]
}

return(list("ps"=peakstart,"pe"=peakend))

}
