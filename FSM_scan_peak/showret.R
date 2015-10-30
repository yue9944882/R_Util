

showret<-function(news,newe,orgs,orge){

showkde<-rep(0,512)

peakstart=mm$news
peakend=mm$newe


sample_rest<-NULL


ma_rest<-NULL
la_rest<-NULL
inten_rest<-NULL

cvr_len<-0



for(i in 1:length(peakstart)){
  
  sel<-which(ma>=peakstart[i] & ma<peakend[i] )
  
  ma_rest<-append(ma_rest,ma[sel],length(ma_rest))
  la_rest<-append(la_rest,la[sel],length(la_rest))
  inten_rest<-append(inten_rest,log10(1+inten[sel]),length(inten_rest))
  
  cvr_len=cvr_len+(peakend[i]-peakstart[i])
}


cols<-rep("grey", length(sel))

rgb.palette <- colorRampPalette(c("blue", "cyan", "green", "yellow", "orange"), space = "rgb",bias=0.5)
all.col<-rgb.palette(100)
for(i in 0:99) cols[inten_rest>=quantile(inten, i/100) & inten_rest<quantile(inten, (i+1)/100)]<-all.col[i+1]

o<-order(inten_rest)
ma_rest<-ma_rest[o]
la_rest<-la_rest[o]
inten_rest<-inten_rest[o]
cols<-cols[o]


malen<-length(ma_rest)
lalen<-length(la_rest)

if(malen==0 | lalen==0){
  return(list("lr"=0,"rr"=0,"r"=0))
}

plot(ma_rest, la_rest,col = cols, cex=.1)

rest_rate<-(length(ma_rest)/length(ma))

len_rate<-cvr_len/(orge-orgs)

rate<- (length(ma_rest)/length(ma))/(cvr_len)*(orge-orgs)

return(list("lr"=len_rate,"rr"=rest_rate,"r"=rate))

}