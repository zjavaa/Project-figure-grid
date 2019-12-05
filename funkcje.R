require(rgl)
require(tidyverse)
 wczytywanie<-function(obiekt){
  LL <- readLines(obiekt)
  # punkty
  xx <- grep('^v ',LL, value = TRUE)
  xx <- gsub('^v','',xx)
  xx <- trimws(xx)
  xx <- strsplit(xx,' ')
  xx <- sapply(xx, as.numeric)
  xx <- t(xx)
  xx<-cbind(xx,1)
  colnames(xx) <- c('x','y','z','w')
  rownames(xx) <- paste(seq(nrow(xx)))
  # wektory normalne
  vn <- grep('^vn ',LL, value = TRUE)
  vn <- gsub('^vn','',vn)
  vn <- trimws(vn)
  vn <- strsplit(vn,' ')
  vn <- sapply(vn, as.numeric)
  vn <- t(vn)
  colnames(vn) <- c('x','y','z')
  # nr sciany i ich wierzchołków
  f <- grep('^f ',LL, value = TRUE)
  f <- gsub('^f','',f)
  f <- trimws(f)
  f <- strsplit(f,"//")
  f0 <- strsplit(sapply(f, paste, collapse=' '), ' ')
  f <- lapply(f0, function(x) as.integer(x[c(TRUE, FALSE)]))
  return(list(xx,vn,f))
 }

MacierzPowiazan<-function(f){
  LP <- expand.grid(seq_along(f), seq_along(f))
  LP <- LP[LP[,1] < LP[,2],]
  LP <- cbind(cbind(LP, NA), NA)
  
  for(i in seq(nrow(LP))){
    f1 <- f[[LP[i, 1]]]
    f2 <- f[[LP[i, 2]]]
    ff <- intersect(f1, f2)
    if(length(ff) == 2){
      LP[i, 3] <- ff[1]
      LP[i, 4] <- ff[2]
    }
  }
  LP <- LP[!is.na(LP[,3]), ]
  colnames(LP) <- c('S1', 'S2', 'P1', 'P2')
  
  LP <- as.tibble(LP)
  return(LP)
}



siatkapopr<-function(pkt,wn){
  srk<-colMeans(pkt[1:(length(pkt[,1])),])
  pkt<-translate3d(pkt,-srk[1],-srk[2],-srk[3])
  lpi<-pi/2
  if(wn[1]==0 && wn[3]<0){
    lpi<-0
  }
  if(wn[1]!=0){
    katy<-as.numeric(acos(wn[3]))
    if(wn[1]<0){
      katy<--katy
    }
    pkt<-rotate3d(pkt,katy,0,1,0)
    wn<-rotate3d(wn,katy,0,1,0)
  }
  
  if(round(wn[1],2)!=0 || round(wn[2],2)!=0){
    katz<-as.numeric(acos(wn[2]))
    katz<-katz-lpi
    pkt<-rotate3d(pkt,katz,1,0,0)
    wn<-rotate3d(wn,katz,1,0,0)
  }
  
  pkt<-rotate3d(pkt,-wn[2],1,0,0)
  wn<-rotate3d(wn,-wn[2],1,0,0)
  pkt<-rotate3d(pkt,wn[1],0,1,0)
  wn<-rotate3d(wn,wn[1],0,1,0)
  if(round(wn[3]==-1)){
    
    pkt<-rotate3d(pkt,pi,1,0,0)
    wn<-rotate3d(wn,pi,1,0,0)}
  
  return(list(round(pkt,2),round(wn,2)))
}
oznaczonekrawedzie<-function(f,xx,vn){
  punkty3d<-vector('list',length(f))
   wektoryy<-vector('list',length(f))
  for(i in 1:length(f)){
    m<-siatkapopr(xx[f[[i]],],vn[i,])
     wektoryy[[i]]<-m[[2]]
    punkty3d[[i]]<-round(m[[1]],2)
  }
  return(punkty3d)
}
Rys <- function(punkty){

   plot(NA,NA,xlim = c(-10.5,10.5),ylim=c(-15,15),bty='n',asp=1,axes = F,xaxt='n',yaxt='n',ann = F)
  
  for(i in 1:length(punkty)){
  
    polygon(punkty[[i]][,1],punkty[[i]][,2])
  } 
}

kat<-function(punkty,s1,s2,pp){
    a<-punkty[[s1]][paste(pp,sep = ""),]
    b<-punkty[[s2]][paste(pp,sep = ""),]
    a<-a[2,]-a[1,]
    b<-b[2,]-b[1,]
    iloczynwektorowy<-det(rbind(a[1:2],b[1:2]))
    iloczynskalarny<-sum(a*b)
    dl<-sqrt(sum(a^2))*sqrt(sum(b^2))
    sinus<-iloczynwektorowy/dl
    cosinus<-iloczynskalarny/dl
    alfa<-acos(cosinus)
    if(sinus<0){alfa=-alfa}
    return(alfa)
  }
Przesuniecie<-function(punkty,s1,s2,pp){
  roznica<-diff(rbind(punkty[[s1]][paste(pp[1],sep = ""),],punkty[[s2]][paste(pp[1],sep = ""),]))
  przesuwanie<-sweep(punkty[[s2]][,1:2],2,roznica[,1:2])
  return(round(przesuwanie,2))
}
obracanie<-function(punkty,s1,s2,pp,katy){
  punktypom<-sweep(punkty[[s2]],2,punkty[[s1]][paste(pp[1],sep = ""),])
  obrot<-rotate3d(punktypom,katy,0,0,1)
  punktypom[,1]<-obrot[,1]+punkty[[s1]][paste(pp[1],sep = ""),1]
  punktypom[,2]<-obrot[,2]+punkty[[s1]][paste(pp[1],sep = ""),2]
  return(round(punktypom,2))
}