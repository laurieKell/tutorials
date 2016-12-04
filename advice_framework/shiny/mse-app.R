library(mpb)
library(FLBRP)

load("/home/laurie/Desktop/flr/tutorials/advice_framework/data/eql.RData")

eql=eqlScen[[1]]
fbar(eql)=c(seq(0,1.5,length.out=51),rep(1.5,50))*refpts(eql)["msy","harvest"]
om =mpb:::FLStock2biodyn(fwd(eql))

params(om)["b0"]=1
om=propagate(mpb:::fwd(om,catch=catch(om)),100)
mp=om
pe=rlnorm(100,FLQuant(0,dimnames=list(year=1:101)),.2)

res=demo(om,mp,pe,start=50)

plot(res)+
  theme_bw()

plot(res[[1]])+
 #geom_hline(aes(ylintercept=))+  cbind(qname=c("Yield","Harvest","Stock"),data.frame(iter(refpts(om),1)))
 geom_line(aes(year,data,group=iter),col="grey",
             data=transform(ldply(FLQuants(res[[1]],
                          "Stock"=stock,
                          "Harvest"=harvest,
                          "Yield"=catch),function(x) 
                                      as.data.frame(iter(x, 1:3),drop=TRUE)),
                          qname=.id))+
  theme_bw()

library(kobe)
kobePhase(subset(kobe(res[[1]]),year%in%60:80))+
  geom_point(aes(stock,harvest,col=year))+
  geom_point(aes(stock,harvest),shape=21,fill="black",col=NA)

aav(stock(  window(res[[1]],start=60,end=80)))
aav(harvest(window(res[[1]],start=60,end=80)))
aav(catch(  window(res[[1]],start=60,end=80)))

as.FLQuant(adply(stock(  window(res[[1]],start=60,end=80)),6,function(x) data.frame(data=mean(x))))
as.FLQuant(adply(harvest(window(res[[1]],start=60,end=80)),6,function(x) data.frame(data=mean(x))))
as.FLQuant(adply(catch(  window(res[[1]],start=60,end=80)),6,function(x) data.frame(data=mean(x))))

kobe:::smry(kobe(res[[1]])[,c("stock","harvest")])

