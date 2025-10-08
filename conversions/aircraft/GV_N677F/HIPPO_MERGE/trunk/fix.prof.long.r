dum=read.table("HIPPO_profiles_100m_intervals_20121129.tbl",header=T)
dum.0=dum
load("/documents/DATA/HIPPO_RELEASE_FINAL/.RData.hippo.all.10")
hn=c(5,5,4,3,1,1,1,2,2)
pn=c(110,130,135,81,70,83,90,73,79)
fxt=NULL
for(k in 1:length(hn)){
lsel=HIPPO.all.10[,"H.no"]==hn[k] & HIPPO.all.10[,"n.prof"]==pn[k]
aw=mean(HIPPO.all.10[lsel&HIPPO.all.10[,"GGLON"]<0,"GGLON"])
ae=mean(HIPPO.all.10[lsel&HIPPO.all.10[,"GGLON"]>0,"GGLON"])
ww=sum(lsel&HIPPO.all.10[,"GGLON"]<0)
we=sum(lsel&HIPPO.all.10[,"GGLON"]>0)
gglon= ((360+aw)*ww+ae*we)/(we+ww)
if(gglon>180)gglon=360-gglon
l.dum=dum[,"H.no"]==hn[k]&dum[,"n.prof"]==pn[k]
dum[l.dum,"GGLON"]=gglon
fxt=rbind(fxt,c(hn[k],pn[k],aw,ae,ww,we,gglon)) 
print(c(hn[k],pn[k],aw,ae,ww,we,gglon))}

iuu=as.numeric(substring(as.character(dum[,1]),1,5))
plot(iuu[trunc(iuu)==5],dum.0[trunc(iuu)==5,"GGLON"])
points(iuu[trunc(iuu)==5],dum[trunc(iuu)==5,"GGLON"],pch=16)

plot(iuu[trunc(iuu)==4],dum.0[trunc(iuu)==4,"GGLON"])
points(iuu[trunc(iuu)==4],dum[trunc(iuu)==4,"GGLON"],pch=16)
plot(iuu[trunc(iuu)==3],dum.0[trunc(iuu)==3,"GGLON"])
points(iuu[trunc(iuu)==3],dum[trunc(iuu)==3,"GGLON"],pch=16)
plot(iuu[trunc(iuu)==2],dum.0[trunc(iuu)==2,"GGLON"])
points(iuu[trunc(iuu)==2],dum[trunc(iuu)==2,"GGLON"],pch=16)
plot(iuu[trunc(iuu)==1],dum.0[trunc(iuu)==1,"GGLON"])
points(iuu[trunc(iuu)==1],dum[trunc(iuu)==1,"GGLON"],pch=16)

write.table(dum,file="HIPPO_profiles_100m_intervals_20140519.tbl",row.names=F,col.names=T,quote=F)


