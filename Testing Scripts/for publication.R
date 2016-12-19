#load datasets
data(YKEnvData,envir = environment()) # Environmental dataset
data(YKBioData,envir = environment()) # Biological dataset

#Calculate summary Metrics
bio.data.test<-benth.met(YKBioData,2,2)
bio.data<-bio.data.test$Summary.Metrics

#Transform remaining metrics to approximate normality
bio.data[,grep("Richness",colnames(bio.data))]<-log(bio.data[,grep("Richness",colnames(bio.data))]+1)
bio.data[,grep("Percent",colnames(bio.data))]<-logit(bio.data[,grep("Percent",colnames(bio.data))])
bio.data<-bio.data[,-c(5,7:10,11,20,21,24,28)]


#scale data, log transform area, rename rows to match bio.data
YKEnvData[,6]<-log(YKEnvData[,6])
YKEnvData<-data.frame(apply(YKEnvData,2,scale))
rownames(YKEnvData)<-bio.data.test$Site.List

#Romeve some environmental variables that add noise to the dataset
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]

#Create output file
output<-data.frame(matrix(nrow=160,ncol=9))
colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
output$Site<-rownames(bio.data)[119:278]
output$Class<-c(rep("D0",40),rep("D1",40),rep("D2",40),rep("D3",40))
output<-rbind(output,output,output,output)
output$Analysis.type<-c(rep("Original",160),rep("Ad.Met.Sel",160),rep("Ad.Site.Sel",160),rep("Combined",160))

#What if you include D0 in the training set?
#output<-data.frame(matrix(nrow=120,ncol=9))
#colnames(output)<-c("Site","Class","Mahal.D","Impair.rank","Met.used","n.Mets","n.Ref","jknife","rand.p")
#output$Site<-rownames(bio.data)[159:278]
#output$Class<-c(rep("D1",40),rep("D2",40),rep("D3",40))
#output<-rbind(output,output,output,output)
#output$Analysis.type<-c(rep("Original",120),rep("Ad.Met.Sel",120),rep("Ad.Site.Sel",120),rep("Combined",120))


#Computational loop
#
#What if you include D0 in the training set?
#Removal of potentially noisy environmental data?
#YKEnvData<-YKEnvData[,-c(2,3,5,40,41,10:16)]
#
for (i in unique(output$Site)){
  for (n in unique(output$Analysis.type)){
    nn.sites<-site.match(Test=YKEnvData[i,-c(1)], Reference=YKEnvData[1:118,-c(1)],
                         k=if (n=="Ad.Site.Sel"|n=="Combined") {NULL} else {30},
                         adaptive=if (n=="Ad.Site.Sel"|n=="Combined") {T} else {F})
    
    taxa.data<-cbind(bio.data[c(names(nn.sites$final.dist),i),],
                     add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),]))
    
    if (n=="Original"|n=="Ad.Site.Sel") {
      taxa.data<-taxa.data[,c("Shannon","Percent.EPT","O:E","Bray-Curtis")]
    }
    
    tsa.result<-tsa.test(Test=taxa.data[nrow(taxa.data),],
                         Reference=taxa.data[1:(nrow(taxa.data)-1),],
                         distance=NULL,
                         outlier.rem=F,
                         m.select= if (n=="Ad.Met.Sel"|n=="Combined") {T} else {F},
                         rank=F,
                         na.cutoff=0.7,
                         outbound=0.1)
    
    output[(output$Site==i & output$Analysis.type==n),3:9]<-c(tsa.result$tsa.results[5,1],
                                  tsa.result$tsa.results[1,1],
                                  tsa.result$general.results[3,1],
                                  tsa.result$general.results[5,1],
                                  tsa.result$general.results[6,1],
                                  tsa.result$jacknife[1,1],
                                  tsa.result$tsa.results[4,1])
  }
}

output2<-data.frame(matrix(nrow=(160),ncol=10))
colnames(output2)<-c("Original","Ad.Met.Sel","Ad.Site.Sel","Combined","Sites","Class",
                     "Original.er",
                     "Ad.Met.Sel.er",
                     "Ad.Site.Sel.er",
                     "Combined.er")
output2$Sites<-unique(output$Site)
output2$Class<-c(rep("D0",40),rep("D1",40),rep("D2",40),rep("D3",40))

output2$Original<-output$Impair.rank[output$Analysis.type=="Original"]
output2$Ad.Met.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Met.Sel"]
output2$Ad.Site.Sel<-output$Impair.rank[output$Analysis.type=="Ad.Site.Sel"]
output2$Combined<-output$Impair.rank[output$Analysis.type=="Combined"]

output2$Original.er[output2$Class=="D0" & output2$Original=="Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class=="D0" & output2$Ad.Met.Sel=="Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class=="D0" & output2$Ad.Site.Sel=="Impaired"]<-1
output2$Combined.er[output2$Class=="D0" & output2$Combined=="Impaired"]<-1

output2$Original.er[output2$Class%in%c("D1","D2","D3") & output2$Original=="Not Impaired"]<-1
output2$Ad.Met.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Met.Sel=="Not Impaired"]<-1
output2$Ad.Site.Sel.er[output2$Class%in%c("D1","D2","D3") & output2$Ad.Site.Sel=="Not Impaired"]<-1
output2$Combined.er[output2$Class%in%c("D1","D2","D3") & output2$Combined=="Not Impaired"]<-1

errors<-data.frame(matrix(nrow=4,ncol=4))
rownames(errors)<-unique(output$Analysis.type)
colnames(errors)<-c("D0","D1","D2","D3")
errors$D0<-colSums(output2[output2$Class=="D0",7:10],na.rm = T)/40
errors$D1<-colSums(output2[output2$Class=="D1",7:10],na.rm = T)/40
errors$D2<-colSums(output2[output2$Class=="D2",7:10],na.rm = T)/40
errors$D3<-colSums(output2[output2$Class=="D3",7:10],na.rm = T)/40
errors

boxplot(sqrt(as.numeric(output$Mahal.D))~as.factor(output$Analysis.type)+as.factor(output$Class))
