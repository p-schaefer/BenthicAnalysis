
if (F){
  user.k<-100
  user.adaptve<-F
  user.outlier<-T
  #user.nn.method<-"ANNA"
  #user.metrics<-NULL
  user.m.select<-F
  user.distance<-F
  user.rank<-F
  
  results<-NULL
  accuracy<-NULL
  for (y in c("YK","GL","ACT")){
    if (y=="YK"){
      data(YKEnvData,envir = environment())
      data(YKBioData,envir = environment())
      input.bio<-YKBioData
      input.env<-YKEnvData
    }
    if (y=="GL"){
      data(GLEnvData,envir = environment())
      data(GLBioData,envir = environment())
      input.bio<-GLBioData
      input.env<-GLEnvData
    }
    if (y=="ACT"){
      data(ACTEnvData,envir = environment())
      data(ACTBioData,envir = environment())
      input.bio<-ACTBioData
      input.env<-ACTEnvData
    }
    
    bio.data.test<-benth.met(input.bio,2,2)
    bio.data<-bio.data.test$Summary.Metrics
    site.names<-bio.data.test$Site.List
    

    x<-input.env[,1]
    input.env<-input.env[,-c(1)]
    
    if (y=="YK"){
      
      rownames(input.env)<-rownames(bio.data)
      results$YK<-data.frame(matrix(nrow=length(which(x==0)),ncol=16))
      rownames(results$YK)<-rownames(input.env)[which(x==0)]
      colnames(results$YK)<-c("Impairment Rank","Interval Test","Equivalence Test","Randomization Test","Test Site D2","Lower Critical","Upper Critical",
                           "TSA Lambda","TSA F Value","Number of Metrics","Number of Reference Sites","Nearest-Neighbour Method","Jacknife Consistency",
                           "Indicator Metrics","Significant Metrics","Reference Sites")
      
      accuracy$YK<-data.frame(matrix(nrow=4,ncol=2))
      rownames(accuracy$YK)<-c("D0","D1","D2","D3")
      colnames(accuracy$YK)<-c("Possibly Impaired/Impaired","Impaired")
      
      for (i in which(x==0)) {
        nn.sites<-site.match(Test=input.env[i,],Reference=input.env[which(x==1),],k=user.k,adaptive=user.adaptve)#,RDA.reference=bio.data.test$Summary.Metrics[which(x==1),c(1,2,4,14)])
        taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
        met.data<-add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[,c(1,2,4,27)]#[,-c(12,13,25,26)]#
        #met.data<-rbind(bio.data.test$Summary.Metrics[names(nn.sites$final.dist),c(1,2,4,14)],bio.data.test$Summary.Metrics[i,c(1,2,4,14)])
        tsa.results<-try(tsa.test(Test=met.data[nrow(met.data),],
                                  Reference=met.data[1:(nrow(met.data)-1),],
                                  distance= if (user.distance) {nn.sites$final.dist} else {NULL}, outlier.rem=user.outlier, m.select=user.m.select, rank=user.rank),T)
        if (is(tsa.results,"try-error")){
          next
        }
        #conf<-tsa.conf(match.object=nn.sites,tsa.object=tsa.results,Reference=NULL,benth.metric=bio.data.test,levels=c(1,5,10,20,30,40),reps=100)
        
        results$YK[(i-118),]<-c(tsa.results$tsa.results[1,],tsa.results$tsa.results[2,],tsa.results$tsa.results[3,],tsa.results$tsa.results[4,],
                       tsa.results$tsa.results[5,],tsa.results$tsa.results[6,],tsa.results$tsa.results[7,],tsa.results$tsa.results[8,],
                       tsa.results$tsa.results[9,],tsa.results$general.results[5,],tsa.results$general.results[6,],nn.sites$method,
                       tsa.results$jacknife[1,],tsa.results$general.results[3,],tsa.results$general.results[4,],tsa.results$general.results[2,])
        #print(i)
      }
      accuracy$YK[1,1]<-(length(which(results$YK[grep("D0",rownames(results$YK)),1]=="Possibly Impaired"|results$YK[grep("D0",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D0",rownames(results$YK)),1]))
      accuracy$YK[2,1]<-(length(which(results$YK[grep("D1",rownames(results$YK)),1]=="Possibly Impaired"|results$YK[grep("D1",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D1",rownames(results$YK)),1]))
      accuracy$YK[3,1]<-(length(which(results$YK[grep("D2",rownames(results$YK)),1]=="Possibly Impaired"|results$YK[grep("D2",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D2",rownames(results$YK)),1]))
      accuracy$YK[4,1]<-(length(which(results$YK[grep("D3",rownames(results$YK)),1]=="Possibly Impaired"|results$YK[grep("D3",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D3",rownames(results$YK)),1]))
      
      accuracy$YK[1,2]<-(length(which(results$YK[grep("D0",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D0",rownames(results$YK)),1]))
      accuracy$YK[2,2]<-(length(which(results$YK[grep("D1",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D1",rownames(results$YK)),1]))
      accuracy$YK[3,2]<-(length(which(results$YK[grep("D2",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D2",rownames(results$YK)),1]))
      accuracy$YK[4,2]<-(length(which(results$YK[grep("D3",rownames(results$YK)),1]=="Impaired")==T)/length(results$YK[grep("D3",rownames(results$YK)),1]))
      accuracy
    }
    
    
    if (y=="GL"){
      rownames(input.env)<-rownames(bio.data)
      results$GL<-data.frame(matrix(nrow=length(which(x==0)),ncol=16))
      rownames(results$GL)<-rownames(input.env)[which(x==0)]
      colnames(results$GL)<-c("Impairment Rank","Interval Test","Equivalence Test","Randomization Test","Test Site D2","Lower Critical","Upper Critical",
                              "TSA Lambda","TSA F Value","Number of Metrics","Number of Reference Sites","Nearest-Neighbour Method","Jacknife Consistency",
                              "Indicator Metrics","Significant Metrics","Reference Sites")
      
      accuracy$GL<-data.frame(matrix(nrow=4,ncol=2))
      rownames(accuracy$GL)<-c("D0","D1","D2","D3")
      colnames(accuracy$GL)<-c("Possibly Impaired/Impaired","Impaired")
      for (i in which(x==0)) {
        nn.sites<-site.match(Test=input.env[i,],Reference=input.env[which(x==1),],k=user.k,adaptive=user.adaptve, RDA.reference=NULL)
        taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
        met.data<-add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[,-c(12,13,25,26)]#[,c(1,2,4,27)]#
        tsa.results<-tsa.test(Test=met.data[nrow(met.data),],
                              Reference=met.data[1:(nrow(met.data)-1),],
                              distance=if (user.distance) {nn.sites$final.dist} else {NULL}, outlier.rem=user.outlier, m.select=user.m.select)
        
        #conf<-tsa.conf(match.object=nn.sites,tsa.object=tsa.results,Reference=NULL,benth.metric=bio.data.test,levels=c(1,5,10,20,30,40),reps=100)
        
        results$GL[i,]<-c(tsa.results$tsa.results[1,],tsa.results$tsa.results[2,],tsa.results$tsa.results[3,],tsa.results$tsa.results[4,],
                          tsa.results$tsa.results[5,],tsa.results$tsa.results[6,],tsa.results$tsa.results[7,],tsa.results$tsa.results[8,],
                          tsa.results$tsa.results[9,],tsa.results$general.results[5,],tsa.results$general.results[6,],nn.sites$method,
                          tsa.results$jacknife[1,],tsa.results$general.results[3,],tsa.results$general.results[4,],tsa.results$general.results[2,])
        print(i)
      }
      accuracy$GL[1,1]<-(length(which(results$GL[grep("D0",rownames(results$GL)),1]=="Possibly Impaired"|results$GL[grep("D0",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D0",rownames(results$GL)),1]))
      accuracy$GL[2,1]<-(length(which(results$GL[grep("D1",rownames(results$GL)),1]=="Possibly Impaired"|results$GL[grep("D1",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D1",rownames(results$GL)),1]))
      accuracy$GL[3,1]<-(length(which(results$GL[grep("D2",rownames(results$GL)),1]=="Possibly Impaired"|results$GL[grep("D2",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D2",rownames(results$GL)),1]))
      accuracy$GL[4,1]<-(length(which(results$GL[grep("D3",rownames(results$GL)),1]=="Possibly Impaired"|results$GL[grep("D3",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D3",rownames(results$GL)),1]))
      
      accuracy$GL[1,2]<-(length(which(results$GL[grep("D0",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D0",rownames(results$GL)),1]))
      accuracy$GL[2,2]<-(length(which(results$GL[grep("D1",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D1",rownames(results$GL)),1]))
      accuracy$GL[3,2]<-(length(which(results$GL[grep("D2",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D2",rownames(results$GL)),1]))
      accuracy$GL[4,2]<-(length(which(results$GL[grep("D3",rownames(results$GL)),1]=="Impaired")==T)/length(results$GL[grep("D3",rownames(results$GL)),1]))
      
    }
    
    
    if (y=="ACT"){
      rownames(input.env)<-rownames(bio.data)
      results$ACT<-data.frame(matrix(nrow=length(which(x==0)),ncol=16))
      rownames(results$ACT)<-rownames(input.env)[which(x==0)]
      colnames(results$ACT)<-c("Impairment Rank","Interval Test","Equivalence Test","Randomization Test","Test Site D2","Lower Critical","Upper Critical",
                              "TSA Lambda","TSA F Value","Number of Metrics","Number of Reference Sites","Nearest-Neighbour Method","Jacknife Consistency",
                              "Indicator Metrics","Significant Metrics","Reference Sites")
      
      accuracy$ACT<-data.frame(matrix(nrow=4,ncol=2))
      rownames(accuracy$ACT)<-c("D0","D1","D2","D3")
      colnames(accuracy$ACT)<-c("Possibly Impaired/Impaired","Impaired")
      
      for (i in which(x==0)) {
        nn.sites<-site.match(Test=input.env[i,],Reference=input.env[which(x==1),],k=user.k,adaptive=user.adaptve)#,RDA.reference=bio.data.test$Summary.Metrics[which(x==1),c(1,2,4,14)])
        taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
        met.data<-add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[,-c(12,13,25,26)]#[,c(1,2,4,27)]#
        #met.data<-rbind(bio.data.test$Summary.Metrics[names(nn.sites$final.dist),c(1,2,4,14)],bio.data.test$Summary.Metrics[i,c(1,2,4,14)])
        tsa.results<-try(tsa.test(Test=met.data[nrow(met.data),],
                                  Reference=met.data[1:(nrow(met.data)-1),],
                                  distance= if (user.distance) {nn.sites$final.dist} else {NULL}, outlier.rem=user.outlier, m.select=user.m.select, rank=user.rank),T)
        if (is(tsa.results,"try-error")){
          next
        }
        #conf<-tsa.conf(match.object=nn.sites,tsa.object=tsa.results,Reference=NULL,benth.metric=bio.data.test,levels=c(1,5,10,20,30,40),reps=100)
        
        results$ACT[(i-87),]<-c(tsa.results$tsa.results[1,],tsa.results$tsa.results[2,],tsa.results$tsa.results[3,],tsa.results$tsa.results[4,],
                          tsa.results$tsa.results[5,],tsa.results$tsa.results[6,],tsa.results$tsa.results[7,],tsa.results$tsa.results[8,],
                          tsa.results$tsa.results[9,],tsa.results$general.results[5,],tsa.results$general.results[6,],nn.sites$method,
                          tsa.results$jacknife[1,],tsa.results$general.results[3,],tsa.results$general.results[4,],tsa.results$general.results[2,])
        #print(i)
      }
      accuracy$ACT[1,1]<-(length(which(results$ACT[grep("D0",rownames(results$ACT)),1]=="Possibly Impaired"|results$ACT[grep("D0",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D0",rownames(results$ACT)),1]))
      accuracy$ACT[2,1]<-(length(which(results$ACT[grep("D1",rownames(results$ACT)),1]=="Possibly Impaired"|results$ACT[grep("D1",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D1",rownames(results$ACT)),1]))
      accuracy$ACT[3,1]<-(length(which(results$ACT[grep("D2",rownames(results$ACT)),1]=="Possibly Impaired"|results$ACT[grep("D2",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D2",rownames(results$ACT)),1]))
      accuracy$ACT[4,1]<-(length(which(results$ACT[grep("D3",rownames(results$ACT)),1]=="Possibly Impaired"|results$ACT[grep("D3",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D3",rownames(results$ACT)),1]))
      
      accuracy$ACT[1,2]<-(length(which(results$ACT[grep("D0",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D0",rownames(results$ACT)),1]))
      accuracy$ACT[2,2]<-(length(which(results$ACT[grep("D1",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D1",rownames(results$ACT)),1]))
      accuracy$ACT[3,2]<-(length(which(results$ACT[grep("D2",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D2",rownames(results$ACT)),1]))
      accuracy$ACT[4,2]<-(length(which(results$ACT[grep("D3",rownames(results$ACT)),1]=="Impaired")==T)/length(results$ACT[grep("D3",rownames(results$ACT)),1]))
      
    }
    
  }
}

  

if (F){

  data(YKEnvData,envir = environment())
  data(YKBioData,envir = environment())
  
  bio.data.test<-benth.met(YKEnvData,2,2)
  bio.data<-bio.data.test$Summary.Metrics
  site.names<-bio.data.test$Site.List

  rownames(YKBioData)<-rownames(bio.data)

  x<-input.env[,1]
  input.env<-input.env[,-c(1)]
  
  results<-data.frame(matrix(nrow=length(which(x==0)),ncol=16))
  rownames(results)<-rownames(input.env)[which(x==0)]
  colnames(results)<-c("Impairment Rank","Interval Test","Equivalence Test","Randomization Test","Test Site D2","Lower Critical","Upper Critical",
                       "TSA Lambda","TSA F Value","Number of Metrics","Number of Reference Sites","Nearest-Neighbour Method","Jacknife Consistency",
                       "Indicator Metrics","Significant Metrics","Reference Sites")
  
  accuracy<-data.frame(matrix(nrow=4,ncol=2))
  rownames(accuracy)<-c("D0","D1","D2","D3")
  colnames(accuracy)<-c("Possibly Impaired/Impaired","Impaired")
  for (i in which(x==0)) {
    nn.sites<-site.match(Test=input.env[i,],Reference=input.env[which(x==1),],k=NULL,adaptive=T)
    taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
    tsa.results<-try(tsa.test(Test=add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[(1+length(nn.sites$final.dist)),],
                          Reference=add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[1:length(nn.sites$final.dist),],
                          distance=nn.sites$final.dist, outlier.rem=F, m.select=T),T)
    
    #conf<-tsa.conf(match.object=nn.sites,tsa.object=tsa.results,Reference=NULL,benth.metric=bio.data.test,levels=c(1,5,10,20,30,40),reps=100)
    
    results[i,]<-c(tsa.results$tsa.results[1,],tsa.results$tsa.results[2,],tsa.results$tsa.results[3,],tsa.results$tsa.results[4,],
                   tsa.results$tsa.results[5,],tsa.results$tsa.results[6,],tsa.results$tsa.results[7,],tsa.results$tsa.results[8,],
                   tsa.results$tsa.results[9,],tsa.results$general.results[5,],tsa.results$general.results[6,],nn.sites$method,
                   tsa.results$jacknife[1,],tsa.results$general.results[3,],tsa.results$general.results[4,],tsa.results$general.results[2,])
    print(i)
  }
  accuracy[1,1]<-(length(which(results[grep("D0",rownames(results)),1]=="Possibly Impaired"|results[grep("D0",rownames(results)),1]=="Impaired")==T)/length(results[grep("D0",rownames(results)),1]))
  accuracy[2,1]<-(length(which(results[grep("D1",rownames(results)),1]=="Possibly Impaired"|results[grep("D1",rownames(results)),1]=="Impaired")==T)/length(results[grep("D1",rownames(results)),1]))
  accuracy[3,1]<-(length(which(results[grep("D2",rownames(results)),1]=="Possibly Impaired"|results[grep("D2",rownames(results)),1]=="Impaired")==T)/length(results[grep("D2",rownames(results)),1]))
  accuracy[4,1]<-(length(which(results[grep("D3",rownames(results)),1]=="Possibly Impaired"|results[grep("D3",rownames(results)),1]=="Impaired")==T)/length(results[grep("D3",rownames(results)),1]))

  accuracy[1,2]<-print(length(which(results[grep("D0",rownames(results)),1]=="Impaired")==T)/length(results[grep("D0",rownames(results)),1]))
  accuracy[2,2]<-print(length(which(results[grep("D1",rownames(results)),1]=="Impaired")==T)/length(results[grep("D1",rownames(results)),1]))
  accuracy[3,2]<-print(length(which(results[grep("D2",rownames(results)),1]=="Impaired")==T)/length(results[grep("D2",rownames(results)),1]))
  accuracy[4,2]<-print(length(which(results[grep("D3",rownames(results)),1]=="Impaired")==T)/length(results[grep("D3",rownames(results)),1]))
}


#################################################################################
#################################################################################
#################################################################################

if (F){
  data(ch.env.data,envir = environment())
  data(ch.bio.data,envir = environment())
  data(ch.user.refsites,envir = environment())

  bio.data.test<-benth.met(ch.bio.data,2,2)
  bio.data<-bio.data.test$Summary.Metrics
  site.names<-bio.data.test$Site.List

  rownames(ch.env.data)<-rownames(bio.data)

  x<-ch.env.data[,3]
  ch.env.data<-ch.env.data[,-c(1,2,3)]

  results<-data.frame(matrix(nrow=length(which(x==0)),ncol=16))
  rownames(results)<-rownames(input.env)[which(x==0)]
  colnames(results)<-c("Impairment Rank","Interval Test","Equivalence Test","Randomization Test","Test Site D2","Lower Critical","Upper Critical",
                       "TSA Lambda","TSA F Value","Number of Metrics","Number of Reference Sites","Nearest-Neighbour Method","Jacknife Consistency",
                       "Indicator Metrics","Significant Metrics","Reference Sites")
  for (i in 1:nrow(ch.user.refsites)) {
    #nn.sites<-site.match(Test=ch.env.data[i,],Reference=ch.env.data[which(x!=0),],k=NULL,adaptive=T)
    nn.sites<-NULL
    nn.sites$final.dist<-ch.user.refsites[i,!is.na(ch.user.refsites[i,])]
    names(nn.sites$final.dist)<-ch.user.refsites[i,!is.na(ch.user.refsites[i,])]
    if (length(nn.sites$final.dist)>8){
      #taxa.data<-rbind(bio.data.test$Raw.Data[names(nn.sites$final.dist),],bio.data.test$Raw.Data[rownames(bio.data[i,]),])
      tsa.results<-tsa.test(Test=add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[(1+length(nn.sites$final.dist)),],
                            Reference=add.met(Test=bio.data.test$Raw.Data[i,],Reference=bio.data.test$Raw.Data[names(nn.sites$final.dist),])[1:length(nn.sites$final.dist),],
                            distance=NULL, outlier.rem=F, m.select=T)
      results[i,]<-c(tsa.results$tsa.results[1,],tsa.results$tsa.results[2,],tsa.results$tsa.results[3,],tsa.results$tsa.results[4,],
                     tsa.results$tsa.results[5,],tsa.results$tsa.results[6,],tsa.results$tsa.results[7,],tsa.results$tsa.results[8,],
                     tsa.results$tsa.results[9,],tsa.results$general.results[5,],tsa.results$general.results[6,],nn.sites$method,
                     tsa.results$jacknife[1,],tsa.results$general.results[3,],tsa.results$general.results[4,],tsa.results$general.results[2,])
      print(i)
    }
  }
}

if (F){
  data<-matrix(nrow=length(rownames(ch.env.data)[which(ch.env.data[,3]==0)]),ncol=40)
  rownames(data)<-rownames(ch.env.data)[which(ch.env.data[,3]==0)]
  refsitenames<-rownames(ch.env.data)[which(ch.env.data[,3]==1)]

  for (i in 1:nrow(ch.user.refsites)){
    for (n in 4:length(which(!is.na(ch.user.refsites[i,])))){
      for (z in 1:length(grep(ch.user.refsites[i,n],refsitenames))){
        if (any(grep(ch.user.refsites[i,n],refsitenames))){
          data[i,(41-length(which(is.na(data[i,]))))]<-refsitenames[grep(ch.user.refsites[i,n],refsitenames)][z]
        }
      }
    }
  }

  data[1,41-length(which(is.na(data[1,])))]<-refsite.names

}
