list.of.packages <- c("shiny", "ggplot2","colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)>0){
  if (!require("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")}
  BiocManager::install("scales")
  install.packages('shiny')
  install.packages("gplots")
  install.packages("colourpicker")
  
}

library(shiny)
library(ggplot2)
library(colourpicker)
library(gplots)

##library(edgeR)
##library(org.Dm.eg.db)
##FB2EG=as.list(org.Dm.egFLYBASE2EG)
##EG2Symbol=as.list(org.Dm.egSYMBOL)
##eg=FB2EG[row.names(cpmdata)]
##i=1
##while(i<=length(eg)){
##  if(length(eg[[i]])>1){
##    eg[[i]]=eg[[i]][1]
##  }
##  i=i+1
##}

##symbol=EG2Symbol[as.vector(unlist(eg))]
##i=1
##while(i<=length(symbol)){
##  if(length(symbol[[i]])>1){
##    symbol[[i]]=symbol[[i]][1]
##  }
##  i=i+1
##}
##symbol=unlist(symbol)
##eg=unlist(eg)
##temp=names(eg)
##names(temp)=as.vector(eg)
##eg=temp
##names(symbol)=as.vector(eg[names(symbol)])
##temp=setdiff(row.names(cpmdata),names(symbol))
##names(temp)=temp
##symbol=c(symbol,temp)
##write.csv(symbol,"/Users/johncsantiago/Documents/GitHub/Fly-Body-Sections-Project/Data/convert.csv")

##countdata<-read.csv("https://raw.githubusercontent.com/johncsantiago/Fly-Body-Sections-Project/master/Data/countdata.csv", row.names = 1)
##groups<-read.csv("https://raw.githubusercontent.com/johncsantiago/Fly-Body-Sections-Project/master/Data/groups.csv", row.names = 1)

##countdata=countdata[,row.names(groups)]
##cut=countdata[1,]>0
##groups=groups[cut,]

##EdgeR comparisons
##x <- countdata[,cut]
##group <- factor(groups$Group)
##y <- DGEList(counts=x,group=group)
##keep <- filterByExpr(y)
##y <- y[keep,,keep.lib.sizes=FALSE] 
##z <- calcNormFactors(y, method = "TMM") 

##cpmdata=cpm(z,base=2)
##write.csv(cpmdata,"/Users/johncsantiago/Documents/GitHub/Fly-Body-Sections-Project/Data/cpmdata.csv")
##design<-model.matrix(~0+group)
##colnames(design) <- levels(group)
##z = estimateGLMCommonDisp(z,design, verbose=T)
##z = estimateGLMTrendedDisp(z,design)
##z = estimateGLMTagwiseDisp(z,design)
##fit <- glmFit(z, design)

##comp = c(
##Head Genotype  
##         "SOFCH-OOFCH SO-OOFCH.csv", 
##         "SOMCH-OOMCH SO-OOMCH.csv",
##         "SOFRH-OOFRH SO-OOFRH.csv",
##         "SOMRH-OOMRH SO-OOMRH.csv", 
##Head Sex
##         "OOMCH-OOFCH OOM-FCH.csv",
##         "OOMRH-OOFRH OOM-FRH.csv",
##         "SOMCH-SOFCH SOM-FCH.csv",
##         "SOMRH-SOFRH SOM-FRH.csv",  
##Head Treatment
##         "OOFRH-OOFCH OOFR-CH.csv",
##         "OOMRH-OOMCH OOMR-CH.csv",
##         "SOFRH-SOFCH SOFR-CH.csv",
##         "SOMRH-SOMCH SOMR-CH.csv",
##Thorax Genotype         
##         "SOFCT-OOFCT SO-OOFCT.csv", 
##         "SOMCT-OOMCT SO-OOMCT.csv",
##         "SOFRT-OOFRT SO-OOFRT.csv",
##         "SOMRT-OOMRT SO-OOMRT.csv", 
##Thorax Sex
##         "OOMCT-OOFCT OOM-FCT.csv",
##         "OOMRT-OOFRT OOM-FRT.csv",
##         "SOMCT-SOFCT SOM-FCT.csv",
##         "SOMRT-SOFRT SOM-FRT.csv",  
##Thorax Treatment
##         "OOFRT-OOFCT OOFR-CT.csv",
##         "OOMRT-OOMCT OOMR-CT.csv",
##         "SOFRT-SOFCT SOFR-CT.csv",
##         "SOMRT-SOMCT SOMR-CT.csv",
##Abdomen Genotype         
##         "SOFCA-OOFCA SO-OOFCA.csv", 
##         "SOMCA-OOMCA SO-OOMCA.csv",
##         "SOFRA-OOFRA SO-OOFRA.csv",
##         "SOMRA-OOMRA SO-OOMRA.csv", 
##Abdomen Sex
##         "OOMCA-OOFCA OOM-FCA.csv",
##         "OOMRA-OOFRA OOM-FRA.csv",
##         "SOMCA-SOFCA SOM-FCA.csv",
##         "SOMRA-SOFRA SOM-FRA.csv",  
##Abdomen Treatment
##         "OOFRA-OOFCA OOFR-CA.csv",
##         "OOMRA-OOMCA OOMR-CA.csv",
##         "SOFRA-SOFCA SOFR-CA.csv",
##         "SOMRA-SOMCA SOMR-CA.csv")


##i=1
##while(i<=36){
##  compare = makeContrasts(strsplit(comp[i],split=" ")[[1]][1], levels=design)
##  lrt <- glmLRT(fit,contrast=as.vector(compare))		
##  G_X_E<-topTags(lrt,adjust.method="BH",n = nrow(z$counts), sort.by="PValue")
##  DEGs=G_X_E$table
##  write.csv(DEGs, paste0("/Users/johncsantiago/Documents/GitHub/Fly-Body-Sections-Project/Data/",strsplit(comp[i],split=" ")[[1]][2]))
##  i=i+1
##}


git.dir = "https://raw.githubusercontent.com/johncsantiago/Fly-Body-Sections-Project/master/Data/"
cpmdata = read.csv(paste0(git.dir,"cpmdata.csv"),row.names = 1)
groups  = read.csv(paste0(git.dir,"groups.csv"),row.names = 1)
convert = read.csv(paste0(git.dir,"convert.csv"),row.names = 1)

edger.outs = c(
  ##Head Genotype  
  "SO-OOFCH.csv", 
  "SO-OOMCH.csv",
  "SO-OOFRH.csv",
  "SO-OOMRH.csv", 
  ##Head Sex
  "OOM-FCH.csv",
  "OOM-FRH.csv",
  "SOM-FCH.csv",
  "SOM-FRH.csv",  
  ##Head Treatment
  "OOFR-CH.csv",
  "OOMR-CH.csv",
  "SOFR-CH.csv",
  "SOMR-CH.csv",
  ##Thorax Genotype         
  "SO-OOFCT.csv", 
  "SO-OOMCT.csv",
  "SO-OOFRT.csv",
  "SO-OOMRT.csv", 
  ##Thorax Sex
  "OOM-FCT.csv",
  "OOM-FRT.csv",
  "SOM-FCT.csv",
  "SOM-FRT.csv",  
  ##Thorax Treatment
  "OOFR-CT.csv",
  "OOMR-CT.csv",
  "SOFR-CT.csv",
  "SOMR-CT.csv",
  ##Abdomen Genotype         
  "SO-OOFCA.csv", 
  "SO-OOMCA.csv",
  "SO-OOFRA.csv",
  "SO-OOMRA.csv", 
  ##Abdomen Sex
  "OOM-FCA.csv",
  "OOM-FRA.csv",
  "SOM-FCA.csv",
  "SOM-FRA.csv",  
  ##Abdomen Treatment
  "OOFR-CA.csv",
  "OOMR-CA.csv",
  "SOFR-CA.csv",
  "SOMR-CA.csv")

sigdata=data.frame(cpmdata[,1:36])
colnames(sigdata)=edger.outs
i=1
while(i<=36){
  temp= read.csv(paste0(git.dir,edger.outs[i]),row.names = 1)
  sigdata[,i]=temp[row.names(sigdata),"FDR"]
  i=i+1
}

