#load data

files<-list.files()
files<-files[grep("txt",files)]

#dataset<-read.table("Natural_Control_Dark.txt")

dataset<-read.table(files[1],header=TRUE)

all_dataset<-dataset

for (fil in files[-1]){
	
	dataset<-read.table(fil,header=TRUE)	
	
	all_dataset<-rbind(all_dataset, dataset)
	
}

attach(all_dataset)


#
#> rcorr(BoutLength,IBendsProportion,type="spearman")
#x     y
#x  1.00 -0.01
#y -0.01  1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> rcorr(BoutLength,GBendsProportion,type="spearman")
#x     y
#x  1.00 -0.03
#y -0.03  1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> rcorr(BoutLength,HBendsProportion,type="spearman")
#x    y
#x 1.00 0.01
#y 0.01 1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> rcorr(BoutLength,JBendsProportion,type="spearman")
#x     y
#x  1.00 -0.01
#y -0.01  1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> rcorr(BoutLength,CBendsProportion,type="spearman")
#x    y
#x 1.00 0.02
#y 0.02 1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> rcorr(BoutLength,OBendsProportion,type="spearman")
#x    y
#x 1.00 0.04
#y 0.04 1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> 
#		> rcorr(BoutLength,ScootsProportion,type="spearman")
#x    y
#x 1.00 0.29
#y 0.29 1.00
#
#n= 7214898 
#
#
#P
#x  y 
#x     0
#y  0   
#> 
		