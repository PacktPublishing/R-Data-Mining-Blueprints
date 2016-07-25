###################################
x1<-c(2.5,1.4,6.3,4.6,9.0)
class(x1)
mode(x1)
length(x1)


##############

> x2<-c(TRUE,FALSE,TRUE,FALSE,FALSE)
> class(x2)
[1] "logical"
> mode(x2)
[1] "logical"
> length(x2)
[1] 5

###################
> x3<-c("DataMining","Statistics","Analytics","Projects","MachineLearning")
> class(x3)
[1] "character"
> length(x3)
[1] 5 [
  
######################
> domains<-c("DataMining","Statistics","Analytics","Projects","MachineLearning",
             + "DataMining","Statistics","Analytics","Projects","MachineLearning",
             + "DataMining","Statistics","Analytics","Projects","MachineLearning",
             + "DataMining","Statistics","Analytics","Projects","MachineLearning",
             + "DataMining","Statistics","Analytics","Projects","MachineLearning")
> factor(domains)
[1] DataMining      Statistics      Analytics       Projects        MachineLearning DataMining     
[7] Statistics      Analytics       Projects        MachineLearning DataMining      Statistics     
[13] Analytics       Projects        MachineLearning DataMining      Statistics      Analytics      
[19] Projects        MachineLearning DataMining      Statistics      Analytics       Projects       
[25] MachineLearning
Levels: Analytics DataMining MachineLearning Projects Statistics6        

#####################################
> x<-data.frame(x1,x2,x3)
> class(x)
[1] "data.frame"
> print(x)
x1    x2              x3
1 12  TRUE       Analytics
2 13 FALSE      DataMining
3 24  TRUE MachineLearning
4 54 FALSE        Projects
5 29  TRUE      Statistics
##################################

> x1<-c(2.5,1.4,6.3,4.6,9.0)
> class(x1)
[1] "numeric"
> x1<-c(2.5,1.4,6.3,4.6,9.0,"cat")
> class(x1)
[1] "character"

#################################
> ls()

########
> mylist<-list(custid=112233, custname="John R", mobile="989-101-1011",
               + email="JohnR@gmail.com")
> mylist
$custid
[1] 112233

$custname
[1] "John R"

$mobile
[1] "989-101-1011"

$email
[1] "JohnR@gmail.com"

###################################
> mylist[[2]]
[1] "John R"
> mylist[2]
$custname
[1] "John R"

##############################
> mylist1<-list(custid=112233, custname="John R", mobile="989-101-1011",
                + email="JohnR@gmail.com")
> mylist2<-list(custid=443322, custname="Frank S", mobile="781-101-6211",
                + email="SFranks@hotmail.com")
> mylist<-cbind(mylist1,mylist2)
> mylist
mylist1           mylist2              
custid   112233            443322               
custname "John R"          "Frank S"            
mobile   "989-101-1011"    "781-101-6211"       
email    "JohnR@gmail.com" "SFranks@hotmail.com"

###########################
> seq(from=1,to=5,length=4)
[1] 1.000000 2.333333 3.666667 5.000000
> seq(length=10,from=-2,by=.2)
[1] -2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2
> rep(15,10)
[1] 15 15 15 15 15 15 15 15 15 15
> gl(2,5,labels=c('Buy','DontBuy'))
[1] Buy     Buy     Buy     Buy     Buy     DontBuy DontBuy DontBuy DontBuy
[10] DontBuy
Levels: Buy DontBuy

########################

> getwd()
[1] "C:/Users/Documents"
> setwd("C:/Users/Documents")

#############################

> dt<-read.csv("E:/Datasets/hs0.csv")
> names(dt)
[1] "X0"      "X70"     "X4"      "X1"      "X1.1"    "general" "X57"    
[8] "X52"     "X41"     "X47"     "X57.1"

###############################
> data<- read.table("E:/Datasets/hs0.csv",header=T,sep=",")
> names(data)
[1] "X0"      "X70"     "X4"      "X1"      "X1.1"    "general" "X57"    
[8] "X52"     "X41"     "X47"     "X57.1"

#################################
> library(xlsx)
Loading required package: rJava
Loading required package: xlsxjars
> library(xlsxjars)
> dat<-read.xlsx("E:/Datasets/hs0.xls","hs0")
> head(dat)
gender  id race ses schtyp  prgtype read write math science socst
1      0  70    4   1      1  general   57    52   41      47    57
2      1 121    4   2      1   vocati   68    59   53      63    61
3      0  86    4   3      1  general   44    33   54      58    31
4      0 141    4   3      1   vocati   63    44   47      53    56
5      0 172    4   2      1 academic   47    52   57      53    61
6      0 113    4   2      1 academic   44    52   51      63    61

##########################################
> library(Hmisc)
> mydata <- spss.get("E:/Datasets/wage.sav", use.value.labels=TRUE)
> head(mydata)
HRS  RATE ERSP ERNO NEIN ASSET  AGE   DEP RACE SCHOOL
1 2157 2.905 1121  291  380  7250 38.5 2.340 32.1   10.5
2 2174 2.970 1128  301  398  7744 39.3 2.335 31.2   10.5
3 2062 2.350 1214  326  185  3068 40.1 2.851   NA    8.9
4 2111 2.511 1203   49  117  1632 22.4 1.159 27.5   11.5
5 2134 2.791 1013  594  730 12710 57.7 1.229 32.5    8.8
6 2185 3.040 1135  287  382  7706 38.6 2.602 31.4   10.7

> library(sas7bdat)
> mydata <- read.sas7bdat("E:/Datasets/sales.sas7bdat")
> head(mydata)
YEAR NET_SALES PROFIT
1 1990       900    123
2 1991       800    400
3 1992       700    300
4 1993       455     56
5 1994       799    299
6 1995       666    199

######################################

> is.numeric(x1)
[1] TRUE
> is.character(x3)
[1] TRUE
> is.vector(x1)
[1] TRUE
> is.matrix(x)
[1] FALSE
> is.data.frame(x)
[1] TRUE

#####################
> as.numeric(x1)
[1] 2.5 1.4 6.3 4.6 9.0
> as.vector(x2)
[1]  TRUE FALSE  TRUE FALSE FALSE
> as.matrix(x)
x1    x2      x3                x4  x5     
[1,] "2.5" " TRUE" "DataMining"      "1" "1+ 0i"
[2,] "1.4" "FALSE" "Statistics"      "2" "6+ 5i"
[3,] "6.3" " TRUE" "Analytics"       "3" "2+ 2i"
[4,] "4.6" "FALSE" "Projects"        "4" "4+ 1i"
[5,] "9.0" "FALSE" "MachineLearning" "5" "6+55i"
> as.data.frame(x)
x1    x2              x3 x4    x5
1 2.5  TRUE      DataMining  1 1+ 0i
2 1.4 FALSE      Statistics  2 6+ 5i
3 6.3  TRUE       Analytics  3 2+ 2i
4 4.6 FALSE        Projects  4 4+ 1i
5 9.0 FALSE MachineLearning  5 6+55i
> as.character(x2)
[1] "TRUE"  "FALSE" "TRUE"  "FALSE" "FALSE"

###########################
> as.factor(x2)
[1] TRUE  FALSE TRUE  FALSE FALSE
Levels: FALSE TRUE

###########################
> # Sorting and Merging Data
  > ArtPiece<-read.csv("ArtPiece.csv")
> names(ArtPiece)
[1] "Cid"                        "Critic.Ratings"             "Acq.Cost"                  
[4] "Art.Category"               "Art.Piece.Size"             "Border.of.art.piece"       
[7] "Art.Type"                   "Prominent.Color"            "CurrentAuctionAveragePrice"
[10] "Brush"                      "Brush.Size"                 "Brush.Finesse"             
[13] "Art.Nationality"            "Top.3.artists"              "CollectorsAverageprice"    
[16] "Min.Guarantee.Cost"        
> attach(ArtPiece)

> sort(Critic.Ratings)
[1] 4.9921 5.0227 5.2106 5.2774 5.4586 5.5711 5.6300 5.7723 5.9789 5.9858 6.5078 6.5328
[13] 6.5393 6.5403 6.5617 6.5663 6.5805 6.5925 6.6536 6.8990 6.9367 7.1254 7.2132 7.2191
[25] 7.3291 7.3807 7.4722 7.5156 7.5419 7.6173 7.6304 7.6586 7.7694 7.8241 7.8434 7.9315
[37] 7.9576 8.0064 8.0080 8.0736 8.0949 8.1054 8.2944 8.4498 8.4872 8.6889 8.8958 8.9046
[49] 9.3593 9.8130

> sort(Critic.Ratings, decreasing = T)
[1] 9.8130 9.3593 8.9046 8.8958 8.6889 8.4872 8.4498 8.2944 8.1054 8.0949 8.0736 8.0080
[13] 8.0064 7.9576 7.9315 7.8434 7.8241 7.7694 7.6586 7.6304 7.6173 7.5419 7.5156 7.4722
[25] 7.3807 7.3291 7.2191 7.2132 7.1254 6.9367 6.8990 6.6536 6.5925 6.5805 6.5663 6.5617
[37] 6.5403 6.5393  6.5328 6.5078 5.9858 5.9789 5.7723 5.6300 5.5711 5.4586 5.2774 5.2106
[49] 5.0227 4.9921

> i2<-ArtPiece[order(Critic.Ratings,Acq.Cost),1:5]
> head(i2)
Cid Critic.Ratings Acq.Cost          Art.Category Art.Piece.Size
9    9         4.9921    39200             Vintage I  26in. X 18in.
50  50         5.0227    52500        Portrait Art I  26in. X 24in.
26  26         5.2106    31500           Dark Art II    1in. X 7in.
45  45         5.2774    79345             Gothic II   9in. X 29in.
21  21         5.4586    33600  Abstract Art Type II  29in. X 29in.
38  38         5.5711    35700 Abstract Art Type III   9in. X 12in

> i2<-ArtPiece[order(Border.of.art.piece, na.last = F),2:6]
> head(i2)
Critic.Ratings Acq.Cost         Art.Category Art.Piece.Size Border.of.art.piece
18         7.5156    34300          Vintage III   29in. X 6in.                    
43         6.8990    59500 Abstract Art Type II  23in. X 21in.                    
1          8.9046    49700  Abstract Art Type I  17in. X 27in.            Border 1
12         7.5419    37100        Silhoutte III   28in. X 9in.           Border 10
14         7.1254    54600           Vintage II   9in. X 12in.           Border 11
16         7.2132    23100           Dark Art I  10in. X 22in.           Border 11

> A<-audit[,c(1,2,3,7,9)]
> names(A)
[1] "ID"         "Age"        "Employment" "Income"     "Deductions"
> B<-audit[,c(1,3,4,5,6)]
> names(B)
[1] "ID"         "Employment" "Education"  "Marital"    "Occupation"

> head(merge(A,B),3)
ID Employment Age   Income Deductions Education   Marital Occupation
1 1004641    Private  38  81838.0          0   College Unmarried    Service
2 1010229    Private  35  72099.0          0 Associate    Absent  Transport
3 1024587    Private  32 154676.7          0    HSgrad  Divorced   Clerical

> head(merge(A,B, all=F),3)
ID Employment Age   Income Deductions Education Marital Occupation
1 1044221    Private  60  7568.23          0   College Married  Executive
2 1047095    Private  74 33144.40          0    HSgrad Married    Service
3 1047698    Private  43 43391.17          0  Bachelor Married  Executive

> head(merge(A,B, all=T),3)
ID Employment Age   Income Deductions Education Marital Occupation
1 1004641    Private  38  81838.0          0      <NA>    <NA>       <NA>
  2 1010229    Private  35  72099.0          0      <NA>    <NA>       <NA>
  3 1024587    Private  32 154676.7          0      <NA>    <NA>       <NA>
  
  
  > head(merge(A,B, all.x = T),3)
ID Employment Age   Income Deductions Education Marital Occupation
1 1004641    Private  38  81838.0          0      <NA>    <NA>       <NA>
  2 1010229    Private  35  72099.0          0      <NA>    <NA>       <NA>
  3 1024587    Private  32 154676.7          0      <NA>    <NA>       <NA>
  
  > head(merge(A,B, all.y = T),3)
ID Employment Age   Income Deductions Education Marital Occupation
1 1044221    Private  60  7568.23          0   College Married  Executive
2 1047095    Private  74 33144.40          0    HSgrad Married    Service
3 1047698    Private  43 43391.17          0  Bachelor Married  Executive
> head(merge(A,B,by="ID"),3)
ID Age Employment.x   Income Deductions Employment.y Education Marital Occupation
1 1044221  60      Private  7568.23          0      Private   College Married  Executive
2 1047095  74      Private 33144.40          0      Private    HSgrad Married    Service
3 1047698  43      Private 43391.17          0      Private  Bachelor Married  Executive

> head(merge(A,B,by=c("ID","Employment")),3)
ID Employment Age   Income Deductions Education Marital Occupation
1 1044221    Private  60  7568.23          0   College Married  Executive
2 1047095    Private  74 33144.40          0    HSgrad Married    Service
3 1047698    Private  43 43391.17          0  Bachelor Married  Executive

> A<-audit[,c(2,7,9)]
> names(A)
[1] "Age"        "Income"     "Deductions"
> B<-audit[,c(4,5,6)]
> names(B)
[1] "Education"  "Marital"    "Occupation"
> head(cbind(A,B),3)
Age   Income Deductions Education   Marital Occupation
1  38  81838.0          0   College Unmarried    Service
2  35  72099.0          0 Associate    Absent  Transport
3  32 154676.7          0    HSgrad  Divorced   Clerical

###########################################
> newdata <- audit[ which(audit$Gender=="Female" & audit$Age > 65), ]
> rownames(newdata)
[1] "49"   "537"  "552"  "561"  "586"  "590"  "899"  "1200" "1598" "1719"

#####################
> newdata <- subset(audit, Gender=="Female" & Age > 65, select=Employment:Income)
> rownames(newdata)
[1] "49"   "537"  "552"  "561"  "586"  "590"  "899"  "1200" "1598" "1719"

########################
> Sys.time()
[1] "2015-11-10 00:43:22 IST"
> dt<-as.Date(Sys.time())
> class(dt)
[1] "Date"

####################
> weekdays(as.Date(Sys.time()))
[1] "Monday"
> months(as.Date(Sys.time()))
[1] "November"
> quarters(as.Date(Sys.time()))
[1] "Q4"
> substr(as.POSIXct(as.Date(Sys.time())),1,4)
[1] "2015"

#####################
> format(Sys.time(),format = "%m %d %y")
[1] "11 10 15"

################
> int<-seq(1:20)
> int
[1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
> myfunc<-function(x){x*x}
> myfunc(int)
[1]   1   4   9  16  25  36  49  64  81 100 121 144 169 196 225 256 289 324 361 400

###################
x<-100:200
y <- NULL # NULL vector as placeholder
for(i in seq(along=x)) {
  if(x[i] < 150) {
    y <- c(y, x[i] - 50)
  } else {
    y <- c(y, x[i] + 50)
  }
}
print(y)

#######################
x <- 100
repeat {
  print(x)
  x = sqrt(x)+10
  if (x > 2.6){
    break
  }
}

#####################
x <- 10
while (x < 60) {
  print(x)
  x = x+10
}

###############
> apply(ArtPiece[,2:3],2,mean)
Critic.Ratings       Acq.Cost 
7.200416   44440.900000 
> apply(ArtPiece[,2:3],1,mean)
[1] 24854.45 26604.68 17153.69 14353.28 14003.47 19604.05 14703.27 15753.29 19602.50
[10] 26954.24 19254.00 18553.77 18903.97 27303.56 24153.74 11553.61 23804.04 17153.76
[19] 19953.30 24854.22 16802.73 20303.33 14354.91 26952.99 24503.28 15752.61 28004.45
[28] 30803.81 29403.27 19604.00 29053.88 17152.81 33253.91 24502.89 37453.92 12604.15
[37] 21353.82 17852.79 28703.83 29753.25 23453.27 18204.34 29753.45 27654.05 39675.14
[46] 24853.61 16102.99 13653.98 14353.66 26252.51

################
> lapply(ArtPiece[,2:3],mean)
$Critic.Ratings
[1] 7.200416

$Acq.Cost
[1] 44440.9

################
> sapply(ArtPiece[,2:3],mean)
Critic.Ratings       Acq.Cost 
7.200416   44440.900000

#############
> head(tapply(Critic.Ratings,Acq.Cost,summary),3)
$`23100`
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7.213   7.213   7.213   7.213   7.213   7.213 

$`25200`
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
8.294   8.294   8.294   8.294   8.294   8.294 

$`27300`
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
7.958   7.958   7.958   7.958   7.958   7.958

###############
> x<-"data Mining is not a difficult subject, anyone can master the subject"
> class(x)
[1] "character"
> substr(x, 1, 12)
[1] "data Mining "

#################
> sub("data mining", "The Data Mining", x, ignore.case =T, fixed=FALSE)
[1] "The Data Mining is not a difficult subject, anyone can master the subject"
> strsplit(x, "")
[[1]]
[1] "d" "a" "t" "a" " " "M" "i" "n" "i" "n" "g" " " "i" "s" " " "n" "o" "t" " " "a" " "
[22] "d" "i" "f" "f" "i" "c" "u" "l" "t" " " "s" "u" "b" "j" "e" "c" "t" "," " " "a" "n"
[43] "y" "o" "n" "e" " " "c" "a" "n" " " "m" "a" "s" "t" "e" "r" " " "t" "h" "e" " " "s"
[64] "u" "b" "j" "e" "c" "t"

##################
> x<-c(12,13,14,21,23,24,NA,25,NA,0,NA)
> is.na(x)
[1] FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE
> mean(x,na.rm=TRUE)
[1] 16.5
> mean(x)
[1] NA

#############
