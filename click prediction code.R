# Machine Learning Final Project
# Team 4 - Emmy, Neha, Boping
# Data Exploration


# TRAINING DATA EXPLORATION --------------------------------------------------------

# Read in the Training Data
setwd("C:/Emory/Machine Learning/ProjectTrainingData.csv")
Labs <- scan("C:/Emory/Machine Learning/ProjectTrainingData.csv", nlines=1, sep=",", what="abc")
TrainData <- matrix(scan("C:/Emory/Machine Learning/TrainData.csv", skip=1, sep=",", what="abc"), ncol=length(Labs), byrow=T)
ValData <- matrix(scan("C:/Emory/Machine Learning/ValData.csv", skip=1, sep=",", what="abc"), ncol=length(Labs), byrow=T)
TestData <- matrix(scan("C:/Emory/Machine Learning/ProjectTestData.csv", skip=1, sep=",", what="abc"), ncol=length(Labs)-1, byrow=T)
# If error thrown - "cannot allocate vector of size ___" use...
memory.limit(size=10000000)


# Label all of the columns in the matrix so that you can call them by name
colnames(TrainData) <- Labs
colnames(ValData) <- Labs

LabsT <- scan("C:/Emory/Machine Learning/ProjectTestData.csv", nlines=1, sep=",", what="abc")
colnames(TestData) <- LabsT
# How big is the Training Data?
dim(Data)
# 31,991,090 instances; 24 variables

# What is the balance between the frequency of click = 1 vs click = 0 in training data?
table(Data[,'click'])
# 0 (no click) = 26556113 (83%)
# 1 (click) = 5434977 (17%)

# Get a count of how many categories exist per variable (for 6 million records - 20% of the data, enough data that we know that not all instances drawn will be click = 1 or click = 0)
# y-variable - binomial
table(Data[1:6000000,'click']) # ~5mil instances of 0 (83%), ~1mil instances of 1 (17%) - this is a good rep of the data!
# x-variables - will come back and write this as For Loop
length(table(Data[1:6000000,'C1'])) #c1 has 7
length(table(Data[1:6000000,'banner_pos'])) #banner_pos has 7
length(table(Data[1:6000000,'site_id'])) #site_id has 3175 
length(table(Data[1:6000000,'site_domain'])) #site_domain has 3949
length(table(Data[1:6000000,'site_category'])) #site_category has 23
length(table(Data[1:6000000,'app_id'])) #app_id has 4768
length(table(Data[1:6000000,'app_domain'])) #app_domain has 333
length(table(Data[1:6000000,'app_category'])) #app_category has 32
length(table(Data[1:6000000,'device_id'])) #device_id has 505533
length(table(Data[1:6000000,'device_model'])) #device_model has 6446
length(table(Data[1:6000000,'device_type'])) #device_type has 4
length(table(Data[1:6000000,'device_conn_type'])) #device_conn_type has 4
length(table(Data[1:6000000,'C14'])) #c14 has 891
length(table(Data[1:6000000,'C15'])) #c15 has 8
length(table(Data[1:6000000,'C16'])) #c16 has 9
length(table(Data[1:6000000,'C17'])) #c17 has 208
length(table(Data[1:6000000,'C18'])) #c18 has 4
length(table(Data[1:6000000,'C19'])) #c19 has 44
length(table(Data[1:6000000,'C20'])) #c20 has 168
length(table(Data[1:6000000,'C21'])) #c21 has 41


####Tree####
library(tree)

TDF <- data.frame(click=as.factor(TrainData[,'click']), 
                  device_type=as.factor(TrainData[,'device_type']),
                  banner_pos = as.factor(TrainData[,'banner_pos']),
                  site_category = as.factor(TrainData[,'site_category']),
                  device_conn_type = as.factor(TrainData[,'device_conn_type']),
                  app_category = as.factor(TrainData[,'app_category']),
                  C15 = as.factor(TrainData[,'C15']), 
                  C18 = as.factor(TrainData[,'C18']),
                  C19 = as.factor(TrainData[,'C19']), 
                  C21 = as.factor(TrainData[,'C21'])
                  )

#T1DF <- TDF[1:5000000,]

##Set least common sub-catgories to the one sub-catgory 'other'.
##Here we only need to set 3 variables because only 3 variables have sub-categories greater than 32
##(32 is the upper limit for tree) 

#For app_category, we take the threshold of 0
ta <- tapply(as.integer(TrainData[,'click']), TrainData[,'app_category'],FUN=mean)
#Here we run the tapply using dataframe, so the output is the number instead of the probability
#but the order is exactly the same, so we only need to make sure the threshold, which is 1 (equal to 0 above).
ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'app_category']),FUN=mean)
sort(ta, decreasing = T)
#Here we set the thresild to be the 0, 
wht <- TDF[,'app_category'] %in% names(ta[ta < 1.01])
TDF[,'app_category'] <- as.character(TDF[,'app_category'])
TDF[wht,'app_category'] <- "other"
TDF[,'app_category'] <- as.factor(TDF[,'app_category'])

#For C19, we take the threhold of 0.13
ta <- tapply(as.integer(TrainData[,'click']), TrainData[,'C19'],FUN=mean)
#Here we run the tapply using dataframe, so the output is the number instead of the probability
#but the order is exactly the same, so we only need to make sure the threshold, which is 1.13 (equal to 0.13 above).
ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C19']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'C19'] %in% names(ta[ta < 1.13])
TDF[,'C19'] <- as.character(TDF[,'C19'])
TDF[wht,'C19'] <- "other"
TDF[,'C19'] <- as.factor(TDF[,'C19'])

#For C21, we take the threhold of 0.13
ta <- tapply(as.integer(TrainData[,'click']), TrainData[,'C21'],FUN=mean)
#Here we run the tapply using dataframe, so the output is the number instead of the probability
#but the order is exactly the same, so we only need to make sure the threshold, which is 1.13 (equal to 0.13 above).
ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C21']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'C21'] %in% names(ta[ta < 1.13])
TDF[,'C21'] <- as.character(TDF[,'C21'])
TDF[wht,'C21'] <- "other"
TDF[,'C21'] <- as.factor(TDF[,'C21'])

#We plan to use 6 milion data based on Pareto Principle 
T1DF <- TDF[1:6000000,]


# Remove row with column labels
which(grepl('click',T1DF[,1]))
T1DF <- T1DF[-c(2416438),]

#Tree
tc <- tree.control(nrow(T1DF),minsize=2,mincut=1,mindev=0.00)
out <- tree(click ~ .,data=T1DF,control=tc)


#Val data
VDF <- data.frame(click=as.factor(ValData[,'click']), 
                  device_type=as.factor(ValData[,'device_type']),
                  banner_pos = as.factor(ValData[,'banner_pos']),
                  site_category = as.factor(ValData[,'site_category']),
                  device_conn_type = as.factor(ValData[,'device_conn_type']),
                  app_category = as.factor(ValData[,'app_category']),
                  C15 = as.factor(ValData[,'C15']), 
                  C18 = as.factor(ValData[,'C18']),
                  C19 = as.factor(ValData[,'C19']), 
                  C21 = as.factor(ValData[,'C21'])
                  )
TDF <- data.frame(click=as.factor(TrainData[,'click']), 
                  device_type=as.factor(TrainData[,'device_type']),
                  banner_pos = as.factor(TrainData[,'banner_pos']),
                  site_category = as.factor(TrainData[,'site_category']),
                  device_conn_type = as.factor(TrainData[,'device_conn_type']),
                  app_category = as.factor(TrainData[,'app_category']),
                  C15 = as.factor(TrainData[,'C15']), 
                  C18 = as.factor(TrainData[,'C18']),
                  C19 = as.factor(TrainData[,'C19']), 
                  C21 = as.factor(TrainData[,'C21'])
)

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'app_category']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'app_category'] %in% names(ta[ta < 1.01])
VDF[,'app_category'] <- as.character(VDF[,'app_category'])
VDF[wht,'app_category'] <- "other"
VDF[,'app_category'] <- as.factor(VDF[,'app_category'])


ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C19']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'C19'] %in% names(ta[ta < 1.13])
VDF[,'C19'] <- as.character(VDF[,'C19'])
VDF[wht,'C19'] <- "other"
VDF[,'C19'] <- as.factor(VDF[,'C19'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C21']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'C21'] %in% names(ta[ta < 1.13])
VDF[,'C21'] <- as.character(VDF[,'C21'])
VDF[wht,'C21'] <- "other"
VDF[,'C21'] <- as.factor(VDF[,'C21'])


#Make the sub-catgories of val data consistent with train data
levels(VDF$site_category) <- levels(T1DF$site_category)
levels(VDF$app_category) <- levels(T1DF$app_category)
levels(VDF$C15) <- levels(T1DF$C15)
levels(VDF$C18) <- levels(T1DF$C18)
levels(VDF$C19) <- levels(T1DF$C19)
levels(VDF$C21) <- levels(T1DF$C21)
levels(VDF$banner_pos) <- levels(T1DF$banner_pos)
levels(VDF$device_type) <- levels(T1DF$device_type)
levels(VDF$device_conn_type) <- levels(T1DF$device_conn_type)

#Val log loss
ValAcc <- rep(NA,12)
for(NNode in seq(2,25,by=1)) {
  cat("\rNNode =",NNode)
  out1 <- prune.tree(out,best=NNode)
  PHat <- predict(out1,newdata=VDF)
  ValAcc[NNode] <- LogLoss(PHat[,'1'],VDF$click)
}

BestK <- which.min(ValAcc)
BestK

out1 <- prune.tree(out, best = 5)

ValPre <- predict(out1, newdata = VDF)
LogLoss(ValPre[,'1'],VDF$click)

#Train log loss
TrainPre <- predict(out1, newdata = T1DF)
LogLoss(TrainPre[,'1'],T1DF$click)


#Test Data
TSDF <- data.frame( 
                  device_type=as.factor(TestData[,'device_type']),
                  banner_pos = as.factor(TestData[,'banner_pos']),
                  site_category = as.factor(TestData[,'site_category']),
                  device_conn_type = as.factor(TestData[,'device_conn_type']),
                  app_category = as.factor(TestData[,'app_category']),
                  C15 = as.factor(TestData[,'C15']), 
                  C18 = as.factor(TestData[,'C18']),
                  C19 = as.factor(TestData[,'C19']), 
                  C21 = as.factor(TestData[,'C21'])
)


TDF <- data.frame(click=as.factor(TrainData[,'click']), 
                  device_type=as.factor(TrainData[,'device_type']),
                  banner_pos = as.factor(TrainData[,'banner_pos']),
                  site_category = as.factor(TrainData[,'site_category']),
                  device_conn_type = as.factor(TrainData[,'device_conn_type']),
                  app_category = as.factor(TrainData[,'app_category']),
                  C15 = as.factor(TrainData[,'C15']), 
                  C18 = as.factor(TrainData[,'C18']),
                  C19 = as.factor(TrainData[,'C19']), 
                  C21 = as.factor(TrainData[,'C21'])
)



ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'app_category']),FUN=mean)
sort(ta, decreasing = T)
#Change Test data
wht <- TSDF[,'app_category'] %in% names(ta[ta >1])
TSDF[,'app_category'] <- as.character(TSDF[,'app_category'])
TSDF[!wht,'app_category'] <- "other"
TSDF[,'app_category'] <- as.factor(TSDF[,'app_category'])


ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C19']),FUN=mean)
sort(ta, decreasing = T)
#Change Test data
wht <- TSDF[,'C19'] %in% names(ta[ta >= 1.13])
TSDF[,'C19'] <- as.character(TSDF[,'C19'])
TSDF[!wht,'C19'] <- "other"
TSDF[,'C19'] <- as.factor(TSDF[,'C19'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C21']),FUN=mean)
sort(ta, decreasing = T)
#Change Test data
wht <- TSDF[,'C21'] %in% names(ta[ta >= 1.13])
TSDF[,'C21'] <- as.character(TSDF[,'C21'])
TSDF[!wht,'C21'] <- "other"
TSDF[,'C21'] <- as.factor(TSDF[,'C21'])

#Make the sub-catgories of Test data consistent with Train data
levels(TSDF$site_category) <- levels(T1DF$site_category)
levels(TSDF$app_category) <- levels(T1DF$app_category)
levels(TSDF$C15) <- levels(T1DF$C15)
levels(TSDF$C18) <- levels(T1DF$C18)
levels(TSDF$C19) <- levels(T1DF$C19)
levels(TSDF$C21) <- levels(T1DF$C21)
levels(TSDF$banner_pos) <- levels(T1DF$banner_pos)
levels(TSDF$device_type) <- levels(T1DF$device_type)
levels(TSDF$device_conn_type) <- levels(T1DF$device_conn_type)


#Test pre
Tpre <- predict(out1, newdata = TSDF)




#####Random Forest#####
if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }

TDF <- data.frame(click=as.factor(TrainData[,'click']), 
                  device_type=as.factor(TrainData[,'device_type']),
                  banner_pos = as.factor(TrainData[,'banner_pos']),
                  site_category = as.factor(TrainData[,'site_category']),
                  device_conn_type = as.factor(TrainData[,'device_conn_type']),
                  app_category = as.factor(TrainData[,'app_category']),
                  C15 = as.factor(TrainData[,'C15']), 
                  C18 = as.factor(TrainData[,'C18']),
                  C19 = as.factor(TrainData[,'C19']), 
                  C21 = as.factor(TrainData[,'C21'])
)

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'app_category']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'app_category'] %in% names(ta[ta > 1])
TDF[,'app_category'] <- as.character(TDF[,'app_category'])
TDF[!wht,'app_category'] <- "other"
TDF[,'app_category'] <- as.factor(TDF[,'app_category'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C19']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'C19'] %in% names(ta[ta >= 1.13])
TDF[,'C19'] <- as.character(TDF[,'C19'])
TDF[!wht,'C19'] <- "other"
TDF[,'C19'] <- as.factor(TDF[,'C19'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C21']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'C21'] %in% names(ta[ta >= 1.13])
TDF[,'C21'] <- as.character(TDF[,'C21'])
TDF[!wht,'C21'] <- "other"
TDF[,'C21'] <- as.factor(TDF[,'C21'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'site_category']),FUN=mean)
sort(ta, decreasing = T)
wht <- TDF[,'site_category'] %in% names(ta[ta > 1])
TDF[,'site_category'] <- as.character(TDF[,'site_category'])
TDF[!wht,'site_category'] <- "other"
TDF[,'site_category'] <- as.factor(TDF[,'site_category'])

T1DF <- TDF[1:6000000,]

# Remove row with column labels
which(grepl('click',T1DF[,1]))
T1DF <- T1DF[-c(2416438),]


VDF <- data.frame(click=as.factor(ValData[,'click']), 
                  device_type=as.factor(ValData[,'device_type']),
                  banner_pos = as.factor(ValData[,'banner_pos']),
                  site_category = as.factor(ValData[,'site_category']),
                  device_conn_type = as.factor(ValData[,'device_conn_type']),
                  app_category = as.factor(ValData[,'app_category']),
                  C15 = as.factor(ValData[,'C15']), 
                  C18 = as.factor(ValData[,'C18']),
                  C19 = as.factor(ValData[,'C19']), 
                  C21 = as.factor(ValData[,'C21'])
)
TDF <- data.frame(click=as.factor(TrainData[,'click']), 
                  device_type=as.factor(TrainData[,'device_type']),
                  banner_pos = as.factor(TrainData[,'banner_pos']),
                  site_category = as.factor(TrainData[,'site_category']),
                  device_conn_type = as.factor(TrainData[,'device_conn_type']),
                  app_category = as.factor(TrainData[,'app_category']),
                  C15 = as.factor(TrainData[,'C15']), 
                  C18 = as.factor(TrainData[,'C18']),
                  C19 = as.factor(TrainData[,'C19']), 
                  C21 = as.factor(TrainData[,'C21'])
)

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'app_category']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'app_category'] %in% names(ta[ta >= 1.01])
VDF[,'app_category'] <- as.character(VDF[,'app_category'])
VDF[!wht,'app_category'] <- "other"
VDF[,'app_category'] <- as.factor(VDF[,'app_category'])


ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C19']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'C19'] %in% names(ta[ta >= 1.13])
VDF[,'C19'] <- as.character(VDF[,'C19'])
VDF[!wht,'C19'] <- "other"
VDF[,'C19'] <- as.factor(VDF[,'C19'])

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'C21']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'C21'] %in% names(ta[ta >= 1.13])
VDF[,'C21'] <- as.character(VDF[,'C21'])
VDF[!wht,'C21'] <- "other"
VDF[,'C21'] <- as.factor(VDF[,'C21'])

#Check the sub-catgories between Val and Train 
levels(VDF$site_category) %in% levels(T2DF$site_category)
levels(VDF$app_category) %in% levels(T2DF$app_category)
levels(VDF$C19) %in% levels(T2DF$C19)
levels(VDF$C21) %in% levels(T2DF$C21)
levels(VDF$banner_pos) %in% levels(T2DF$banner_pos)
levels(VDF$device_type) %in% levels(T2DF$device_type)
levels(VDF$device_conn_type) %in% levels(T2DF$device_conn_type)
levels(VDF$C15) %in% levels(T2DF$C15)
levels(VDF$C18) %in% levels(T2DF$C18)
levels(VDF$click) %in% levels(T2DF$click)

ta <- tapply(as.integer(TDF[,'click']),as.character(TDF[,'site_category']),FUN=mean)
sort(ta, decreasing = T)
#Change validation data
wht <- VDF[,'site_category'] %in% names(ta[ta > 1])
VDF[,'site_category'] <- as.character(VDF[,'site_category'])
VDF[!wht,'site_category'] <- "other"
VDF[,'site_category'] <- as.factor(VDF[,'site_category'])

VTDF <- VDF

levels(VTDF$site_category) <- levels(T2DF$site_category)
levels(VTDF$app_category) <- levels(T2DF$app_category)
levels(VTDF$C15) <- levels(T2DF$C15)
levels(VTDF$C18) <- levels(T2DF$C18)
levels(VTDF$C19) <- levels(T2DF$C19)
levels(VTDF$C21) <- levels(T2DF$C21)
levels(VTDF$banner_pos) <- levels(T2DF$banner_pos)
levels(VTDF$device_type) <- levels(T2DF$device_type)
levels(VTDF$device_conn_type) <- levels(T2DF$device_conn_type)

T2DF <- T1DF[1:500000,]
T2DF$click <- droplevels(T2DF$click)

out2 <- randomForest(click ~ ., data=T2DF, ntree=500)
out2_val_pre <- predict(out2, newdata=VTDF, type="prob")

#Log loss
LogLoss(out2_val_pre[,'1'],VTDF$click)  #result is 13, which means RandomForest is not better than the tree

#Log loss for train
out2_train_pre <- predict(out2, newdata=T1DF, type="prob")
LogLoss(out2_train_pre[,'1'],T1DF$click) 


# LOG LOSS FUNCTION FOR MODEL EVALUATION-------------------------------------------------------------------------

# Write the Log-Loss function to generate Log-Loss values for each prediction method
# This is how we will evaluate which is best

LogLoss <- function(PHat,Click) {
  Y <- as.integer(Click)
  eps <- 1e-15
  out <- -mean(Y*log(pmax(PHat,eps))+(1-Y)*log(pmax(1-PHat,eps)))
  return(out)
}

# FINAL SUBMISSION -----------------------------------------------------------------------

# Final Submission - loading predicted probabilities into column 2 of final csv doc

# Read in the submission file with correct data types
Data <- read.table("C:/Emory/Machine Learning/ProjectSubmission-Team4.csv",colClasses=c("character","numeric"),header=T,sep=",")

# Your code here puts the probabilities in Data[[2]]
Data <- data.frame(id = Data[,'id'],
                   p = Tpre[,'1'])

# Round to 10 digits accuracy and prevent scientific notation.
# This converts Data[[2]] to strings.
Data[[2]] <- format(round(Data[[2]],10), scientific = FALSE)

# Write out the data in the correct format.
write.table(Data,file="C:/Emory/Machine Learning/ProjectSubmission-Team4--.csv",quote=F,sep=",",row.names=F,col.names=c("id","P(click)"))


