setwd("~/Dropbox/Consulting/Project1/Data")




## Meal data. plotting Z levels of glucose, insulin, and glucagon for each subject.  Color and title indicate diagnosis of person.

Z_Meal <- read.csv("ZR_AllData_Meal.csv", header=FALSE)

glc_cols <- paste0("glc",1:49)
ins_cols <- paste0("ins",1:49)
ggn_cols <- paste0("ggn",1:49)

z_meal_names <- c(glc_cols, ins_cols, ggn_cols, "classification")

names(Z_Meal) <- z_meal_names

#  plot Meal z values per subject and substance
for (subjnum in 1:76){
  print(subjnum)
  subj <- as.data.frame(cbind(t(Z_Meal[subjnum,1:49]), t(Z_Meal[subjnum,50:98]),  t(Z_Meal[subjnum,99:147])))
  diagnosis <- Z_Meal[subjnum, 148]
  diagnosis_name <- ifelse(diagnosis==1, "NFG_NGT", 
                           ifelse(diagnosis==2, "NFG_IGT", 
                                  ifelse(diagnosis==3, "IFG_NGT", 
                                         ifelse(diagnosis==4, "IFG_IGT",
                                                ifelse(diagnosis==5, "IFG_DM",
                                                       ifelse(diagnosis==6, "DM",
                                                              "Other"))))))
  diagnosis_color <- ifelse(diagnosis==1, "red", 
                            ifelse(diagnosis==2, "orange", 
                                ifelse(diagnosis==3, "yellow", 
                                       ifelse(diagnosis==4, "green",
                                              ifelse(diagnosis==5, "blue",
                                                     ifelse(diagnosis==6, "purple",
                                                            "black"))))))
  rownames(subj) <- 1:49
  names(subj) <- c( 'glc', 'ins', 'ggn')
  par(mfrow=c(3,1)) 
  plot(rownames(subj), subj$glc,
       xlab='t',
       main= paste("Z Glucose Subj", subjnum, diagnosis_name),
       col=diagnosis_color)
  plot(rownames(subj), subj$ins,
       xlab='t'
       ,main="Z Insulin"
       ,col=diagnosis_color)
  plot(rownames(subj), subj$ggn,
       xlab='t'
       ,main='Z Glucagon'
       ,col=diagnosis_color
       )

}




## Overnight data. plotting Z levels of glucose, insulin, and glucagon for each subject.  Color and title indicate diagnosis of person.

Z_Night <- read.csv("ZR_AllData_Night.csv", header=FALSE)

glc_cols <- paste0("glc",1:55)
ins_cols <- paste0("ins",1:55)
ggn_cols <- paste0("ggn",1:55)

z_night_names <- c(glc_cols, ins_cols, ggn_cols, "classification")

names(Z_Night) <- z_night_names

# plot Meal z values per subject and substance
for (subjnum in 1:41){
  print(subjnum)
  subj <- as.data.frame(cbind(t(Z_Night[subjnum,1:55]), t(Z_Night[subjnum,56:110]),  t(Z_Night[subjnum,111:165])))
  diagnosis <- Z_Night[subjnum, 166]
  diagnosis_name <- ifelse(diagnosis==0, "Non-Diabetic", 
                           ifelse(diagnosis==1, "Diabetic", 
                                  "Other"))
  diagnosis_color <- ifelse(diagnosis==0, "red", 
                           ifelse(diagnosis==1, "blue",
                                  "black"))
  rownames(subj) <- 1:55
  names(subj) <- c( 'glc', 'ins', 'ggn')
  par(mfrow=c(3,1)) 
  plot(rownames(subj), subj$glc,
       xlab='t',
       main= paste("Z Glucose Subj", subjnum, diagnosis_name),
       col=diagnosis_color)
  plot(rownames(subj), subj$ins,
       xlab='t'
       ,main="Z Insulin"
       ,col=diagnosis_color)
  plot(rownames(subj), subj$ggn,
       xlab='t'
       ,main='Z Glucagon'
       ,col=diagnosis_color
  )
  
}

