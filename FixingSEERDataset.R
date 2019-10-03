library(caTools)
library(dplyr)
library(ggplot2)
library(class)

df <- read.csv("SeerDatasetMinorFinal.csv",stringsAsFactors = FALSE)
df$X <- NULL

colnames(df)[6] <- "Stage.Cancer"

####FIXING RACE
impute_race <- function(race){ #1=Other, 2=White, 3=Black
  outR <- race
  for(i in 1:length(race)){
    if(race[i] == "Other (American Indian/AK Native, Asian/Pacific Islander)"){
      outR[i] <- 1
    }
    else if(race[i] == "White"){
      outR[i] <- 2
    }else{
      outR[i] <- 3
    }
  }
  return(outR)
}

fixed.Race <- impute_race(df$Race)

df$Race <- as.numeric(fixed.Race)

####FIXING MARITAL STATUS
impute_marital_status <- function(ms){ #1=Married, 2=Divorced, 3=Single, 4=Widowed, 5=Separated
  outMS <- ms
  for(i in 1:length(ms)){
    if(ms[i] == "Married (including common law)"){
      outMS[i] <- 1
    }
    else if(ms[i] == "Divorced"){
      outMS[i] <- 2
    }
    else if(ms[i] == "Single (never married)"){
      outMS[i] <- 3
    }
    else if(ms[i] == "Widowed"){
      outMS[i] <- 4
    }
    else{
      outMS[i] <- 5
    }
  }
  return(outMS)
}

fixed.Marital.Status <- impute_marital_status(df$Marital.Status)
df$Marital.Status <- as.numeric(fixed.Marital.Status)


####FIXING T-STAGE
impute_Tstage <- function(Tstage){ #1=T1, 2=T2, 3=T3, 4=T4
  outT <- Tstage
  for(i in 1:length(Tstage)){
    if(Tstage[i] == "T1"){
      outT[i] <- 1
    }
    else if(Tstage[i] == "T2"){
      outT[i] <- 2
    }
    else if(Tstage[i] == "T3"){
      outT[i] <- 3
    }
    else{
      outT[i] <- 4
    }
  }
  return(outT)
}

fixed.Tstage <- impute_Tstage(df$T.Stage)
df$T.Stage <- as.numeric(fixed.Tstage)


####FIXING N-STAGE
impute_Nstage <- function(Nstage){ #1=N1, 2=N2, 3=N3
  outN <- Nstage
  for(i in 1:length(Nstage)){
    if(Nstage[i] == "N1"){
      outN[i] <- 1
    }
    else if(Nstage[i] == "N2"){
      outN[i] <- 2
    }
    else{
      outN[i] <- 3
    }
  }
  return(outN)
}

fixed.Nstage <- impute_Nstage(df$N.Stage)
df$N.Stage <- as.numeric(fixed.Nstage)


####FIXING CANCER STAGE
impute_cancer_stage <- function(cs){ #1=2a, 2=2b, 3=3a, 4=3b, 5=3c
  outCS <- cs
  for(i in 1:length(cs)){
    if(cs[i] == "IIA"){
      outCS[i] <- 1
    }
    else if(cs[i] == "IIB"){
      outCS[i] <- 2
    }
    else if(cs[i] == "IIIA"){
      outCS[i] <- 3
    }
    else if(cs[i] == "IIIB"){
      outCS[i] <- 4
    }
    else{
      outCS[i] <- 5
    }
  }
  return(outCS)
}

fixed.Cancer.Stage <- impute_cancer_stage(df$Stage.Cancer)
df$Stage.Cancer <- as.numeric(fixed.Cancer.Stage)


####FIXING GRADE
impute_Grade <- function(grade){ #1=Grade1, 2=Grade2, 3=Grade3, 4=Grade4
  outG <- grade
  for(i in 1:length(grade)){
    if(grade[i] == "Well differentiated; Grade I"){
      outG[i] <- 1
    }
    else if(grade[i] == "Moderately differentiated; Grade II"){
      outG[i] <- 2
    }
    else if(grade[i] == "Poorly differentiated; Grade III"){
      outG[i] <- 3
    }
    else{
      outG[i] <- 4
    }
  }
  return(outG)
}

fixed.Grade <- impute_Grade(df$Grade)
df$Grade <- as.numeric(fixed.Grade)

####FIXING A-STAGE
impute_Astage <- function(Astage){ #1=Regional, 2=Distant
  outA <- Astage
  for(i in 1:length(Astage)){
    if(Astage[i] == "Regional"){
      outA[i] <- 1
    }
    else{
      outA[i] <- 2
    }
  }
  return(outA)
}

fixed.Astage <- impute_Astage(df$A.Stage)
df$A.Stage <- as.numeric(fixed.Astage)

####FIXING ESTROGEN STATUS
impute_estrogen_status <- function(es){ #1=Positive, 0=Negative
  outES <- es
  for(i in 1:length(es)){
    if(es[i] == "Positive"){
      outES[i] <- 1
    }
    else{
      outES[i] <- 0
    }
  }
  return(outES)
}

fixed.estrogen.status <- impute_estrogen_status(df$Estrogen.Status)
df$Estrogen.Status <- as.numeric(fixed.estrogen.status)


####FIXING PROGESTERONE STATUS
impute_progesterone_status <- function(ps){ #1=Positive, 0=Negative
  outPS <- ps
  for(i in 1:length(ps)){
    if(ps[i] == "Positive"){
      outPS[i] <- 1
    }
    else{
      outPS[i] <- 0
    }
  }
  return(outPS)
}

fixed.progesterone.status <- impute_progesterone_status(df$Progesterone.Status)
df$Progesterone.Status <- as.numeric(fixed.progesterone.status)

write.csv(df,"SEERDatasetMinorFinalFileToBeUsed.csv",row.names = FALSE)
