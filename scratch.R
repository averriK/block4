library(readxl)
library(data.table)
library(xstats)
library(bida)
library(gt)
library(gmsp)
library(buildTable)
PATH <- "data.xlsx"
AUX <- readxl::read_excel(path = PATH,sheet ="DATA" ) |> data.table::as.data.table()

DATA <- rbindlist(
  list(
    data.table(ID="t",PPV=AUX$Vt,PGA=AUX$At),
    data.table(ID="l",PPV=AUX$Vl,PGA=AUX$Al),
    data.table(ID="v",PPV=AUX$Vv,PGA=AUX$Av)
  ))
DATA <- DATA[,.(A=PGA,LnV=log(PPV),LnA=log(PGA),IA=1/PGA)]
AUX <- fitModel(.data=DATA,regression="lm",response="LnV")
PGA <- c(0.174,0.348,0.024,0.091)
PPV <- predict(AUX$model,newdata=data.table(A=PGA,IA=1/PGA,LnA=log(PGA))) |> exp()


DT <- NULL
for (RM in c("lm","rf","kknn")){
  AUX <- fitModel(.data=DATA,regression=RM,response="LnV")
  DT <- rbindlist(list(DT,data.table(ID=RM,R2=AUX$R2,MSE=AUX$MSE,RMSE=AUX$RMSE)))
}


TBL <- DT[,lapply(.SD,function(x){round(x,digits=2)}),by=.(ID)]
TBL[ID=="lm",`:=`(model="Linear Model",p="9-parameters")]
TBL[ID=="rf",`:=`(model="Random Forest",p="non-parametric")]
TBL[ID=="kknn",`:=`(model="K-Nearest Neighbors",p="non-parametric")]

# Example. PPV Ranges for PGA=0.097 g
# PGA <- 0.097
# PPV <- predict(AUX$model,newdata=data.table(A=PGA,IA=1/PGA,LnA=log(PGA))) |> exp()
