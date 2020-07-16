library(deSolve)
library(dplyr)
setwd('C:/repo/covid-19/Shiny App/')

source("models.R")

Cp = 5840795
n1 = 1513005
n2 = 1685869
n3 = 1902963
n4 = 738958

#copied from the app's defaults
sd1in=0.65
sd2in=0.65
sd3in=0.85
maskin=0.5
piin=0.4545
kapin=0

args = commandArgs(trailingOnly=TRUE)

if (args[1]=="--direct") {
  icu0=as.numeric(args[2])
  icu1=as.numeric(args[3])
  icu2=as.numeric(args[4])
  icu3=as.numeric(args[5])
  icu4=as.numeric(args[6])
  icu5=as.numeric(args[7])
  icu6=as.numeric(args[8])
} else {
  # read in OptDef generated input .csv
  inFile = read.csv(args[1], sep = ",", header = FALSE)

  # parse values to their appropriate property and place in list
  for (i in 1:nrow(inFile)) {
    firstCol = inFile[i, "V1"]
    paramNameSplit = strsplit(firstCol, "-")
    paramName = paramNameSplit[[1]][2]
    varDefVal = inFile[i, "V2"]
    if (paramName=="icu0"){
      icu0=as.numeric(varDefVal)
    } else if (paramName=="icu1"){
      icu1=as.numeric(varDefVal)
    } else if (paramName=="icu2"){
      icu2=as.numeric(varDefVal)
    } else if (paramName=="icu3"){
      icu3=as.numeric(varDefVal)
    } else if (paramName=="icu4"){
      icu4=as.numeric(varDefVal)
    } else if (paramName=="icu5"){
      icu5=as.numeric(varDefVal)
    } else if (paramName=="icu6"){
      icu6=as.numeric(varDefVal)
    }
  }
}

parms <- c(beta = 0.4793, # transmission rate
           cap = 1800,
           gamma = 1/9,
           alpha = 4,
           Cp = Cp,
           n1 = n1,
           n2 = n2,
           n3 = n3,
           n4 = n4,
           ef1_1 = 0.706,
           ef1_2 = sd1in,
           ef1_3 = sd2in,
           ef4p =  sd3in, #proportion of adults over 65 social distancing at 80%
           ef2_1 = 0.55,
           ef2_2 = 0.55,
           ef2_3 = 0.55,
           ef3_1 = 0.55,
           ef3_2 = 0.55,
           ef3_3 = 0.55,
           ef4_1 = 0.7544,
           ef4_2 = 0.675,
           ef4_3 = 0.675,
           ef1 = 0,
           ef2 = 0,
           ef3 = 0,
           ef4 = 0,
           dh1 = 0, dh2 = 0, dh3 = 0.0045, dh4 = 0.0923,
           dc1 = 0.0417, dc2 = 0.0392, dc3 = 0.1543, dc4 = 0.3956,
           dc = 0.0323,
           pS1 = 0.110023, ## proportion of infectious individuals symptomatic (0-19)
           pS2 = 0.35705, ## proportion of infectious individuals symptomatic (20-39)
           pS3 = 0.561205, ## proportion of infectious individuals symptomatic (40-64)
           pS4 = 0.774879, ## proportion of infectious individuals symptomatic (65+)
           pID = 0.4, ## proportion of infections identified
           siI = 0.438,## Proportion of symptomatic individuals self isolate
           lambda = 1.395, ##difference in infectiousness symptomatic/asymptomatic
           hosp1 = 0.01108, 
           cc1 = 0.00486,
           hosp2 = 0.03139, 
           cc2 = 0.0114,
           hosp3 = 0.04711, 
           cc3 = 0.02153,
           hosp4 = 0.05825, 
           cc4 = 0.05656,
           mag1 = 0.497,
           mag2 = 0.7946,
           mag3 = 0.805,
           mag3a = 0.8,
           mag4 = 0.9,
           mag4a = 0.8462,
           mag4b = 0.933,
           t1 = 41,
           t2 = 53,
           t3 = 63,
           t4 = 94,
           t4a = 101,
           t5 = 108,
           t5a = 115,
           t5b = 122,
           t6 = 129,
           t7 = difftime(as.Date("2020-06-19"), as.Date("2020-01-23")),
           t8 = 205,
           #ramp = ifelse(input$ramp == "Yes", .00407, 0),
           ramp = .00407,
           maska = 0.5,
           maskb = maskin,
           kap = kapin, #average number of contacts traced per detected case
           pCT = 0.4, #proportion of identified cases with contacts traced
           pi = piin,
           om = 0.0609, #probability a contact traced individual is infected
           temp_on = 0,
           ICU0 = icu0, ICU1 = icu1, ICU2 = icu2, ICU3 = icu3, ICU4 = icu4, ICU5 = icu5, ICU6 = icu6,
           SD0 = 0.00, SD1 = 0.35, SD2 = 0.70, SD3 = 0.75, SD4 = 0.85, SD5 = 0.90,  SD6 = 0.95,  SD7 = 1.00,
           UE0 = 0.00, UE1 = 0.25, UE2 = 0.65, UE3 = 0.70, UE4 = 0.72, UE5 = 0.75,  UE6 = 0.80,  UE7 = 0.95,
           LDTMin=21
)

## Run model for CC, H, and I
dt      <- seq(0, 3*365, 1)
inits      <- c(S1 = n1 - 1, E1 = 0, I1 = 1, II1 = 0, Ih1 = 0, Ic1 = 0, A1 = 0, R1 = 0, Rh1 = 0, Rc1 = 0, D1 = 0,
                S2 = n2,     E2 = 0, I2 = 0, II2 = 0, Ih2 = 0, Ic2 = 0, A2 = 0, R2 = 0, Rh2 = 0, Rc2 = 0, D2 = 0,
                S3 = n3,     E3 = 0, I3 = 0, II3 = 0, Ih3 = 0, Ic3 = 0, A3 = 0, R3 = 0, Rh3 = 0, Rc3 = 0, D3 = 0,
                S4 = n4,     E4 = 0, I4 = 0, II4 = 0, Ih4 = 0, Ic4 = 0, A4 = 0, R4 = 0, Rh4 = 0, Rc4 = 0, D4 = 0)
N  <- Cp
out1 <- lsoda(inits, dt, seir1, parms = parms)
out <- as.data.frame(out1)

## Calculate daily totals for CC, H, I
out$dailyInfections <-  round(out$I1 + out$I2 + out$I3 + out$I4, 0)
out$dailyHospitalizations <- round(out$Ih1+out$Ih2+out$Ih3 + out$Ih4, 0)
out$dailyCriticalCare <- round(out$Ic1 + out$Ic2 + out$Ic3 + out$Ic4, 0)
out$cumulativeInfections <- round(out$R1 + out$R2 + out$R3 + out$R4 +
                                    out$I1 + out$I2 + out$I3 + out$I4)
out$cumulativeHospitalizations <- round(out$Rh1 + out$Rh2 + out$Rh3 + out$Rh4 +
                                          out$Ih1 + out$Ih2 + out$Ih3 + out$Ih4)
out$cumulativeCriticalCare <- round(out$Rc1 + out$Rc2 + out$Rc3 + out$Rc4 +
                                      out$Ic1 + out$Ic2 + out$Ic3 + out$Ic4)
out$date <- as.Date(out$time, format="%m/%d/%Y", origin="01/24/2020")

out <- out %>%
  select(date, dailyInfections, dailyHospitalizations, dailyCriticalCare,
         cumulativeInfections, cumulativeHospitalizations, cumulativeCriticalCare,LDLev,SD,UE)

# ## Run model for deaths
# initsD      <- c(S1 = n1 - 1, E1 = 0, I1 = 1, II1 = 0, Ih1 = 0, A1 = 0, R1 = 0, Rh1 = 0, Ic = 0, Rc = 0, D = 0,
#                  S2 = n2,     E2 = 0, I2 = 0, II2 = 0, Ih2 = 0, A2 = 0, R2 = 0, Rh2 = 0,
#                  S3 = n3,     E3 = 0, I3 = 0, II3 = 0, Ih3 = 0, A3 = 0, R3 = 0, Rh3 = 0,
#                  S4 = n4,     E4 = 0, I4 = 0, II4 = 0, Ih4 = 0, A4 = 0, R4 = 0, Rh4 = 0)
# N  <- Cp
# outD <- lsoda(initsD, dt, seir1D, parms = parms) %>% as.data.frame()

# ## Calculate cumulative and daily deaths
# outD$cumulativeDeaths <- round(outD$D, 0)
# outD$dailyDeaths <- c(outD$cumulativeDeaths[1], diff(outD$cumulativeDeaths))
# outD$date <- as.Date(outD$time, format="%m/%d/%Y", origin="01/24/2020")

# outD <- outD %>%
#   select(date, cumulativeDeaths, dailyDeaths)

# ## Combine CC, H, I and Deaths outputs to pass to function
# out <- full_join(out, outD, by = "date") 

if (args[1]=="--direct") {
  write.csv(out, 'c:/repo/covid-19/Shiny App/out.csv', row.names = F)
} else {
  optdefout=c("output-icu"=sum(out$dailyCriticalCare),
              "output-ue"=sum(out$UE))
  write.table(optdefout, args[2], sep=",", col.names=FALSE)
}
