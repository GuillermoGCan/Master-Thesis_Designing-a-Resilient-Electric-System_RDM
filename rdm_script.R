#Defino la función para generar mis experimentos

library(tidyverse)

exp.design.table <-
  function(dir.exp.inputs,Limits.File,sample.size
           ,Policies.File,Policy.Switch
           ){
    
    Limits <-
      read.csv(paste(dir.exp.inputs,Limits.File,sep="")) #type1->constant,type2->factorial, type3->latinhypercube
    
    Limits$Description <- NULL
    Limits$Comments <- NULL
    
    Policies <-
      read.csv(paste(dir.exp.inputs,Policies.File,sep=""))

    #Separate variables into the three different strings
    Constants <-
      Limits[Limits$Type==1,"VarName"]
    Factorial <-
      Limits[Limits$Type==2,"VarName"]
    LatinHyperCube <- 
      Limits[Limits$Type==3,"VarName"]
    
    #Create factorial part of the experimental design
    
    Create.factorial.exp<-function(Limits){
      if (length(Factorial)==0){
        c()
      } else{
        Limits.Factorial <- 
          Limits[Limits$Type==2,]
        
        row.names(Limits.Factorial) <- 
          Limits.Factorial$VarName
        
        Limits.Factorial <- 
          Limits.Factorial[,c("Min","Max","Gradient")]
        
        Limits.Factorial$Gradient <-
          ifelse(Limits.Factorial$Gradient >1,
                 Limits.Factorial$Gradient,
                 2) #this allows the instruction below to use gradient=1 as input and work ok
        
        l <-
          lapply(data.frame(t(Limits.Factorial)),
                 function (x) {
                   seq(x[1],x[2],by=(x[2]-x[1])/(x[3]-1))
                 }
          ) #if in doubt about the function look at the form of data.frame(t(Limits.Factorial))
        
        return(expand.grid(l))
      }
    }
    
    Exp.design.factorial <-
      Create.factorial.exp(Limits)
    
    #Create Latin hyper cube part of experimental design
    
    Create.LHC.exp<-function(Limits){
      if (length(LatinHyperCube)==0){
        c()
      } else {
        library(lhs)
        
        sample <- 
          randomLHS(sample.size,length(LatinHyperCube))
        
        Limits.LHC <-
          Limits[Limits$Type==3,]
        
        row.names(Limits.LHC) <-
          Limits.LHC$VarName
        
        Limits.LHC <- 
          Limits.LHC[,c("Min","Max")]
        
        Limits.LHC$Row.Number <-
          c(1:nrow(Limits.LHC))
        
        lhc.p1 <-
          as.data.frame(apply(Limits.LHC,1,
                              function (x){
                                sample[,x[3]]*(x[2]-x[1])+x[1]
                              }
          )
          )
        
        lhc.p1$lhc_type <-
          "Sample"
        
        #add extreme values to sample table
        lhc.p2 <- 
          Limits[Limits$Type==3,]
        
        row.names(lhc.p2) <-
          lhc.p2$VarName
        
        lhc.p2 <-
          lhc.p2[,c("Min","Max","Gradient")]
        
        lhc.p2$Gradient <-
          2
        
        lhc.p2$Gradient <-
          ifelse(lhc.p2$Gradient > 1,
                 lhc.p2$Gradient,
                 2) #this allows the instruction below to use gradient=1 as input and work ok
        
        lhc.p2 <-
          lapply(data.frame(t(lhc.p2)),
                 function (x) {
                   seq(x[1],x[2],by=(x[2]-x[1])/(x[3]-1))
                 }
          ) #if in doubt about the function look at the form of data.frame(t(lhc.p2))
        
        lhc.p2 <-
          expand.grid(lhc.p2)
        
        lhc.p2$lhc_type <-
          "Extremes"
        
        #rbind both tables
        lhc <-
          rbind(lhc.p1,lhc.p2)
        return(lhc)
      }
    }
    
    Exp.design.LHC <-
      Create.LHC.exp(Limits)
    
    #Create constant part of the experimental design
    
    Create.Constant.exp <- 
      function(Limits)
      {
        if (length(Constants)==0)
        {
          c()
        } else {
          Limits.Constant <-
            Limits[Limits$Type==1,]
          
          varnames <-
            Limits.Constant$VarName
          
          Limits.Constant <-
            data.frame(t(Limits.Constant[,"Cte"]))
          
          colnames(Limits.Constant) <- 
            varnames
          
          return(Limits.Constant)
        }
      }
    
    Exp.design.Constant <- 
      Create.Constant.exp(Limits)
    
    #Join the three experimental design tables to create total number of futures
    Exp.design <-
      if(length(Factorial)==0 && length(LatinHyperCube)==0){
        Exp.design.Constant
      } else {
        if(length(Factorial)==0){
          Exp.design.LHC
        } else {
          if(length(LatinHyperCube)==0){
            Exp.design.factorial
          } else {
            merge(Exp.design.LHC,Exp.design.factorial)
          }
        }
      }
    
    Exp.design <-
      if(length(Factorial)==0 && length(LatinHyperCube)==0){
        Exp.design
      } else {
        if(length(Constants)==0){
          Exp.design
        } else {
          merge(Exp.design,Exp.design.Constant)
        }
      }
    
    #Finally add reference Future.ID
    #Exp.design.reference<-Limits
    #varnames<-Exp.design.reference$VarName
    #Exp.design.reference<-data.frame(t(Exp.design.reference[,"Cte"]))
    #colnames(Exp.design.reference)<-varnames
    
    #rbind with rest of experiment
    # Exp.design<-rbind(Exp.design,Exp.design.reference)

    Exp.design$future_id <-
      as.numeric(row.names(Exp.design))
    
    #Join total number of futures with policies to create total number of runs
    Policies <-
      if(Policy.Switch==FALSE) {Policies[1,]}else{Policies}

    Exp.design <-
      merge(Exp.design,Policies)
    
    Exp.design$run_id <-
      as.numeric(row.names(Exp.design))
    
    return(Exp.design)
  }

#define root directory for experiment's inputs

dir.exp.inputs <-
  r"(C:\Users\guill\OneDrive\Documents\GitHub\Master-Thesis_Designing-a-Resilient-Electric-System_RDM)"

Limits.File <-
  "Limits.csv"
Policies.File <-
  "Policies.csv"
# Climate.File <-
#   "Climate_1.csv"

#Muestreo

sample.size <- 500
Policy.Switch <- TRUE
# Climate.Switch <- FALSE

set.seed(55555)

Exp.design <-
  exp.design.table(dir.exp.inputs,
                   Limits.File,
                   sample.size
                   ,Policies.File,
                   Policy.Switch
                   # ,Climate.File,
                   # Climate.Switch
                   )

plot(Exp.design %>% 
       select(demand, natgas_price, carbon_tax))

write.csv(Exp.design,"diseño_experimental2.csv")

# Cargamos la librería
library(dplyr)
library(decisionSupport)

# Directorio donde están los archivos .csv
mydir <- r"(C:\Users\guill\OneDrive\Documents\GitHub\Master-Thesis_Designing-a-Resilient-Electric-System_RDM\leap_output)"

# Lista de nombres de archivos .csv en el directorio
files <- list.files(path=mydir, pattern="\\.csv$")

# Leer y combinar todos los archivos .csv
leap_results <- lapply(files, function(x) read.csv(file.path(mydir, x))) %>%
  bind_rows()

exp.out <- 
  merge(Exp.design,
        leap_results,
        by = "run_id")

# Estimación del margen de reserva mínimo bajo cada política
## Política PIIRCE 2023

P2023_RM <-
  subset(exp.out,
         policy_name =="PIIRCE 2023" & year >= 2018)

P2023_RM <-
  aggregate(P2023_RM[,"reserve_margin"],
            list(run_id = P2023_RM$run_id),
            min,
            na.rm=TRUE)

## Política PIIRCE 2018

P2018_RM <-
  subset(exp.out,
         policy_name =="PIIRCE 2018" & year >= 2018)

P2018_RM <-
  aggregate(P2018_RM[,"reserve_margin"],
            list(run_id = P2018_RM$run_id),
            min,
            na.rm=TRUE)

## Política de referencia óptima

Popt_RM <-
  subset(exp.out,
         policy_name =="Optimization Policy" & year >= 2018)

Popt_RM <-
  aggregate(Popt_RM[,"reserve_margin"],
            list(run_id = Popt_RM$run_id),
            min,
            na.rm=TRUE)

# Estimación de la suma de los costos de producción para cada política
## Política PIIRCE 2023

P2023_CP <-
  subset(exp.out,
         policy_name =="PIIRCE 2023" & year >= 2018)

P2023_CP$cost_of_production <- 
  P2023_CP$cost_of_production/P2023_CP$energy_generation

# P2023_CP <-
#   aggregate(P2023_CP[,"cost_of_production"],
#             list(run_id = P2023_CP$run_id),
#             sum,
#             na.rm=TRUE)

P2023_CP0 <- data.frame(run_id = 1:508)

for (r in P2023_CP0$run_id){
  P2023_CP0$cost_of_production[r] <-
    discount(filter(P2023_CP, run_id == r)$cost_of_production, 7.7, calculate_NPV = TRUE)
}

P2023_CP <-
  P2023_CP0

## Política PIIRCE 2018

P2018_CP <-
  subset(exp.out,
         policy_name =="PIIRCE 2018" & year >= 2018)

P2018_CP$cost_of_production <- 
  P2018_CP$cost_of_production/P2018_CP$energy_generation

P2018_CP0 <- data.frame(run_id = 509:1016)

for (r in P2018_CP0$run_id){
  P2018_CP0$cost_of_production[r-508] <- 
    discount(filter(P2018_CP, run_id == r)$cost_of_production, 7.7, calculate_NPV = TRUE)
}

P2018_CP <- 
  P2018_CP0

## Política de referencia óptima

Popt_CP <-
  subset(exp.out,
         policy_name =="Optimization Policy" & year >= 2018)

Popt_CP$cost_of_production <- 
  Popt_CP$cost_of_production/Popt_CP$energy_generation

Popt_CP0 <- data.frame(run_id = 1017:1524)

for (r in Popt_CP0$run_id){
  Popt_CP0$cost_of_production[r-1016] <- 
    discount(filter(Popt_CP, run_id == r)$cost_of_production, 7.7, calculate_NPV = TRUE)
}

Popt_CP <- 
  Popt_CP0

# Estimación de las emisiones de GEI en el año 2030

## Política PIIRCE 2023

P2023_GEI <-
  subset(exp.out,
         policy_name =="PIIRCE 2023" & year == 2030)[,c("run_id","GHG_emissions")]

## Política PIIRCE 2018

P2018_GEI <-
  subset(exp.out,
         policy_name =="PIIRCE 2018" & year == 2030)[,c("run_id","GHG_emissions")]

## Política de referencia óptima

Popt_GEI <-
  subset(exp.out,
         policy_name =="Optimization Policy" & year == 2030)[,c("run_id","GHG_emissions")]

# Elaboración de base de datos para cada política
## Política 2023
P2023 <-  
  merge(P2023_RM,
        P2023_CP,
        by = "run_id")

P2023 <- 
  merge(P2023,
        P2023_GEI,
        by = "run_id") %>% 
  transmute(run_id = run_id,
            reserve_margin = x,
            cost_of_production = cost_of_production,
            GHG_emissions = GHG_emissions/1E9)

library(knitr)

kable(head(P2023),
      align = c("c"))

## Política 2018
P2018 <-  
  merge(P2018_RM,
        P2018_CP,
        by = "run_id")

P2018 <- 
  merge(P2018,
        P2018_GEI,
        by = "run_id") %>% 
  transmute(run_id = run_id,
            reserve_margin = x,
            cost_of_production = cost_of_production,
            GHG_emissions = GHG_emissions/1E9)

kable(head(P2018),
      align = c("c"))

## Política de referencia óptima
Popt <-  
  merge(Popt_RM,
        Popt_CP,
        by = "run_id")

Popt <- 
  merge(Popt,
        Popt_GEI,
        by = "run_id") %>% 
  transmute(run_id = run_id,
            reserve_margin = x,
            cost_of_production = cost_of_production,
            GHG_emissions = GHG_emissions/1E9)

kable(head(Popt),
      align = c("c"))

# Resumen de resultados de cada política
summary(P2023)
summary(P2018)
summary(Popt)

### Incorporación de objetivos de métricas de desempeño a bases de datos de políticas ###

# Política 2023

## Objetivo 1
P2023$Obj1RM <- 
  ifelse(P2023$reserve_margin > 21.403, 1, 0)

## Objetivo 2
P2023$Obj2CP <-
  ifelse(P2023$cost_of_production < 148.6273, 1, 0)

# Obj2CP <- 
#   function (P2023, P2018, Popt){
#     P2023$future_id <- 
#       1:508
#     
#     P2018$future_id <-
#       1:508
#     
#     Popt$future_id <-
#       1:508
#     
#     Pprov <- 
#       merge(P2023,
#             P2018,
#             by = "future_id")
#     
#     Pprov <- 
#       merge(Pprov,
#             Popt,
#             by = "future_id")
#     
#     obj <- ifelse((Pprov$cost_of_production.x <= Pprov$cost_of_production.y) &
#                     (Pprov$cost_of_production.x <= Pprov$cost_of_production), 1, 0)
#     return (obj)
#   }
# 
# 
# P2023$Obj2CP <- 
#   Obj2CP(P2023 = P2023, P2018 = P2018, Popt = Popt)

## Objetivo 3
P2023$Obj3GEI <- 
  ifelse(P2023$GHG_emissions <= 121, 1, 0)

## Objetivos 1 y 2
P2023$Obj12 <- 
  ifelse(P2023$Obj1RM == 1 & P2023$Obj2CP == 1, 1, 0)

## Objetivos 1 y 3
P2023$Obj13 <- 
  ifelse(P2023$Obj1RM == 1 & P2023$Obj3GEI == 1, 1, 0)

## Objetivos 2 y 3
P2023$Obj23 <- 
  ifelse(P2023$Obj2CP == 1 & P2023$Obj3GEI == 1, 1, 0)

## Objetivos 1, 2 y 3
P2023$Obj123 <- 
  ifelse(P2023$Obj1RM == 1 & P2023$Obj2CP == 1 & P2023$Obj3GEI == 1, 1, 0)

# Política 2018

## Objetivo 1
P2018$Obj1RM <- 
  ifelse(P2018$reserve_margin > 21.403, 1, 0)

## Objetivo 2
P2018$Obj2CP <-
  ifelse(P2018$cost_of_production <= 148.6273, 1, 0)

# Obj2CP <- 
#   function (P2023, P2018, Popt){
#     P2023$future_id <- 
#       1:508
#     
#     P2018$future_id <-
#       1:508
#     
#     Popt$future_id <-
#       1:508
#     
#     Pprov <- 
#       merge(P2023,
#             P2018,
#             by = "future_id")
#     
#     Pprov <- 
#       merge(Pprov,
#             Popt,
#             by = "future_id")
#     
#     obj <- ifelse((Pprov$cost_of_production.y <= Pprov$cost_of_production.x) &
#                     (Pprov$cost_of_production.y <= Pprov$cost_of_production), 1, 0)
#     return (obj)
#   }
# 
# P2018$Obj2CP <- 
#   Obj2CP(P2023 = P2023, P2018 = P2018, Popt = Popt)

## Objetivo 3
P2018$Obj3GEI <- 
  ifelse(P2018$GHG_emissions <= 121, 1, 0)

## Objetivos 1 y 2
P2018$Obj12 <- 
  ifelse(P2018$Obj1RM == 1 & P2018$Obj2CP == 1, 1, 0)

## Objetivos 1 y 3
P2018$Obj13 <- 
  ifelse(P2018$Obj1RM == 1 & P2018$Obj3GEI == 1, 1, 0)

## Objetivos 2 y 3
P2018$Obj23 <- 
  ifelse(P2018$Obj2CP == 1 & P2018$Obj3GEI == 1, 1, 0)

## Objetivos 1, 2 y 3
P2018$Obj123 <- 
  ifelse(P2018$Obj1RM == 1 & P2018$Obj2CP == 1 & P2018$Obj3GEI == 1, 1, 0)

# Política de referencia óptima

## Objetivo 1
Popt$Obj1RM <- 
  ifelse(Popt$reserve_margin > 21.403, 1, 0)

## Objetivo 2
Popt$Obj2CP <-
  ifelse(Popt$cost_of_production <= 148.6273, 1, 0)

# Obj2CP <- 
#   function (P2023, P2018, Popt){
#     P2023$future_id <- 
#       1:508
#     
#     P2018$future_id <-
#       1:508
#     
#     Popt$future_id <-
#       1:508
#     
#     Pprov <- 
#       merge(P2023,
#             P2018,
#             by = "future_id")
#     
#     Pprov <- 
#       merge(Pprov,
#             Popt,
#             by = "future_id")
#     
#     obj <- ifelse((Pprov$cost_of_production <= Pprov$cost_of_production.x) &
#                     (Pprov$cost_of_production <= Pprov$cost_of_production.y), 1, 0)
#     return (obj)
#   }
# 
# Popt$Obj2CP <- 
#   Obj2CP(P2023 = P2023, P2018 = P2018, Popt = Popt)

## Objetivo 3
Popt$Obj3GEI <- 
  ifelse(Popt$GHG_emissions <= 121, 1, 0)

## Objetivos 1 y 2
Popt$Obj12 <- 
  ifelse(Popt$Obj1RM == 1 & Popt$Obj2CP == 1, 1, 0)

## Objetivos 1 y 3
Popt$Obj13 <- 
  ifelse(Popt$Obj1RM == 1 & Popt$Obj3GEI == 1, 1, 0)

## Objetivos 2 y 3
Popt$Obj23 <- 
  ifelse(Popt$Obj2CP == 1 & Popt$Obj3GEI == 1, 1, 0)

## Objetivos 1, 2 y 3
Popt$Obj123 <- 
  ifelse(Popt$Obj1RM == 1 & Popt$Obj2CP == 1 & Popt$Obj3GEI == 1, 1, 0)

# Resumen de resultados de cada política
summary(P2023[,5:11])
summary(P2018[,5:11])
summary(Popt[,5:11])

### Tabla comparativa de porcentaje de incumplimiento de objetivos para cada alternativa de política ###

mat_incumplimiento <- 
  matrix(nrow = 3, 
         ncol = 8)

mat_incumplimiento[1,1] <- "PIIRCE 2023"
mat_incumplimiento[2,1] <- "PIIRCE 2018"
mat_incumplimiento[3,1] <- "Política de referencia óptima"

# Política 2023
## Objetivo Margen de reserva
mat_incumplimiento[1,2] <- 
  round(1-mean(P2023$Obj1RM),4)

## Objetivo Costo de producción
mat_incumplimiento[1,3] <- 
  round(1-mean(P2023$Obj2CP),4)

## Objetivo Emisiones de GEI
mat_incumplimiento[1,4] <- 
  round(1-mean(P2023$Obj3GEI),4)

## Objetivos Margen de reserva y Costo de producción
mat_incumplimiento[1,5] <- 
  round(1-mean(P2023$Obj12),4)

## Objetivos Margen de reserva y Emisiones de GEI
mat_incumplimiento[1,6] <- 
  round(1-mean(P2023$Obj13),4)

## Objetivos Costo de producción y Emisiones de GEI
mat_incumplimiento[1,7] <- 
  round(1-mean(P2023$Obj23),4)

## Tres objetivos
mat_incumplimiento[1,8] <- 
  round(1-mean(P2023$Obj123),4)

# Política 2018
## Objetivo Margen de reserva
mat_incumplimiento[2,2] <- 
  round(1-mean(P2018$Obj1RM),4)

## Objetivo Costo de producción
mat_incumplimiento[2,3] <- 
  round(1-mean(P2018$Obj2CP),4)

## Objetivo Emisiones de GEI
mat_incumplimiento[2,4] <- 
  round(1-mean(P2018$Obj3GEI),4)

## Objetivos Margen de reserva y Costo de producción
mat_incumplimiento[2,5] <- 
  round(1-mean(P2018$Obj12),4)

## Objetivos Margen de reserva y Emisiones de GEI
mat_incumplimiento[2,6] <- 
  round(1-mean(P2018$Obj13),4)

## Objetivos Costo de producción y Emisiones de GEI
mat_incumplimiento[2,7] <- 
  round(1-mean(P2018$Obj23),4)

## Tres objetivos
mat_incumplimiento[2,8] <- 
  round(1-mean(P2018$Obj123),4)

# Política de referencia óptima
## Objetivo Margen de reserva
mat_incumplimiento[3,2] <- 
  round(1-mean(Popt$Obj1RM),4)

## Objetivo Costo de producción
mat_incumplimiento[3,3] <- 
  round(1-mean(Popt$Obj2CP),4)

## Objetivo Emisiones de GEI
mat_incumplimiento[3,4] <- 
  round(1-mean(Popt$Obj3GEI),4)

## Objetivos Margen de reserva y Costo de producción
mat_incumplimiento[3,5] <- 
  round(1-mean(Popt$Obj12),4)

## Objetivos Margen de reserva y Emisiones de GEI
mat_incumplimiento[3,6] <- 
  round(1-mean(Popt$Obj13),4)

## Objetivos Costo de producción y Emisiones de GEI
mat_incumplimiento[3,7] <- 
  round(1-mean(Popt$Obj23),4)

## Tres objetivos
mat_incumplimiento[3,8] <- 
  round(1-mean(Popt$Obj123),4)

kable(mat_incumplimiento,
      align = "l",
      col.names = 
        c("Políticas",
          "Futuros en incumplimiento del Objetivo Margen de reserva",
          "Futuros en incumplimiento del Objetivo Costo de producción",
          "Futuros en incumplimiento del Objetivo Emisiones de GEI",
          "Futuros en incumplimiento del Objetivo Margen de reserva y Costo de producción",
          "Futuros en incumplimiento del Objetivo Margen de reserva y Emisiones de GEI",
          "Futuros en incumplimiento del Objetivo Costo de producción y Emisiones de GEI",
          "Futuros en incumplimiento de los tres objetivos"),
      caption = "Tabla comparativa de porcentaje de incumplimiento de objetivos para cada alternativa de política.")

### Estimación del nivel de arrepentimiento para cada política y para cada futuro ###

# Cálculo del nivel de arrepentimiento de cada política implementada en cada futuro

for (i in 1:nrow(P2023)){
  ## Política 2023
  P2023$Regret.RM[i] <- 
    max(P2023$reserve_margin[i],
        P2018$reserve_margin[i],
        Popt$reserve_margin[i]) -
    P2023$reserve_margin[i]
  
  P2023$Regret.CP[i] <- 
    min(P2023$cost_of_production[i],
        P2018$cost_of_production[i],
        Popt$cost_of_production[i]) -
    P2023$cost_of_production[i]
  
  P2023$Regret.GEI[i] <- 
    min(P2023$GHG_emissions[i],
        P2018$GHG_emissions[i],
        Popt$GHG_emissions[i]) -
    P2023$GHG_emissions[i]
  
  ## Política 2018
  P2018$Regret.RM[i] <- 
    max(P2023$reserve_margin[i],
        P2018$reserve_margin[i],
        Popt$reserve_margin[i]) -
    P2018$reserve_margin[i]
  
  P2018$Regret.CP[i] <- 
    min(P2023$cost_of_production[i],
        P2018$cost_of_production[i],
        Popt$cost_of_production[i]) -
    P2018$cost_of_production[i]
  
  P2018$Regret.GEI[i] <- 
    min(P2023$GHG_emissions[i],
        P2018$GHG_emissions[i],
        Popt$GHG_emissions[i]) -
    P2018$GHG_emissions[i]
  
  ## Política de referencia óptima
  Popt$Regret.RM[i] <- 
    max(P2023$reserve_margin[i],
        P2018$reserve_margin[i],
        Popt$reserve_margin[i]) -
    Popt$reserve_margin[i]
  
  Popt$Regret.CP[i] <- 
    min(P2023$cost_of_production[i],
        P2018$cost_of_production[i],
        Popt$cost_of_production[i]) -
    Popt$cost_of_production[i]
  
  Popt$Regret.GEI[i] <- 
    min(P2023$GHG_emissions[i],
        P2018$GHG_emissions[i],
        Popt$GHG_emissions[i]) -
    Popt$GHG_emissions[i]
}

### Estimación del valor esperado de arrepentimiento para cada métrica bajo la implementación de cada política ###

mat_regrets <- 
  matrix(nrow = 3, 
         ncol = 4)

mat_regrets[1,1] <- "PIIRCE 2023"
mat_regrets[2,1] <- "PIIRCE 2018"
mat_regrets[3,1] <- "Política de referencia óptima"

# Política 2023
## Margen de reserva
mat_regrets[1,2] <- 
  round(mean(P2023$Regret.RM),4)

## Costo de producción
mat_regrets[1,3] <- 
  round(mean(P2023$Regret.CP),4)

## Emisiones de GEI
mat_regrets[1,4] <- 
  round(mean(P2023$Regret.GEI)/1E9,4)

# Política 2018
## Margen de reserva
mat_regrets[2,2] <- 
  round(mean(P2018$Regret.RM),4)

## Costo de producción
mat_regrets[2,3] <- 
  round(mean(P2018$Regret.CP),4)

## Emisiones de GEI
mat_regrets[2,4] <- 
  round(mean(P2018$Regret.GEI)/1E9,4)

# Política de referencia óptima
## Margen de reserva
mat_regrets[3,2] <- 
  round(mean(Popt$Regret.RM),4)

## Costo de producción
mat_regrets[3,3] <- 
  round(mean(Popt$Regret.CP),4)

## Emisiones de GEI
mat_regrets[3,4] <- 
  round(mean(Popt$Regret.GEI)/1E9,4)

kable(mat_regrets,
      align = "l",
      col.names = 
        c("Políticas",
          "Valor Esperado de Arrepentimiento por el Margen de reserva [%]",
          "Valor Esperado de Arrepentimiento por el Costo de producción [miles de millones de USD]",
          "Valor Esperado de Arrepentimiento por las Emisiones de GEI [millones de tCO2e]"),
      caption = "Tabla comparativa del valor esperado del nivel de arrepentimiento por la implementación de cada política para cada métrica de desempeño.")

### Unificación de bases de datos de resultados de políticas con diseño experimental ###

# Política 2023
P2023 <- 
  merge(P2023,
        Exp.design,
        by = "run_id")

# Política 2018
P2018 <- 
  merge(P2018,
        Exp.design,
        by = "run_id")

# Política de referencia optimizada
Popt <- 
  merge(Popt,
        Exp.design,
        by = "run_id")

library(rpart)
library(rpart.plot)

### Elaboración de árboles de regresión ###

# Objetivo 1

## Política 2023
rpart_P2023_RM <- 
  rpart(Obj1RM ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2023,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2023_RM)

## Política 2018
rpart_P2018_RM <- 
  rpart(Obj1RM ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2018,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2018_RM)

## Política de referencia optimizada
rpart_Popt_RM <- 
  rpart(Obj1RM ~
          demand +
          natgas_price +
          carbon_tax, 
        data = Popt,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_Popt_RM)

# Objetivo 2

## Política 2023
rpart_P2023_CP <- 
  rpart(Obj2CP ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2023,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2023_CP)

## Política 2018
rpart_P2018_CP <- 
  rpart(Obj2CP ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2018,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2018_CP)

## Política de referencia optimizada
rpart_Popt_CP <- 
  rpart(Obj2CP ~
          demand +
          natgas_price +
          carbon_tax, 
        data = Popt,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_Popt_CP)

# Objetivo 3

## Política 2023
rpart_P2023_GEI <- 
  rpart(Obj3GEI ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2023,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2023_GEI)

## Política 2018
rpart_P2018_GEI <- 
  rpart(Obj3GEI ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2018,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2018_GEI)

## Política de referencia optimizada
rpart_Popt_GEI <- 
  rpart(Obj3GEI ~
          demand +
          natgas_price +
          carbon_tax, 
        data = Popt,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_Popt_GEI)

# Objetivos 123

## Política 2023
rpart_P2023_123 <- 
  rpart(Obj123 ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2023,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2023_123)

## Política 2018
rpart_P2018_123 <- 
  rpart(Obj123 ~
          demand +
          natgas_price +
          carbon_tax, 
        data = P2018,
        method = "anova",
        maxdepth = 2)

### Gráfico de árbol de regresión
rpart.plot(rpart_P2018_123)

## Política de referencia optimizada
rpart_Popt_123 <- 
  rpart(Obj123 ~
          demand +
          natgas_price +
          carbon_tax, 
        data = Popt,
        method = "anova",
        maxdepth = 3)

### Gráfico de árbol de regresión
rpart.plot(rpart_Popt_123)

rdm_results <- 
  rbind(P2023,P2018,Popt)

write.csv(rdm_results,"rdm_results.csv")

leap_results$policy_name <- 
  ifelse(leap_results$run_id <= 508, "PIIRCE 2023",
         ifelse(leap_results$run_id <= 1016, "PIIRCE 2018",
                "Política de referencia óptima"))

leap_results$cost_of_production <- 
  leap_results$cost_of_production/
  (leap_results$energy_generation/3600)/
  (1 + 0.077)^(leap_results$year - min(leap_results$year))

write.csv(leap_results, "leap_results.csv")
