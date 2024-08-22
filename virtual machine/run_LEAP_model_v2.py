# cd "C:\Users\Guillermo García\Dropbox\TEC MTY MPE\Producto Académico de Investigación\LEAP\memo_leap\memo"
# iPython

import win32com.client as win32
from utils import *
import pandas as pd
import os 
import sys 

run_id = int(sys.argv[1])

print("++++++++++++++++++++++++++++++++++")
print("++++++++++++++++++++++++++++++++++")

print(f"\nInicia Experimento {run_id}\n")

print("++++++++++++++++++++++++++++++++++")
print("++++++++++++++++++++++++++++++++++")

experimental_design = pd.read_csv("experimental_design_leap.csv")

# Agregar columnas de incertidumbres adicionales
demand, natgas_price, carbon_tax, lhc_type, future_id, policy_name, run_id = experimental_design.iloc[run_id]

"""
### Inicializa modelo
"""

print("Inicia LEAP")
LEAP = win32.Dispatch("LEAP.LEAPApplication")

print("Activa Area")
LEAP.ActiveArea = "Mexico VFFF"

print("Abre Analysis View")
LEAP.ActiveView = "Analysis"

LEAP.Verbose = 0

print("Set scenario")
setscenario = "Reference"
LEAP.ActiveScenario = LEAP.Scenario(setscenario)

LEAP.Verbose = 4

# Leer el archivo CSV policies
pol = pd.read_csv("policies.csv", index_col=0)  # Asumiendo que los nombres de las filas están en la primera columna

# Crear un vector con los nombres de las filas a partir de la segunda fila
technologies = pol.index.to_list()[1:]

# Defino y configuro la política que voy a correr
if policy_name == "PIIRCE 2023":
    policy_id = "piirce_2023"
elif policy_name =="PIIRCE 2018":
    policy_id = "piirce_2018"
elif policy_name == "Optimization Policy":
    policy_id = "opt"
else:
    raise ValueError("Policy Name no reconocido.")  # Añadido para manejo de otros casos

if future_id == 1:
    print("Cambio de política...")
    for technology in technologies:
        LEAP.BranchVariable(f"\\Transformation\\Electricity Generation\\Processes\\{technology}\\:Exogenous Capacity").Expression = get_policy_value(technology, policy_id)
       
"""
#### Actualiza expresiones
"""
print("Actualiza expresiones")
### Actualiza parámetro de demanda
LEAP.BranchVariable("\\Demand\\National Demand\\Demand\\:Total Energy").Expression=change_demand(demand)

### Actualiza parámetro de precio gas
LEAP.BranchVariable("\\Resources\\Primary\\Natural Gas\\:Indigenous Cost").Expression=change_natgas_price(natgas_price)

### Actualiza parámetro del impuesto al carbono
LEAP.BranchVariable("\\Effects\\Carbon Dioxide\\:Externality Cost").Expression=change_CO2carbon_tax(carbon_tax)
LEAP.BranchVariable("\\Effects\\Methane\\:Externality Cost").Expression=change_CH4carbon_tax(carbon_tax)
LEAP.BranchVariable("\\Effects\\Nitrous Oxide\\:Externality Cost").Expression=change_N2Ocarbon_tax(carbon_tax)

"""
#### Ejecuta Modelo
"""
print("Ejecuta modelo")
LEAP.Calculate()

"""
#### Exporta resultados
"""
#LEAP.Verbose = 4
LEAP.Visible = True

LEAP.ActiveScenario = "Reference"

for i in LEAP.Branches:
    if i.Name == "Electricity Generation":
        result_leap = i
        break

df_result = pd.DataFrame({"year":range(2014,2033),
                          "run_id":[run_id]*len(range(2014,2033)),
                          "energy_generation":[result_leap.Variables.Item("Energy Generation").Value(i) for i in range(2014,2033)],
                          "reserve_margin":[result_leap.Variables.Item("Reserve Margin").Value(i) for i in range(2014,2033)],
                          "cost_of_production":[result_leap.Variables.Item("Cost of Production").Value(i) for i in range(2014,2033)],
                          "GHG_emissions":[result_leap.Variables.Item("One_Hundred Year GWP Direct At Point of Emissions").Value(i) for i in range(2014,2033)]})

df_result.to_csv(os.path.join("output",f"{run_id}_leap_result.csv"),index = False)
