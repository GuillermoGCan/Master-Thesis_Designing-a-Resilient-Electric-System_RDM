import win32com.client as win32
from utils import change_demand
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
run_id, demanda, precio_gas = experimental_design.iloc[run_id]


print("""
### Inicializa modelo
""")

print("""
#### Actualiza expresiones
""")

print("""
#### Ejecuta Modelo
""")

print("""
#### Ejecuta Modelo
""")

print("""
#### Exporta resultados
""")

"""
### Inicializa modelo
"""

print("Inicia LEAP")
LEAP = win32.Dispatch("LEAP.LEAPApplication")

print("Activa Area")
LEAP.ActiveArea = "Mexico VFF"

print("Abre Analysis View")
LEAP.ActiveView = "Analysis"

print("Set scenario")
setscenario = "Current Accounts"
LEAP.ActiveScenario = LEAP.Scenario(setscenario)

#LEAP.Scenario(setscenario)
"""
#### Actualiza expresiones
"""
print("Actualiza expresiones")
### Actualiza parámetro de demanda
LEAP.BranchVariable("\\Demand\\National Demand\\Demand\\:Total Energy").Expression=change_demand(demanda)
### Actualiza parámetro de precio gas


"""
#### Ejecuta Modelo
"""
print("Ejecuta modelo")
LEAP.Calculate()

LEAP.ActiveScenario = LEAP.Scenario(setscenario)
"""
#### Exporta resultados
"""
"""
WEAP.LoadFavorite(mi_favorito)

file_name = os.path.join(actual_path,"output",f"run_id_{action_id}_{j}.csv")
WEAP.ExportResults(file_name, True, True, True, False, False)


mis_favoritos = ["margen_reserva", "costos_produccion", "emisiones_gei"]

for favorito in mis_favoritos:
    #WEAP.LoadFavorite(favorito)

    file_name = os.path.join("output",f"{run_id}_{favorito}.csv")
    print(file_name)
    #WEAP.ExportResults(file_name, True, True, True, False, False)
"""