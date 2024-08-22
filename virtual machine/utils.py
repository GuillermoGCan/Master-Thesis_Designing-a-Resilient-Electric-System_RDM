import pandas as pd

def get_policy_value(branch, policy_id):
    df = pd.read_csv("policies.csv", index_col=0)
    try:
        return df.loc[branch, policy_id]
    except KeyError:
        raise ValueError(f"No se pudo encontrar un valor para branch={branch} y policy={policy_id}")

def change_demand(percent_growth):
    return f"Interp(2014, 228635, 2015, 237199, 2016, 248192, 2017, 258971, {percent_growth}%)"

def change_natgas_price(price_2032):
    return f"Interp(2014, 5.1362, 2015, 3.2189, 2016, 2.8731, 2017, 3.3853, 2032, {price_2032})"

def change_CO2carbon_tax(carbon_tax):
    return f"Step(2014, 0.39*Key\MXNUMAs[MXN] /Key\MXNUSD[MXN/USD],2018, {carbon_tax}*Key\MXNUMAs[MXN] /Key\MXNUSD[MXN/USD])"

def change_CH4carbon_tax(carbon_tax):
    return f"Step(2014, 0, 2018, {carbon_tax}*30*Key\MXNUMAs[MXN] /Key\MXNUSD[MXN/USD]/1000)"

def change_N2Ocarbon_tax(carbon_tax):
    return f"Step(2014, 0, 2018, {carbon_tax}*265*Key\MXNUMAs[MXN] /Key\MXNUSD[MXN/USD]/1000)"