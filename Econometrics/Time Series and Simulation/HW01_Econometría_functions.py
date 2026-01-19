# -*- coding: utf-8 -*-
"""
Created on Sun Oct  5 19:57:01 2025

@author: Antón Salvadores
"""

import numpy as np
from numpy.random import default_rng

import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import acf, pacf
from sklearn.metrics import mean_absolute_error

from typing import Union

def simulate_normal_distributions(
        n_times: float,
        n_samples: float,
        mean: float,
        stdev: float,
        seed: Union[int, None] = None,
) -> np.ndarray:
    
    rng = default_rng(seed)
    
    samples = rng.normal(loc=mean, scale=stdev, size=(n_samples, n_times))
    
    return samples

def simulate_t_student_distributions(
        n_times: float,
        n_samples: float,
        nu: float,
        seed: Union[int, None] = None,
) -> np.ndarray:
    
    rng = default_rng(seed)
    
    samples = rng.standard_t(df=nu, size=(n_samples, n_times))
    
    return samples
    
def acf_pacf(y, lags):

    valor_critico = 1.96 / np.sqrt(len(y))

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(8, 6))

    plot_acf(y, lags=lags, ax=ax1, zero=False)
    ax1.axhspan(-valor_critico, valor_critico, color='lightgray', alpha=0.5, label='Región de no significancia')
    ax1.set_ylim(-valor_critico - 0.1, valor_critico + 0.1)
    ax1.legend()

    plot_pacf(y, lags=lags, ax=ax2, zero=False)
    ax2.axhspan(-valor_critico, valor_critico, color='lightgray', alpha=0.5, label='Región de no significancia')
    ax2.set_ylim(-valor_critico - 0.1, valor_critico + 0.1)
    ax2.legend()

    acf_vals = acf(y, nlags=lags, alpha=0.05)[0]
    pacf_vals = pacf(y, nlags=lags, alpha=0.05, method='ywm')[0]
    acf_lags = np.where(np.abs(acf_vals)>valor_critico)
    pacf_lags = np.where(np.abs(pacf_vals)>valor_critico)

    print(f'Retardos ACF significativos: {acf_lags}')
    print(f'Retardos PACF significativos: {pacf_lags}')
    
def pred(y_test, y_train, modelo):
    forecast = modelo.get_forecast(steps=len(y_test))
    pred_mean = forecast.predicted_mean
    pred_ci = forecast.conf_int()

    errors = y_test - pred_mean
    mae = np.mean(np.abs(errors))
    rmse = np.sqrt(np.mean(errors**2))
    mape = np.mean(np.abs(errors / y_test)) * 100
    smape = 100 * np.mean(2 * np.abs(pred_mean - y_test) / (np.abs(pred_mean) + np.abs(y_test)))

    print("=== Predicciones del modelo SARIMAX ===")
    print(pred_mean)
    print("\n=== Intervalos de confianza ===")
    print(pred_ci)
    print(f"MAE sobre el conjunto de prueba: {mae:.4f}")
    print(f"\nRMSE sobre el conjunto de prueba: {rmse:.4f}")
    print(f"\n\n  Versión porcentual")
    print(f"\n MAE sobre el conjunto de prueba: {smape:.2f}%")
    print(f"\nRMSE sobre el conjunto de prueba:{mape:.2f}%")
    
    
    plt.figure(figsize=(10, 5))
    plt.plot(y_train, label='Entrenamiento')
    plt.plot(y_test.index, y_test, label='Datos reales (test)', color='black')
    plt.plot(pred_mean.index, pred_mean, label='Predicción', color='red')
    plt.fill_between(pred_ci.index, pred_ci.iloc[:, 0], pred_ci.iloc[:, 1], color='pink', alpha=0.3)
    plt.legend()
    plt.title('Predicción vs Datos Reales')
    plt.show()







