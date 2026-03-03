# -*- coding: utf-8 -*-
"""
Created on Fri Nov 14 10:22:19 2025

@author: Antón Salvadores
"""

import numpy as np
import pandas as pd
from numpy.random import default_rng
from scipy.stats import norm
from statsmodels.tsa.api import VAR
from statsmodels.tsa.regime_switching.markov_regression import MarkovRegression
from typing import Union

# Función que calcula la descomposición del error de predicción a horizonte H
def gfevd(var_res, H):
    A = var_res.coefs #matrices de coeficientes del VAR
    Sigma = np.asarray(var_res.sigma_u)
    K = Sigma.shape[0] #numero de variables
    p = var_res.k_ar #orden del VAR
    PH = np.zeros((H, K, K)) #inicializa lista de matrices A(k*k): función de respuesta a un impulso
    PH[0] = np.eye(K) #A_0 es la matriz identidad
    for h in range(1, H): #construye matrices A_h: respuesta a un impulso en h periodos -> coeficientes de representacion SMA de VAR
        for i in range(1, min(h, p) + 1):
            PH[h] += PH[h-i] @ A[i-1]
    theta = np.zeros((K, K)) #inicializa matriz de conectividades
    for i in range(K): #rellena matriz de conectividades
        for j in range(K):
            num = 0; den = 0
            ej = np.zeros(K); ej[j] = 1
            ei = np.zeros(K); ei[i] = 1
            for h in range(H):
                Ah = PH[h] @ Sigma @ ej
                num += (ei @ Ah)**2 / Sigma[j, j]
                den += (ei @ PH[h] @ Sigma @ PH[h].T @ ei)
            theta[i, j] = num / den
    return theta / theta.sum(axis=1, keepdims=True) #normaliza filas de matriz de conectividades para que cada fila sume 1

def lfv_DCC(thtr, eps_est):
    """
    Traducción directa del código MATLAB proporcionado:

      function [lfv,vrho]=lfv_DCC(thtr)

    Entradas:
      - thtr : parámetros transformados (2x1)
      - eps_est : matriz T x 2 de residuos estandarizados

    Salidas:
      - lfv : log-verosimilitud del componente DCC
      - vrho : vector (T,) con correlaciones dinámicas Rt[0,1]
    """

    # ----- 1. Recuperar parámetros alpha y beta desde transformación logística -----
    alpha = np.exp(thtr[0]) / (1 + np.exp(thtr[0]) + np.exp(thtr[1]))
    beta  = np.exp(thtr[1]) / (1 + np.exp(thtr[0]) + np.exp(thtr[1]))

    T = eps_est.shape[0]

    # ----- 2. Matriz de correlación incondicional -----
    Qbar = np.corrcoef(eps_est.T)

    # ----- 3. Inicialización -----
    Qt = Qbar.copy()
    vrho = np.zeros(T)
    lfv = 0.0

    # ----- 4. Bucle principal del DCC -----
    for t in range(T):

        # Actualización del Qt (DCC original de Engle 2002)
        et = eps_est[t, :].reshape(2, 1)                    # vector columna 2x1
        Qt = ((1 - alpha - beta) * Qbar
              + alpha * (et @ et.T)
              + beta * Qt)

        # Convertir Qt a Rt (matriz de correlaciones)
        Qtilde = np.sqrt(np.diag(Qt))
        Qtilde_inv = np.diag(1 / Qtilde)
        Rt = Qtilde_inv @ Qt @ Qtilde_inv

        # Guardar correlación dinámica ρ_t
        vrho[t] = Rt[0, 1]

        # Contribución a la log-verosimilitud
        lfv += np.log(np.linalg.det(Rt)) + (et.T @ np.linalg.inv(Rt) @ et)[0, 0]

    # Factor final (igual al MATLAB)
    lfv = 0.5 * lfv

    return lfv, vrho


def dynamic_hedge_ratio_MS(returns):
    """
    Estima un modelo Markov Switching con 2 regímenes para:

        Δs_t = γ0_s + γ1_s Δf_t + u_t

    y calcula el ratio de cobertura dinámico:

        γ1_(t|t-1) = ξ_(1,t|t-1)*γ1_1 + (1-ξ_(1,t|t-1))*γ1_2

    Parámetros
    ----------
    returns : pd.DataFrame
        Primera columna: futuro
        Segunda columna: spot

    Returns
    -------
    dict con:
        - gammas: coeficientes γ1 por régimen
        - xi_filtered: probabilidades filtradas ξ_(t|t-1)
        - hedge_ratio: ratio de cobertura dinámico
        - result: objeto de resultados del modelo
    """

    # -----------------------------
    # 1) Retornos
    # -----------------------------
    df = returns.copy()
    # Corrected: Use column names 'futuro' and 'spot' instead of integer indices
    df["df"] = returns["futuro"]   # Δf
    df["ds"] = returns["spot"]    # Δs
    df = df.dropna()

    # -----------------------------
    # 2) Estimar Markov Switching
    # Δs_t = γ0_s + γ1_s * Δf_t + error
    # -----------------------------
    model = MarkovRegression(
        df["ds"],
        k_regimes=2,
        exog=df[["df"]],
        switching_variance=True,
        switching_exog=True  # γ0_s y γ1_s cambian por régimen
    )

    res = model.fit()

    # -----------------------------
    # 3) Extraer parámetros γ1
    # -----------------------------
    param_names = res.model.param_names
    params = res.params

    # Encontrar índices de los γ1_s
    # Original: gamma1_idx = [i for i, name in enumerate(param_names) if "df" in name]
    # Correction: Use 'x1' as it appears in statsmodels' parameter names for exogenous variables
    gamma1_idx = [i for i, name in enumerate(param_names) if "x1" in name]

    gamma1_regime1 = params[gamma1_idx[0]]
    gamma1_regime2 = params[gamma1_idx[1]]

    gammas = np.array([gamma1_regime1, gamma1_regime2])

    # -----------------------------
    # 4) Probabilidades filtradas ξ_(t|t−1)
    # -----------------------------
    xi_filtered = res.filtered_marginal_probabilities[1]  # régimen 1

    # -----------------------------
    # 5) Ratio de cobertura dinámico
    # γ1_(t|t-1) = ξ1*γ1_1 + (1-ξ1)_*_
    hedge_ratio = xi_filtered * gammas[0] + (1 - xi_filtered) * gammas[1]

    return {
        "gammas": gammas,
        "xi_filtered": xi_filtered,
        "hedge_ratio": hedge_ratio,
        "result": res
    }


def fit_AR_GARCH_ML_gaussian_noise_SLSQP(
    X, 
    phi_0_seed, 
    phi_seed, 
    kappa_seed,
    alpha_seed,
    beta_seed,
):
    """ ML estimation of an AR+GARCH model with Gaussian noise."""
    from scipy.optimize import minimize
    
    def minus_log_likelihood_AR_GARCH_gaussian_noise(parameters, X):
        """ Minus log-likelihood of AR(p) model given the data (Gaussian)."""

        phi_0 = parameters[0]
        phi = parameters[1:p+1]
        kappa = parameters[p+1]
        alpha = parameters[p+2:p+r+2]
        beta = parameters[p+r+2:]

        u, h = residuals_ARMA_GARCH(X, phi_0, phi, [], kappa, alpha, beta)

        return - np.mean(
            norm.logpdf(u, loc=0.0, scale=np.sqrt(h))
        ) * np.shape(X)[0]

    
    p = len(phi_seed)
    r = len(alpha_seed)
    s = len(beta_seed)
    parameters_seed = np.zeros((1 + p) + (1 + r + s))
    parameters_seed[0] = phi_0_seed
    parameters_seed[1:p+1] = phi_seed
    parameters_seed[p+1] = kappa_seed
    parameters_seed[p+2:p+r+2] = alpha_seed
    parameters_seed[p+r+2:] = beta_seed
    
    # Bounds: phi ∈ (-1, 1), kappa > 0, alpha >= 0, beta >= 0
    bounds = (
        [(None, None)] +                  # phi_0 (no restricción)
        [(-1 + 1e-6, 1 - 1e-6)] * p +     # phi_i ∈ (-1, 1)
        [(1e-6, None)] +                  # kappa > 0
        [(0.0, None)] * r +               # alpha_i ≥ 0
        [(0.0, None)] * s                # beta_j ≥ 0
        )

    # Restricción: sum(alpha) + sum(beta) < 1
    def make_garch_stationarity_constraint(p, r, s):
        def constraint(parameters):
            alpha = parameters[p+2:p+2+r]
            beta = parameters[p+2+r:p+2+r+s]
            return 1.0 - (np.sum(alpha) + np.sum(beta))  # >= 0
        return constraint
    
    constraints = [{
        'type': 'ineq',
        'fun': make_garch_stationarity_constraint(p, r, s)
        }]

    
    info_optimization = minimize(
        lambda parameters: minus_log_likelihood_AR_GARCH_gaussian_noise(
            parameters, X),
        parameters_seed,  
        method='SLSQP',
        bounds=bounds,
        constraints=constraints,
        options={'xatol': 1e-8, 'disp': False}
    )
    parameters = info_optimization.x  
    phi_0 = parameters[0]
    phi = parameters[1:p+1]
    kappa = parameters[p+1]
    alpha = parameters[p+2:p+r+2]
    beta = parameters[p+r+2:]
    return phi_0, phi, kappa, alpha, beta, info_optimization

def residuals_ARMA_GARCH(X, phi_0, phi, theta, kappa, alpha, beta):
    """Residuals of an AR + GARCH model."""

    p = len(phi)
    q = len(theta)
    r = len(alpha)    
    s = len(beta)
    delay = max([p, q, r, s])

    vector = (X.ndim == 1)
    if vector:
        X = X[np.newaxis, :] # time series as a two dimensional array

    n_times = np.shape(X)[1]    

    u = np.empty_like(X)
    h = np.empty_like(X)
    
    # Assume that the initial values of the innovations 
    # are errors of the prediction by the unconditional mean.`
    u[:, :delay] = X[:, :delay] - phi_0 / (1.0 - np.sum(phi)) 

    # Assume that the initial values of the volatility term is the 
    # unconditional variance.
    h[:, :delay] = kappa / (1.0 - np.sum(alpha) - np.sum(beta))
    
    for t in range(delay, n_times):
        u[:, t] = (
            X[:, t] - (
                phi_0
                + X[:, t - np.arange(1, p + 1)] @ phi
                + u[:, t - np.arange(1, q + 1)] @ theta
            )
        )
        
        h[:, t] = (
            kappa
            + u[:, t - np.arange(1, r + 1)]**2 @ alpha
            + h[:, t - np.arange(1, s + 1)] @ beta
        )
        
    if vector:  # Return values as one-dimensional arrays.
        return np.ravel(u), np.ravel(h) 
    else:
        return u, h

def fit_AR_GARCH_ML_student_t_noise_SLSQP(
    X, 
    phi_0_seed, 
    phi_seed, 
    kappa_seed,
    alpha_seed,
    beta_seed,
    nu_seed,
):
    """ ML estimation of an AR+GARCH model with Student's t noise."""
    from scipy.optimize import minimize
    from scipy.special import gammaln

    p = len(phi_seed)
    r = len(alpha_seed)
    s = len(beta_seed)
    
    def minus_log_likelihood_AR_GARCH_t_student_noise(parameters, X):
        """ Minus log-likelihood of AR(p) model given the data (Gaussian)."""

        phi_0 = parameters[0]
        phi = parameters[1:p+1]
        kappa = parameters[p+1]
        alpha = parameters[p+2:p+r+2]
        beta = parameters[p+r+2: -1]
        nu = parameters[-1]

        u, h = residuals_ARMA_GARCH(X, phi_0, phi, [], kappa, alpha, beta)

        log_likelihood = ( gammaln((nu + 1) / 2)
                          - 0.5 * np.log(np.pi * (nu-2))
                          - gammaln(nu / 2) - 0.5 * np.log(h)
                          - ((nu + 1) / 2) * np.log(1 + (u**2) / ((nu-2) * h))

        )
        return - np.sum(log_likelihood) 

    parameters_seed = np.zeros((1 + p) + (1 + r + s + 1))
    parameters_seed[0] = phi_0_seed
    parameters_seed[1:p+1] = phi_seed
    parameters_seed[p+1] = kappa_seed
    parameters_seed[p+2:p+r+2] = alpha_seed
    parameters_seed[p+r+2: -1] = beta_seed
    parameters_seed[-1] = nu_seed
    # Bounds
    bounds = [(None, None)]                  # phi_0 (no restricción)
    bounds += [(-0.999, 0.999)] * p          # |phi_i| < 1
    bounds += [(1e-6, None)]                 # kappa > 0
    bounds += [(0.0, None)] * r              # alpha_i >= 0
    bounds += [(0.0, None)] * s              # beta_j >= 0
    bounds += [(2.0001, None)]               # nu > 2

    # Constraint: sum(alpha) + sum(beta) < 1 → 1 - (sum) >= 0
    def garch_stationarity_constraint(parameters):
        alpha = parameters[p+2:p+2+r]
        beta = parameters[p+2+r:p+2+r+s]
        return 1.0 - (np.sum(alpha) + np.sum(beta))

    constraints = [{
        'type': 'ineq',
        'fun': garch_stationarity_constraint
        }]

    info_optimization = minimize(
        lambda parameters: minus_log_likelihood_AR_GARCH_t_student_noise(
            parameters, X),
        parameters_seed,  
        method='SLSQP',
        bounds=bounds,
        constraints=constraints,
        options={'xatol': 1e-8, 'disp': False}
    )
    parameters = info_optimization.x  
    phi_0 = parameters[0]
    phi = parameters[1:p+1]
    kappa = parameters[p+1]
    alpha = parameters[p+2:p+r+2]
    beta = parameters[p+r+2:-1]
    nu = parameters[-1]

    return phi_0, phi, kappa, alpha, beta, nu, info_optimization

def residuals_ARMA_GARCH_M(
        X,
        phi_0,
        phi,
        theta,
        kappa,
        alpha,
        beta,
        c):
    """Residuals of an ARMA + GARCH-M model."""
    
    p = len(phi)
    q = len(theta) 
    r = len(alpha)    
    s = len(beta)
    delay = max([p, q, r, s])

    vector = (X.ndim == 1)
    if vector:
        X = X[np.newaxis, :] # time series as a two dimensional array
    
    n_times = np.shape(X)[1]    
    
    u = np.zeros_like(X) 
    h = np.zeros_like(X) 
    
    # Assume that the initial values of the innovations 
    # are errors of the prediction by the unconditional mean.`
    u[:, :delay] = X[:, :delay] - phi_0 / (1.0 - np.sum(phi)) 

    # Assume that the initial values of the volatility term is the 
    # unconditional variance.
    h[:, :delay] = kappa / (1.0 - np.sum(alpha) - np.sum(beta))
    
    for t in range(delay, n_times):
        h[:, t] = (
            kappa
            + u[:, t - np.arange(1, r + 1)]**2 @ alpha
            + h[:, t - np.arange(1, s + 1)] @ beta
        )

        u[:, t] = (
            X[:, t] - (
                phi_0
                + X[:, t - np.arange(1, p + 1)] @ phi
                + u[:, t - np.arange(1, q + 1)] @ theta
                + c * h[:, t]
            )
        )
        
    if vector:  # Return values as one-dimensional arrays.
        return np.ravel(u), np.ravel(h)
    else:
        return u, h

def fit_AR_GARCH_M_ML_gaussian_noise_SLSQP(
    X, 
    phi_0_seed, 
    phi_seed, 
    kappa_seed, 
    alpha_seed, 
    beta_seed, 
    c_seed  
):
    """ ML estimation of an AR+GARCH-M model with Gaussian noise."""
    from scipy.optimize import minimize

    def minus_log_likelihood_AR_GARCH_M_gaussian_noise(parameters, X):
        phi_0 = parameters[0]
        phi = parameters[1:p+1]
        kappa = parameters[p+1]
        alpha = parameters[p+2:p+r+2]
        beta = parameters[p+r+2: -1]
        c = parameters[-1] 

        u, h = residuals_ARMA_GARCH_M(X, phi_0, phi, [], kappa, alpha, beta, c)

        return - np.mean(
            norm.logpdf(u, loc=0.0, scale=np.sqrt(h))
        ) * np.shape(X)[0]

    
    p = len(phi_seed)
    r = len(alpha_seed)
    s = len(beta_seed)
    parameters_seed = np.zeros((1 + p) + (1 + r + s + 1))
    parameters_seed[0] = phi_0_seed
    parameters_seed[1:p+1] = phi_seed
    parameters_seed[p+1] = kappa_seed
    parameters_seed[p+2:p+r+2] = alpha_seed
    parameters_seed[p+r+2: -1] = beta_seed
    parameters_seed[-1] = c_seed
    
    # Bounds: phi ∈ (-1, 1), kappa > 0, alpha >= 0, beta >= 0
    bounds = (
        [(None, None)] +          # phi_0 (no restricción)
        [(-0.999, 0.999)] * p +   # phi ∈ (-1, 1)
        [(1e-6, None)] +          # kappa > 0
        [(0.0, None)] * r +       # alpha_i >= 0
        [(0.0, None)] * s +       # beta_j >= 0
        [(None, None)]            # c (no restricción)
        )
    
    # Restricción: sum(alpha) + sum(beta) < 1
    def make_garch_stationarity_constraint(p, r, s):
        def constraint(parameters):
            alpha = parameters[p+2:p+r+2]
            beta = parameters[p+2+r: p+2+r+s]
            return 1.0 - (np.sum(alpha) + np.sum(beta)) #>=0
        return constraint
    constraints = [{
        'type': 'ineq',
        'fun': make_garch_stationarity_constraint(p, r, s)}]

    info_optimization = minimize(
        lambda parameters: minus_log_likelihood_AR_GARCH_M_gaussian_noise(
            parameters, X),
        parameters_seed,  
        method='SLSQP',
        bounds=bounds,
        constraints=constraints,
        options={'xatol': 1e-8, 'disp': False}
    )
    
    parameters = info_optimization.x
    phi_0 = parameters[0]
    phi = parameters[1:p+1]
    kappa = parameters[p+1]
    alpha = parameters[p+2:p+r+2]
    beta = parameters[p+r+2:-1]
    c = parameters[-1]

    return phi_0, phi, kappa, alpha, beta, c, info_optimization


def residuals_ARMA_EGARCH(
        X,
        phi_0,
        phi,
        theta,
        kappa,
        alpha,
        beta,
        gamma):
    """Residuals of an ARMA + EGARCH model."""
    
    p = len(phi)
    q = len(theta) 
    r = len(alpha)    
    s = len(beta)
    delay = max([p, q, r, s])
    
    if r != len(gamma):
        raise ValueError(
            f"Error: len(alpha) = {r} debe ser igual a len(gamma) = {len(gamma)}."
    )

    vector = (X.ndim == 1)
    if vector:
        X = X[np.newaxis, :] # time series as a two dimensional array
    
    n_times = np.shape(X)[1]    
    
    u = np.zeros_like(X) 
    logh = np.zeros_like(X) 
    
    # Assume that the initial values of the innovations 
    # are errors of the prediction by the unconditional mean.`
    u[:, :delay] = X[:, :delay] - phi_0 / (1.0 - np.sum(phi)) 

    # Assume that the initial values of the log-variance term  
    # is the sample log-variance.
    logh[:, :delay] = np.log(np.var(X, axis=1))[:, np.newaxis]
    
    for t in range(delay, n_times):
        logh[:, t] = (
            kappa
            + ( (np.abs(u[:, t - np.arange(1, r + 1)])
               + u[:, t - np.arange(1, r + 1)] * gamma
               ) / np.exp(0.5 * logh[:, t - np.arange(1, r + 1)])
               )@ alpha
            + logh[:, t - np.arange(1, s + 1)] @ beta
        )

        u[:, t] = (
            X[:, t] - (
                phi_0
                + X[:, t - np.arange(1, p + 1)] @ phi
                + u[:, t - np.arange(1, q + 1)] @ theta
            )
        )
        
    if vector:  # Return values as one-dimensional arrays.
        return np.ravel(u), np.ravel(logh)
    else:
        return u, logh

def fit_AR_EGARCH_ML_gaussian_noise_SLSQP(
    X, 
    phi_0_seed, 
    phi_seed, 
    kappa_seed, 
    alpha_seed, 
    beta_seed, 
    gamma_seed  
):
    """ ML estimation of an AR+EGARCH model with Gaussian noise."""
    from scipy.optimize import minimize

    def minus_log_likelihood_AR_EGARCH_gaussian_noise(parameters, X):
        phi_0 = parameters[0]
        phi = parameters[1:p+1]
        kappa = parameters[p+1]
        alpha = parameters[p+2:p+r+2]
        beta = parameters[p+r+2: p+r+s+2]
        gamma = parameters[p+r+s+2:p+r+s+2+r] 

        u, logh = residuals_ARMA_EGARCH(X, phi_0, phi, [], kappa, alpha, beta, gamma)

        return - np.mean(
            norm.logpdf(u, loc=0.0, scale=np.exp(0.5 *logh))
        ) * np.shape(X)[0]

    
    p = len(phi_seed)
    r = len(alpha_seed)
    s = len(beta_seed)
    
    parameters_seed = np.zeros((1 + p) + (1 + r + s + r))
    parameters_seed[0] = phi_0_seed
    parameters_seed[1:p+1] = phi_seed
    parameters_seed[p+1] = kappa_seed
    parameters_seed[p+2:p+r+2] = alpha_seed
    parameters_seed[p+r+2:p+r+s+2] = beta_seed
    parameters_seed[p+r+s+2:p+r+s+2+r] = gamma_seed
    
    # Bounds: phi ∈ (-1, 1), beta ∈ (-1, 1)
    bounds = (
        [(None, None)] +          # phi_0 (no restricción)
        [(-0.999, 0.999)] * p +   # phi ∈ (-1, 1)
        [(None, None)]        +   # kappa (no restricción)
        [(None, None)]    * r +   # alpha_i (no restricción)
        [(-0.999, 0.999)] * s +   # beta ∈ (-1, 1)
        [(None, None)]    * r     # gamma (no restricción)
        )
    

    info_optimization = minimize(
        lambda parameters: minus_log_likelihood_AR_EGARCH_gaussian_noise(
            parameters, X),
        parameters_seed,  
        method='SLSQP',
        bounds=bounds,
        options={'xatol': 1e-8, 'disp': False}
    )
    
    parameters = info_optimization.x
    phi_0 = parameters[0]
    phi = parameters[1:p+1]
    kappa = parameters[p+1]
    alpha = parameters[p+2:p+r+2]
    beta = parameters[p+r+2:p+r+s+2]
    gamma = parameters[p+r+s+2:p+r+s+2+r]

    return phi_0, phi, kappa, alpha, beta, gamma, info_optimization

def residuals_ARMA_TGARCH(
        X,
        phi_0,
        phi,
        theta,
        kappa,
        alpha,
        beta,
        gamma):
    """Residuals of an ARMA + TGARCH(1,1) model."""
    
    p = len(phi)
    q = len(theta) 
    r = len(alpha)    
    s = len(beta)
    delay = max([p, q, r, s])
    
    if r != 1 or s != 1:
        raise ValueError("TGARCH(1,1) requiere len(alpha)=1 y len(beta)=1.")

    vector = (X.ndim == 1)
    if vector:
        X = X[np.newaxis, :] # time series as a two dimensional array
    
    n_times = np.shape(X)[1]    
    
    u = np.zeros_like(X) 
    h = np.zeros_like(X) 
    
    # Assume that the initial values of the innovations 
    # are errors of the prediction by the unconditional mean.`
    u[:, :delay] = X[:, :delay] - phi_0 / (1.0 - np.sum(phi)) 

    # Assume that the initial values of the volatility term is the 
    # unconditional variance.
    h[:, :delay] = kappa / (1.0 - np.sum(alpha) - np.sum(beta) - gamma / 2.0)
    
    for t in range(delay, n_times):
        I_neg = (u[:, t-1] < 0).astype(float)
        h[:, t] = (
            kappa
            + u[:, t - np.arange(1, r + 1)]**2 * (alpha + gamma * I_neg)
            + h[:, t - np.arange(1, s + 1)] @ beta
        )

        u[:, t] = (
            X[:, t] - (
                phi_0
                + X[:, t - np.arange(1, p + 1)] @ phi
                + u[:, t - np.arange(1, q + 1)] @ theta
            )
        )
        
    if vector:  # Return values as one-dimensional arrays.
        return np.ravel(u), np.ravel(h)
    else:
        return u, h

def fit_AR_TGARCH_ML_gaussian_noise_SLSQP(
    X, 
    phi_0_seed, 
    phi_seed, 
    kappa_seed, 
    alpha_seed, 
    beta_seed, 
    gamma_seed  
):
    """ ML estimation of an AR+TGARCH(1,1) model with Gaussian noise."""
    from scipy.optimize import minimize

    def minus_log_likelihood_AR_GARCH_M_gaussian_noise(parameters, X):
        phi_0 = parameters[0]
        phi = parameters[1:p+1]
        kappa = parameters[p+1]
        alpha = parameters[p+2:p+r+2]
        beta = parameters[p+r+2: -1]
        gamma = parameters[-1] 

        u, h = residuals_ARMA_TGARCH(X, phi_0, phi, [], kappa, alpha, beta, gamma)

        return - np.mean(
            norm.logpdf(u, loc=0.0, scale=np.sqrt(h))
        ) * np.shape(X)[0]

    
    p = len(phi_seed)
    r = len(alpha_seed)
    s = len(beta_seed)
    parameters_seed = np.zeros((1 + p) + (1 + r + s + 1))
    parameters_seed[0] = phi_0_seed
    parameters_seed[1:p+1] = phi_seed
    parameters_seed[p+1] = kappa_seed
    parameters_seed[p+2:p+r+2] = alpha_seed
    parameters_seed[p+r+2: -1] = beta_seed
    parameters_seed[-1] = gamma_seed
    
    # Bounds: phi ∈ (-1, 1), kappa > 0, alpha >= 0, beta >= 0
    bounds = (
        [(None, None)] +          # phi_0 (no restricción)
        [(-0.999, 0.999)] * p +   # phi ∈ (-1, 1)
        [(1e-6, None)] +          # kappa > 0
        [(0.0, None)] * r +       # alpha_i >= 0
        [(0.0, None)] * s +       # beta_j >= 0
        [(None, None)]            # gamma (no restricción)
        )
    
    # Restricción: sum(alpha) + sum(beta) < 1
    def stationarity_constraint(p, r, s):
        def constraint(parameters):
            alpha = parameters[p+2:p+r+2]
            beta = parameters[p+2+r: p+2+r+s]
            gamma = parameters[-1] 
            return 1.0 - (np.sum(alpha) + np.sum(beta) + gamma / 2.0) #>=0
        return constraint
    def positivity_constraint(p, r):
        def constraint(parameters):
            alpha = parameters[p+2:p+r+2]
            gamma = parameters[-1] 
            return alpha + gamma #>=0
        return constraint
    
    constraints = [
        {'type': 'ineq', 'fun': stationarity_constraint(p, r, s)},
        {'type': 'ineq', 'fun': positivity_constraint(p, r)}
    ]

    info_optimization = minimize(
        lambda parameters: minus_log_likelihood_AR_GARCH_M_gaussian_noise(
            parameters, X),
        parameters_seed,  
        method='SLSQP',
        bounds=bounds,
        constraints=constraints,
        options={'xatol': 1e-8, 'disp': False}
    )
    
    parameters = info_optimization.x
    phi_0 = parameters[0]
    phi = parameters[1:p+1]
    kappa = parameters[p+1]
    alpha = parameters[p+2:p+r+2]
    beta = parameters[p+r+2:-1]
    gamma = parameters[-1]

    return phi_0, phi, kappa, alpha, beta, gamma, info_optimization

def plot_volatility(ax, true_vol, est_vol, title):
    ax.plot(true_vol, label='Varianza Real ($\sigma^2_t$)', color='blue', alpha=0.5)
    ax.plot(est_vol, label='Varianza Estimada ($\hat{\sigma}^2_t$)', color='red', linestyle='--')
    ax.set_title(title)
    ax.legend()
    ax.grid(True, linestyle=':', alpha=0.6)

