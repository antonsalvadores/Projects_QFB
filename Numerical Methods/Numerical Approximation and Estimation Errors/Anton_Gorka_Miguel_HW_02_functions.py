# -*- coding: utf-8 -*-
"""
Created on Wed Apr 16 17:47:35 2025

@author: Antón Salvadores
"""

import math
import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm 
from scipy.integrate import quad
import sys
from typing import Callable, Tuple
from numpy.random import default_rng
from tools_qfb import compare_histogram_pdf


def taylor_approximation(
    x: np.ndarray,
    function: Callable[[float], float],
    function_derivative: Callable[[float, int], float],
    K: int,
    x_0: float = 0.0,
) -> np.ndarray:
    """ Taylor approximation of a function

    Args:
        x: Point at which the approximation es evaluated.
        function: Function to be approximated.
        function_derivative: Derivative of the function.
        K: Order of the approximation (degree of the Taylor polynomial).
        x_0: Point about which the approximation is made.

    Returns:
        Array with the Taylor approximations of the function for different orders.
        The kth row in the array yields the order k approximation of the function.

    Example:
        >>> x = np.array([-1.0, 0.0, 1.0])
        >>> y = taylor_approximation(
        ...         x,
        ...         function=lambda x: np.exp(- x),
        ...         function_derivative=lambda x, k: (1- 2 * (k % 2)) * np.exp(- x),
        ...         K=8,
        ...     )
        >>> print(np.round(y, 6))
        [[1.       1.       1.      ]
         [2.       1.       0.      ]
         [2.5      1.       0.5     ]
         [2.666667 1.       0.333333]
         [2.708333 1.       0.375   ]
         [2.716667 1.       0.366667]
         [2.718056 1.       0.368056]
         [2.718254 1.       0.367857]
         [2.718279 1.       0.367882]]
        >>> print(np.round(np.exp(- x), 6))
        [2.718282 1.       0.367879]
    """
    approximation = np.empty((K+1, len(x)))
    approximation[0, :] = function(x_0)

#   x -= x_0 # Center at x_0

    for k in range(1, K+1):
        approximation[k, :] = (
            approximation[k-1, :]
            + function_derivative(x_0, k) * (x-x_0)**k / math.factorial(k)
        )

    return approximation

def plot_taylor_approximation(
    function: Callable[[float], float],
    function_derivative: Callable[[float, int], float],
    K: int,
    x_0: float,
    interval_plot: Tuple[float, float],
    figure_size: Tuple[int, int],
) -> None:

    x = np.linspace(*interval_plot, num=1000)
    y_exact = function(x)
    y_taylor = taylor_approximation(x, function, function_derivative, K, x_0)

    fig, axs = plt.subplots(K + 1, 2, sharex=True, figsize=figure_size)

    for k, y in enumerate(y_taylor):
        axs[k, 0].plot(x, y_exact, label='exact')
        axs[k, 0].plot(x, y_taylor[k, :], label='Taylor(k = {:d})'.format(k))
        axs[k, 0].set_xlabel('$x$')
        axs[k, 0].set_ylabel('$f(x)$')
        axs[k, 0].legend()

        error = y_taylor[k, :] - y_exact
        axs[k, 1].plot(x, error, label='error')
        axs[k, 1].set_xlabel('$x$')
        axs[k, 1].set_ylabel('$error$')
        axs[k, 1].legend()
        axs[k, 1].axhline(y = 0.0, color = 'k', linestyle = ':')
        
def taylor_approximation_new(
    x: np.ndarray,
    function: Callable[[float], float],
    c : Callable[[int], float],
    K: int,
    x_0: float = 0.0,
) -> np.ndarray:

    approximation = np.empty((K+1, len(x)))
    approximation[0, :] = function(x_0)

    for k in range(1, K+1):
        approximation[k, :] = (
            approximation[k-1, :]
            + c(k) * (x-x_0)**k
        )

    return approximation         
        
def plot_taylor_approximation_new(
    function: Callable[[float], float],
    c: Callable[[int], float],
    K: int,
    x_0: float,
    interval_plot: Tuple[float, float],
    figure_size: Tuple[int, int],
) -> None:

    x = np.linspace(*interval_plot, num=1000)
    y_exact = function(x)
    y_taylor = taylor_approximation_new(x, function, c, K, x_0)

    fig, axs = plt.subplots(K + 1, 2, sharex=True, figsize=figure_size)

    for k, y in enumerate(y_taylor):
        axs[k, 0].plot(x, y_exact, label='exact')
        axs[k, 0].plot(x, y_taylor[k, :], label='Taylor(k = {:d})'.format(k))
        axs[k, 0].set_xlabel('$x$')
        axs[k, 0].set_ylabel('$f(x)$')
        axs[k, 0].legend()

        error = y_taylor[k, :] - y_exact
        axs[k, 1].plot(x, error, label='error')
        axs[k, 1].set_xlabel('$x$')
        axs[k, 1].set_ylabel('$error$')
        axs[k, 1].legend()
        axs[k, 1].axhline(y = 0.0, color = 'k', linestyle = ':')

def left_derivate(
        f: Callable[[np.ndarray], np.ndarray],
        x0: float,
        h: float = 1.0e-6,
        scale_h: bool = True,
    ) -> float:
        if scale_h:
            return (f(x0) - f(x0 * (1.0-h))) / (x0 * h)
        else:                    
            return (f(x0)  - f(x0 - h)) / (h)


def right_derivate(
        f: Callable[[np.ndarray], np.ndarray],
        x0: float,
        h: float = 1.0e-6,
        scale_h: bool = True,
    ) -> float:        
        if scale_h:
            return (f(x0 * (1.0+h)) - f(x0)) / (x0 * h)
        else:                    
            return (f(x0 + h)  - f(x0)) / (h)


def central_derivate(
        f: Callable[[np.ndarray], np.ndarray],
        x0: float,
        h: float = 1.0e-6,
        scale_h: bool = True,
    ) -> float:
        if scale_h:
            return (f(x0 * (1.0+h)) - f(x0 * (1.0-h))) / (2.0 * x0 * h)
        else:                    
            return (f(x0 + h)  - f(x0 - h)) / (2.0 * h)
        

def h_optimo_derivate(
    f: Callable[[np.ndarray], np.ndarray],
    f_derivative: Callable[[np.ndarray], np.ndarray],
    derivative_func: Callable[
        [Callable[[np.ndarray], np.ndarray], float, float, bool],
        float
    ],
    x_0: float = 1.0,
    scale_h: bool = True,              
) -> float:
    '''

    Parameters
    ----------
    f : función que queremos aproximar su derivada en x_0
    f_derivative : función derivada de la función que queremos aproximar su
                   derivada, para poder calcular el error y el h óptimo.
    derivative_func : fórmula de derivación numérica que queremos utilizar
                      para calcular el h óptimo.
    x_0 : punto en el que queremos aproximar la derivada. Por defecto, x_0 = 1
    scale_h : el usuario elige si quiere emplear la fórmula utilizando
              el error relativo (por defecto, scale_h = True) o utilizando
              el error absoluto (scale_h = False). Si x_0 = 0.0, la función
              por defecto utilizará la fórmula con el error absoluto

    Returns
    -------
    Devuelve el valor óptimo del h para el que se minimiza el error de la 
    derivada.

    '''
    if x_0 == 0.0:
        scale_h_internal = False
    else:
        scale_h_internal = scale_h

    h_vals = np.logspace(-1, -16, 1600)
    true_value = f_derivative(x_0)

    errores = []
    for h in h_vals:
        approx = derivative_func(f, x_0, h, scale_h=scale_h_internal)
        errores.append(abs(approx - true_value))

    idx_min = np.argmin(errores)
    return h_vals[idx_min]


        
def euler_integration(
    a: Callable[[float, float], float],
    t_0: float,
    f_0: float,
    T: float,
    N: int = 0.0,
) -> Tuple[np.ndarray, np.ndarray]:
    """ Euler integration method for an explicit first-order ODE.

    Args:
        a: Expersion of the derivative of f(t) as a function of t and f(t).
        t_0: intial time.
        f_0: initial condition,
        T: length of the integration interval.
        N: number of integration steps.

    Returns:
        A tuple with two elements:
        A one-dimensional array with the points in time at which the trajectory is monitored.
        A one-dimensional array with the values of the trajectory at the monitoring points.

    Example:
    >>> t, f = euler_integration(
    ...    a=lambda t, f: np.cos(t) * np.exp(- t),
    ...    t_0=-2.0,
    ...    f_0=1.0,
    ...    T=2.0,
    ...    N=5,
    ... )
    >>> print(t)
    [-2.  -1.6 -1.2 -0.8 -0.4  0. ]
    >>> print(f)
    [ 1.         -0.22997293 -0.2878234   0.19340464  0.81362436  1.36324898]

    """
    delta_T = T / N # integratrion step

    t = t_0 + np.arange(N+1) * delta_T  # integration grid
    f = np.empty_like(t) # reserve memory

    f[0] = f_0
    for n in range(N):
        f[n + 1] = f[n] + a(t[n], f[n]) * delta_T

    return t, f
        
def euler_integration_new(
    a: Callable[[float, float], float],
    t_0: float,
    f_0: float,
    T: float,
    N: int = 0.0,
) -> Tuple[np.ndarray, np.ndarray]:
    """ Euler integration method for an explicit first-order ODE.

    Args:
        a: Expersion of the derivative of f(t) as a function of t and f(t).
        t_0: intial time.
        f_0: initial condition,
        T: length of the integration interval.
        N: number of integration steps.

    Returns:
        A tuple with two elements:
        A one-dimensional array with the points in time at which the trajectory is monitored.
        A one-dimensional array with the values of the trajectory at the monitoring points.

    Example:
    >>> t, f = euler_integration(
    ...    a=lambda t, f: np.cos(t) * np.exp(- t),
    ...    t_0=-2.0,
    ...    f_0=1.0,
    ...    T=2.0,
    ...    N=5,
    ... )
    >>> print(t)
    [-2.  -1.6 -1.2 -0.8 -0.4  0. ]
    >>> print(f)
    [ 1.         -0.22997293 -0.2878234   0.19340464  0.81362436  1.36324898]

    """
    delta_T = T / N # integratrion step

    t = t_0 + np.arange(N+1) * delta_T  # integration grid
    f = np.empty_like(t) # reserve memory

    f[0] = f_0
    for n in range(N):
        t_m = t[n] + delta_T / 2
        f_m = f[n] + a(t[n], f[n]) * (delta_T / 2)
        f[n + 1] = f[n] + a(t_m, f_m) * delta_T

    return t, f        
        
        
def demo_CLT(random_number_generator, sample_size, n_repetitions, n_plot):

    n_terms = np.arange(1, (sample_size + 1))

    X = random_number_generator((n_plot, sample_size))
    cum_mean_X = np.cumsum(X, axis=1) / n_terms

    fig, axs = plt.subplots(2, 1, figsize=(10,10))

    for mean in cum_mean_X:
        axs[0].plot(n_terms, mean)
        axs[0].set_xlabel(r'$M$')
        axs[0].set_ylabel(r'$ \left< g(X) \right>_M$')

    X = random_number_generator((sample_size, n_repetitions))

    mean_X = np.mean(X, axis=0)   # n_repetitions samples of mean estimates
    std_mean_X = np.std(X) / np.sqrt(sample_size)  # standard deviation of the mean estimates (CLT)

    _ = compare_histogram_pdf(
        mean_X,
        pdf=lambda x: norm.pdf(x, np.mean(mean_X), std_mean_X),
        n_bins=50,
        ax=axs[1],
    )        
        
        
        
        
        
        
        
        
        
        
        
        
        