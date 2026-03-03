import numpy as np
from typing import List
from scipy.interpolate import RegularGridInterpolator


class NormalVolatilitySurface:
    def __init__(self,
                 tenors: List[float],
                 strikes: List[float],
                 volatilities: np.ndarray,
                 interpolation: str):
        self.tenors = tenors
        self.strikes = strikes
        self.volatilities = volatilities
        self.interpolation = interpolation

        # Al interpolar una matriz (tenors y strikes) necesitamos un interpolador2D,
        # al contrario de lo que pasaba al interpolar la curva de tipos, que son simples
        # puntos en determinadas fechas.

        self.interpolator = RegularGridInterpolator(
            points=(self.tenors, self.strikes),
            values=self.volatilities,
            method=self.interpolation,
            bounds_error=False,
            fill_value=None
        )

    def get_volatility(self, time_to_expiry: float, strike: float) -> float:
        """
        Interpola la volatilidad normal para un tiempo a vencimiento (t)
        y un nivel de strike (K) determinados.
        """
        return float(self.interpolator((time_to_expiry, strike)))