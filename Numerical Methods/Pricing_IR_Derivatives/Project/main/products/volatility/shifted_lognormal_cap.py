from typing import List, Dict
from scipy.optimize import brentq
from products.volatility.shifted_lognormal_caplet import ShiftedLognormalCaplet
from underlyings.interest_rate_curve import InterestRateCurve


class ShiftedLognormalCap:
    def __init__(self,
                 notional: float,
                 strike: float,
                 day_count: str,
                 schedule: List[Dict]):
        self.notional = notional
        self.strike = strike
        self.day_count = day_count
        self.schedule = schedule
        self.caplets = []

        # Instanciación de la cartera de Caplets
        for period in self.schedule:
            caplet = ShiftedLognormalCaplet(
                notional=self.notional,
                strike=self.strike,
                start_date=period['start_date'],
                end_date=period['end_date'],
                fixing_date=period['fixing_date'],
                day_count=self.day_count
            )
            self.caplets.append(caplet)

    def npv(self,
            discount_curve: InterestRateCurve,
            forward_curve: InterestRateCurve,
            implied_volatility: float,
            shift: float) -> float:
        """
        Calcula el valor presente neto del Cap sumando el valor individual
        de todos los Caplets que lo componen, utilizando el Modelo Shifted Lognormal.
        """
        npv = 0.0
        for caplet in self.caplets:
            npv += caplet.npv(discount_curve, forward_curve, implied_volatility, shift)

        return npv

    def implied_volatility(self,
                           target_npv: float,
                           discount_curve: InterestRateCurve,
                           forward_curve: InterestRateCurve,
                           shift: float) -> float:
        """
        Encuentra la volatilidad implícita que iguala el NPV del modelo a un precio objetivo.
        """
        # Definimos la función a minimizar de forma anónima (lambda)
        objective = lambda vol: self.npv(discount_curve, forward_curve, vol, shift) - target_npv

        try:
            # Buscamos la raíz entre un 0.01% y un 200% de volatilidad
            return brentq(objective, 0.0001, 2.0)
        except ValueError:
            return 0.0