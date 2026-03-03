from typing import List, Dict
from products.volatility.normal_caplet import NormalCaplet
from underlyings.interest_rate_curve import InterestRateCurve
from underlyings.normal_volatility_surface import NormalVolatilitySurface


class NormalCap:
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
            caplet = NormalCaplet(
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
            vol_surface: NormalVolatilitySurface) -> float:
        """
        Calcula el valor presente neto del Cap sumando el valor individual
        de todos los Caplets que lo componen, utilizando el Modelo de Bachelier.
        """
        npv = 0.0
        for caplet in self.caplets:
            npv += caplet.npv(discount_curve, forward_curve, vol_surface)
        return npv