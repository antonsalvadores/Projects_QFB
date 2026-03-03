from datetime import datetime
from typing import List
from scipy.interpolate import interp1d
from basics.day_counter import DayCounter

# Crearemos la curva de tipos a través de interpolación de datos conocidos

class InterestRateCurve:
    def __init__(self,
                 start_date: datetime,
                 end_dates: List[datetime],
                 discount_factors: List[float],
                 interpolation: str,
                 day_count: str):
        self.start_date = start_date
        self.end_dates = end_dates
        self.discount_factors = discount_factors
        self.interpolation = interpolation
        self.day_count = day_count
        self.delta_times = [DayCounter.year_fraction(self.day_count,
                                                     self.start_date,
                                                     date)
                            for date in self.end_dates]

        self.interpolator = interp1d(x=self.delta_times,
                                     y=self.discount_factors,
                                     kind=self.interpolation,
                                     fill_value="extrapolate")

    def interpolate(self, date: datetime) -> float:
        """
        Devuelve el Factor de Descuento (DF) para una fecha dada.
        Utilizado principalmente por la curva OIS.
        """
        delta = DayCounter.year_fraction(self.day_count, self.start_date, date)
        return float(self.interpolator(delta))

    def forward_rate(self, start_date: datetime, end_date: datetime) -> float:
        """
        Calcula el tipo forward simplemente compuesto (F) proyectado entre dos fechas.
        Utilizado principalmente por la curva Euribor-6M.
        """
        df_start = self.interpolate(start_date)
        df_end = self.interpolate(end_date)

        # La fracción de año (delta_t) entre inicio y fin del periodo
        delta = DayCounter.year_fraction(self.day_count, start_date, end_date)
        if delta == 0:
            return 0.0
        return ((df_start / df_end) - 1.0) / delta