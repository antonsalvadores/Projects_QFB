import pandas as pd
from pandas.tseries.holiday import AbstractHolidayCalendar, Holiday, EasterMonday, GoodFriday
from datetime import datetime

# Creación calendario Target

class TargetHolidayRules(AbstractHolidayCalendar):
    """Reglas oficiales de festivos del calendario TARGET de la Eurozona."""
    rules = [
        Holiday('New Years Day', month=1, day=1),
        GoodFriday,
        EasterMonday,
        Holiday('Labour Day', month=5, day=1),
        Holiday('Christmas Day', month=12, day=25),
        Holiday('Boxing Day', month=12, day=26)
    ]


class TargetCalendar:
    def __init__(self):

        # Creamos el calendario de días hábiles con los festivos previamente creados

        self.bday = pd.offsets.CustomBusinessDay(calendar=TargetHolidayRules())

    def add_business_days(self, date: datetime, days: int) -> datetime:
        """
        Añade días hábiles para calcular el Spot Lag o el Fixing Date.
        """
        result = pd.Timestamp(date) + days * self.bday
        return result.to_pydatetime()

    def modified_following(self, date: datetime) -> datetime:
        """
        Ajusta la fecha teórica a la fecha real de pago según la convención Modified Following.
        """
        ts_date = pd.Timestamp(date)

        # Si la fecha es un día no hábil, pasa al siguiente hábil

        next_bday = ts_date + 0 * self.bday

        # Comprobar si el salto ha provocado un cambio de mes
        if next_bday.month != ts_date.month:
            # Regla Modified Following: Si cambia de mes, retrocedemos al día hábil anterior.
            # Al restar 1 bday desde el next_bday (que ya sabemos que es el primer
            # día hábil del mes siguiente), caemos en el último día hábil del mes original.
            prev_bday = next_bday - 1 * self.bday
            return prev_bday.to_pydatetime()

        return next_bday.to_pydatetime()

    def is_end_of_month(self, date: datetime) -> bool:
        """
        Comprueba si una fecha dada es el último día hábil de su mes.
        Útil para la convención Rolling Backward EOM.
        """
        ts_date = pd.Timestamp(date)
        # Avanzar al siguiente día hábil
        next_bday = ts_date + 1 * self.bday
        # Si el mes cambia, significa que ts_date era el último día hábil del mes
        return next_bday.month != ts_date.month

