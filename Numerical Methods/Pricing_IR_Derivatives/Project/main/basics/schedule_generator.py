from datetime import datetime
from dateutil.relativedelta import relativedelta
from basics.target_calendar import TargetCalendar


class ScheduleGenerator:
    def __init__(self, calendar: TargetCalendar):
        self.calendar = calendar

    def generate_schedule(self,
                          valuation_date: datetime,
                          spot_lag: int,
                          tenor_years: int,
                          frequency_months: int) -> list:
        """
        Genera el calendario de pagos iterando hacia atrás (Rolling Backward EOM).
        Devuelve una lista de diccionarios con las fechas clave de cada periodo.
        """
        # Fechas clave iniciales
        effective_date = self.calendar.add_business_days(valuation_date, spot_lag)
        maturity_date = effective_date + relativedelta(years=tenor_years)

        # Comprobación regla EOM
        is_eom = self.calendar.is_end_of_month(maturity_date)

        schedule = []
        current_teoric = maturity_date

        # Calcular el número de periodos necesarios
        num_periods = int((tenor_years * 12) / frequency_months)

        # Bucle hacia atrás
        for _ in range(num_periods):
            prev_teoric = current_teoric - relativedelta(months=frequency_months)

            # Ajuste End Date (Payment Date)
            if is_eom:
                primer_dia_mes_sig = datetime(current_teoric.year, current_teoric.month, 1) + relativedelta(months=1)
                end_date_adj = self.calendar.add_business_days(primer_dia_mes_sig, -1)
            else:
                end_date_adj = self.calendar.modified_following(current_teoric)

            # Ajuste Start Date
            if is_eom:
                primer_dia_mes_sig_prev = datetime(prev_teoric.year, prev_teoric.month, 1) + relativedelta(months=1)
                start_date_adj = self.calendar.add_business_days(primer_dia_mes_sig_prev, -1)
            else:
                start_date_adj = self.calendar.modified_following(prev_teoric)

            # Fixing date (-2 días hábiles desde el Start Date ajustado)
            fixing_date = self.calendar.add_business_days(start_date_adj, -2)

            schedule.append({
                'start_date': start_date_adj,
                'end_date': end_date_adj,
                'fixing_date': fixing_date
            })

            current_teoric = prev_teoric

        # Invertir para devolver en orden cronológico
        schedule.reverse()
        return schedule, effective_date, maturity_date