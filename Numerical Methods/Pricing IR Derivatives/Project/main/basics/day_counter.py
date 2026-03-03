from datetime import datetime

# Conteo de días según convención

class DayCounter:

    @staticmethod
    def year_fraction(day_count: str,
                      start_date: datetime,
                      end_date: datetime) -> float:
        if day_count == 'act365':
            return (end_date - start_date).days / 365
        elif day_count == 'act360':
            return (end_date - start_date).days / 360
        elif day_count == '30360':
            d1, m1, y1 = start_date.day, start_date.month, start_date.year
            d2, m2, y2 = end_date.day, end_date.month, end_date.year
            d1 = min(d1, 30)
            d2 = min(d2, 30) if d1 == 30 else d2
            return (360 * (y2 - y1) + 30 * (m2 - m1) + (d2 - d1)) / 360
        else:
            raise ValueError(f"Convención de días {day_count} no implementada.")

