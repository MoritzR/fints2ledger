import collections
from mt940.models import Date

def update_dict(d, u):
    for k, v in u.items():
        if isinstance(v, collections.Mapping):
            d[k] = update_dict(d.get(k, {}), v)
        else:
            d[k] = v
    return d


def date_string_to_mt940_date(date_string):
    parts = date_string.split("/")
    return Date(year=parts[0], month=parts[1], day=parts[2])