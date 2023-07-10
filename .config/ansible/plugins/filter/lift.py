import typing
import typing as t

F = t.TypeVar("F", bound=t.Callable[..., t.Any])
K = t.TypeVar("K")
V = t.TypeVar("V")

# could probably use overloading


def lift(el: t.Union[t.Any, t.Iterable[V]], **kwargs: t.Any) -> t.Iterable[V]:
    key_name = kwargs.pop("key_name", "key")
    value_name = kwargs.pop("value_name", "value")

    if kwargs:
        raise FilterArgumentError(
            f"Unexpected keyword argument {next(iter(kwargs))!r}")
    else:
        return {key_name: el, value_name: el}


def liftid(el: t.Union[t.Any, t.Iterable[V]]) -> t.Iterable[V]:
    return {el: el}


def liftkey(el: t.Union[t.Any, t.Iterable[V]], key_name: str) -> t.Iterable[V]:
    return {key_name: el}


FILTERS = {'lift': lift, 'liftid': liftid, 'liftkey': liftkey}


class FilterModule(object):
    ''' Lift primatives to the category of dictionaries '''
    def filters(self):
        return FILTERS


# class FilterModule(object):
#     ''' Lift primatives to the category of dictionaries '''
#     def filters(self):
#         return {
#             'lift': self.lift,
#             'liftid': self.liftid,
#             'liftkey': self.liftkey
#         }
