
##################################

# Extra declarations appended to typeshed/stdlib/builtins.pyi

class NoneType:
    def __bool__(self) -> bool: ...


None = NoneType()  # TODO: pytype (and mypy?) has (in effect) None: Type[None]

False = bool(0)
True  = bool(1)
