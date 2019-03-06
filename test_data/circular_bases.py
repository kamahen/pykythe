# This example is taken from typeshed/stdlib/2and3/builtins.pyi

# TODO: revisit this if we start to evaluate top-level
#       "if sys.version_info ..." statements.
# Probably the simples way of handling sys.version_info is to
# do a string substition and then use eval(..., {}, {}).
# e.g.:
# if sys.version_info[:2] == (2, 7) or sys.version_info >= (3, 3):
# ==> eval('(3,7,2)[:2] == (2, 7) or (3,7,2) >= (3, 3)', {}, {})

import sys

#- { @Exception defines/binding Exception }
#- { Exception.node/kind record }
class Exception: ...

if sys.version_info >= (3,):
    #- { @OSError defines/binding OSError }
    #- { OSError.node/kind record }
    #- { @Exception ref Exception }
    class OSError(Exception): ...
    #- { @OSError ref OSError }
    #- { @EnvironmentError defines/binding EnvironmentError }
    #- { EnvironmentError.node/kind variable }
    EnvironmentError = OSError
    # IOError = OSError
else:
    #- { @EnvironmentError defines/binding EnvironmentError }
    class EnvironmentError(StandardError): ...
    #- { @OSError defines/binding OSError }
    #- { @EnvironmentError ref EnvironmentError }
    class OSError(EnvironmentError): ...
    # class IOError(EnvironmentError): ...

# TODO: The above isn't sufficient to test builtins.TimeoutError et al,
#       which get into a superclass loop, it seems.
