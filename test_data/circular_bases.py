# This example is taken from typeshed/stdlib/builtins.pyi

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
    #- // The following aren't processed (because of the if-then-else)
    #- // so these verification tests are commented out.
    #- // { @EnvironmentError defines/binding EnvironmentError }
    class EnvironmentError(StandardError): ...
    #- // { @OSError defines/binding OSError }
    #- // { @EnvironmentError ref EnvironmentError }
    class OSError(EnvironmentError): ...
    # class IOError(EnvironmentError): ...
