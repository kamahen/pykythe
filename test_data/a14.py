# From /usr/lib/python3.7/cgi.py (minimal)
# TODO: move this test to bindings.py  (and s/_a14/_bindings/g

# - doesn't resolve `list` properly in MiniFieldStorage


class MiniFieldStorage:

    #- { @list defines/binding MiniFieldStorage_list=vname("${ROOT_FQN}.test_data.a14.MiniFieldStorage.list", _, _, "", python) }
    list = None
    type = None
    #- { @file defines/binding MiniFieldStorage_file=vname("${ROOT_FQN}.test_data.a14.MiniFieldStorage.file", _, _, "", python) }
    file = None

    def __init__(self, name, value):
        """Constructor from field name and value."""
        self.name = name
        self.value = value


    def __repr__(self):
        """Return printable representation."""
        return "MiniFieldStorage(%r, %r)" % (self.name, self.value)


    # This isn't part of the original code, but shows the bug where `list`
    # is looked up and gets self.list instead of builtins.list
    def getvalue_a14(self, key, default=None):
        """Dictionary style get() method, including 'value' lookup."""
        if key in self:
            # TODO: value: Union[MIniFieldStrorage, List[MiniFieldStorage]]
            value = self[key]
            #- { @list ref LIST_BUILTINS=vname("${BUILTINS_FQN}.builtins.list", _, _, "", python) }
            if isinstance(value, list):
                return [x.value for x in value]
            else:
                return value.value
        else:
            return default

    # Also, not part of the original code
    def get_list_value(self):
        #- { @list ref MiniFieldStorage_list }
        return self.list

    # Also, not part of the original code
    def get_file_value(self):
        #- { @file ref MiniFieldStorage_file }
        return self.file


def standalone_func_a14(key) :
    m = MiniFieldStorage('foo', 'bar')
    print(m.get_value_a14('foo'))
    #- { @list ref MiniFieldStorage_list }
    print(MiniFieldStorage.list)
    #- { @list ref MiniFieldStorage_list }
    print(m.list)
    #- { @list ref LIST_BUILTINS }
    print(list)

    #- { @file ref MiniFieldStorage_file }
    print(MiniFieldStorage.file)
    #- { @file ref MiniFieldStorage_file }
    print(m.file)
    #- { @file ref MiniFieldStorage_file }
    print(MiniFieldStorage.file)
    #- { @extra ref MiniFieldStorage_extra }
    print(m.extra)
    #- { @extra ref MiniFieldStorage_extra }
    print(MiniFieldStorage.extra)
    #- { @file ref vname("${ROOT_FQN}.test_data.a14.file", _, _, "", python) } // module level (because not found)
    print(file)


#- { @extra defines/binding MiniFieldStorage_extra=vname("${ROOT_FQN}.test_data.a14.MiniFieldStorage.extra", _, _, "", python) }
MiniFieldStorage.extra = 'abc'


