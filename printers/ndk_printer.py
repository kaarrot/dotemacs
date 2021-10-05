'''
set print pretty on
p b.__r_.__value_.__s.__data_

Debuggin pretty printes with GDB:
set python print-stack full
set history save on
and set_trace inside the funtion which calls each class - this is the best entrry point
    
'''
import re
import gdb
import sys
from builtins import chr


def make_type_re(typename):
    return re.compile('^std::__[a-zA-Z0-9]+::' + typename + '<.*>$')


def get_node_value(it, ptr):
    t = gdb.lookup_type(it.type.name + '::__node_pointer')
    return ptr.cast(t)['__value_']


def get_node_value_from_pointer(ptr):
    return get_node_value(ptr.dereference(), ptr)


def get_node_value_from_iterator(it):
    return get_node_value(it, it['__ptr_'])


# Starting with the type ORIG, search for the member type NAME.  This
# handles searching upward through superclasses.  This is needed to
# work around http://sourceware.org/bugzilla/show_bug.cgi?id=13615.
def find_type(orig, name):
    typ = orig.strip_typedefs()
    while True:
        search = str(typ) + '::' + name
        try:
            return gdb.lookup_type(search)
        except RuntimeError:
            pass
        # The type was not found, so try the superclass.  We only need
        # to check the first superclass, so we don't bother with
        # anything fancier here.
        field = typ.fields()[0]
        if not field.is_base_class:
            raise ValueError("Cannot find type %s::%s" % (orig, name))
        typ = field.type


def pair_to_tuple(val):
    if make_type_re('__compressed_pair').match(val.type.name):
        t1 = val.type.template_argument(0)
        t2 = val.type.template_argument(1)

        base1 = val.type.fields()[0].type
        base2 = val.type.fields()[1].type

        return ((val if base1.template_argument(2) else
                 val.cast(base1)["__value_"]).cast(t1),
                (val if base2.template_argument(2) else
                 val.cast(base2)["__value_"]).cast(t2))
    return (val['first'], val['second'])


void_type = gdb.lookup_type('void')


def ptr_to_void_ptr(val):
    if gdb.types.get_basic_type(val.type).code == gdb.TYPE_CODE_PTR:
        return val.cast(void_type.pointer())
    else:
        return val


class StringPrinter(object):
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return self.val['__r_']['__value_']['__s']['__data_'].string()

class VectorPrinter:
    "Print a std::vector"
    Iterator = object
    class _iterator(Iterator):
        def __init__(self, start, finish_or_size, bits_per_word, bitvec):
            self.bitvec = bitvec
            if bitvec:
                self.item = start
                self.so = 0
                self.size = finish_or_size
                self.bits_per_word = bits_per_word
            else:
                self.item = start
                self.finish = finish_or_size
            self.count = 0

        def __iter__(self):
            return self

        def __next__(self):
            count = self.count
            self.count = self.count + 1
            if self.bitvec:
                if count == self.size:
                    raise StopIteration
                elt = self.item.dereference()
                if elt & (1 << self.so):
                    obit = 1
                else:
                    obit = 0
                self.so = self.so + 1
                if self.so >= self.bits_per_word:
                    self.item = self.item + 1
                    self.so = 0
                return ('[%d]' % count, obit)
            else:
                if self.item == self.finish:
                    raise StopIteration
                elt = self.item.dereference()
                self.item = self.item + 1
                return ('[%d]' % count, elt)

    def __init__(self, typename, val):
        self.typename = typename
        self.val = val
        self.is_bool = 0
        for f in val.type.fields():
            if f.name == '__bits_per_word':
                self.is_bool = 1

    def children(self):
        if self.is_bool:
            return self._iterator(self.val['__begin_'], self.val['__size_'],
                                  self.val['__bits_per_word'], self.is_bool)
        else:
            return self._iterator(self.val['__begin_'], self.val['__end_'], 0,
                                  self.is_bool)

    def to_string(self):
        if self.is_bool:
            length = self.val['__size_']
            capacity = pair_to_tuple(
                self.val['__cap_alloc_'])[0] * self.val['__bits_per_word']
            if length == 0:
                return 'empty %s<bool> (capacity=%d)' % (self.typename,
                                                         int(capacity))
            else:
                # return '%s<bool> (length=%d, capacity=%d)' % (
                #    self.typename, int(length), int(capacity))
                return ''
        else:
            # import pdb
            # pdb.set_trace()
            start = ptr_to_void_ptr(self.val['__begin_'])
            finish = ptr_to_void_ptr(self.val['__end_'])
            end = ptr_to_void_ptr(pair_to_tuple(self.val['__end_cap_'])[0])
            length = finish - start
            capacity = end - start
            if length == 0:
                return 'empty %s (capacity=%d)' % (self.typename,
                                                   int(capacity))
            else: # After this point Children method is called
                #return '%s (length=%d, capacity=%d)' % (
                #    self.typename, int(length), int(capacity))
                return ''

    def display_hint(self):
        return 'array'
class TemplateTypePrinter(object):
    r"""
    A type printer for class templates with default template arguments.

    Recognizes specializations of class templates and prints them without
    any template arguments that use a default template argument.
    Type printers are recursively applied to the template arguments.

    e.g. replace "std::vector<T, std::allocator<T> >" with "std::vector<T>".
    """

    def __init__(self, name, defargs):
        self.name = name
        self.defargs = defargs
        self.enabled = True

    class _recognizer(object):
        "The recognizer class for TemplateTypePrinter."

        def __init__(self, name, defargs):
            self.name = name
            self.defargs = defargs
            # self.type_obj = None

        def recognize(self, type_obj):
            """
            If type_obj is a specialization of self.name that uses all the
            default template arguments for the class template, then return
            a string representation of the type without default arguments.
            Otherwise, return None.
            """

            if type_obj.tag is None:
                return None

            if not type_obj.tag.startswith(self.name):
                return None

            template_args = get_template_arg_list(type_obj)
            displayed_args = []
            require_defaulted = False
            for n in range(len(template_args)):
                # The actual template argument in the type:
                targ = template_args[n]
                # The default template argument for the class template:
                defarg = self.defargs.get(n)
                if defarg is not None:
                    # Substitute other template arguments into the default:
                    defarg = defarg.format(*template_args)
                    # Fail to recognize the type (by returning None)
                    # unless the actual argument is the same as the default.
                    try:
                        if targ != gdb.lookup_type(defarg):
                            return None
                    except gdb.error:
                        # Type lookup failed, just use string comparison:
                        if targ.tag != defarg:
                            return None
                    # All subsequent args must have defaults:
                    require_defaulted = True
                elif require_defaulted:
                    return None
                else:
                    # Recursively apply recognizers to the template argument
                    # and add it to the arguments that will be displayed:
                    displayed_args.append(self._recognize_subtype(targ))

            # This assumes no class templates in the nested-name-specifier:
            template_name = type_obj.tag[0:type_obj.tag.find('<')]
            template_name = strip_inline_namespaces(template_name)

            return template_name + '<' + ', '.join(displayed_args) + '>'

        def _recognize_subtype(self, type_obj):
            """Convert a gdb.Type to a string by applying recognizers,
            or if that fails then simply converting to a string."""

            if type_obj.code == gdb.TYPE_CODE_PTR:
                return self._recognize_subtype(type_obj.target()) + '*'
            if type_obj.code == gdb.TYPE_CODE_ARRAY:
                type_str = self._recognize_subtype(type_obj.target())
                if str(type_obj.strip_typedefs()).endswith('[]'):
                    return type_str + '[]' # array of unknown bound
                return "%s[%d]" % (type_str, type_obj.range()[1] + 1)
            if type_obj.code == gdb.TYPE_CODE_REF:
                return self._recognize_subtype(type_obj.target()) + '&'
            if hasattr(gdb, 'TYPE_CODE_RVALUE_REF'):
                if type_obj.code == gdb.TYPE_CODE_RVALUE_REF:
                    return self._recognize_subtype(type_obj.target()) + '&&'

            type_str = gdb.types.apply_type_recognizers(
                    gdb.types.get_type_recognizers(), type_obj)
            if type_str:
                return type_str
            return str(type_obj)

    def instantiate(self):
        "Return a recognizer object for this type printer."
        return self._recognizer(self.name, self.defargs)


def add_one_template_type_printer(obj, name, defargs):
    r"""
    Add a type printer for a class template with default template arguments.

    Args:
        name (str): The template-name of the class template.
        defargs (dict int:string) The default template arguments.

    Types in defargs can refer to the Nth template-argument using {N}
    (with zero-based indices).

    e.g. 'unordered_map' has these defargs:
    { 2: 'std::hash<{0}>',
      3: 'std::equal_to<{0}>',
      4: 'std::allocator<std::pair<const {0}, {1}> >' }

    """
    printer = TemplateTypePrinter('std::'+name, defargs)
    gdb.types.register_type_printer(obj, printer)

    # Add type printer for same type in debug namespace:
    printer = TemplateTypePrinter('std::__debug::'+name, defargs)
    gdb.types.register_type_printer(obj, printer)

    if _versioned_namespace:
        # Add second type printer for same type in versioned namespace:
        ns = 'std::' + _versioned_namespace
        # PR 86112 Cannot use dict comprehension here:
        defargs = dict((n, d.replace('std::', ns)) for (n,d) in defargs.items())
        printer = TemplateTypePrinter(ns+name, defargs)
        gdb.types.register_type_printer(obj, printer)

    
# regstration
def my_fun(val):
    import pdb
    # pdb.set_trace()
    #with open('/data/data/com.termux/files/home/temp/gdb_out.txt', 'a') as f:
    #    f.write("%s\n" % str(val.type))
    if str(val.type) == "std::__ndk1::string": # in gdb whatis 'variable'
        return  StringPrinter(val)
    elif "std::__ndk1::__vector_base" in str(val.type):
        return VectorPrinter(val.type.name, val)
    #add_one_template_type_printer(obj, 'vector', { 1: 'std::allocator<{0}>'})

#def register_libstdcxx_printers (obj):
#    "Register libstdc++ pretty-printers with objfile Obj."
#
    
gdb.pretty_printers.clear()
gdb.pretty_printers.append(my_fun)
#add_one_template_type_printer(obj, 'vector', { 1: 'std::allocator<{0}>'})

