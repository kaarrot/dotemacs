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
    
# regstration
def my_fun(val):
    import pdb
    #with open('/data/data/com.termux/files/home/temp/gdb_out.txt', 'a') as f:
    #    f.write("%s\n" % str(val.type))
    if str(val.type) == "std::__ndk1::string": # in gdb whatis 'variable'
        return  StringPrinter(val)
    elif "std::__ndk1::__vector_base" in str(val.type):
        return VectorPrinter(val.type.name, val)

gdb.pretty_printers.clear()
gdb.pretty_printers.append(my_fun)

