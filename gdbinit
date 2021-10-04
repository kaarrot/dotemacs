set python print-stack full
set history save on

python
import sys, os
sys.path.insert(0, '{0}/dotemacs/'.format(os.environ['HOME']))
from printers import register_libstdcxx_printers
register_libstdcxx_printers(None)
end
