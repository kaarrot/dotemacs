import os.path, os
import pdb

# save this in .pdbrc.py in your home directory
def complete(self, text, state):
    """return the next possible completion for text, using the current frame's
       local namespace

       This is called successively with state == 0, 1, 2, ... until it
       returns None.  The completion should begin with 'text'.
    """
    try:
        import rlcompleter
        # keep a completer class, and make sure that it uses the current local scope
        if not hasattr(self, 'completer'):
            self.completer = rlcompleter.Completer(self.curframe.f_locals)
        else:
            self.completer.namespace = self.curframe.f_locals
        return self.completer.complete(text, state)
    except Exception,e:
        print e

## Update the current view in sublime
def show_line_in_sublime():
    home = os.environ['HOME']
    print "SASASASSA"
    ff = open("%s/pdbout.txt" % home, "w")
    pdb.traceback.print_stack(file=ff)
    ff.close()
    ff = open("%s/pdbout.txt" % home, "r")
    lines = ff.readlines(); ff.close()
    import re
    ## the regex removes non-paths and non-standarpython module
    stackframe = [(re.findall('["/](.*?)"', line), re.findall('(?s)(?<=line ).*?[0-9]+', line)) for line in lines]
    print stackframe
    stackframe = [i for i in stackframe if len(i[1]) > 0 and \
                              i[0][0].find('bdb.py')==-1 and \
                              i[0][0].find('.pdbrc.py')==-1 and \
                              i[0][0].find('pdb.py')==-1 and \
                              i[0][0].find('cmd.py')==-1 and \
                              i[0][0].find('.py')!=-1]

    filepath = stackframe[-1][0][0]
    linenum = stackframe[-1][1][0]
    print filepath
    print linenum
    import subprocess
    cmd = "/{0}/bin/sublime_text_3/sublime_text -b {1}:{2}".format(home,filepath, linenum)
    print cmd
    process = subprocess.Popen(cmd, shell=True)
    # Move to the next line in sublime

## Persistent history
def _pdbrc_init():
    import readline
    histfile = ".pdb-pyhist"
    try:
        readline.read_history_file(histfile)
    except IOError:
        pass
    import atexit
    atexit.register(readline.write_history_file, histfile)
    readline.set_history_length(500)

_pdbrc_init()
del _pdbrc_init
