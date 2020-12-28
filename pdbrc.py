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
    except Exception as e:
        print(e)
    pass

def show_line_in_editor():
    import os, pdb
    #home = os.environ['HOME']
    home = os.path.expanduser("~")
    ff = open("%s/pdbout.txt" % home, "w")
    pdb.traceback.print_stack(file=ff)
    ff.close()
    ff = open("%s/pdbout.txt" % home, "r")
    lines = ff.readlines(); ff.close()
    import re
    ## the regex removes non-paths and non-standarpython module
    stackframe = [(re.findall('["/](.*?)"', line), re.findall('(?s)(?<=line ).*?[0-9]+', line)) for line in lines]
    stackframe = [i for i in stackframe if len(i[1]) > 0 and \
                              i[0][0].find('bdb.py')==-1 and \
                              i[0][0].find('.pdbrc.py')==-1 and \
                              i[0][0].find('pdb.py')==-1 and \
                              i[0][0].find('cmd.py')==-1 and \
                              i[0][0].find('.py')!=-1]
    
    filepath = stackframe[-1][0][0]
    linenum = stackframe[-1][1][0]
    # on windows we need to replace the last backslash (before file name)
    # handling this in Elisp and properly escaping quotes is too hard.
    filepath = filepath.replace('\\', '/')
    #print filepath, linenum
    
    import subprocess
    # sublime cmd
    #cmd = '''"C:/Program Files/Sublime Text 3/sublime_text.exe" --command 'focus_group {"group": 1}' --background %s:%s''' % (filepath, linenum)
    # emacs cmd
    # Original Emacs command but on Windows with emacsclinent --eval returns: "value as variable is void:" error
    # So this version does not have --eval but every time we need to go back to focus into eshell window
    # where the pdb session is running. This is very ineffceint from the UX perspective!
    #cmd = '''"C:/Program Files (x86)/Emacs/i686/bin/emacsclient.exe" +%s %s --quiet''' % (linenum, filepath)

    # Better approach - handle everything in Elisp but assumes the path (on Windows) does not contain backslashes
    cmd = '''"C:/Program Files (x86)/Emacs/i686/bin/emacsclient.exe"  --eval "(progn (find-file \\\"%s\\\") (goto-line %s) (windmove-down))"''' % (filepath, linenum)
    #print(cmd)
    process = subprocess.Popen(cmd, shell=True)
    # Move to the next line in sublime
    pass


## Persistent histor
def _pdbrc_init():
    # TODO: not realdine module on windows
    import readline
    histfile = ".pdb-pyhist"
    try:
        readline.read_history_file(histfile)
    except IOError:
        pass
    import atexit
    atexit.register(readline.write_history_file, histfile)
    readline.set_history_length(500)
    pass


#_pdbrc_init()
#del _pdbrc_init
