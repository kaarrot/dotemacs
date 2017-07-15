import sublime
import sublime_plugin

history = [("no history","")]
verbose = True

class ShowHistoryCommand(sublime_plugin.TextCommand):
    
    def run(self, edit):
        global history
        
        def filterArgs(args):
            '''Filter relevant information if the args string is too long
            Right now, only applied to extract build variant
            '''
            if args:
                if 'variant' in args:
                    return args['variant']
                else:
                    return args
            else:
                return "no args"
        
        def runCommand(index):
            '''Callback'''
            global history
            if index >= 0 and index < len(history):
                cmd = history[index]
                if verbose: print (cmd)
                
                sublime.active_window().run_command(cmd[0], cmd[1])
                
                ## Insert to the top of the list again
                num_duplicates = [i for i,val in enumerate(history) if val==cmd]
                [history.remove(cmd) for dup in num_duplicates]
                history.insert(0,cmd)
                
        history_list = ["%s\n   %s" % (pair[0], filterArgs(pair[1])) for pair in history]
        flags = sublime.KEEP_OPEN_ON_FOCUS_LOST
        sublime.active_window().show_quick_panel(history_list, lambda x: runCommand(x), flags)
        
class CaptureWindowCommand(sublime_plugin.EventListener):
    
    def appendCommand(self, cmd, args):
        global history
        item = (cmd, args)
        ## Blacklist certain repetitive commands
        blacklist = ['show_panel',
                     'quick_panel',
                     'hide_panel',
                     'focus_group',
                     ]
        ## Skip opening panels etc.
        if cmd in blacklist:
            return
        ## Skip selection: build {'select':True}
        if 'select' in args and len(args.keys()) == 1:
            return
        ## Remove existing duplicates first
        if item in history:
            num_duplicates = [i for i,val in enumerate(history) if val==item]
            [history.remove(item) for dup in num_duplicates]
        
        ## Insert new item and keep list limit to 20
        history.insert(0,item)
        limit = 20
        history = history[:limit]
    
    def on_post_window_command(self, window, command_name, args):
        self.appendCommand(command_name, args)