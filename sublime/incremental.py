import sublime, sublime_plugin
class PromptIncrementNumberWithPaddingCommand(sublime_plugin.WindowCommand):
    def run(self):
    self.window.show_input_panel('Enter a starting number, step and zero padding.', '1, 1, 1', self.on_done, None, None)
    pass

def on_done(self, text):
    try:
        (current, step, padding) = map(str, text.split(","))
        if self.window.active_view():
            self.window.active_view().run_command("increment_number_with_padding", {"current" : current, "step" : step, "padding" : padding} )
    except ValueError:
        pass
class IncrementNumberWithPaddingCommand(sublime_plugin.TextCommand):
    def run(self, edit, current, step, padding):
# def run(self, edit, current, step):
    sublime.status_message("Inserting #")
    current = int(current)
    for region in self.view.sel():
        sublime.status_message("Inserting #" + str(current))
        self.view.replace(edit, region, str('%0*d' %(int(padding), int(current))))
        current = current + int(step)