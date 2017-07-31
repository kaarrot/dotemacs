#//// find header in the head file not in the same folder


import sublime
import sublime_plugin
import re,os

## Globals
VIEWS = []
base_classes = []
CLASS_IDS = {}
CLASS_IDS_ORD_LIST = []  # allows to keep class in the current first on the list (and the menu)
SYMBOLS = []
MENU = []

def traverseBaseClass(pos, class_name, v):
    ''' pos is the rowcol tuple'''
    global base_classes, CLASS_IDS, CLASS_IDS_ORD_LIST
    global VIEWS, base_classes, CLASS_IDS, SYMBOLS, CLASS_IDS_ORD_LIST, MENU


    w =sublime.active_window();
    # v = sublime.active_window().active_view()

    ## Option 2: get string up to the next {
    # row = pos[0] -1
    # print ("row", row)
    # start = v.text_point(pos[0]-1,0)
    # end = v.find("{", start, sublime.LITERAL).end()  # return the end position
    # fullLine = v.substr(sublime.Region(start, end))  # can be multiple line
    # # print ("_++_+_+", fullLine)

    # # #########   Find classes we are inheriting from
    # list_a = re.findall('(?s)(?<=:).*(?={)', fullLine, re.MULTILINE)

    ## Option 3
    start = v.text_point(pos[0]-1,pos[1])
    end = v.find("{", start, sublime.LITERAL).end()  # return the end position
    fullLine = v.substr(sublime.Region(start, end))  # can be multiple line
    # print (fullLine)

    # #########   Find base_classes we are inheriting from
    list_a = re.findall('(?s)(?<=:).*(?={)', fullLine, re.MULTILINE)
    # print (list_a)

    ####################



    if len(list_a) == 0:
        ## TODO list all the entries only in a single class
        ##      defined here on in the hear file
        return

    toks =  re.split('[\n\s,]', list_a[0]) ## remove trailing comma after the class name
    # print ("toks", toks)
    base_classes =  [s for s in toks if s.strip()!='' and s[0].istitle()]
    print ("base_classes: ", base_classes)


    ###########
    ## for each class visit file
    for name in base_classes:
        # print (w.lookup_symbol_in_index(name))
        for v in (w.lookup_symbol_in_index(name)):  #"BaseTest"
            path = v[0]
            pos = v[2]

            # print ("____",path, pos, name)

            ## Check if the tab is already opened - on loaded script will not be executed
            ## Temporary workaround is to force-close and reopen which is not ideal
            views_paths = [v.file_name() for v in w.views()]
            if path in views_paths:
                index = views_paths.index(path)
                print ("Already opened", path, index)
                w.views()[index].set_scratch(True)
                w.views()[index].close()

            ## TODO: Best way to handl this:
            # 1. Unify traverseToBase() with traverseBaseClass()
            # 2. extract all the code from on_load() into a separate class/function
            # 3. Call this code from on_load - if the tab does not exist
            #    and in this if/else branch if the tab is open. This will avoid closing unsaved files.

            ## TRANSIENT prevents from creating additional tabs - we just want to open them for indexing
            vTemp = w.open_file('%s:%s:%s' % (path, pos[0], pos[1]), sublime.ENCODED_POSITION | sublime.TRANSIENT )
            # print ("vTemp", vTemp)
            id = vTemp.id()
            k = {'path':path, 'point':pos, 'id':id, 'name':name}
            ## for each one keep id with the file path so that we could reference
            CLASS_IDS[id] = k
            CLASS_IDS_ORD_LIST.append(k)


def getLine(v,row):
    '''view, row, returs: substring'''
    start = v.text_point(row-1,0)
    region = v.find("^.*$", start)
    s = v.substr(region)
    return  s

def fileName(path):
    '''returns just the name of the file'''
    return os.path.split(path)[1]

###################


class ClassHierarchyCommand(sublime_plugin.TextCommand):
    def run(self, edit):
        global VIEWS, SYMBOLS, CLASS_IDS_ORD_LIST, MENU, ENABLE
        print ("===============================")
        w =sublime.active_window(); v = sublime.active_window().active_view()

        ## jump to the definition in the file
        pos = v.sel()[0].begin()
        # print (v.substr(v.word(pos)))

        CLASS_IDS_ORD_LIST = []
        MENU = []
        SYMBOLS = []            # THis does not include symbols from this file
        VIEWS = []              # clear at each command
        ENABLE = True

        class_name = v.substr(v.word(pos))
        print ("+++", class_name)

        VIEWS.append((v, pos, class_name))  # keep the original place  (view, point)

        ## Find the line with the class definition (This file needs to be on the Project path)
        current_file_symbols = (w.lookup_symbol_in_index(class_name))
        print (current_file_symbols)
        ## find the point in the same file

        ## position of the line on the class definition
        ## pos: is the rowcol tuple'''
        pos = None
        for entry in current_file_symbols:
            pos = entry[2]
            point = v.text_point(pos[1]-1, 1)
            if (fileName(v.file_name())) == fileName(entry[0]):
                # print (entry)
                break

        if not pos:
            print ("No symbols found!!!, Check if the current file is on the Project path")
            return


        ## Add the current file to the global symvo
        symb_key = {'name': class_name, 'symbols': v.symbols(), 'path':v.file_name()}
        SYMBOLS.append(symb_key)

        ## for each one keep id with the file path so that we could reference
        print (symb_key)
        ######## Move to the beginning of the line where the class keyword
        traverseBaseClass(pos, class_name, v)


        ## TODO: list memebers in this file or .h - Assumes the header is in the same directory
        # print ("symbols this file:", v.symbols())    # view.sel()[0].begin()
        this_file = fileName(v.file_name())
        folder = os.path.split(v.file_name())[0]
        header_path = '%s/%s.h' % (folder, os.path.splitext(this_file)[0])
        ## TODO seach for other type of header extensions



        ## TODO there is no way to jump to header file withing the current project
        # v = w.run_command("show_overlay", {"overlay": "goto", "show_files": True, "text": "ccc.h"})

        ## Probably find wher thy symbols are defined !!!!!!!!!


        # v.run_command("switch_file", {"extensions": ["cpp", "cxx", "cc", "c", "hpp", "hxx", "hh", "h", "ipp", "inl", "m", "mm"]})

        # if os.path.exists(header_path):
        #     # vTemp = w.find_file('%s' % (header_path), sublime.TRANSIENT )
        #     # print (vTemp.symbols())    # view.sel()[0].begin()
        #     # SYMBOLS.append(vTemp.symbols())

        ## add from the current buffer
        # SYMBOLS.append(v.symbols())

        # print (SYMBOLS)
        ##############
        # region = sublime.active_window().active_view().run_command("expand_selection", {"to": "brackets"})
        # print (v.symbols(sublime.Region(region)))


class CaptureWindowCommand(sublime_plugin.EventListener):


    def on_load_async(self, view):
        ''' Having part of the logic in on_loaded allows us to leverage sublimes indexing which updates list of symbols in this view
        '''
        global VIEWS, base_classes, CLASS_IDS, SYMBOLS, CLASS_IDS_ORD_LIST, MENU

        # print (VIEWS)
        # print ("Load---------", view.id())
        print ("view.file_name()", view.file_name())
        # print (view.id())



        if view.id() in CLASS_IDS:
            viewInfo = (CLASS_IDS[view.id()])
            # print ("....view.file_name()", view.file_name())

            ## Accumulate views(later use to jump to) - not sure since the view are transient they can not be switch to
            rc = viewInfo['point']
            pos = view.text_point(rc[0], rc[1])
            VIEWS.append( (view, pos,  viewInfo['name']) )

            # print ("viewInfo",viewInfo['point'])
            line = (getLine(view,viewInfo['point'][0]))  # row
            # print (line)
            ## if the lines has a 'class' recurse - can you ?
            if line.find('class')!=-1:
                # print ("___" ,line)
                traverseBaseClass(viewInfo['point'], "_", view)


            # print ("symbols on_load: ", view.symbols())    # view.sel()[0].begin()

            ## Add the syumbols from the current view to the main list
            symb_key = {'name': viewInfo['name'], 'symbols': view.symbols(), 'path':viewInfo['path']}
            SYMBOLS.append(symb_key)


    ###########################################################
    ## The only sensible way to collect all the output from all async loads

    def on_hover(self, view, point, hover_zone):
        ## used to triger the display of the menu once all the on_load scripts complete and all asyc-entries will be available
        global VIEWS, MENU, ENABLE, menu_locations
        # print (" on hover ", hover_zone)


        def visitMember(index):
            # print (menu_locations)
            w = sublime.active_window()
            # print (menu_locations[index])
            path = menu_locations[index][0]
            region = menu_locations[index][1].begin()

            # print (path, region)

            vTemp = w.open_file('%s' % (path), sublime.TRANSIENT)
            ## TODO: best will be to show in another View
            vTemp.show(region)

            row = vTemp.rowcol(region)[0] + 1
            vTemp.run_command("goto_line", {"line": row})
            pass

        ## Dispaly only once
        if ENABLE:

            print ("SYMBOLS length", len(SYMBOLS))

            # Output (in order) symbols to be included in the menu
            # for symbol in SYMBOLS:
            #     print (symbol)

            for source_file in SYMBOLS:
                if 'name' in source_file:
                    entry = "%s: %s" % (source_file['name'], source_file['path'])
                    # entry = "%s" % (source_file['name'])

                    if entry not in MENU:  # sublime stops building menu if there are duplicates
                        MENU.append(entry)


                    if 'symbols' in source_file:
                        ## find the same symbol location as the source file which is a class_name
                        ## This is required to have continues indexing and all the files in the menu take you to some place
                        ##
                        location = (source_file['path'], sublime.Region(0,0))
                        menu_locations.append(location)

                        for sym in source_file['symbols']:
                            # here position to the symbol is given by a region - we need to turn it into a row/col
                            # which can be only extracted given the view
                            # This is reqwuired since teh open_file accepsts
                            pos = sym[0]
                            #print (source_file)
                            sym_name = sym[1]
                            MENU.append("\t%s" % sym_name)

                            location = (source_file['path'], pos)
                            #print (location)
                            menu_locations.append(location)

            if len(VIEWS) > 0:
                sublime.active_window().focus_view(VIEWS[0][0])
                sublime.active_window().show_quick_panel(MENU, None, 0, 0, lambda x: visitMember(x))
                # sublime.active_window().show_quick_panel(MENU,0 )  # no action on changin positions
            ENABLE = False

    ### find all externs:     ^.*( extern ).*$