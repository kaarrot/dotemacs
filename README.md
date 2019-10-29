This is my self contained dotemacs configuration with some extra routines added over the years. You can find some extra packages also avilable on melpa but for various reasons I preferred to kept them baked into this repo. The goal is to keep the configuration relatively small and only add features which frequently used. adhering as much as possible to standard Emacs UX.

#### Packages (in the order of importance)
- desktop+ - managing sets of buffers (per project)
- dumb-jump - simply and rock-solid 'jump-to-definiton' for python projects
- multiple-cursors - similar to sublime
- tabbar - grouping buffers of the same time into tabs
- bookmarks (bm) - primarily used for sending parts of a buffer to python interpreter
- ace-jump-mode - jump to a character in a buffer
- (auto-complete)
- company-mode
- yasnipets
- cmake-mode
- markdown-mode

#### Optional External dependencies
- rclone (required by dropbox-* functions)
- cquery (c++-mode) - manually enabled in a buffer `C-c 5`
- clangd (c++-mode) - manually enabled in a buffer `C-x 5`

#### Custom routines
These are located in `emacs.d/modules/myfuncs.el` and are mostly add-hock experimetnations. Some are useful enough to be include in .emacs. 
##### Check out the following:
- `dropbox-ls` / `dropbox-get` / `dropbox-send` - sync files with dropbox using rclone
- `dumb-jump-set-include-paths` - override project path used by dumbjump (can include multiple roots)
- `selectback-exec` - find the text region (between bookmarks) to send to python interpreter
- `add-to-global-ring` / `go-ring-back` - my version of moving back/forth between cursor portions in buffers
- `comment-or-uncomment-this` - more intuitive line comment insertion
- `occur-methods` - list python/c++ functions
- `sort-buffers`
- `tabbar-move-current-tab-one-place-right` (and `left`)

### Some useful features (WIP)
- display and copy the current file path into the clipboard.
![](images/copy_current_file_path.gif)
- sync current file with dropbox (using rclone)
![](images/rclone_dropbox.gif)
