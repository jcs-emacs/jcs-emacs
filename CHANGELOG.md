# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


### 2020-05-27

* Fixed `cc-mode` docstring with keywords. (jcs-emacs-init)
* Enabled JSX docstring with `rjsx-mode`. (jcs-emacs-init)

### 2020-05-26

* Added Unreal C++ scripting template files for `header`/`source`. (jcs-emacs-init)
* Implemented ask for which header insertion in `c++-mode`. (jcs-emacs-init)
* Declared c/c++ header and source extension list. (jcs-emacs-init)
* Fixed `counsel`/`counsel-projectile` find file other window logic. (jcs-emacs-init)

### 2020-05-25

* Added `isearch` configuration by showing the match count. (jcs-emacs-init)

### 2020-05-23

* Removed `preproc` face from `face` module. (jcs-emacs-init)
* Removed `preproc` function, no longer needed. (jcs-emacs-init)
* Removed `oop` highlight faces for fixing OOP docstring implementation. (jcs-emacs-init)

### 2020-05-22

* Removed manually installed package `shift-select`. (jcs-emacs-init)
* Fixed mark whole buffer action after switching window. (jcs-emacs-init)
* Fixed `ivy` skipping input selection logic. (jcs-emacs-init)

### 2020-05-20

* Enabled `auto-rename-tag` for certain web related modes. (jcs-emacs-init)

### 2020-05-19

* Improved `JSX` coding experience by supply more helper plugins. (jcs-emacs-init)
* Fixed performance issue when reload active mode. (jcs-emacs-init)
* Fixed performance issue when enable/disable truncate lines in `web-mode`. (jcs-emacs-init)

### 2020-05-18

* Added auto install dependencies flag. (jcs-emacs-init)
* Config now automatically install needed dependencies after first startup. (jcs-emacs-init)

### 2020-05-17

* Added safe active LSP util function. (jcs-emacs-init)
* Semi-fixed performance when refresh dashboard. (jcs-emacs-init)
* Split buffer shown `-count` and `-p` function in window module. (jcs-emacs-init)
* Fixed `undo-tree` slow response when multiple undo/redo actions. (jcs-emacs-init)
* Implemented `lsp` connected flag util function. (jcs-emacs-init)

### 2020-05-16

* Implemented find directories ignore directories function. (jcs-emacs-init)
* Implemented find files ignore directories function. (jcs-emacs-init)
* Fixed `make`/`run` script find directories/files time spend too long issue. (jcs-emacs-init)
* Installed `rjsx-mode` for supporting `jsx` file. (jcs-emacs-init)
* Added JSX default snippets. (jcs-emacs-init)
* Added condition to limit `lsp-deffered` is called. (jcs-emacs-init)

### 2020-05-15

* Minor fixed for `cc-mode` indent block settings. (jcs-emacs-init)

### 2020-05-13

* Config `counsel`'s `find-file` preselect flag. (jcs-emacs-init)

### 2020-05-03

* Fixed deep directory tree when using `counsel` find file action. (jcs-emacs-init)

### 2020-04-22

* Implemented `inc`/`dec` string distance level for `multiple-cursors` similarity. (jcs-emacs-init)

### 2020-03-29

* Installed new package `dashboard-ls`. (jcs-emacs-init)

### 2020-03-28

* Removed package `focus`. (jcs-emacs-init)

### 2020-03-25

* Implemented safe refresh `dashboard` functionality. (jcs-emacs-init)

### 2020-03-24

* Fixed OOP docstring functionalitiy when no parameters. (jcs-emacs-init)

### 2020-03-23

* Changed source for package `emoji-github` from `quelpa` to `melpa`. (jcs-emacs-init)
* Updated `diminsh-buffer` list for more of the LSP buffer. (jcs-emacs-init)

### 2020-03-22

* Installed new package `lsp-java`. (jcs-emacs-init)
* Installed new package `lsp-origami`. (jcs-emacs-init)

### 2020-03-20

* Moved more mode autoload from `jcs-cc-mode` to `jcs-mode` file. (jcs-emacs-init)

### 2020-03-17

* Added the lsp ui doc delay when safely show lsp ui doc tooltip. (jcs-emacs-init)

### 2020-03-16

* Installed new package `manage-minor-mode-table`. (jcs-emacs-init)
* Installed new package `buffer-wrap`. (jcs-emacs-init)

### 2020-03-13

* Installed new package `masm-mode`. (jcs-emacs-init)
* Implemented `asm` mode behaviour for all Assembly Language
 related `major-mode`s. (jcs-emacs-init)

### 2020-03-12

* Changed source for `company-quickhelp-terminal` package from
`quelpa` to `melpa`. (jcs-emacs-init)

### 2020-03-11

* Fixed not refresh issue header string from `buffer-menu`. (jcs-emacs-init)

### 2020-03-09

* Fixed `ivy` overlap logic. (jcs-emacs-init)
* Changed using minor mode for `company-quickhelp-terminal` package. (jcs-emacs-init)

### 2020-03-06

* Installed new package `company-emojify`. (jcs-emacs-init)
* Installed new package `emoji-github` manually. (jcs-emacs-init)

### 2020-03-05

* Installed new package `emojify`. (jcs-emacs-init)
* Fixed buffer wrap can't correct goes to corresponding first line isssue. (jcs-emacs-init)

### 2020-03-03

* Use local variable for `tabulated-list`'s header string. (jcs-emacs-init)
* Diminish mode `buffer-wrap-mode` for `buffer-wrap` package. (jcs-emacs-init)

### 2020-03-01

* Switch `ffmpeg-player` from source `quelpa` to `melpa`. (jcs-emacs-init)

### 2020-02-25

* Removed `dashboard` mute when insert project sections. (jcs-emacs-init)

### 2020-02-22

* Installed new package manually `buffer-wrap`. (jcs-emacs-init)
* Removed package manually `tabulated-list-wrap`. (jcs-emacs-init)

### 2020-02-21

* Installed new package `command-log-mode`. (jcs-emacs-init)

### 2020-02-20

* Mute dashboard insert project log. (jcs-emacs-init)
* Fixed load `eww` issue from emacs version `27.0.60`. (jcs-emacs-init)

### 2020-02-17

* Installed new package `flycheck-grammarly`. (jcs-emacs-init)

### 2020-02-16

* Implemented `neotree` recording the last window. (jcs-emacs-init)

### 2020-02-14

* Tweak `neotree` customizable variables' value. (jcs-emacs-init)
* Changed `output`/`compilation` buffer maybe kill buffer default action to
 change to other output buffer as higher priority. (jcs-emacs-init)

### 2020-02-13

* Implemented switch to next window in height utility function in window module. (jcs-emacs-init)
* Removed package `sr-speedbar`. (jcs-emacs-init)
* Installed new package `neotree`. (jcs-emacs-init)
* Replace config from `sr-speedbar` to `neotree`. (jcs-emacs-init)
* Implemented automatically refresh `neotree` using timer. (jcs-emacs-init)

### 2020-02-11

* Fixed return type with empty string in `oop` module. (jcs-emacs-init)

### 2020-02-10

* Removed manually installed package `vs-light-theme`. (jcs-emacs-init)
* Removed manually installed package `vs-dark-theme`. (jcs-emacs-init)
* Installed new package `vs-light-theme`. (jcs-emacs-init)
* Installed new package `vs-dark-theme`. (jcs-emacs-init)
* Implemented docstring for `Go` programming language. (jcs-emacs-init)
  - Support two way to comment document string.
* Fixed `ivy` find file logic with regular expression. (jcs-emacs-init)

### 2020-02-07

* Added expression face for `feebleline` lsp. (jcs-emacs-init)
* Fixed region delete for smart delete word and capital word. (jcs-emacs-init)
* Indent `yank` in `python-mode` gives weird result, use normal `yank` instead. (jcs-emacs-init)

### 2020-02-05

* Chnaged the constant keywords' face in `go-mode`. (jcs-emacs-init)
* Implemented LSP information to feebleline. (jcs-emacs-init)

### 2020-02-04

* Added own `save-excursion` function. (jcs-emacs-init)
* Fixed indent error while untabify/tabify buffer in `go-mode`. (jcs-emacs-init)
* Fixed `company-fuzzy` renable issue with `lsp-mode`. (jcs-emacs-init)
* Disabled debug on error while LSP server is active. (jcs-emacs-init)

### 2020-02-03

* Update `go` programming language default template. (jcs-emacs-init)

### 2020-01-25

* Reimplemented OOP docstring module, mainly for clean up. (jcs-emacs-init)

### 2020-01-24

* Fixed buffer/file name prompting error while reopening the file. (jcs-emacs-init)

### 2020-01-23

* Installed new package => `manage-minor-mode`. (jcs-emacs-init)

### 2020-01-20

* Fixed requiring `cl` using `loop` in util module. (jcs-emacs-init)

### 2020-01-19

* Ignored `lsp` buffer with weather the dash `-` exists or not. (jcs-emacs-init)
* Removed `yascroll` issue with Emacs 27. (jcs-emacs-init)

### 2020-01-14

* Fixed `feebleline` compatbile with `lsp-mode` ignore case issue on `(buffer-name)`
 we use `(buffer-file-name)` beforehand. (jcs-emacs-init)

### 2020-01-13

* Updated `jQuery` snippet in `html-mode`, not slime use normal minified version. (jcs-emacs-init)

### 2020-01-10

* Fixed `null` face highlighting regular expression. (jcs-emacs-init)

### 2020-01-09

* Update upgrade manually installed package logic. (jcs-emacs-init)
* Installed new package manually => `tabulated-list-wrap`. (jcs-emacs-init)
* Fixed visualize `undo-tree` on the other window doesn't work with `lsp-ui-doc`. (jcs-emacs-init)
* Fixed hanging when execute `lsp--execute-command` function from `lsp-ui-sideline`. (jcs-emacs-init)
* Fixed opening `css` virtual buffer with virtual directory issue with loading `eww`. (jcs-emacs-init)

### 2020-01-08

* Fixed some of the regular expression faces in `typescript-mode`. (jcs-emacs-init)
* Implemented describe path info at point function and embedded to describe thing
 at point. (jcs-emacs-init)

### 2020-01-07

* Changed python class' template format. (jcs-emacs-init)
* Added the `keywords` to the template config file. (jcs-emacs-init)

### 2020-01-06

* Installed new package => `grammarly`. (jcs-emacs-init)
* Added title source for package missing prompt. (jcs-emacs-init)
* Removed manually installed package => `ivy-resize`. (jcs-emacs-init)

### 2020-01-03

* Fixed buffer menu logic fit the `search`/`filtering` when multiple buffer menu 
 buffer exists. (jcs-emacs-init)
* Fixed buffer menu other window that doesn't goes to line `2` if the header appears. (jcs-emacs-init)

### 2020-01-02

* Fixed display not ready while `filtering`/`searching` in buffer menu. (jcs-emacs-init)
* Added wrapping functionality for buffer menu mode. (jcs-emacs-init)
* Established template default template's naming convention. (jcs-emacs-init)
* Improved buffer menu `searching`/`filtering` user experience. (jcs-emacs-init)
* Fixed buffer menu refresh logic after killing. (jcs-emacs-init)
* Added wrapping to buffer menu buffer. (jcs-emacs-init)

### 2019-12-31

* Added switch to output buffer utility function for development use. (jcs-emacs-init)
* Added get buffers utility functions by using `regexp` and `string`. (jcs-emacs-init)
* Change `define-it` package from manually installed to automatically installed
 using `melpa`. (jcs-emacs-init)
* Installed new package => `define-it`. (jcs-emacs-init)
* Fixed `undo`/`redo` logic and work with `lsp-ui`. (jcs-emacs-init)

### 2019-12-30

* Add no advice other window flag for other function that doesn't want to configure
 buffer in window while switching window/frame. (jcs-emacs-init)

### 2019-12-28

* Change default `define-it` to `'view` instead of `'pop`. (jcs-emacs-init)

### 2019-12-27

* Fixed `lsp-ui` show prompting error while switching windows. (jcs-emacs-init)
* Implemented `lsp-ui` show doc anytime functionalities. (jcs-emacs-init)
* Increase standard string distance level from `8` to `20`. (jcs-emacs-init)
* Enhanced the `multiple-cursors`'s similar functions string comparison algorithm.
 Make improvements for the use of the `multiple-cursors` similar occurrence command. (jcs-emacs-init)
* Added new make frame for selecting new frame after created. (jcs-emacs-init)
* Used `hide-frame` instead of `delete-frame` while showing `lsp-ui-doc`, this
 should fixed while re-focus back to Emacs' frame issue while jumping away from Emacs. (jcs-emacs-init)

### 2019-12-26

* According to [company-lsp/131](https://github.com/tigersoldier/company-lsp/issues/131),
 revert `flx` support with `company-lsp`. Just set `company-lsp-cache-candidates`
 to `auto`. (jcs-emacs-init)
* Removed package => `dimmer`. (jcs-emacs-init)
* Installed new package => `lsp-ui`. (jcs-emacs-init)
* Implemented `record` and `restore` windows status once utility functions. (jcs-emacs-init)

### 2019-12-25

* Installed new package `ivy-resize` manually. (jcs-emacs-init)
* Added `define-it` config to change default output choice to `pop`. (jcs-emacs-init)
* Installed new package `company-lsp`. (jcs-emacs-init)
* ~~Implemented fuzzy match using `flx` with `company-lsp`. (jcs-emacs-init)~~

### 2019-12-24

* Applied `*lsp-` related buffer to diminish buffer list. (jcs-emacs-init)
* Reverted the message wouldn't work the first time issue from Emacs 27. (jcs-emacs-init)
* Added completed `lsp` configuration. (jcs-emacs-init)
* Integrated better `lsp` signature buffer to dual window users/configurations. (jcs-emacs-init)
* Organized to use `other-window` advice/hook instead of function wrapper. (jcs-emacs-init)

### 2019-12-23

* Added window size change hook. (jcs-emacs-init)
* Fixed after resized frame ivy window doesn't get resize issue. (jcs-emacs-init)
* Added `license` template functionalities. (jcs-emacs-init)
* Added `changelog` template functionalities. (jcs-emacs-init)
* Added `typescript` template for [Cocos Creator](https://www.cocos.com/en/creator) scripting. (jcs-emacs-inti)

### 2019-12-22

* Made `eldoc` compatible/interact with mouse click. (jcs-emacs-init)
* Added peek frame util function. (jcs-emacs-init)
* Fixed `feebleline` works with multiple frames. (jcs-emacs-init)

### 2019-12-21

* Added multiple cursors previous/next similar functions. (jcs-emacs-init)
* Added simple make frame util function. (jcs-emacs-init)

### 2019-12-20

* Fixed `ivy` missing `ffap` dependency. (jcs-emacs-init)
* Fixed message wouldn't work when `dashboard` is showing inside
 window. (jcs-emacs-init)
* Add minibuffer first setup hook. (jcs-emacs-init)

### 2019-12-19

* Fixed `yascroll` wrong arguments error after Emacs version 27. (jcs-emacs-init)

### 2019-12-17

* Fixed `ivy` switch to buffer other window logic. (jcs-emacs-init)

### 2019-12-16

* Match enlarge/shrink windows times to `media` and `shell` windows. (jcs-emacs-init)

### 2019-12-15

* Implemented new way to complete path using `ivy`. (jcs-emacs-init)
* Implemented auto resize in `ivy` minibuffer window. (jcs-emacs-init)
* Reverted `ivy` slash key for other usage; mainly due to it's
 own functionality is weird to me. (jcs-emacs-init)
* Fixed `css-mode` weird face highlighting issue. (jcs-emacs-init)

### 2019-12-11

* Minor fixed for renaming inside minibuffer using `ivy`. (jcs-emacs-init)

### 2019-12-10

* Installed new package => `company-quickhelp-terminal` manually. (jcs-emacs-init)
* Switched from `helm` to `ivy` due to `helm`'s instability. (jcs-emacs-init)

### 2019-12-09

* Removed manually installed package => `reveal-in-folder`. (jcs-emacs-init)
* Installed new package => `reveal-in-folder`. (jcs-emacs-init)
* Installed new package => `parse-it`. (jcs-emacs-init)
* Installed new package => `vs-light-theme` manually. (jcs-emacs-init)
* Installed new package => `vs-dark-theme` manually. (jcs-emacs-init)
* Starting from version `5.9.3`, theme work outside of this packages. (jcs-emacs-init)

### 2019-12-08

* Installed new package `file-header` manually. (jcs-emacs-init)
* Configure package `file-header`. (jcs-emacs-init)
* Fixed hex code color check function compatible to terminal. (jcs-emacs-init)
* Add more commands to eldoc activation trigger list. (jcs-emacs-init)

### 2019-12-03

* Fixed Visual Studio dark theme color from line numbers. (jcs-emacs-init)

### 2019-12-02

* Quick fixed for `helm` incompatible `completion-styles`. (jcs-emacs-init)
* Revert `helm` incompatible `completion-styles` changes. (jcs-emacs-init)
* Implemented set font util function. (jcs-emacs-init)

### 2019-12-01

* Minor fixed for iedit command when no kill ring. (jcs-emacs-init)

###2019-11-29

* Minor fixed for bury buffer function, it active only when
 `diminsh-buffer-mode` is on. (jcs-emacs-init)
* Fixed media window logic. (jcs-emacs-init)

### 2019-11-28

* Fixed toggle shell window's logic. (jcs-emacs-init)
* Fixed toggle video player window's logic. (jcs-emacs-init)
* Make feebline compatible to video player. (jcs-emacs-init)

### 2019-11-26

* Fixed iedit `kill-ring` issue when enable/disable `iedit-mode`. (jcs-emacs-init)
* Installed new package manually => `ffmpeg-player`. (jcs-emacs-init)
* Implemented video player feature. (jcs-emacs-init)

### 2019-11-25

* Fixed isearch not showing with the first two characters. (jcs-emacs-init)

### 2019-11-15

* Implemented switch `output`/`compilation` buffer keys. (jcs-emacs-init)
* Make mode-line's color compatible to light theme. (jcs-emacs-init)

### 2019-11-13

* Changed default key bindings for search in project. (jcs-emacs-init)

### 2019-11-12

* Fixed `flycheck-pos-tip` will kill describe thing pos-tip
 functionalities. (jcs-emacs-init)

### 2019-11-11

* Installed new package => `flycheck-pos-tip`. (jcs-emacs-init)

### 2019-11-09

* Manually installed new package => `reveal-in-folder`. (jcs-emacs-init)
* Fixed `multiple-cursors` lazy loading in navigate blank keys. (jcs-emacs-init)

### 2019-11-08

* Implemented `lsp-mode` to `goto-definition` function. (jcs-emacs-init)

### 2019-11-07

* Add quelpa upgrade process to standard upgrade process. (jcs-emacs-init)

### 2019-11-06

* Installed new package => `helm-describe-modes`. (jcs-emacs-init)

### 2019-11-04

* Fixed `multi-shell` maybe shell buffer logic. (jcs-emacs-init)

### 2019-11-01

* Defined goto definition functions. (jcs-emacs-init)
* Installed new package => `elisp-def`. (jcs-emacs-init)
* Added default save all buffers function. (jcs-emacs-init)
* Added reverse tab/untab save buffer function. (jcs-emacs-init)

### 2019-10-31

* Diminish buffer to `shell` and `eshell`. (jcs-emacs-init)

### 2019-10-30

* Add `multi-shell` config. (jcs-emacs-init)

### 2019-10-29

* Implemented multiple terminal functionalities. (jcs-emacs-init)
* Manually installed new package => `multi-shell`. (jcs-emacs-init)

### 2019-10-27

* Unbind return from mc/keymap. (jcs-emacs-init)
* Fixed default emmet expand line key, not doing any action
 by default. (jcs-emacs-init)

### 2019-10-26

* Implemented simplify safe jump to buffer window function. (jcs-emacs-init)
* Changed no more jump to unwanted buffer. (jcs-emacs-init)

### 2019-10-25

* Installed new package => `request`. (jcs-emacs-init)

### 2019-10-24

* Fixed C-ret not doing anything with default. (jcs-emacs-init)
* Installed new package => `wiki-summary`. (jcs-emacs-init)

### 2019-10-22

* Removed useless hl keyword => `OR`. (jcs-emacs-init)

### 2019-10-19

* Instanlled new package => `visual-regexp`. (jcs-emacs-init)
* Fixed key definition => `jcs-env.el`. (jcs-emacs-init)

### 2019-10-18

* Make tab and space compatible to VSCode's behaviour standards. (jcs-emacs-init)

### 2019-10-17

* Minor fixed with walk windows with multiple frames. (jcs-emacs-init)

### 2019-10-15

* Start supported language => `LESS`. (jcs-emacs-init)
* Start supported language => `Kotlin`. (jcs-emacs-init)
* Start supported language => `Dockerfile`. (jcs-emacs-init)

### 2019-10-14
* Set `smart-indent` as one option to move previous/next line. (jcs-emacs-init)

### 2019-10-10

* Minor bug fixed - first window line pos inaccurate. (jcs-emacs-init)
* Minor bug fixed - avoid enable/disable line number mode if not
 needed because is quite expensive. (jcs-emacs-init)

### 2019-10-09

* Bind backward/forward word capital to higher priority keys. (jcs-emacs-init)
* Completely mimic VSCode `multiple-cursors` behaviour. (jcs-emacs-init)

### 2019-10-08

* Fixed `actionscript-mode`'s mutliline comment line up issue. (jcs-emacs-init)
* Use `web-mode` instead of `vue-mode` for editing `.vue` file. (jcs-emacs-init)
* Supported multiple compilation process. (jcs-emacs-init)

### 2019-10-07

* Enabled more autoload for `origami`. (jcs-emacs-init)

### 2019-10-06

* Installed new package => `vue-mode`. (jcs-emacs-init)

### 2019-10-05

* Minor fixed with commenting with openting `/*`. (jcs-emacs-init)
* Fixed `python-mode` double quote key logic. (jcs-emacs-init)

### 2019-10-04

* Fixed remove end lines issue. (jcs-emacs-init)

### 2019-10-03

* Supported R programming language. (jcs-emacs-init)

### 2019-10-02

* Minor fixed for shell completion. (jcs-emacs-init)
* Minor fixed for shell behaviour. (jcs-emacs-init)

### 2019-10-01

* Moved preferred settings to `prog-mode`. (jcs-emacs-init)
* Implemented more `lsp-mode` to default `prog-mode`. (jcs-emacs-init)

### 2019-09-30

* Removed refresh font in `post-command-hook` functionality. (jcs-emacs-init)
* Redefined color to => oop `tag`, `type`, `value` face. (jcs-emacs-init)

### 2019-09-28

* Fixed `highlight-indent-guides` execute `guide-region` multiple
 times when using `jit-lock-register` function. (jcs-emacs-init)

### 2019-09-27

* Fixed `typescript-mode`'s highlighting. (jcs-emacs-init)

### 2019-09-25

* Fixed empty param issue list. (jcs-emacs-init)

### 2019-09-24

* Implemented ask line endings to set coding system interactive
 util function. (jcs-emacs-init)
* Add managed full test case for CI. (jcs-emacs-init)
* Fixed log multiple times issue. (jcs-emacs-init)

### 2019-09-22

* Installed new package => `diminish-buffer`. (jcs-emacs-init)

### 2019-09-21

* Installed new package => `markdown-toc`. (jcs-emacs-init)
* Installed new package => `browse-kill-ring`. (jcs-emacs-init)

### 2019-09-20

* Eliminate return with `void` type for typescript docstring. (jcs-emacs-init)
* Fixed goto address not copy issue. (jcs-emacs-init)
* Fixed mute apply with current message. (jcs-emacs-init)

### 2019-09-19

* Fixed buffer removal when reverting empty temporary file. (jcs-emacs-init)

### 2019-09-18

* Implemented `auto-highlight-symbol` with light/dark theme
 consideration. (jcs-emacs-init)
* Redesign comment faces with light/dark theme consideration. (jcs-emacs-init)

### 2019-09-17

* Installed new package => `org-bullets`. (jcs-emacs-init)

### 2019-09-16

* Fixed smart backspace/delete word key behaviour. (jcs-emacs-init)
* Installed new package => `quelpa`. (jcs-emacs-init)

### 2019-09-13

* Fixed refresh buffer menu bug when switch buffer. (jcs-emacs-init)

### 2019-09-11

* Fixed shell mode key bindings. (jcs-emacs-init)

### 2019-09-10

* Implemented `buffer-menu` filtering with `flx`. (jcs-emacs-init)
* Implemented `mute-apply` util function. (jcs-emacs-init)

### 2019-09-09

* Removed package => `beacon`. (jcs-emacs-init)
* Improved `feebleline` read-only config. (jcs-emacs-init)

### 2019-09-05

* Add read-only symbol to `feebleline` design. (jcs-emacs-init)

### 2019-09-04

* Fixed helm scrolling with window line height. (jcs-emacs-init)
* Fixed keys `C-c` and `C-x`. (jcs-emacs-init)

### 2019-09-03

* Removed package `helm-flx`. (jcs-emacs-init)
* Installed new package => `helm-fuzzy`. (jcs-emacs-init)

### 2019-08-29

* Fixed some missing dependencies in some lazy loading functions. (jcs-emacs-init)

### 2019-08-28

* Fixed helm weird scrolling on the last selection issue. (jcs-emacs-init)
* Rearrange key specify by mode. (jcs-emacs-init)

### 2019-08-26

* Clean up unused code from `jcs-buffer-menu.el` file. (jcs-emacs-init)

### 2019-08-25

* Update buffer menu list when navigating through windows. (jcs-emacs-init)

### 2019-08-24

* Installed new package => `helm-flx`. (jcs-emacs-init)

### 2019-08-23

* Implemented horizontal center util function. (jcs-emacs-init)
* Installed new package => `dap-mode`. (jcs-emacs-init)
* Removed package => `sublimity`. (jcs-emacs-init)
* Stopped support feature `smooth scroll`. (jcs-emacs-init)
* Stopped support feature `minimap`. (jcs-emacs-init)

### 2019-08-22

* Installed new package => `lsp-mode`. (jcs-emacs-init)
* Diminish `emmet-mode`. (jcs-emacs-init)

### 2019-08-19

* Diminish `company-fuzzy-mode`. (jcs-emacs-init)

### 2019-08-17

* Complete key bindings document. (jcs-emacs-init)
* Installed new package => `flx`. (jcs-emacs-init)
* Installed new package => `company-fuzzy`. (jcs-emacs-init)

### 2019-08-14

* Minor tweak for `company` configuration for selection highlighting. (jcs-emacs-init)

### 2019-08-13

* Bind balance split window key as default split window behaviour. (jcs-emacs-init)

### 2019-08-12

* Minor tweak for `company` configuration. (jcs-emacs-init)
* Make tab key compatible with `company`. (jcs-emacs-init)

### 2019-08-08

* Implemented ask csharp template functionalities to `csharp-mode`. (jcs-emacs-init)

### 2019-08-07

* Removed package => `company-statistics`. (jcs-emacs-init)
* Minor changes for `company` package. (jcs-emacs-init)

### 2019-08-05

* Config eldoc trigger commands. (jcs-emacs-init)

### 2019-08-01

* Show tooltip even with one valid candidate in `company-mode`. (jcs-emacs-init)
* Fixed display not ready issue on buffer menu. (jcs-emacs-init)

### 2019-07-30

* Fixed minor documentation issue. (jcs-emacs-init)

### 2019-07-29

* Supply `ruby-mode` and `rust-mode` indentation level's config. (jcs-emacs-init)

### 2019-07-26

* Implemented ability to record down the tab width across all
 major mode. (jcs-emacs-init)

### 2019-07-25

* Make tab width record to the next buffer with the same mode. (jcs-emacs-init)

### 2019-07-24

* Implemented non-verbose beginning/end of buffer functions. (jcs-emacs-init)
* Bind non-verbose beginning/end of buffer key functions. (jcs-emacs-init)

### 2019-07-23

* Implemented increment/decrement tab size functions. (jcs-emacs-init)
* Customize `feebleline` with system spaces or tabs displayed. (jcs-emacs-init)
* Customize `feebleline` with tab size displayed. (jcs-emacs-init)

### 2019-07-22

* Complete more preprocessor highlighting. (jcs-emacs-init)

### 2019-07-18

* Enabled `so-long-mode` as default. (jcs-emacs-init)

### 2019-07-16

* Implemented `buffer menu`'s return key. (jcs-emacs-init)

### 2019-07-15

* Implemented realtime updating buffer menu. (jcs-emacs-init)
* Implemented filter functionality to buffer menu. (jcs-emacs-init)

### 2019-07-14

* Customize `snippet-mode` by adding `jcs-snippet-mode.el` file. (jcs-emacs-init)
* Add snippet for `snippet-mode`. (jcs-emacs-init)
* Fixed shell toggle logic, it no longer depends on the function
 state and now compatible to mutliple frames. (jcs-emacs-init)

### 2019-07-13

* Removed package => `indent-info`. (jcs-emacs-init)
* Implemented remove carriage return symbol function. (jcs-emacs-init)
* Fixed deletetion logic with tab width. (jcs-emacs-init)

### 2019-07-12

* Prevent loggin when refreshing dashboard, too verbose loggnig. (jcs-emacs-init)

### 2019-07-11

* Update `feebleline` customization. (jcs-emacs-init)

### 2019-07-10

* Installed new package => `feebleline`. (jcs-emacs-init)
* Customize `feebleline` for default mode-line toggle. (jcs-emacs-init)
* Implemented electric delete key. (jcs-emacs-init)

### 2019-07-09

* Implemented vs sharp key and bind to these following modes. (jcs-emacs-init)
  => c-mode
  => c++-mode
  => csharp-mode

### 2019-07-08

* Reverted package => `line-reminder`. (jcs-emacs-init)
* Removed manually installed package => `line-indicators`. (jcs-emacs-init)
* Set use linum when inside terminal for `line-reminder` package. (jcs-emacs-init)

### 2019-07-07

* Manually installed new package => `line-indicators`. (jcs-emacs-init)
* Removed package => `line-reminder`. (jcs-emacs-init)

### 2019-07-05

* Installed new package => `centaur-tabs`. (jcs-emacs-init)
* Installed new package => `company-statistics`. (jcs-emacs-init)
* Removed package => `tabbar`. (jcs-emacs-init)

### 2019-07-04

* Fixed certain modes that does not apply `highlight-indent-guides`
 minor mode. (jcs-emacs-init)
* Rename backward/forward capitcal word keys, much better naming. (jcs-emacs-init)
* Fixed certain modes require error. (jcs-emacs-init)

### 2019-07-03

* Bind `package-list-packages` to `C-x C-p` instead of `C-p`. (jcs-emacs-init)
* Fixed iedit-mode logic. (jcs-emacs-init)
* Removed inconsistent key bindings for `c-mode` and `c++-mode`. (jcs-emacs-init)
* Add unity snippets => `csharp-mode`. (jcs-emacs-init)

### 2019-07-02

* Install new package => `highlight-indent-guides`. (jcs-emacs-init)
* Fixed `oop-func`'s built in docstring autoload. (jcs-emacs-init)

### 2019-07-01

* Installed new package => `alt-codes`. (jcs-emacs-init)
* Implemented scratch other window function. (jcs-emacs-init)

### 2019-06-30

* Installed new package => `helm-file-preview`. (jcs-emacs-init)

### 2019-06-29

* Fixed multiple with-eval-after-load function. (jcs-emacs-init)
* Implemented maybe kill `*scratch*` buffer function. (jcs-emacs-init)

### 2019-06-28

* Require `undo-tree` when needed. (jcs-emacs-init)
* Try using first time post command startup. (jcs-emacs-init)
* Implemented multiple with-eval-after-load function. (jcs-emacs-init)

### 2019-06-25

* Implemeneted cheat sheet functions. (jcs-emacs-init)
* Use regexp to ignore line numbers mode. (jcs-emacs-init)
* Installed new mode => `gdscript-mode` for editing Godot Script
 file. (jcs-emacs-init)
* Added `gdscript-mode` snippets using `yasnippets`. (jcs-emacs-init)
* Had `helm-ag` requires pattern to `2`. (jcs-emacs-init)

### 2019-06-24

* Removed startup mode files, and moved their config to
 `jcs-mode.el` file. (jcs-emacs-init)
   - `jcs-elisp-mode.el`
   - `jcs-lisp-mode.el`
   - `jcs-text-mode.el`
* Implemented insert header if buffer empty function for inserting
 file/mode header. (jcs-emacs-init)
* Implemented html preview function. (jcs-emacs-init)

### 2019-06-23

* Clean up code for better load speed. (jcs-emacs-init)
* Fixed `helm-file-files` inserting `/` logic. (jcs-emacs-init)
* Removed switch window by `M-0` to `M-9` keys. (jcs-emacs-init)
* Implemented the display ascii-table function. (jcs-emacs-init)

### 2019-06-22

* Implemented remove item from `*dashboard*` buffer. (jcs-emacs-init)
* Completed CI test. (jcs-emacs-init)
* Update the `oop-func` logic, better and does not requires
 font lock implementation. Now it uses `search-string` instead. (jcs-emacs-init)

### 2019-06-21

* Upate text banner. (jcs-emacs-init)

### 2019-06-20

* Fixed dashboard next/prev blank line logic. (jcs-emacs-init)
* Add text banner file => `./.emacs.jcs/banner/sink.txt`. (jcs-emacs-init)
* Implemented autoloads functionalities to manually installed
 packages. (jcs-emacs-init)

### 2019-06-19

* Implemented better dashboard buffer controlling util functions. (jcs-emacs-init)
* Clean up customizes code section to => `~/.emacs.d/.jcs-custom.el`
 file. (jcs-emacs-init)

### 2019-06-18

* Removed package => `helm-gtags`. (jcs-emacs-init)
* Installed new packae => `dumb-jump`. (jcs-emacs-init)
* Use `dumb-jump` replacing `helm-gtags` functionalities. (jcs-emacs-init)

### 2019-06-16

* Fixed focus in, refresh dashboard buffer hanging issue. (jcs-emacs-init)
* Clean up log code. (jcs-emacs-init)

### 2019-06-15

* Use default helm display path option from `relative` to `root`. (jcs-emacs-init)
* Installed new pacakge => `region-occurrences-highlighter`. (jcs-emacs-init)

### 2019-06-14

* Renamed `jcs-corresponding-file.el` to just `jcs-file.el`. (jcs-emacs-init)
* Renamed `jcs-file-info-format.el` to `jcs-template.el`. (jcs-emacs-init)
* Use find file in project instead of just find file for searching
 corresponding file functionalities. (jcs-emacs-init)

### 2019-06-13

* Update `*dashboard*` buffer when access recent projects list. (jcs-emacs-init)

### 2019-06-12

* Removed some of useless `require`s. (jcs-emacs-init)
* Removed some of useless plugin's config. (jcs-emacs-init)
* Ready the configuration for Emacs version 27. (jcs-emacs-init)

### 2019-06-10

* Disable `multiple-cursors` when navgiating blank line. (jcs-emacs-init)
* Installed new pacakge => `yascroll`. (jcs-emacs-init)
* Add customize `yascroll` face by theme color function. (jcs-emacs-init)

### 2019-06-07

* Fixed `helm-projectile` return key not exiting minibuffer issue. (jcs-emacs-init)
* Re-implements `helm-files` related functions. For find files
 other windows. (jcs-emacs-init)

### 2019-06-06

* Clean up some compile warningins. (jcs-emacs-init)

### 2019-06-05

* Optimized configuration down to startup time around from `2`
 to `6` seconds. (jcs-emacs-init)

### 2019-06-03

* Optimized configuration down to startup time around from `4`
 to `8` seconds. (jcs-emacs-init)
* Add more `helm` find files keymap to match OS's file explorer's
 navigation system. (jcs-emacs-init)
* Add `jcs-emacs-version`. (jcs-emacs-init)

### 2019-06-02

* Installed new pacakge => `esup`. (jcs-emacs-init)

### 2019-06-01

* Clean package initialization using `require` keyword. (jcs-emacs-init)

### 2019-05-31

* Fixed `50%` of config compile issues. (jcs-emacs-init)

### 2019-05-30

* Enable compile version of this config. (jcs-emacs-init)

### 2019-05-29

* Fixed `helm` theme inconsistent to the `vs-light` theme. (jcs-emacs-init)

### 2019-05-27

* Fixed `right-click-context` package's bug #2 and #7 issues. (jcs-emacs-init)
* Removed package `pdf-tools`. (jcs-emacs-init)
* Implemented automatically enable `read-only-mode` when view source
 or library files. (jcs-emacs-init)

### 2019-05-25

* Removed package `floobits`. (jcs-emacs-init)
* Add ignore activating line numbers by major mode list. (jcs-emacs-init)

### 2019-05-24

* Updated line numbers ignore buffer list. (jcs-emacs-init)

### 2019-05-22

* Remove before/after init files. (jcs-emacs-init)
* Optimized more plugins to `jcs-plugin.el` file. (jcs-emacs-init)

### 2019-05-21

* Fixed `compilation-mode-hook` from `jcs-env.el` file. (jcs-emacs-init)

### 2019-05-20

* Reduced duplicated code in `jcs-comment.el` file. (jcs-emacs-init)

### 2019-05-19

* Removed manually installed package => `show-eol`. (jcs-emacs-init)
* Installed new pacakge => `show-eol`. (jcs-emacs-init)
* Make `comment` and `uncomment` related functions compatbile to
 `line-reminder` package. (jcs-emacs-init)
* Add is behind last char at line util function. (jcs-emacs-init)
* Add `point` option to infront first char at line util function. (jcs-emacs-init)

### 2019-05-17

* Add `*Package-Lint*` to line numbers not displayed list. (jcs-emacs-init)
* Manually updated `show-eol` package manually => `20190517.001`. (jcs-emacs-init)

### 2019-05-14

* Start supports `dart` by using `dart-mode`. (jcs-emacs-init)
* Start supports `pascal` by using `pascal-mode`. (jcs-emacs-init)
* Start supports `Object Pascal`/`Delphi` by using `opascal-mode`. (jcs-emacs-init)
* Add `dart-mode`'s snippets. (jcs-emacs-init)
* Add `pascal-mode`'s snippets. (jcs-emacs-init)
* Add `opascal-mode`'s snippets. (jcs-emacs-init)

### 2019-05-13

* Manually updated `show-eol` package manually => `20190513.002`. (jcs-emacs-init)
* Manually updated `show-eol` package manually => `20190513.001`. (jcs-emacs-init)

### 2019-05-10

* Implements self defined comment or string util function. (jcs-emacs-init)

### 2019-05-08

* Remove `jcs-top-level-active` global `defvar` for keyboard quit
 check. (jcs-emacs-init)
* Remove `jcs-minibuffer-active` global `defvar` for minibuffer
 active check. (jcs-emacs-init)

### 2019-05-06

* Fixed `hl-todo-mode` not working in `web-mode` by redefine
 highlighting condition => `jcs-plugin.el` file. (jcs-emacs-init)

### 2019-05-05

* Implements calc eval region function for calculating the region
 and replace it with the calculated result. (jcs-emacs-init)
* Implements backward/forward symbol functions for interactive use. (jcs-emacs-init)

### 2019-05-04

* Revert `haxe-mode` so it works for now, but still leave with no
 maintainer with this mode. (jcs-emacs-init)

### 2019-05-03

* Implements `get window` and `get window id` util functions. (jcs-emacs-init)
* Fixed reset dashboard banner not refresh issue. (jcs-emacs-init)
* Installed new pacakge => `hl-todo`. (jcs-emacs-init)

### 2019-05-02

* Implements check if light or dark color util functions. (jcs-emacs-init)
* Added default light theme. (jcs-emacs-init)

### 2019-05-01

* Manually installed package `show-eol`. (jcs-emacs-init)
* Make `text-mode` to the top for ready to override by other mode. (jcs-emacs-init)

### 2019-04-29

* Organized configuration's directory structure. (jcs-emacs-init)
* Remove `jcs-font.el` file and put the `font` config to the
 `jcs-env.el` and `jcs-plugin.el` files. (jcs-emacs-init)

### 2019-04-28

* Update dependency list. (jcs-emacs-init)

### 2019-04-27

* Add deactive all line numbers modes util function. (jcs-emacs-init)
* Fixed toggle mode line key binding. (jcs-emacs-init)
* Fixed active line numbers by mode logic, we use to deactive the
 line numbers mode for now instead of just ignore it. (jcs-emacs-init)
* Fixed modes not activated after revert issue. (jcs-emacs-init)

### 2019-04-24

* Implements `toggle-mode-line`. (jcs-emacs-init)
* Cleanup `web-mode`'s util functions. (jcs-emacs-init)
* Unbind `web-mode` util functions from `jcs-web-func.el` file. (jcs-emacs-init)

### 2019-04-23

* Minor fixed with some typo. (jcs-emacs-init)

### 2019-04-22

* Installed new pacakge => `goto-char-preview`. (jcs-emacs-init)
* Add new snippet for `react.js` in html. (jcs-emacs-init)
* Add new snippet for `bootstrap` in html. (jcs-emacs-init)
* Add new snippet for `three.js` in html. (jcs-emacs-init)
* If region active, when `isearch` is activated we use region instead. (jcs-emacs-init)
* Fixed `css-mode` return key. (jcs-emacs-init)
* Fixed css number not highlighting correctly. (jcs-emacs-init)

### 2019-04-21

* Installed new pacakge => `isearch-project`. (jcs-emacs-init)
* Bind `isearch-project-forward` to implement `cross-mode` search
 through project ability. (jcs-emacs-init)
* Implements helm projectile find file other window function. (jcs-emacs-init)

### 2019-04-20

* Split electric pair pairs to each specific mode. (jcs-emacs-init)

### 2019-04-19

* Remove `shift-select` package, the package is still remained
 unstable. (jcs-emacs-init)
* Sort keys in alphabetic order category. (jcs-emacs-init)
* Revert `shift-select` package => version `20190419.001`. (jcs-emacs-init)
* Implements is symbol contain in list of symbol util function. (jcs-emacs-init)

### 2019-04-18

* Add more key bindings for switching windows. (jcs-emacs-init)
* Add remove trailing lines at the end of buffer util function. (jcs-emacs-init)
* Implements self design mark whole buffer. (jcs-emacs-init)
* Remove README, LICENSE, bochsrc files default to `org-mode`. (jcs-emacs-init)
* Add `html-mode` and `js-mode` snippets. (jcs-emacs-init)

### 2019-04-17

* Manually installed `shift-select` package. (jcs-emacs-init)

### 2019-04-16

* Add more `:defer` to more packages. (jcs-emacs-init)
* Removed many unused packages. (jcs-emacs-init)
* Fixed `jcs-flycheck-mode` logic. (jcs-emacs-init)
* Remove smart shift select home/end functions. (jcs-emacs-init)
* Complete set of manual install package section. (jcs-emacs-init)

### 2019-04-15

* Implements selecting windows by using windows' index. (jcs-emacs-init)
* Removed `elpy` package. (jcs-emacs-init)
* Removed `find-file-in-project` package. (jcs-emacs-init)
* Removed `ivy` package. (jcs-emacs-init)
* Installed `projectile` package. (jcs-emacs-init)
* Removed `js2-refactor` package. (jcs-emacs-init)
* Implements `multiple-cursors` quick hand functions. (jcs-emacs-init)
* Fixed vs curly bracket logic. (jcs-emacs-init)
* Start supports `elixir` by using `elixir-mode`. (jcs-emacs-init)
* Start supports `erlang` by using `erlang-mode`. (jcs-emacs-init)
* Installed `helm-projectile` package. (jcs-emacs-init)

### 2019-04-14

* Installed new pacakge => `buffer-move`. (jcs-emacs-init)
* Fixed `same file other window` bug. (jcs-emacs-init)

### 2019-04-13

* Fixed `undo-tree` occurs error when trying to kill its
 parent buffer. (jcs-emacs-init)
* Starts featuers documentation under `./features/` folder. (jcs-emacs-init)
* Split `.ini` and `.properties` mode. (jcs-emacs-init)
* Add `jcs-properties-mode.el` for supporting java
 properties file. (jcs-emacs-init)

### 2019-04-12

* Installed new pacakge => `dashboard`. (jcs-emacs-init)
* Installed new pacakge => `beacon`. (jcs-emacs-init)
* Minor fixed from version `5.3.2`. (jcs-emacs-init)

### 2019-04-11

* Add `gitconfig` configurations. (jcs-emacs-init)
* Use `with-eval-after-load` macro to speed up startup time. (jcs-emacs-init)
* Huge update on the startup time, now the average startup time
 is lower than `10` seconds. (jcs-emacs-init)

### 2019-04-10

* Customize `company`'s appearance close to `auto-complete`'s
 appearance. (jcs-emacs-init)
* Add config to make `company` a bit more close to `auto-complete`'s
 behavior. (jcs-emacs-init)
* Add `show hover` function related to VSCode `Show Hover` key. (jcs-emacs-init)

### 2019-04-09

* Kill `undo-tree-visualizer` when killing undoing buffer. (jcs-emacs-init)
* Start adding own snippets using `yasnippet`. (jcs-emacs-init)
* Rename all `cs` related naming to `csharp` for consistency. (jcs-emacs-init)
* Rename all `elisp` related naming to `emacs-lisp` for consistency. (jcs-emacs-init)
* Rename all `cbl` related naming to `cobol` for consistency. (jcs-emacs-init)
* Split `cmake-mode` and `makefile-mode` into two files. (jcs-emacs-init)
* Installed new pacakge => `company-quickhelp`. (jcs-emacs-init)
* Remove `auto-complete` and use `company` instead. (jcs-emacs-init)
* Start supports GLSL file, using `glsl-mode`. (jcs-emacs-init)

### 2019-04-08

* Removed manually installed `verilog-mode`, it mode is already
 merged into GNU Emacs. (jcs-emacs-init)

### 2019-04-07

* `polymode` package added by system. (jcs-emacs-init)
* Don't use `narrow-to-region`, instead we just pass in the
 `start` point and `end` point. (jcs-emacs-init)
* Installed new pacakge => `origami`. (jcs-emacs-init)
* Use `origami` as default folding system to this config. (jcs-emacs-init)

### 2019-04-06

* Implements `jcs-message-func.el` file. (jcs-emacs-init)
  -> Erase *Messages* buffer.
  -> Erase *Messages* buffer without closing it.

### 2019-04-05

* Make oop docstring compatible with ref and pointer in c and
 c++ mode. (jcs-emacs-init)
* Fixed kill buffer after exit buffer menu mode. (jcs-emacs-init)

### 2019-04-04

* Fully implements TypeScript docstring. (jcs-emacs-init)

### 2019-04-03

* Implements ActionScript docstirng. (jcs-emacs-init)

### 2019-04-02

* Implements `web-mode`'s version front curly bracket key and
 bind it to web-mode. (jcs-emacs-init)
* Fixed docstring display issue in `web-mode`'s php file. (jcs-emacs-init)
* Fixed vs curly bracket logic. (jcs-emacs-init)

### 2019-03-31

* Add optional to scroll up/down line functions. (jcs-emacs-init)
* Complete line related util functions. (jcs-emacs-init)
* Remove `Alex Shinn`'s `css-mode`, use Emacs's default `css-mode`
 instead. (jcs-emacs-init)
* Add larger window height check util funtion. (jcs-emacs-init)

### 2019-03-30

* Implements ActionScript docstring entry point. (jcs-emacs-init)
* Implements CSharp docstring entry point. (jcs-emacs-init)
* Fixed only one file opened, switch to default Emacs buffer
 issue. (jcs-emacs-init)
* Installed new pacakge => `yasnippet-snippets`. (jcs-emacs-init)
* Add configuration for `yasnippet` and `yasnippet-snippets`. (jcs-emacs-init)

### 2019-03-29

* Bind electric backspace key to certain modes as default key
 binding. (jcs-emacs-init)
* Improved undo/redo keys performance when using `undo-tree`. (jcs-emacs-init)
* Simplify code in `jcs-oop.el` file. (jcs-emacs-init)

### 2019-03-28

* Implements `typescript-mode` docstring. (jcs-emacs-init)
* Add `Startup Time` section in the `README.md` file for
 describing the current condition for using this configuration
 when starting up Emacs. (jcs-emacs-init)

### 2019-03-27

* Add advice to `save-buffer` key to disable `undo-tree`. (jcs-emacs-init)

### 2019-03-26

* Fixed compile target script, wrong param name. (jcs-emacs-init)
* Move `jcs-helm.el` functions to `jcs-helm-func.el` file and
 delete `jcs-helm.el` file. (jcs-emacs-init)
* Manually update `reload-emacs` package => 20190326.001. (jcs-emacs-init)
* Add first visible line pos util functions. (jcs-emacs-init)
* Make revert window state to reopn this buffer key. (jcs-emacs-init)
* Fixed reopen this buffer key, make compatible with opening the
 same buffer in different/mutliple windows. (jcs-emacs-init)

### 2019-03-25

* Remove global linum mode when using undo-tree. (jcs-emacs-init)
* Implements reopen this buffer key. (jcs-emacs-init)
* Add line number related functions => `jcs-function.el` file. (jcs-emacs-init)
* Add mixed of using `display-line-numbers-mode` and `linum-mode`,
 for any file that uses `line-reminder` mode use `linum-mode`.
 Other we use `display-line-numbers-mode`. (jcs-emacs-init)
* Fixed `overwrite-mode` cursor not working. (jcs-emacs-init)
* Add walk through each window util function. (jcs-emacs-init)

### 2019-03-20

* Bind reload emacs and restart emacs. (jcs-emacs-init)
* Remove self design reload emacs function. (jcs-emacs-init)
* Manually install package => `reload-emacs`. (jcs-emacs-init)
* Config `reload-emacs` package using `use-package` in =>
 `jcs-plugin.el` file. (jcs-emacs-init)

### 2019-03-19

* Rename plugin advice function name for accuracy purpose =>
 `jcs-plugin.el` file. (jcs-emacs-init)

### 2019-03-18

* Bind isearch forward at point key. (jcs-emacs-init)
* Remove search forward/backward at point functions. (jcs-emacs-init)

### 2019-03-17

* Installed new pacakge => `move-text`. (jcs-emacs-init)

### 2019-03-13

* Bind rebind keys after define `jcs-global-key-rebind` function. (jcs-emacs-init)

### 2019-03-12

* Installed new pacakge => `restart-emacs`. (jcs-emacs-init)
* Retain reload emacs functionalities. (jcs-emacs-init)
* Fixed smart indent up/down keys in `css-mode`. (jcs-emacs-init)
* Remove unused packages. (jcs-emacs-init)
  - auto-complete-c-headers
  - google-c-style

### 2019-03-11

* Speedup Emacs startup time. (jcs-emacs-init)
* Move erase buffer to somewhere more reasonable. (jcs-emacs-init)
* Use require instead of load path. (jcs-emacs-init)
* Fixed check `truncate-lines`, this isn't minor-mode is actually a
 variable with t or nil. (jcs-emacs-init)

### 2019-03-10

* Installed new pacakge => `indicators`. (jcs-emacs-init)
* Implements toggle transparency window that will record dowwn
 the last transparent alpha level. This feature polished the
 user experience wise. (jcs-emacs-init)

### 2019-03-08

* Implements switch window group layout vertically/horizontally
 key's functionality. (jcs-emacs-init)
* Installed new pacakge => `focus`. (jcs-emacs-init)

### 2019-03-07

* Installed new package => `dimmer`. (jcs-emacs-init)
* Fixed speedbar not starting in the correct directory tree
 using `default-directory` variable instead of fiddle method of
 fixing this issue. (jcs-emacs-init)
* Manage most plugin configurations using `use-package` package. (jcs-emacs-init)
* Revert part of the code, fixed indentation incorrect when doing
 docstring comment style. (jcs-emacs-init)

### 2019-03-06

* Add screen config section => `jcs-env.el` file. (jcs-emacs-init)
* Add goto-line-preview section and configurations. (jcs-emacs-init)
* Start using `use-package` in the config, add `Package Management`
 section to the feature list. (jcs-emacs-init)
* Fixed speedbar not opening the current file directory issue. (jcs-emacs-init)
* Rebind some key bindings for more reasonable reason, see
 `./doc/keybindings.txt` file. (jcs-emacs-init)

### 2019-03-05

* Diminish minor modes, `overwrite-mode` and `eldoc-mode`. (jcs-emacs-init)
* Make toggle terminal command compatible to vscode preset's
 key bindings. (jcs-emacs-init)
* Bind `describe-bindings` key to `C-k C-s`, compatible to vscode
 preset's key bindings. (jcs-emacs-init)
* Upgrade with more math functions => `jcs-math.el` file. (jcs-emacs-init)
* Rebind toggle cross/depends mode key to `C-~` key. (jcs-emacs-init)
* Fixed transparent window util functions and reduced duplicate
 code. (jcs-emacs-init)
* Rebind text scalle up/down key to `C-=` and `C--`. (jcs-emacs-init)
* Rebind transparent frame increament/decreament key to `M-=` and
 `M--`. (jcs-emacs-init)
* Update some key bindings to `./doc/keybindings.txt` file. (jcs-emacs-init)

### 2019-03-04

* Add typescript docstring configurations. (jcs-emacs-init)
* Add `.properties` extension to default as `ini-mode`. (jcs-emacs-init)
* Fixed css and web return key => `jcs-web-func.el` file. (jcs-emacs-init)

### 2019-03-03

* No longer needed resolve `goto-line-preview-goto-line` that does
 not go back to original position issue, the package resolved
 itself. (jcs-emacs-init)
* Reserve `goto-line-preview` config section. (jcs-emacs-init)
* Update key command from => `goto-line-preview-goto-line` to
 `goto-line-preview`.

### 2019-03-02

* Installed new package => `goto-line-preview`. (jcs-emacs-init)
* Remove `goto-line` key, instead we use package `goto-line-preview`
 from melpa. (jcs-emacs-init)
* Make compatible with old `jcs-goto-line` key, by having check
 in `jcs-hook.el` file. (jcs-emacs-init)
* Reserve minibuffer post command hook. (jcs-emacs-init)
* Add `goto-lnie-preview` config section in the `jcs-plugin.el`
 file. (jcs-emacs-init)

### 2019-03-01

* Add top level activation flag. (jcs-emacs-init)
* Move minibuffer hook to hook file. (jcs-emacs-init)
* Add improved goto line navigation functionalities and bind to
 original `goto-line` key. (jcs-emacs-init)

### 2019-02-28

* Set keys compatible to VS Code default key bindnigs. (jcs-emacs-init)
* Fixed toggle vertical/horizontal editor layout functionality
 that does not works on the second window in the current frame.
 Notice this is only a temporary fixed. (jcs-emacs-init)

### 2019-02-25

* Fixed magit installation error by updating its' dependencies. (jcs-emacs-init)

### 2019-02-23

* Update beginning/end of visual line the same behaviours as the
 VSCode text editor's key behaviours. (jcs-emacs-init)
* Make ALT-z toggle `truncate-line-mode`, so it compatible to
 VSCode's key presets. (jcs-emacs-init)

### 2019-02-20

* Add jcs home and end keys functionalities. (jcs-emacs-init)
* Bind home and end keys functions. (jcs-emacs-init)

### 2019-02-12

* Move jcs web mode truncate line functionality to hook instead of
 locate every key functions. (jcs-emacs-init)
* Remove web left/right key functions/functionalities. (jcs-emacs-init)
* Avoid auto truncate line functionalities while navigating empty
 lines in web-mode. (jcs-emacs-init)
* Revert jcs set init face. (jcs-emacs-init)
* Load set init face in js2-mode. (jcs-emacs-init)

### 2019-02-06

* Manually update manual packages. (jcs-emacs-init)

### 2019-02-04
* Fixed readme description. (jcs-emacs-init)

### 2019-02-03

* Add key bindings description to readme file. (jcs-emacs-init)

### 2019-02-02

* Update project description and elaborates more about it. (jcs-emacs-init)

### 2019-02-01

* Implements symbol util functions. (jcs-emacs-init)
* Remove single line comments font lock keywords => mapc. (jcs-emacs-init)

### 2019-01-31

* Add jcs python docstring face. (jcs-emacs-init)

### 2019-01-29

* Fixed python tab key binding with weird action. (jcs-emacs-init)
* Ensure python tab width is 4 instead of default of 8. (jcs-emacs-init)

### 2019-01-21

* Remove load todo, load log and insert-timeofday command functions. (jcs-emacs-init)

### 2019-01-15

* Use defense programming in current char string util function. (jcs-emacs-init)

### 2019-01-08

* Add is-killed returned value to jcs-maybe-kill-this-buffer util function. (jcs-emacs-init)
* Fixed jcs' count window util function. (jcs-emacs-init)
* Fixed re-builder's maybe kill this buffer function using is-killed variable. (jcs-emacs-init)

### 2019-01-05

* Implements python return function => jcs-python-func.el. (jcs-emacs-init)
* Organize legacy code => jcs-python-func.el. (jcs-emacs-init)

### 2019-01-04

* Fixed python insert docstring function, for second situation, between two
 double quotes. (jcs-emacs-init)

### 2019-01-02

* Remove history, is no longer needed. (jcs-emacs-init)
* Add load face order, and just reload instead of operate the list functions. (jcs-emacs-init)
* Add sharp single line comment face. (jcs-emacs-init)
* Compatible to electric pair command in python mode. (jcs-emacs-init)
* Fixed move forward/backward word navigation util functions. (jcs-emacs-init)
* Add ask python template and use it when creating new python file. (jcs-emacs-init)
* Add python plain and class template. (jcs-emacs-init)
* Mark version 5.2.1 and release one version. (jcs-emacs-init)

### 2019-01-01

* Revert maybe kill this buffer function and add ecp-same arg. (jcs-emacs-init)

### 2018-12-31
* Update definition for maybe kill this buffer function => jcs-edit.el. (jcs-emacs-init)
* Optimize switch to prev/next buffer util functions. (jcs-emacs-init)
* Diminish right click context mode. (jcs-emacs-init)

### 2018-12-28

* Some modifications for maybe kill buffer key. (jcs-emacs-init)
* Add next/prev buffer util functions. (jcs-emacs-init)
* Add print buffer util functions. (jcs-emacs-init)

### 2018-12-25

* Install new package => right-click-context. (jcs-emacs-init)
* Add package to pre-install package list => right-click-context. (jcs-emacs-init)
* Enable right-click-context as default in plugin config file => jcs-plugin.el. (jcs-emacs-init)

### 2018-12-24

* Start support INI file, customize the `ini-mode' with jcs-ini-mode.el
 file. (jcs-emacs-init)
* Fixed coding style => jcs-file-info-format.el. (jcs-emacs-init)

### 2018-12-23

* Add electric backspace util function. (jcs-emacs-init)
* Add electric open/close pair related functions. (jcs-emacs-init)
* Fixed verbose char to byte and char to string util functions. (jcs-emacs-init)

### 2018-12-21

* Add yaml func file for yaml mode functions => jcs-yaml-func.el. (jcs-emacs-init)

### 2018-12-16

* Add new package 'auto-rename-tag' to preinstall package list. (jcs-emacs-init)
* Add new package 'htmltagwrap' to preinstall package list. (jcs-emacs-init)
* Diminish the 'auto-rename-tag' minor mode. (jcs-emacs-init)
* Active diminish by requiring the package you want to diminish =>
 'auto-rename-tag', bug fixed. (jcs-emacs-init)

### 2018-12-14

* Add hex and char section to last-command-event doc => doc/last-command-event.txt. (jcs-emacs-init)

### 2018-12-13

* Add doc/last-command-event.txt for record all the last-command-event's returns
 value. (jcs-emacs-init)

### 2018-12-11

* Add indent-info package and it's config. (jcs-emacs-init)

### 2018-12-06

* Fixed insert header only when buffer-file-name variable available. (jcs-emacs-init)

### 2018-12-04

* Fixed bug by adding percise check => jcs-maybe-kill-this-buffer function
 in jcs-edit.el file. (jcs-emacs-init)
* Implements check how many times the same buffer shown in different windows
 => jcs-buffer-showns function in jcs-window.el file. (jcs-emacs-init)

### 2018-12-03

* Mark version 5.1.9 and release one version. (jcs-emacs-init)

### 2018-12-01

* Bug fixed, make percise return key for web-mode => jcs-web-return-key. (jcs-emacs-init)

### 2018-11-26

* Add gitattribute custom mode hook. (jcs-emacs-init)

### 2018-11-25

* Make one history => ### 2018-11-25. (jcs-emacs-init)
* Implement YAML mode hook, => jcs-yaml-mode.el file. (jcs-emacs-init)

### 2018-11-21

* Start support Swift file, customize the `swift-mode' with jcs-swift-mode.el
 file. (jcs-emacs-init)

### 2018-11-17

* Start support Rust file, customize the `rust-mode' with jcs-rust-mode.el
 file. (jcs-emacs-init)

### 2018-11-13

* Start support Ruby file, customize the `ruby-mode' with jcs-ruby-mode.el
 file. (jcs-emacs-init)

### 2018-11-04

* Fixed web-mode highlighting missing when apply ASP.NET Razor v3 comment
 highlighting rule. (jcs-emacs-init)
* Add `jcs-post-command-hook' in `jcs-hook.el' in order to fix highlihging
 missing when editing file using web-mode. (jcs-emacs-init)

### 2018-11-03

* Start support YAML file, install major mode 'yaml-mode'. (jcs-emacs-init)

### 2018-10-22

* Implement web return key functionalities. (jcs-emacs-init)
* Increase readabilities for util module. (jcs-emacs-init)

### 2018-10-18

* Start support Markdown file, install major mode 'markdown-mode'. (jcs-emacs-init)
* Add customize markdown mode configurations. (jcs-emacs-init)

### 2018-10-17

* Completey remove neotree. (jcs-emacs-init)
* Use `speedbar' and `sr-speedbar' instead of `neotree'. (jcs-emacs-init)
* Implement `speedbar' and `sr-speedbar' customize functions. (jcs-emacs-init)
* Implement `nhexl-mode' configurations. (jcs-emacs-init)

### 2018-10-15

* Long-overdue support language `Verilog', starting from now on support
 this language. (jcs-emacs-init)
* Implement deleting between functionailities, and add some custom function
 for certain generally use symbol in programming. (jcs-emacs-init)

### 2018-10-13

* Fixed check current character occurs error issue at point of
 beginning of the buffer. (jcs-emacs-init)
* Rename template to be more specific and precise on the naming. (jcs-emacs-init)

### 2018-10-12

* Add Lisp header template. (jcs-emacs-init)
* Fixed weird insert header file format's function description in
 each mode file. (jcs-emacs-init)

### 2018-10-11

* Start support TypeScript file, install major mode 'typescript-mode'. (jcs-emacs-init)
* Add typescript header format template. (jcs-emacs-init)
* Make `jayces-mode' to package. (jcs-emacs-init)
* Add `tabbar' package and set the env settings/key bindings. (jcs-emacs-init)
* Add `javadoc-lookup' package and set the env settings/key bindings. (jcs-emacs-init)
* Start support Clojure, ClojureScript and Clojure Source file,
 install major mode 'clojure-mode'. (jcs-emacs-init)
* Update license and prorject version to 5.1.7. (jcs-emacs-init)

### 2018-10-07

* Improve enable/disable truncate lines mode. (jcs-emacs-init)
* Remove web return key, seems like we no longer need this key
 function anymore. (jcs-emacs-init)
* Rename, remove emacs prefix to all doc. (jcs-emacs-init)
* Add Emacs' syntax table document. (jcs-emacs-init)
* Add Emacs' regular expression document. (jcs-emacs-init)

### 2018-10-05

* Add recentf-file mode environment settings => jcs-env.el. (jcs-emacs-init)
* Bind open recent files key => jcs-global-key.el. (jcs-emacs-init)
* Update key binding note => open recent files key. (jcs-emacs-init)

### 2018-10-03

* Implemented jcs-emmet-expand-line wrapper in order to fix on link
 goto address issue. (jcs-emacs-init)
* Bind the key in emmet mode keymap. (jcs-emacs-init)

### 2018-10-01

* Add jcs-count-frames function for multiple window's frame count. (jcs-emacs-init)
* Fixed maybe kill this buffer function with the same file name
 but different directory issue. (jcs-emacs-init)

### 2018-09-29

* Add more face to fixme mode list. (jcs-emacs-init)
* Move face settings to jcs-face.el. (jcs-emacs-init)
* Load fixedme face after all initialize, so we cover all the faces. (jcs-emacs-init)

### 2018-09-27

* Change the `default-directory' variable when compiling a script
 to the directory the current script is currently at. (jcs-emacs-init)

### 2018-09-26

* Implement the following three util functions.. (jcs-emacs-init)
   => jcs-current-whitespace-p
   => jcs-current-tab-p
   => jcs-current-whitespace-or-tab-p
* Implement jcs-text-scale-increase and jcs-text-scale-decrease
 function in order to fix the `line-reminder' plugin issue. (jcs-emacs-init)
* Fix everytime it search forward recursive, it will centerl the
 window issue. But does not happens in search backward recursive...
 Weird! => locate in jcs-nav.el file. (jcs-emacs-init)
* Update license and prorject version to 5.1.5. (jcs-emacs-init)

### 2018-09-19

* Add example package files for future package example and
 installation location standard. (jcs-emacs-init)
* Implemented VS like cut key in jcs-vs-func.el file. (jcs-emacs-init)
* Bind the vs like cut key as default cuty key in global mode. (jcs-emacs-init)

### 2018-09-16

* Add search forward/backward colon/semicolon/greater and less than
 sign in jcs-nav.el module. (jcs-emacs-init)

### 2018-09-03

* Rename function check first forward/backward character with in line
 post-fix. (jcs-emacs-init)
* Add check fist forward/backward character to limit to the whole buffer. (jcs-emacs-init)

### 2018-09-01

* VS like function implemented => jcs-vs-func.el file. (jcs-emacs-init)
* Load vs like functions to each related mode. (jcs-emacs-init)
* Remove vs like function key binding as global key, instead we declare
 it inside specific mode that needed to have vs like function key bindings
 in it. (jcs-emacs-init)
* Rename next/previous blank line function with jcs prefix. (jcs-emacs-init)

### 2018-08-29

* Fixed haxe-mode cannot switch frame issue. (jcs-emacs-init)

### 2018-08-27

* Add quote symbol to specify the correct extension to the correct major
 mode. (jcs-emacs-init)

### 2018-08-18

* Start support Haxe file, install major mode 'haxe-mode'. (jcs-emacs-init)
* jcs-haxe-mode for own control of editing Haxe file. (jcs-emacs-init)
* Add haxe_template.txt for Haxe file's header. (jcs-emacs-init)

### 2018-07-28

* Package dependencis changes through melpa package manager updates. (jcs-emacs-init)

### 2018-07-27

* Package dependecies list changes while update packages on melpa. (jcs-emacs-init)

### 2018-07-23

* Rearrange package dependencies package list. (jcs-emacs-init)

### 2018-07-20

* Add 'use-package' package to pre-install package list. (jcs-emacs-init)
* Update license and prorject version to 5.1.3. (jcs-emacs-init)

### 2018-07-19

* Add json-mode package to package dependency list. (jcs-emacs-init)

### 2018-07-14

* 'wgrep' package added back to install list, know idea why it
 seems like get reject by Emacs. Anyway, is back on Emacs again. (jcs-emacs-init)

### 2018-07-06

* Install new package 'project-abbrev', and remove manually install
 code for this package. (jcs-emacs-init)

### 2018-07-05

* Add jcs-ex-pkg example package, for future self package development. (jcs-emacs-init)
* Remove manually install 'line-reminder' package, install it on
 melpa. The package 'line-reminder' is currently on melpa. (jcs-emacs-init)

### 2018-07-02

* Change package name from 'custom-abbrev' to 'project-abbrev'. (jcs-emacs-init)

### 2018-06-22

* Add double dash comment font lock face for mode it uses '--' to do
 single line comment. (jcs-emacs-init)
* Add java save functionalities/functions work with `organize-imports-java'
 package, when first time save reload local source paths. (jcs-emacs-init)
* Fixed get current point face name for Emcas version 26. (jcs-emacs-init)
* Use util jcs- prefix check current face function instead of same code
 everywhere. (jcs-emacs-init)
* Add `null' and `void' face to modifier face. (jcs-emacs-init)

### 2018-06-20

* Add haskell to support language list -> README.md. (jcs-emacs-init)
* Add haskell mode .el file. (jcs-emacs-init)
* Add Haskell template. (jcs-emacs-init)

### 2018-06-19

* Add math module for future math use. (jcs-emacs-init)
* Remove trans window module to just window module. (jcs-emacs-init)
* Bind message buffer keymap with earse message buffer. (jcs-emacs-init)
* Load math module and remove load trans window module. (jcs-emacs-init)
* Simplify trans window module's code. (jcs-emacs-init)
* Add print timestamps with multiple version function/functionality. (jcs-emacs-init)
* Rebind 're-builder' key and 'Rename current buffer/filename' key. (jcs-emacs-init)
* Remove timestamp version 3 properties and it function. (jcs-emacs-init)
* Update project version to 5.1.1. (jcs-emacs-init)
* Re-arrange readme file to sort support languages by alphabetic order. (jcs-emacs-init)
* Bind save buffer key with set file coding system functionality in
 `sh-mode'. (jcs-emacs-init)

### 2018-06-16

* Add # to all interactive function operative and add new
 key binding toggle enlarge window selected key. (jcs-emacs-init)
* Add few balance window functions and enlarget current selected
 window function. (jcs-emacs-init)
* Add set all local variable function. Pass in as symbol. (jcs-emacs-init)
* Rename duplicate line function with prefex 'jcs-' infront. (jcs-emacs-init)
* Add enlarge current selected window key binding doc. (jcs-emacs-init)
* Add overwride mode rewrapper function functionality. (jcs-emacs-init)
* Force maximize frame after reload Emacs and remove helm function module. (jcs-emacs-init)
* Rename jcs-new-window to jcs-new-frame for better naming and
 understanding. (jcs-emacs-init)
* Separate helm function to individual helm-func file. (jcs-emacs-init)
* Add frame func file/module. (jcs-emacs-init)
* Make one history => ### 2018-06-16. (jcs-emacs-init)
* Add package-autoremove key binding note to project doc. (jcs-emacs-init)
* Replace 'blank-mode' pacakge to 'whitespace' package, is built-in now. (jcs-emacs-init)
* Add certain more keyword to highlight for programming usage, check on
 `jcs-env.el' file. (jcs-emacs-init)
* Add jcs-compile function rewrapper functionality. (jcs-emacs-init)
* Remove 'blank-mode' from pre-install package list. (jcs-emacs-init)
* Update project version to 5.1.0. (jcs-emacs-init)
* Fixed normal web comment highlighting. (jcs-emacs-init)
* Better way of checking if beginning of line using 'current-column'
 function. (jcs-emacs-init)

### 2018-06-14

* Add toggle read-only mode key binding and make note to emacs
 key bindings doc. (jcs-emacs-init)

### 2018-06-13

* Revert back to error handling with custom-abbrev expansion key. (jcs-emacs-init)

### 2018-06-11

* Add is default face functionality to utility module. (jcs-emacs-init)
* Update emacs version record to Emacs 26.1. (jcs-emacs-init)
* Update project compatible with Emacs version 26.1. (jcs-emacs-init)
* Make one history for Emacs version 26.1 => ### 2018-06-11. (jcs-emacs-init)
* Update project version to 5.0.5. (jcs-emacs-init)

### 2018-06-10

* Fixed error handle still going url after custom expansion with key
 bindings 'ctrl + return'. (jcs-emacs-init)
* Rename function from duplicate-line to jcs-duplicate-line for
 consistency. (jcs-emacs-init)

### 2018-06-09

* Manually upgrade pacakge 'use-ttf' to version 20180609. (jcs-emacs-init)

### 2018-06-05
* Remove casey text mode hook. (jcs-emacs-init)
* Error handling decision on not finding the version control root
 directory. (jcs-emacs-init)
* Add find file in project and current directory, also a design decision.
 Plus add the `jcs-find-file-in-project-and-current-dir' and
 `jcs-select-find-file-current-dir' functions. Fixed the bug
 for `jcs-select-find-file-in-project' function. (jcs-emacs-init)


### 2018-06-04

* Create 'jcs-dev.el' file for development related functions file put
 here. (jcs-emacs-init)
* Remake open-todo, open-update-log, makescript/runscript without asking. (jcs-emacs-init)

### 2018-06-03

* Upgrade package 'line-reminder' package manually => 20180603. (jcs-emacs-init)
* Manually install package 'custom-abbrev'. (jcs-emacs-init)
* Implement `jcs-ctrl-return-key' functionality for JayCeS default
 control return key. It uses priority function list to handle each
 requirement. (jcs-emacs-init)
* Update keybindings doc describe ctrl-return key. (jcs-emacs-init)

### 2018-06-02

* Remove manually isntall 'com-css-sort' package, use melpa package
 manager instead. (jcs-emacs-init)
* Diminish line-reminder pacakge. (jcs-emacs-init)
* Update project version to 5.0.1. (jcs-emacs-init)

### 2018-06-01

* Upgrade package 'line-reminder' package manually => 20180601. (jcs-emacs-init)

### 2018-05-31

* Upgrade package 'line-reminder' package manually => 20180531. (jcs-emacs-init)

### 2018-05-29

* Manually install 'line-reminder' package => 20180529. (jcs-emacs-init)
* Remove none needed autoload prefix function from jcs-util file. (jcs-emacs-init)
* Set global line reminder mode enable as default. (jcs-emacs-init)

### 2018-05-28

* Add redo key to 'org-mode'. (jcs-emacs-init)
* Wrong according key bindings, place it by categories. (jcs-emacs-init)
* Add triple char style comment prefix check functionalities. (jcs-emacs-init)
* Fixed Lua comment active docstring error. (jcs-emacs-init)
* Fixed Visual CSharp comment active docstring error. (jcs-emacs-init)
* Add prefix message and value delimiter arguments for `jcs-log-list'
 function. (jcs-emacs-init)

### 2018-05-26

* Manually update 'use-ttf' package to 20180526. (jcs-emacs-init)
* Package 'organize-imports-java' is on melpa, no longer need to
 manually install the package. (jcs-emacs-init)

### 2018-05-25

* Update 'use-ttf' package manually to 20180525. (jcs-emacs-init)
* Trasnfer data from `.emacs.d' to `.emacs.jcs'. (jcs-emacs-init)
* Make one history => ### 2018-05-25. (jcs-emacs-init)
* Future history for first version of .emacs.jcs directory tree
 view/template. (jcs-emacs-init)
* Update project version to 5.0.0, huge data transfer/rename from
 '.emacs.d' folder to '.emacs.jcs' folder. (jcs-emacs-init)

### 2018-05-23

* Update 'use-ttf' package manually to 20180523. (jcs-emacs-init)

### 2018-05-22

* Add first version of fonts. (jcs-emacs-init)
* Make shown prefex for better function readability for certain function.
 Like `jcs-jump-to-shown-buffer' instead just `jcs-jump-to-buffer.' (jcs-emacs-init)
* New manage file, jcs-font.el file. (jcs-emacs-init)
* Split `jcs-font.el' module to individual package => `use-ttf'. (jcs-emacs-init)
* Manually install package => `use-ttf'. (jcs-emacs-init)

### 2018-05-20

* Use cl-lib instead of my own ugly method implementation. (jcs-emacs-init)
* Jump to *Messages* window after do the logging/message. (jcs-emacs-init)
* Use undo tree with the better performance without opening/closing the
 undo-tree-visualizer mode all the time. (jcs-emacs-init)
* Add jump to buffer functionality (jcs-emacs-init)
* Goto *Messages* buffer and end of buffer when do jcs type of logging
 functions. (jcs-emacs-init)
* Fixed Visual Studio's C# type of commenting method. Weird action when
 having two slashes, does not detect the Visual Studio's type of prefix
 comment symbol pretty well. (jcs-emacs-init)
* Add `all-the-icons' package to preinstall package list. (jcs-emacs-init)
* Close *undo-tree* buffer when save. (jcs-emacs-init)

### 2018-05-19

* Remove sorting CSS attributes before save, I think is never been useful. (jcs-emacs-init)
* Manually update packages 'com-css-sort' and 'organize-imports-java'. (jcs-emacs-init)
* Add shell key up and key down functionalities. (jcs-emacs-init)
* Remap shell-mode key bindings for trying simulate the real shell action. (jcs-emacs-init)
* Add shell command completion functionality/function and map it in the
 shell-mode. (jcs-emacs-init)
* Fixed css-mode comment font face does not work with dash. (jcs-emacs-init)

### 2018-05-18

* Bind upgrade package to package mode. (jcs-emacs-init)
* Fixed depends mode not active before we enter at least once of
 the minibuffer. (jcs-emacs-init)
* Add completion key binding to shell-mode. (jcs-emacs-init)

### 2018-05-17

* Add deletetion series of functionalities and key bindgins. (jcs-emacs-init)
* Add start/last line of buffer boolean check functionalities for future
 utility use. (jcs-emacs-init)
* Improve reload emacs functionality. (jcs-emacs-init)
* Better default with shell mode control, more like the normal/general/
 common terminal prompt. (jcs-emacs-init)

### 2018-05-16

* Add undo tree keymap comment or uncomment overwirte. (jcs-emacs-init)
* Add save excursion for revert all buffer. (jcs-emacs-init)

### 2018-05-15

* Add toggle undo tree related functions. (jcs-emacs-init)
* Add package upgrade all function. (jcs-emacs-init)
* Make one history => ### 2018-05-15, upgrade all package to the newest version. (jcs-emacs-init)

### 2018-05-14

* Add shell backspace functionality. (jcs-emacs-init)
* Add undo/redo key bindings with `undo-tree'. (jcs-emacs-init)
* Add `jcs-shell-mode.el' file for shell mode managing. (jcs-emacs-init)

### 2018-05-12

* Complete all naming convention to `jcs'. (jcs-emacs-init)
* Make one history => ### 2018-05-11. (jcs-emacs-init)
* Make comment methods exactly the same as Visual Studio IDE in csharp
 mode. (jcs-emacs-init)
* Add header smart indent down and up for csharp mode in `jcs-cs-func.el'
 file. (jcs-emacs-init)
* Split file into `jcs-hook.el' file from `jcs-after-init.el' file. (jcs-emacs-init)
* Update project version to 4.8.1. (jcs-emacs-init)

### 2018-05-10

* Fixed csharp distinguish between docstring and normal comment line. (jcs-emacs-init)

### 2018-05-09

* Set default f7 key bindings to find file other window. (jcs-emacs-init)
* Organize corresponding functions in c/c++ mode to function file. (jcs-emacs-init)
* Manually install update `organize-imports-java' package. (jcs-emacs-init)

### 2018-05-08

* Add first character forward/backward check functionalaities. (jcs-emacs-init)
* Use first character forward/backward check instead of string-match to
 the end of line and beginning of line. (jcs-emacs-init)
* Fixed shell not prompt the first time issue with error handling. (jcs-emacs-init)

### 2018-05-07

* Add remove trailing whitespace current line functionality. (jcs-emacs-init)
* Change all files header to classic JayCeS header. (jcs-emacs-init)
* Set default major mode to org-mode. (jcs-emacs-init)
* Add comment regexp for web-mode, compatible with ASP.NET project. (jcs-emacs-init)

### 2018-05-05

* Add check current char string match functionality. (jcs-emacs-init)
* Auto truncate lines in web mode, add `default' face check twice. Make
 default check on the first character. (jcs-emacs-init)
* Make kill whole line function goto the same column as current line. (jcs-emacs-init)
* Add web-mode rewrapper function for future handy use. (jcs-emacs-init)

### 2018-05-04

* Check end of line trigger enable/disable auto truncate lines effect. (jcs-emacs-init)
* Remove unuse readme file -> README.txt. (jcs-emacs-init)
* Update project version to 4.7.4. (jcs-emacs-init)

### 2018-05-03

* Implement auto truncate lines functionalties. (jcs-emacs-init)
* Enable auto truncate lines functions in web mode as default
 functionalities. (jcs-emacs-init)
* Add get current line number functionalities. (jcs-emacs-init)

### 2018-05-01

* Update `organize-imports-java' package manually. (jcs-emacs-init)
* Install com-css-sort package manually.　(jcs-emacs-init)

### 2018-04-30

* Add error handling for `helm-do-ag-this-file' command. No need to
 switch to cross mode manually anymore. It will just switch to cross
 mode automatically. The implementation can be found in
 `jcs-helm-do-ag-this-file' function. (jcs-emacs-init)
* Fixed some autoload/interactive functions. (jcs-emacs-init)
* Fixed failed search on move to next/previous blank line functions. (jcs-emacs-init)
*  Fixed CSS type face highlighting. (jcs-emacs-init)

### 2018-04-29

* Change indentation and change mode to from if statement to cond
 statement. (jcs-emacs-init)
* Add toggle web mode offsetless elements. (jcs-emacs-init)
* Set web mode offsetless element variable. (jcs-emacs-init)
* Key bindings comment detail update. (jcs-emacs-init)
* Change `html' template's head and body tag to the same level as html
 tag. (jcs-emacs-init)
* Add default ASP .NET's extension to web-mode. (jcs-emacs-init)
* Add manually install section in .emacs file. (jcs-emacs-init)
* History removal, keep one history is enough. (jcs-emacs-init)
* Revert history files. (jcs-emacs-init)

### 2018-04-26

* Install organize-imports-java' package manully. (jcs-emacs-init)
* Add plugin organize-imports-java' solve Java imports functionaltity. (jcs-emacs-init)

### 2018-04-22

* Temporary install `organize-imports-java' package manully, ready for
 melpa to publish it. (jcs-emacs-init)

### 2018-04-18

* Add some functionalities for modefiying list object. (jcs-emacs-init)
* Reformat some parameters in order to match flycheck standard. (jcs-emacs-init)

### 2018-04-17

* Check default directory for java packaging path including
 functionalities. (jcs-emacs-init)
* Fixed comment file path match flycheck standard. (jcs-emacs-init)

### 2018-04-16

* Add search version control directory and it related functionalities. (jcs-emacs-init)

### 2018-04-07

* Fixed c++ mode namespace font lock face and compatible with c mode. (jcs-emacs-init)
* Add java package declaration functionality. (jcs-emacs-init)
* Add empty line between two lines of code functionalities. (jcs-emacs-init)

### 2018-04-04

* Add `which-key' package to pre-install list. (jcs-emacs-init)
* Add `which-key' config. (jcs-emacs-init)
* Update project version to 4.7.1. (jcs-emacs-init)

### 2018-03-24

* Add doc string properties file. (jcs-emacs-init)
* Apply hot reloading to doc string properties customization. (jcs-emacs-init)

### 2018-03-18

* Add processing mode config, starting support processing. (jcs-emacs-init)
* Add processing template file. (jcs-emacs-init)

### 2018-03-05

* Rename save buffer to proper naming with tabify and untabify. (jcs-emacs-init)
* Add save tabify to global keymap. (jcs-emacs-init)

### 2018-02-24

* Add spaces and tabs regexp for editing preprocessor type programming
 langauges. (jcs-emacs-init)
* Add tabs to all spaces regexp. (jcs-emacs-init)
* Add change font functionalities and section. (jcs-emacs-init)
* Add change font key binding and update key bindings note/doc. (jcs-emacs-init)

### 2018-02-15

* Add BASIC template. (jcs-emacs-init)
* Add BASIC mode. (jcs-emacs-init)
* Fixed :graph: regexp to limited version -> a-zA-Z0-9. (jcs-emacs-init)

### 2018-02-12

* Old manage file info function call fixed to current version. (jcs-emacs-init)

### 2018-02-10

* Make compatbile with c regexp missing fixed. (jcs-emacs-init)
* Cancel using c++ for .c file, back to c-mode. (jcs-emacs-init)

### 2018-02-05

* Clean check all templates and remove template function's keywrods. (jcs-emacs-init)

### 2018-02-04

* Make all modes compatible with template hot reloading system. (jcs-emacs-init)
* Add `jcs-perl-mode.el' file for Perl script editing. (jcs-emacs-init)
* Add after init hook for preload template for half hot reloading. (jcs-emacs-init)
* Add first version of all supported language. (jcs-emacs-init)
* Add missing language to language support list. (jcs-emacs-init)

### 2018-02-03

* Add scala mode for scale file editing. (jcs-emacs-init)
* Remove coding template insertion, add template 3rd hot reloading
 system. (jcs-emacs-init)
* Add prompt instead of text questioning in minibuffer for template
 questioning function. (jcs-emacs-init)
* First add templates file for general coding languages. (jcs-emacs-init)
* Add ini file for keyword variables in template. (jcs-emacs-init)
* Add make file tempate for python and java, identify those in cc
 too. (jcs-emacs-init)
* Update version to 4.6.0. (jcs-emacs-init)

### 2018-02-02

* Add org table navigation key bindgs to org mode. (jcs-emacs-init)
* Add org table navigation functionalities. (jcs-emacs-init)
* Use blank and graph regexp instead of guessing all needed
 characters for comment highlighting in org-mode. (jcs-emacs-init)
* Add `bochsrc' as default extension in org-mode, meaning use
 org-mode when editing bochsrc file. (jcs-emacs-init)

### 2018-02-01

* Add `jcs-tab-key' function for global tab key command and key
 bindings. (jcs-emacs-init)
* Insert spaces by tab width function implemented and located in
 util module. (jcs-emacs-init)

### 2018-01-31

* Add jcs-log-list functionaly for debug usage. (jcs-emacs-init)
* Fixed c/c++ mode define oop doc string array list reverse order
 issue with built-in lisp's `revers' function. (jcs-emacs-init)
* Fixed comment closing c style comment block and organize elisp
 for better usage. (jcs-emacs-init)

### 2018-01-28

* Add `.' for oop value's regexp. (jcs-emacs-init)
* Make special key's regexp compatible without space. (jcs-emacs-init)
* Update project version to 4.5.6. (jcs-emacs-init)

### 2018-01-24

* Make align function repeat as default. (jcs-emacs-init)
* Remove special key `' key bindings because we use built-int
 electric pair system instead. (jcs-emacs-init)
* Add txt func file for editing jayces own use text file. (jcs-emacs-init)

### 2018-01-18

* Add delete space infront of line functionalities. (jcs-emacs-init)
* Add smart indent up/down for web-mode. (jcs-emacs-init)
* Add web return key functionality. (jcs-emacs-init)

### 2018-01-16

* Set default org mode no fold. (jcs-emacs-init)
* Add align comment functionalities. (jcs-emacs-init)

### 2018-01-15

* Deactive mark to unselect current region if align region not
 active. (jcs-emacs-init)

### 2018-01-13

* Add ignore-errors for helm find gtags in order to restore previous
 cross/local mode. (jcs-emacs-init)

### 2018-01-08

* Fixed nasm comment key forward and spacing issue. (jcs-emacs-init)
* Fixed nasm return key at the end of buffer can not make new
 line issue. (jcs-emacs-init)
* Align repeat functionality implemented. (jcs-emacs-init)

### 2018-01-07

* Add `!' and `.' to special keyword regexp. (jcs-emacs-init)
* Add special key electric functionaltity. (jcs-emacs-init)
* Add special key key binding to global keymap. (jcs-emacs-init)
  - Here special key mean any word between/inside ` and ' is a
   special keyword.
* Make sure back to original mode (cross/depend) once exit gtags's
 mini-buffer. (jcs-emacs-init)
* Use default nasm comment function before do our own functionalities
 in `jcs-nasm-comment' function. (jcs-emacs-init)
* Fixed python and lua mode regexp problem for doc-string indentifier. (jcs-emacs-init)

### 2018-01-06

* Make oop doc string support multiple line coding style. (jcs-emacs-init)
* Add current word equal functionality. (jcs-emacs-init)
* Add move forward/backward to word functionalities. (jcs-emacs-init)
* Add support python multi-lines coding style. (jcs-emacs-init)
* Refactor env file for better readability, majorly use defacce
 instead of make-face. (jcs-emacs-init)
* Apply option comment doc-string option choice to the function. (jcs-emacs-init)
* Add ` between ' special key word highlighting. (jcs-emacs-init)
* Add highlight restriction to value regexp. (jcs-emacs-init)

### 2018-01-05

* Add nasm return and comment key. (jcs-emacs-init)
* Add doc stirng check before insert docstring. (jcs-emacs-init)
* Add goto first char of line functonalities for reduce duplicate code. (jcs-emacs-init)

### 2018-01-04

* Ignore all tabs and spaces after preproc keywords. (jcs-emacs-init)
* Add logging/debug functionalities. (jcs-emacs-init)
* Add convert tab to space and space to tab functionalities. (jcs-emacs-init)
* Add real backspace and real space functionalities. (jcs-emacs-init)

### 2018-01-03

* Get notice about Ctrl-r key bindings will be rebind everytime,
 fix backspace key bindings issue. (jcs-emacs-init)
* Add default link command to both application/library makefile
 templates. (jcs-emacs-init)
* Fixed wrong spelling for diasm command in makefile template. (jcs-emacs-init)

### 2018-01-02

* Change jcs-delete-forward/backward-current-char-repeat's key
 bindings, to avoid bind key with backspace. (jcs-emacs-init)

### 2018-01-01

* Make sure delete region before delete line for kill whole
 line command. (jcs-emacs-init)
* Complete .asm .s .S support assembly makefile template. (jcs-emacs-init)
* Space check for compatibility py backspace command to tab. (jcs-emacs-init)

### 2017-12-31

* Refactor util with flycheck checker. (jcs-emacs-init)
* Add python like space keybindings to nasm mode. (jcs-emacs-init)
* Modefied key bindings compatible with search, captial navigation
 and kill navigation. (jcs-emacs-init)
* Add nasm func file for nasm mode functionalities. (jcs-emacs-init)

### 2017-12-29

* Refactor jcs-edit file to jcs-buffer-menu file. (jcs-emacs-init)

### 2017-12-28

* Make percise to shell toggle function. (jcs-emacs-init)
* Add find file hook. (jcs-emacs-init)
* Add buffer menu functions. (jcs-emacs-init)
* Make history/backup. (jcs-emacs-init)

### 2017-12-26

* Add isearch at point forward and backward functionalities. (jcs-emacs-init)
* Enable auto complete mode in nasm mode as default. (jcs-emacs-init)
* Change CMake bindings work like Python mode key bindings or
 functionalities. (jcs-emacs-init)
* Add 'functions' for makefile info. (jcs-emacs-init)
* Make compatible with .S file extension. (jcs-emacs-init)
* Add `mount' and `buildimg' commands for makefile template. (jcs-emacs-init)

### 2017-12-22

* Add enum doc string for c and c++ mode. (jcs-emacs-init)
* Add linux and macosx build env and header file list. (jcs-emacs-init)

### 2017-12-21

* Add assembly language commands and flags for makefile file
 formant. (jcs-emacs-init)

### 2017-12-19

* Add '@' symbol for normal bash commands. (jcs-emacs-init)

### 2017-12-16

* Add emacs keybindings note with open update log and todo list. (jcs-emacs-init)
* Move move empty line up and down to nav file. (jcs-emacs-init)
* Add open update log and open todo list functionalities. (jcs-emacs-inti))

### 2017-12-14

* Add 'jcs-preproc-fun.el' file. (jcs-emacs-init)
* Make compatible define docstring with preproc face type. (jcs-emacs-init)
* Refactor using when instead of if statement. (jcs-emacs-init)

### 2017-12-13

* Add oop init set face function. (jcs-emacs-init)
* Add local face modefication like visual studio type style. (jcs-emacs-init)
* Customize cmake division comment. (jcs-emacs-init)

### 2017-12-11

* Refactor using 'when' instead of 'if' statement. (jcs-emacs-init)
* Change class comment type in Python mode. (jcs-emacs-init)
* Add clean a lib and so lib command to 'realclean' command. (jcs-emacs-init)
* Place casey's global key bindings to jcs global key binding file. (jcs-emacs-init)
* Add c-flags to build dynamic library command.

### 2017-12-10

* Add package filter. (jcs-emacs-init)
* Add 'all' option for package filter. (jcs-emacs-init)

### 2017-12-08

* Add web-mode and php-mode doc string. (jcs-emacs-init)

### 2017-12-07

* Makefile format work with static lib and dynamic lib. (jcs-emacs-init)
* End with keyword with non nil keyword at third param,
 fix font lock not working issue. (jcs-emacs-init)
* Add goto start of the comment and goto end of the comment
 functionalities. (jcs-emacs-init)

### 2017-12-06

* Properly name default static and dynamic lib default build name. (jcs-emacs-init)
* Fixed CMake not applying issue. (jcs-emacs-init)

### 2017-12-05

* Fixed makefile format. static to .a, dynamic to .so, $< to $^
 sign. (jcs-emacs-init)

### 2017-12-04

* Add css highlighting. (jcs-emacs-init)
* Add makefile templates. (jcs-emacs-init)
* Add cmake/mke file ask template functionalities. (jcs-emacs-init)
* load txt mode for later since delete org mode as default mode. (jcs-emacs-init)

### 2017-12-03

* Make after confirm adding header or not, change into corresponding
 c-mode or c++-mode. (jcs-emacs-init)
* Add `togetherly' package. (jcs-emacs-init)
* Add `floobits' package. (jcs-emacs-init)
* Move cs to top in order to override the bug from csharp-mode. (jcs-emacs-init)
* Add list of ext for c and c++ mode. (jcs-emacs-init)

### 2017-12-01

* Change copyright character. (jcs-emacs-init)
* Change regexp for strict type langauge in oop-func file for correct
 variable font face. (jcs-emacs-init)

### 2017-11-30

* Add tag string and bracket sign for oop doc customization. (jcs-emacs-init)
* Make sure curly bracket and bracket can be replace with each
 other in regexp, so it compatible with js doc comment style. (jcs-emacs-init)
* Implement python docstring. (jcs-emacs-init)
* Add variable type face issue. (jcs-emacs-init)
* Be more specific on variable name regexp. (jcs-emacs-init)

### 2017-11-29

* Make comment with auto add oop doc comment style. (jcs-emacs-init)
* Add cs comment doc functionality. (jcs-emacs-init)
* Add comment style doc and java-mode match with own doc
 highlighting. (jcs-emacs-init)
* Force underscore a character. (jcs-emacs-init)

### 2017-11-28

* Make defface instead of make face function in oop-func file. (jcs-emacs-init)
* Fixed oop face regular expression highlighting issues. (jcs-emacs-init)
* Fixed comment line break indentation issue. (jcs-emacs-init)
* Update regular expression in oop/jsdoc for type, value and tag. (jcs-emacs-init)

### 2017-11-27

* Add `visual-regexp' package. (jcs-emacs-init)
* Add jsdoc type highlighting functions. (jcs-emacs-init)

### 2017-11-22

* Enable column highlighting in web-mode. (jcs-emacs-init)
* Fixed web-mode with correct PHP key bindings. (jcs-emacs-init)

### 2017-11-21

* Update key bindings note with 'auto-highlight-symbols' package. (jcs-emacs-init)
* Update global key, so it does not conflict with 'auto-highlight-
 symbols' package. (jcs-emacs-init)
* Add jcs-oop-func file for Object Oriented Programming languages
 highlighting. (jcs-emacs-init)
* Move diminish to after load setting file. (jcs-emacs-init)

### 2017-11-20

* split c and c++ mode into two file and leave cc with c and c++
 common. (jcs-emacs-init)
* Make backup -> ### 2017-11-20. (jcs-emacs-init)

### 2017-11-19

* Change emacs cc-mode comment face. (jcs-emacs-init)
* Add `auto-highlight-symbol' package to diminish list. (jcs-emacs-init)
* Add `flycheck' package to diminish list. (jcs-emacs-init)
* Add `flymake' package to diminish list. (jcs-emacs-init)
* Add `helm' package to diminish list. (jcs-emacs-init)

### 2017-11-18

* First draft readme. (jcs-emacs-init)
* Change nasm key bindings indent up and down. (jcs-emacs-init)

### 2017-11-17

* Add package `package-build' to list. (jcs-emacs-init)
* Add package `package-lint' to list. (jcs-emacs-init)

### 2017-11-15

* Add `auto-highlight-symbol' package. (jcs-emacs-init)
* Customize package `auto-highlight-sybmol'. (jcs-emacs-init)
* Fixed auto-highlight-symbol package face settings for my own
 use. (jcs-emacs-init)

### 2017-11-13

* Update `web-mode' package with prefix indentation bug fixed. (jcs-emacs-init)
 -> #939: https://github.com/fxbois/web-mode/issues/939
* Resolve performance issue while moving the cursor around the
 unicode text by adding '(setq inhibit-compacting-font-caches t)'
 at `jcs-env.el' file. (jcs-emacs-init)
 -> #273: https://github.com/purcell/emacs.d/issues/273

### 2017-11-12

* Add `xwidgete' package. (jcs-emacs-init)
* Add `pdf-tools' package. (jcs-emacs-init)

### 2017-11-10

* Remove some of the web-mode functionalities, seems like the
 original `web-mode' package had improved. (jcs-emacs-init)
* Make backup. (jcs-emacs-init)
* Revert 'web-mode' functionalities because I was wrong with the
 bug fixed from original package. (jcs-emacs-init)

### 2017-11-08
* Remove thought was impatient mode settings. (jcs-emacs-init)

### 2017-11-06

* Update key bindings note and add category Util for handy key
 bindings in emacs. (jcs-emacs-init)
* Add emacs_commands file for record important commands for future
 usage and note. (jcs-emacs-init)
* Add header for emacs lisp mode. (jcs-emacs-init)
* Revaluate the css-sort function with local variable. (jcs-emacs-init)
* Make backup ### 2017-11-06. (jcs-emacs-init)
* Add save-window-excursion with css sort, make sure no moving
 point and scroll. (jcs-emacs-init)

### 2017-11-05

* Make hide shell command proper UX. (jcs-emacs-init)
* Add jump-to-window function to jcs-nav. (jcs-emacs-init)

### 2017-11-04

* Fixed plugin bugs css-sort cursor moves after sort occurs, preferences
 issue make. (jcs-emacs-init)

### 2017-11-03

* Add css-sort package. (jcs-emacs-init)
* Add css sort key bindings to css and scss mode. (jcs-emacs-init)
* Add css save function as default css-mode and scss-mode default
 save buffer functionality. Now save CSS and SCSS file with sort
 attribute before saving it. (jcs-emacs-init)

### 2017-11-02

* Enable auto-complete package for web-mode's minor mode when
 editing PHP file. (jcs-emacs-init)
* Add vimrc-mode package. (jcs-emacs-init)
* Add magit package. (jcs-emacs-init)
* Add global key bindings for magit. (jcs-emacs-init)
* Add move current line up/down functionalities. (jcs-emacs-init)
* Add move current line up/down global key bindings. (jcs-emacs-init)
* Update key bindings note. (jcs-emacs-init)
* Disable yasnippet for while editing PHP file or minor mode
 in web-mode. (jcs-emacs-init)

### 2017-10-31

* Enable auto-complete package for global as default. (jcs-emacs-init)
* Fixed COBOL mode auto-complete not showing issue. (jcs-emacs-init)
* Fixed JavaScript mode auto-complete ac-timer and ac-update bugs by
 disable the ac-js2 minor mode. (jcs-emacs-init)

### 2017-10-30

* COBOL header format fixed data division missing. (jcs-emacs-init)
* Makefile initial format changes. (jcs-emacs-init)

### 2017-10-29

* Change COBOL header format. (jcs-emacs-init)
* Add initialize format for COBOL file. (jcs-emacs-init)

### 2017-10-28

* More detail and completion to cmake file format info. (jcs-emacs-init)

### 2017-10-27

* Add .ac extension to cmake mode for autotool, autoconf, automake
 family. (jcs-emacs-init)

### 2017-10-26

* Add cobol-mode for editing COBOL file. (jcs-emacs-init)
* Add jcs-cbl-mode for my customization. (jcs-emacs-init)
* Add COBOL header in header format info file. (jcs-emacs-init)

### 2017-10-24

* Fixed cs mode switch window previous key bindings. (jcs-emacs-init)

### 2017-10-16

* Make sure comment it well on indentation with scripting language
 in web-mode. (jcs-emacs-init)

### 2017-10-12

* Add jcs-vim-mode only for editing VimScript. (jcs-emacs-init)
* Correct year file format in lua mode and vim mode. (jcs-emacs-init)

### 2017-10-09

* Correct filename info. (jcs-emacs-init)
* php paste will occur web-mode get interrupt, bug fixed by renable
 it again. (jcs-emacs-init)

### 2017-10-05

* Add jcs-go-mode for GO programming language. (jcs-emacs-init)
* Fix and Add jcs-align-region-or-document function for align
 variable cross language. (jcs-emacs-init)
* Choose either single quote or double quote for markup. (jcs-emacs-init)

### 2017-10-01

* Include header format file once bug fixed. (jcs-emacs-init)

### 2017-09-28

* Add check current character a proper character not a sign or
 something else. (jcs-emacs-init)
* Fix Web mode backward delete word and backward delete word
 capital. (jcs-emacs-init)
* Add timestamp version 3 to jcs-util file. (jcs-emacs-init)
* Change init format for SQL file. (jcs-emacs-init)

### 2017-09-27

* Add `jcs-backward-delete-word' and `jcs-backward-delete-word-capital'
 for PHP variable naming convention. (jcs-emacs-init)
* Add current uppercase letter and lowercase letter check in
 'jcs-util' file. (jcs-emacs-init)

### 2017-09-25

* Fix file naming for some file in ex-mode directory. (jcs-emacs-init)
* add `jcs-sql-mode.el' to ex-mode directory, meanwhile my emacs
 confi support editing SQL file. (jcs-emacs-init)

### 2017-09-21

* Add adaptive-wrap package for HTML editing. (jcs-emacs-init)
* Re-enable visual-line-mode when save in web-mode. (jcs-emacs-init)

### 2017-09-12

* Add generated code for jcs-cc-mode specific for constructor
 and destructor. (jcs-emacs-init)

### 2017-08-17

* Add jcs-sass-mode for sass language. (jcs-emacs-init)

### 2017-08-11

* jcs python mode module test failed, fixed bugs to the better
 jcs python mode. (jcs-emacs-init)
* add 'use strict' while create the js file. (jcs-emacs-init)

### 2017-08-08

* Fix move forward capital char bug. (jcs-emacs-init)
* Update `jcs-java-mode'. (jcs-emacs-init)
* Intall package `meghanada'. (jcs-emacs-init)

### 2017-08-07

* Install package `google-translate'. (jcs-emacs-init)
* Install package `google-maps'. (jcs-emacs-init)
* Install package `go-mode'. (jcs-emacs-init)
* Install package `google-this'. (jcs-emacs-init)

### 2017-08-06

* Implements `python-mode' compatible with whitespace, so I do
not necessary use tab for programming python. (jcs-emacs-init)
* Update some of the `jcs-shell-mode-hook' key bindings. (jcs-
 emacs-init)

### 2017-08-05

* Remove package `dashboard', seems like the package no longer
 working. (jcs-emacs-init)
* Change the theme color abit. (jcs-emacs-init)

### 2017-08-04

* Install package `dashboard'. (jcs-emacs-init)
* Intall package `powerline'. (jcs-emacs-init)
* Install package `diminish'. (jcs-emacs-init)

### 2017-08-02

* Make `helm-colors' minibuffer RET insert name M-RET insert
 hex-code. (jcs-emacs-init)

### 2017-07-29

* Fix bug capital search backward/forward and capital delete
 backward. (jcs-emacs-init)

### 2017-05-25

* Add xml mode. (jcs-emacs-init)

### 2017-05-23

* Add Sublimity to emacs file. (jcs-emacs-init)
* Add Minimap and Animate scrolling. (jcs-emacs-init)

### 2017-05-21

* Add increment/decrement transparency frame. (jcs-emacs-init)
* Update Log file build. (jcs-emacs-init)
