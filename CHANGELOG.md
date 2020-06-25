# Change Log

All notable changes to this project will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.


### 2020-06-25

* Installed new package manually `transwin`.
* Fixed `ivy` logic when trying to find files in home directory.
* Updated `feebleline`'s UX when using terminal for window divider.

### 2020-06-24

* Fixed maybe kill buffer logic by not using regex for buffer check.
* Updated buffer check with strict for not using regex check.
* Installed new package `github-browse-file`.
* Added toggle diminish buffer mode key to global map.

### 2020-06-23

* Added valid buffer boolean check utility function.
* Updated buffer menu return logic.
* Updated maybe kill buffer function to switch to valid buffer logic.

### 2020-06-21

* Updated diminish buffer list, `*Backtract*`, `*Compile-Log*`, `*Help*`.
* Cleaned up generic maybe kill this buffer function.
* Fixed dashboard is missing after maybe kill function is called.

### 2020-06-19

* Fixed ivy skip input line logic.

### 2020-06-18

* Fixed `feebleline` string type error from prepare operations.
* Updated `feebleline` for better priority.

### 2020-06-17

* Updated `jsx` default template for React.js.

### 2020-06-14

* Updated `feebleline` format to better standard/format.

### 2020-06-13

* Renamed repo to just `jcs-emacs`.
* Improved certain modes' default template files.
* Fixed csharp-like mixed c-like comment/docstring return logic.

### 2020-06-12

* Fixed goto dashboard command when dashboard buffer already shown issue.

### 2020-06-10

* Added minimum required Emacs version check.

### 2020-06-05

* Installed `csproj-mode` for supporting `.csproj` type of file.
* Added auto save config to env module.

### 2020-06-05

* `json-mode` doesn't defined tab-width, use default instead.

### 2020-05-28

* Fixed window name matching issue in shell module.
* Implemented source control information in `feebleline`.
* Changed `feebleline` using cache for better speed and performance.

### 2020-05-27

* Fixed `cc-mode` docstring with keywords.
* Enabled JSX docstring with `rjsx-mode`.
* Added Unreal C++ template information functions.

### 2020-05-26

* Added Unreal C++ scripting template files for `header`/`source`.
* Implemented ask for which header insertion in `c++-mode`.
* Declared c/c++ header and source extension list.
* Fixed `counsel`/`counsel-projectile` find file other window logic.

### 2020-05-25

* Added `isearch` configuration by showing the match count.

### 2020-05-23

* Removed `preproc` face from `face` module.
* Removed `preproc` function, no longer needed.
* Removed `oop` highlight faces for fixing OOP docstring implementation.

### 2020-05-22

* Removed manually installed package `shift-select`.
* Fixed mark whole buffer action after switching window.
* Fixed `ivy` skipping input selection logic.

### 2020-05-20

* Enabled `auto-rename-tag` for certain web related modes.

### 2020-05-19

* Improved `JSX` coding experience by supply more helper plugins.
* Fixed performance issue when reload active mode.
* Fixed performance issue when enable/disable truncate lines in `web-mode`.

### 2020-05-18

* Added auto install dependencies flag.
* Config now automatically install needed dependencies after first startup.

### 2020-05-17

* Added safe active LSP util function.
* Semi-fixed performance when refresh dashboard.
* Split buffer shown `-count` and `-p` function in window module.
* Fixed `undo-tree` slow response when multiple undo/redo actions.
* Implemented `lsp` connected flag util function.

### 2020-05-16

* Implemented find directories ignore directories function.
* Implemented find files ignore directories function.
* Fixed `make`/`run` script find directories/files time spend too long issue.
* Installed `rjsx-mode` for supporting `jsx` file.
* Added JSX default snippets.
* Added condition to limit `lsp-deffered` is called.

### 2020-05-15

* Minor fixed for `cc-mode` indent block settings.

### 2020-05-13

* Config `counsel`'s `find-file` preselect flag.

### 2020-05-03

* Fixed deep directory tree when using `counsel` find file action.

### 2020-04-22

* Implemented `inc`/`dec` string distance level for `multiple-cursors` similarity.

### 2020-03-29

* Installed new package `dashboard-ls`.

### 2020-03-28

* Removed package `focus`.

### 2020-03-25

* Implemented safe refresh `dashboard` functionality.

### 2020-03-24

* Fixed OOP docstring functionalitiy when no parameters.

### 2020-03-23

* Changed source for package `emoji-github` from `quelpa` to `melpa`.
* Updated `diminsh-buffer` list for more of the LSP buffer.

### 2020-03-22

* Installed new package `lsp-java`.
* Installed new package `lsp-origami`.

### 2020-03-20

* Moved more mode autoload from `jcs-cc-mode` to `jcs-mode` file.

### 2020-03-17

* Added the lsp ui doc delay when safely show lsp ui doc tooltip.

### 2020-03-16

* Installed new package `manage-minor-mode-table`.
* Installed new package `buffer-wrap`.

### 2020-03-13

* Installed new package `masm-mode`.
* Implemented `asm` mode behaviour for all Assembly Language related `major-mode`s.

### 2020-03-12

* Changed source for `company-quickhelp-terminal` package fro `quelpa` to `melpa`.

### 2020-03-11

* Fixed not refresh issue header string from `buffer-menu`.

### 2020-03-09

* Fixed `ivy` overlap logic.
* Changed using minor mode for `company-quickhelp-terminal` package.

### 2020-03-06

* Installed new package `company-emojify`.
* Installed new package `emoji-github` manually.

### 2020-03-05

* Installed new package `emojify`.
* Fixed buffer wrap can't correct goes to corresponding first line isssue.

### 2020-03-03

* Use local variable for `tabulated-list`'s header string.
* Diminish mode `buffer-wrap-mode` for `buffer-wrap` package.

### 2020-03-01

* Switch `ffmpeg-player` from source `quelpa` to `melpa`.

### 2020-02-25

* Removed `dashboard` mute when insert project sections.

### 2020-02-22

* Installed new package manually `buffer-wrap`.
* Removed package manually `tabulated-list-wrap`.

### 2020-02-21

* Installed new package `command-log-mode`.

### 2020-02-20

* Mute dashboard insert project log.
* Fixed load `eww` issue from emacs version `27.0.60`.

### 2020-02-17

* Installed new package `flycheck-grammarly`.

### 2020-02-16

* Implemented `neotree` recording the last window.

### 2020-02-14

* Tweak `neotree` customizable variables' value.
* Changed `output`/`compilation` buffer maybe kill buffer default action to
 change to other output buffer as higher priority.

### 2020-02-13

* Implemented switch to next window in height utility function in window module.
* Removed package `sr-speedbar`.
* Installed new package `neotree`.
* Replace config from `sr-speedbar` to `neotree`.
* Implemented automatically refresh `neotree` using timer.

### 2020-02-11

* Fixed return type with empty string in `oop` module.

### 2020-02-10

* Removed manually installed package `vs-light-theme`.
* Removed manually installed package `vs-dark-theme`.
* Installed new package `vs-light-theme`.
* Installed new package `vs-dark-theme`.
* Implemented docstring for `Go` programming language.
  - Support two way to comment document string.
* Fixed `ivy` find file logic with regular expression.

### 2020-02-07

* Added expression face for `feebleline` lsp.
* Fixed region delete for smart delete word and capital word.
* Indent `yank` in `python-mode` gives weird result, use normal `yank` instead.

### 2020-02-05

* Chnaged the constant keywords' face in `go-mode`.
* Implemented LSP information to feebleline.

### 2020-02-04

* Added own `save-excursion` function.
* Fixed indent error while untabify/tabify buffer in `go-mode`.
* Fixed `company-fuzzy` renable issue with `lsp-mode`.
* Disabled debug on error while LSP server is active.

### 2020-02-03

* Update `go` programming language default template.

### 2020-01-25

* Reimplemented OOP docstring module, mainly for clean up.

### 2020-01-24

* Fixed buffer/file name prompting error while reopening the file.

### 2020-01-23

* Installed new package => `manage-minor-mode`.

### 2020-01-20

* Fixed requiring `cl` using `loop` in util module.

### 2020-01-19

* Ignored `lsp` buffer with weather the dash `-` exists or not.
* Removed `yascroll` issue with Emacs 27.

### 2020-01-14

* Fixed `feebleline` compatbile with `lsp-mode` ignore case issue on `(buffer-name)`
 we use `(buffer-file-name)` beforehand.

### 2020-01-13

* Updated `jQuery` snippet in `html-mode`, not slime use normal minified version.

### 2020-01-10

* Fixed `null` face highlighting regular expression.

### 2020-01-09

* Update upgrade manually installed package logic.
* Installed new package manually => `tabulated-list-wrap`.
* Fixed visualize `undo-tree` on the other window doesn't work with `lsp-ui-doc`.
* Fixed hanging when execute `lsp--execute-command` function from `lsp-ui-sideline`.
* Fixed opening `css` virtual buffer with virtual directory issue with loading `eww`.

### 2020-01-08

* Fixed some of the regular expression faces in `typescript-mode`.
* Implemented describe path info at point function and embedded to describe thing at point.

### 2020-01-07

* Changed python class' template format.
* Added the `keywords` to the template config file.

### 2020-01-06

* Installed new package => `grammarly`.
* Added title source for package missing prompt.
* Removed manually installed package => `ivy-resize`.

### 2020-01-03

* Fixed buffer menu logic fit the `search`/`filtering` when multiple buffer menu buffer exists.
* Fixed buffer menu other window that doesn't goes to line `2` if the header appears.

### 2020-01-02

* Fixed display not ready while `filtering`/`searching` in buffer menu.
* Added wrapping functionality for buffer menu mode.
* Established template default template's naming convention.
* Improved buffer menu `searching`/`filtering` user experience.
* Fixed buffer menu refresh logic after killing.
* Added wrapping to buffer menu buffer.

### 2019-12-31

* Added switch to output buffer utility function for development use.
* Added get buffers utility functions by using `regexp` and `string`.
* Change `define-it` package from manually installed to automatically installed using `melpa`.
* Installed new package => `define-it`.
* Fixed `undo`/`redo` logic and work with `lsp-ui`.

### 2019-12-30

* Added no advice other window flag for other function that doesn't want to configure
 buffer in window while switching window/frame.

### 2019-12-28

* Change default `define-it` to `'view` instead of `'pop`.

### 2019-12-27

* Fixed `lsp-ui` show prompting error while switching windows.
* Implemented `lsp-ui` show doc anytime functionalities.
* Increase standard string distance level from `8` to `20`.
* Enhanced the `multiple-cursors`'s similar functions string comparison algorithm.
 Make improvements for the use of the `multiple-cursors` similar occurrence command.
* Added new make frame for selecting new frame after created.
* Used `hide-frame` instead of `delete-frame` while showing `lsp-ui-doc`, this
 should fixed while re-focus back to Emacs' frame issue while jumping away from Emacs.

### 2019-12-26

* According to [company-lsp/131](https://github.com/tigersoldier/company-lsp/issues/131),
 revert `flx` support with `company-lsp`. Just set `company-lsp-cache-candidates`
 to `auto`.
* Removed package => `dimmer`.
* Installed new package => `lsp-ui`.
* Implemented `record` and `restore` windows status once utility functions.

### 2019-12-25

* Installed new package `ivy-resize` manually.
* Added `define-it` config to change default output choice to `pop`.
* Installed new package `company-lsp`.
* ~~Implemented fuzzy match using `flx` with `company-lsp`.~~

### 2019-12-24

* Applied `*lsp-` related buffer to diminish buffer list.
* Reverted the message wouldn't work the first time issue from Emacs 27.
* Added completed `lsp` configuration.
* Integrated better `lsp` signature buffer to dual window users/configurations.
* Organized to use `other-window` advice/hook instead of function wrapper.

### 2019-12-23

* Added window size change hook.
* Fixed after resized frame ivy window doesn't get resize issue.
* Added `license` template functionalities.
* Added `changelog` template functionalities.
* Added `typescript` template for [Cocos Creator](https://www.cocos.com/en/creator) scripting.

### 2019-12-22

* Made `eldoc` compatible/interact with mouse click.
* Added peek frame util function.
* Fixed `feebleline` works with multiple frames.

### 2019-12-21

* Added multiple cursors previous/next similar functions.
* Added simple make frame util function.

### 2019-12-20

* Fixed `ivy` missing `ffap` dependency.
* Fixed message wouldn't work when `dashboard` is showing inside window.
* Added minibuffer first setup hook.

### 2019-12-19

* Fixed `yascroll` wrong arguments error after Emacs version 27.

### 2019-12-17

* Fixed `ivy` switch to buffer other window logic.

### 2019-12-16

* Match enlarge/shrink windows times to `media` and `shell` windows.

### 2019-12-15

* Implemented new way to complete path using `ivy`.
* Implemented auto resize in `ivy` minibuffer window.
* Reverted `ivy` slash key for other usage; mainly due to it's own functionality is weird to me.
* Fixed `css-mode` weird face highlighting issue.

### 2019-12-11

* Minor fixed for renaming inside minibuffer using `ivy`.

### 2019-12-10

* Installed new package => `company-quickhelp-terminal` manually.
* Switched from `helm` to `ivy` due to `helm`'s instability.

### 2019-12-09

* Removed manually installed package => `reveal-in-folder`.
* Installed new package => `reveal-in-folder`.
* Installed new package => `parse-it`.
* Installed new package => `vs-light-theme` manually.
* Installed new package => `vs-dark-theme` manually.
* Starting from version `5.9.3`, theme work outside of this packages.

### 2019-12-08

* Installed new package `file-header` manually.
* Configure package `file-header`.
* Fixed hex code color check function compatible to terminal.
* Added more commands to eldoc activation trigger list.

### 2019-12-03

* Fixed Visual Studio dark theme color from line numbers.

### 2019-12-02

* Quick fixed for `helm` incompatible `completion-styles`.
* Revert `helm` incompatible `completion-styles` changes.
* Implemented set font util function.

### 2019-12-01

* Minor fixed for iedit command when no kill ring.

###2019-11-29

* Minor fixed for bury buffer function, it active only when `diminsh-buffer-mode` is on.
* Fixed media window logic.

### 2019-11-28

* Fixed toggle shell window's logic.
* Fixed toggle video player window's logic.
* Make feebline compatible to video player.

### 2019-11-26

* Fixed iedit `kill-ring` issue when enable/disable `iedit-mode`.
* Installed new package manually => `ffmpeg-player`.
* Implemented video player feature.

### 2019-11-25

* Fixed isearch not showing with the first two characters.

### 2019-11-15

* Implemented switch `output`/`compilation` buffer keys.
* Make mode-line's color compatible to light theme.

### 2019-11-13

* Changed default key bindings for search in project.

### 2019-11-12

* Fixed `flycheck-pos-tip` will kill describe thing pos-tip functionalities.

### 2019-11-11

* Installed new package => `flycheck-pos-tip`.

### 2019-11-09

* Manually installed new package => `reveal-in-folder`.
* Fixed `multiple-cursors` lazy loading in navigate blank keys.

### 2019-11-08

* Implemented `lsp-mode` to `goto-definition` function.

### 2019-11-07

* Added quelpa upgrade process to standard upgrade process.

### 2019-11-06

* Installed new package => `helm-describe-modes`.

### 2019-11-04

* Fixed `multi-shell` maybe shell buffer logic.

### 2019-11-01

* Defined goto definition functions.
* Installed new package => `elisp-def`.
* Added default save all buffers function.
* Added reverse tab/untab save buffer function.

### 2019-10-31

* Diminish buffer to `shell` and `eshell`.

### 2019-10-30

* Added `multi-shell` config.

### 2019-10-29

* Implemented multiple terminal functionalities.
* Manually installed new package => `multi-shell`.

### 2019-10-27

* Unbind return from mc/keymap.
* Fixed default emmet expand line key, not doing any action by default.

### 2019-10-26

* Implemented simplify safe jump to buffer window function.
* Changed no more jump to unwanted buffer.

### 2019-10-25

* Installed new package => `request`.

### 2019-10-24

* Fixed C-ret not doing anything with default.
* Installed new package => `wiki-summary`.

### 2019-10-22

* Removed useless hl keyword => `OR`.

### 2019-10-19

* Instanlled new package => `visual-regexp`.
* Fixed key definition => `jcs-env.el`.

### 2019-10-18

* Make tab and space compatible to VSCode's behaviour standards.

### 2019-10-17

* Minor fixed with walk windows with multiple frames.

### 2019-10-15

* Start supported language => `LESS`.
* Start supported language => `Kotlin`.
* Start supported language => `Dockerfile`.

### 2019-10-14

* Set `smart-indent` as one option to move previous/next line.

### 2019-10-10

* Minor bug fixed - first window line pos inaccurate.
* Minor bug fixed - avoid enable/disable line number mode if not needed because is quite expensive.

### 2019-10-09

* Bind backward/forward word capital to higher priority keys.
* Completely mimic VSCode `multiple-cursors` behaviour.

### 2019-10-08

* Fixed `actionscript-mode`'s mutliline comment line up issue.
* Use `web-mode` instead of `vue-mode` for editing `.vue` file.
* Supported multiple compilation process.

### 2019-10-07

* Enabled more autoload for `origami`.

### 2019-10-06

* Installed new package => `vue-mode`.

### 2019-10-05

* Minor fixed with commenting with openting `/*`.
* Fixed `python-mode` double quote key logic.

### 2019-10-04

* Fixed remove end lines issue.

### 2019-10-03

* Supported R programming language.

### 2019-10-02

* Minor fixed for shell completion.
* Minor fixed for shell behaviour.

### 2019-10-01

* Moved preferred settings to `prog-mode`.
* Implemented more `lsp-mode` to default `prog-mode`.

### 2019-09-30

* Removed refresh font in `post-command-hook` functionality.
* Redefined color to => oop `tag`, `type`, `value` face.

### 2019-09-28

* Fixed `highlight-indent-guides` execute `guide-region` multiple times when
using `jit-lock-register` function.

### 2019-09-27

* Fixed `typescript-mode`'s highlighting.

### 2019-09-25

* Fixed empty param issue list.

### 2019-09-24

* Implemented ask line endings to set coding system interactive util function.
* Added managed full test case for CI.
* Fixed log multiple times issue.

### 2019-09-22

* Installed new package => `diminish-buffer`.

### 2019-09-21

* Installed new package => `markdown-toc`.
* Installed new package => `browse-kill-ring`.

### 2019-09-20

* Eliminate return with `void` type for typescript docstring.
* Fixed goto address not copy issue.
* Fixed mute apply with current message.

### 2019-09-19

* Fixed buffer removal when reverting empty temporary file.

### 2019-09-18

* Implemented `auto-highlight-symbol` with light/dark theme consideration.
* Redesign comment faces with light/dark theme consideration.

### 2019-09-17

* Installed new package => `org-bullets`.

### 2019-09-16

* Fixed smart backspace/delete word key behaviour.
* Installed new package => `quelpa`.

### 2019-09-13

* Fixed refresh buffer menu bug when switch buffer.

### 2019-09-11

* Fixed shell mode key bindings.

### 2019-09-10

* Implemented `buffer-menu` filtering with `flx`.
* Implemented `mute-apply` util function.

### 2019-09-09

* Removed package => `beacon`.
* Improved `feebleline` read-only config.

### 2019-09-05

* Added read-only symbol to `feebleline` design.

### 2019-09-04

* Fixed helm scrolling with window line height.
* Fixed keys `C-c` and `C-x`.

### 2019-09-03

* Removed package `helm-flx`.
* Installed new package => `helm-fuzzy`.

### 2019-08-29

* Fixed some missing dependencies in some lazy loading functions.

### 2019-08-28

* Fixed helm weird scrolling on the last selection issue.
* Rearrange key specify by mode.

### 2019-08-26

* Clean up unused code from `jcs-buffer-menu.el` file.

### 2019-08-25

* Update buffer menu list when navigating through windows.

### 2019-08-24

* Installed new package => `helm-flx`.

### 2019-08-23

* Implemented horizontal center util function.
* Installed new package => `dap-mode`.
* Removed package => `sublimity`.
* Stopped support feature `smooth scroll`.
* Stopped support feature `minimap`.

### 2019-08-22

* Installed new package => `lsp-mode`.
* Diminish `emmet-mode`.

### 2019-08-19

* Diminish `company-fuzzy-mode`.

### 2019-08-17

* Complete key bindings document.
* Installed new package => `flx`.
* Installed new package => `company-fuzzy`.

### 2019-08-14

* Minor tweak for `company` configuration for selection highlighting.

### 2019-08-13

* Bind balance split window key as default split window behaviour.

### 2019-08-12

* Minor tweak for `company` configuration.
* Make tab key compatible with `company`.

### 2019-08-08

* Implemented ask csharp template functionalities to `csharp-mode`.

### 2019-08-07

* Removed package => `company-statistics`.
* Minor changes for `company` package.

### 2019-08-05

* Config eldoc trigger commands.

### 2019-08-01

* Show tooltip even with one valid candidate in `company-mode`.
* Fixed display not ready issue on buffer menu.

### 2019-07-30

* Fixed minor documentation issue.

### 2019-07-29

* Supply `ruby-mode` and `rust-mode` indentation level's config.

### 2019-07-26

* Implemented ability to record down the tab width across all major mode.

### 2019-07-25

* Make tab width record to the next buffer with the same mode.

### 2019-07-24

* Implemented non-verbose beginning/end of buffer functions.
* Bind non-verbose beginning/end of buffer key functions.

### 2019-07-23

* Implemented increment/decrement tab size functions.
* Customize `feebleline` with system spaces or tabs displayed.
* Customize `feebleline` with tab size displayed.

### 2019-07-22

* Complete more preprocessor highlighting.

### 2019-07-18

* Enabled `so-long-mode` as default.

### 2019-07-16

* Implemented `buffer menu`'s return key.

### 2019-07-15

* Implemented realtime updating buffer menu.
* Implemented filter functionality to buffer menu.

### 2019-07-14

* Customize `snippet-mode` by adding `jcs-snippet-mode.el` file.
* Added snippet for `snippet-mode`.
* Fixed shell toggle logic, it no longer depends on the function state and now compatible to mutliple frames.

### 2019-07-13

* Removed package => `indent-info`.
* Implemented remove carriage return symbol function.
* Fixed deletetion logic with tab width.

### 2019-07-12

* Prevent loggin when refreshing dashboard, too verbose loggnig.

### 2019-07-11

* Update `feebleline` customization.

### 2019-07-10

* Installed new package => `feebleline`.
* Customize `feebleline` for default mode-line toggle.
* Implemented electric delete key.

### 2019-07-09

* Implemented vs sharp key and bind to these following modes.
  => c-mode
  => c++-mode
  => csharp-mode

### 2019-07-08

* Reverted package => `line-reminder`.
* Removed manually installed package => `line-indicators`.
* Set use linum when inside terminal for `line-reminder` package.

### 2019-07-07

* Manually installed new package => `line-indicators`.
* Removed package => `line-reminder`.

### 2019-07-05

* Installed new package => `centaur-tabs`.
* Installed new package => `company-statistics`.
* Removed package => `tabbar`.

### 2019-07-04

* Fixed certain modes that does not apply `highlight-indent-guides` minor mode.
* Rename backward/forward capitcal word keys, much better naming.
* Fixed certain modes require error.

### 2019-07-03

* Bind `package-list-packages` to `C-x C-p` instead of `C-p`.
* Fixed iedit-mode logic.
* Removed inconsistent key bindings for `c-mode` and `c++-mode`.
* Added unity snippets => `csharp-mode`.

### 2019-07-02

* Install new package => `highlight-indent-guides`.
* Fixed `oop-func`'s built in docstring autoload.

### 2019-07-01

* Installed new package => `alt-codes`.
* Implemented scratch other window function.

### 2019-06-30

* Installed new package => `helm-file-preview`.

### 2019-06-29

* Fixed multiple with-eval-after-load function.
* Implemented maybe kill `*scratch*` buffer function.

### 2019-06-28

* Require `undo-tree` when needed.
* Try using first time post command startup.
* Implemented multiple with-eval-after-load function.

### 2019-06-25

* Implemeneted cheat sheet functions.
* Use regexp to ignore line numbers mode.
* Installed new mode => `gdscript-mode` for editing Godot Script file.
* Added `gdscript-mode` snippets using `yasnippets`.
* Had `helm-ag` requires pattern to `2`.

### 2019-06-24

* Removed startup mode files, and moved their config to `jcs-mode.el` file.
   - `jcs-elisp-mode.el`
   - `jcs-lisp-mode.el`
   - `jcs-text-mode.el`
* Implemented insert header if buffer empty function for inserting file/mode header.
* Implemented html preview function.

### 2019-06-23

* Clean up code for better load speed.
* Fixed `helm-file-files` inserting `/` logic.
* Removed switch window by `M-0` to `M-9` keys.
* Implemented the display ascii-table function.

### 2019-06-22

* Implemented remove item from `*dashboard*` buffer.
* Completed CI test.
* Update the `oop-func` logic, better and does not requires font lock implementation. Now it uses `search-string` instead.

### 2019-06-21

* Upate text banner.

### 2019-06-20

* Fixed dashboard next/prev blank line logic.
* Added text banner file => `./.emacs.jcs/banner/sink.txt`.
* Implemented autoloads functionalities to manually installed packages.

### 2019-06-19

* Implemented better dashboard buffer controlling util functions.
* Clean up customizes code section to => `~/.emacs.d/.jcs-custom.el` file.

### 2019-06-18

* Removed package => `helm-gtags`.
* Installed new packae => `dumb-jump`.
* Use `dumb-jump` replacing `helm-gtags` functionalities.

### 2019-06-16

* Fixed focus in, refresh dashboard buffer hanging issue.
* Clean up log code.

### 2019-06-15

* Use default helm display path option from `relative` to `root`.
* Installed new pacakge => `region-occurrences-highlighter`.

### 2019-06-14

* Renamed `jcs-corresponding-file.el` to just `jcs-file.el`.
* Renamed `jcs-file-info-format.el` to `jcs-template.el`.
* Use find file in project instead of just find file for searching corresponding file functionalities.

### 2019-06-13

* Update `*dashboard*` buffer when access recent projects list.

### 2019-06-12

* Removed some of useless `require`s.
* Removed some of useless plugin's config.
* Ready the configuration for Emacs version 27.

### 2019-06-10

* Disable `multiple-cursors` when navgiating blank line.
* Installed new pacakge => `yascroll`.
* Added customize `yascroll` face by theme color function.

### 2019-06-07

* Fixed `helm-projectile` return key not exiting minibuffer issue.
* Re-implements `helm-files` related functions. For find files other windows.

### 2019-06-06

* Clean up some compile warningins.

### 2019-06-05

* Optimized configuration down to startup time around from `2` to `6` seconds.

### 2019-06-03

* Optimized configuration down to startup time around from `4` to `8` seconds.
* Added more `helm` find files keymap to match OS's file explorer's navigation system.
* Added `jcs-emacs-version`.

### 2019-06-02

* Installed new pacakge => `esup`.

### 2019-06-01

* Clean package initialization using `require` keyword.

### 2019-05-31

* Fixed `50%` of config compile issues.

### 2019-05-30

* Enable compile version of this config.

### 2019-05-29

* Fixed `helm` theme inconsistent to the `vs-light` theme.

### 2019-05-27

* Fixed `right-click-context` package's bug #2 and #7 issues.
* Removed package `pdf-tools`.
* Implemented automatically enable `read-only-mode` when view source or library files.

### 2019-05-25

* Removed package `floobits`.
* Added ignore activating line numbers by major mode list.

### 2019-05-24

* Updated line numbers ignore buffer list.

### 2019-05-22

* Remove before/after init files.
* Optimized more plugins to `jcs-plugin.el` file.

### 2019-05-21

* Fixed `compilation-mode-hook` from `jcs-env.el` file.

### 2019-05-20

* Reduced duplicated code in `jcs-comment.el` file.

### 2019-05-19

* Removed manually installed package => `show-eol`.
* Installed new pacakge => `show-eol`.
* Make `comment` and `uncomment` related functions compatbile to `line-reminder` package.
* Added is behind last char at line util function.
* Added `point` option to infront first char at line util function.

### 2019-05-17

* Added `*Package-Lint*` to line numbers not displayed list.
* Manually updated `show-eol` package manually => `20190517.001`.

### 2019-05-14

* Start supports `dart` by using `dart-mode`.
* Start supports `pascal` by using `pascal-mode`.
* Start supports `Object Pascal`/`Delphi` by using `opascal-mode`.
* Added `dart-mode`'s snippets.
* Added `pascal-mode`'s snippets.
* Added `opascal-mode`'s snippets.

### 2019-05-13

* Manually updated `show-eol` package manually => `20190513.002`.
* Manually updated `show-eol` package manually => `20190513.001`.

### 2019-05-10

* Implements self defined comment or string util function.

### 2019-05-08

* Remove `jcs-top-level-active` global `defvar` for keyboard quit check.
* Remove `jcs-minibuffer-active` global `defvar` for minibuffer active check.

### 2019-05-06

* Fixed `hl-todo-mode` not working in `web-mode` by redefine highlighting condition => `jcs-plugin.el` file.

### 2019-05-05

* Implements calc eval region function for calculating the region and replace it with the calculated result.
* Implements backward/forward symbol functions for interactive use.

### 2019-05-04

* Revert `haxe-mode` so it works for now, but still leave with no maintainer with this mode.

### 2019-05-03

* Implements `get window` and `get window id` util functions.
* Fixed reset dashboard banner not refresh issue.
* Installed new pacakge => `hl-todo`.

### 2019-05-02

* Implements check if light or dark color util functions.
* Added default light theme.

### 2019-05-01

* Manually installed package `show-eol`.
* Make `text-mode` to the top for ready to override by other mode.

### 2019-04-29

* Organized configuration's directory structure.
* Remove `jcs-font.el` file and put the `font` config to the `jcs-env.el` and `jcs-plugin.el` files.

### 2019-04-28

* Update dependency list.

### 2019-04-27

* Added deactive all line numbers modes util function.
* Fixed toggle mode line key binding.
* Fixed active line numbers by mode logic, we use to deactive the line numbers mode for now instead of just ignore it.
* Fixed modes not activated after revert issue.

### 2019-04-24

* Implements `toggle-mode-line`.
* Cleanup `web-mode`'s util functions.
* Unbind `web-mode` util functions from `jcs-web-func.el` file.

### 2019-04-23

* Minor fixed with some typo.

### 2019-04-22

* Installed new pacakge => `goto-char-preview`.
* Added new snippet for `react.js` in html.
* Added new snippet for `bootstrap` in html.
* Added new snippet for `three.js` in html.
* If region active, when `isearch` is activated we use region instead.
* Fixed `css-mode` return key.
* Fixed css number not highlighting correctly.

### 2019-04-21

* Installed new pacakge => `isearch-project`.
* Bind `isearch-project-forward` to implement `cross-mode` search through project ability.
* Implements helm projectile find file other window function.

### 2019-04-20

* Split electric pair pairs to each specific mode.

### 2019-04-19

* Remove `shift-select` package, the package is still remained unstable.
* Sort keys in alphabetic order category.
* Revert `shift-select` package => version `20190419.001`.
* Implements is symbol contain in list of symbol util function.

### 2019-04-18

* Added more key bindings for switching windows.
* Added remove trailing lines at the end of buffer util function.
* Implements self design mark whole buffer.
* Remove README, LICENSE, bochsrc files default to `org-mode`.
* Added `html-mode` and `js-mode` snippets.

### 2019-04-17

* Manually installed `shift-select` package.

### 2019-04-16

* Added more `:defer` to more packages.
* Removed many unused packages.
* Fixed `jcs-flycheck-mode` logic.
* Remove smart shift select home/end functions.
* Complete set of manual install package section.

### 2019-04-15

* Implements selecting windows by using windows' index.
* Removed `elpy` package.
* Removed `find-file-in-project` package.
* Removed `ivy` package.
* Installed `projectile` package.
* Removed `js2-refactor` package.
* Implements `multiple-cursors` quick hand functions.
* Fixed vs curly bracket logic.
* Start supports `elixir` by using `elixir-mode`.
* Start supports `erlang` by using `erlang-mode`.
* Installed `helm-projectile` package.

### 2019-04-14

* Installed new pacakge => `buffer-move`.
* Fixed `same file other window` bug.

### 2019-04-13

* Fixed `undo-tree` occurs error when trying to kill its parent buffer.
* Starts featuers documentation under `./features/` folder.
* Split `.ini` and `.properties` mode.
* Added `jcs-properties-mode.el` for supporting java properties file.

### 2019-04-12

* Installed new pacakge => `dashboard`.
* Installed new pacakge => `beacon`.
* Minor fixed from version `5.3.2`.

### 2019-04-11

* Added `gitconfig` configurations.
* Use `with-eval-after-load` macro to speed up startup time.
* Huge update on the startup time, now the average startup time is lower than `10` seconds.

### 2019-04-10

* Customize `company`'s appearance close to `auto-complete`'s appearance.
* Added config to make `company` a bit more close to `auto-complete`'s behavior.
* Added `show hover` function related to VSCode `Show Hover` key.

### 2019-04-09

* Kill `undo-tree-visualizer` when killing undoing buffer.
* Start adding own snippets using `yasnippet`.
* Rename all `cs` related naming to `csharp` for consistency.
* Rename all `elisp` related naming to `emacs-lisp` for consistency.
* Rename all `cbl` related naming to `cobol` for consistency.
* Split `cmake-mode` and `makefile-mode` into two files.
* Installed new pacakge => `company-quickhelp`.
* Remove `auto-complete` and use `company` instead.
* Start supports GLSL file

### 2019-04-08

* Removed manually installed `verilog-mode`, it mode is already merged into GNU Emacs.

### 2019-04-07

* `polymode` package added by system.
* Don't use `narrow-to-region`, instead we just pass in the `start` point and `end` point.
* Installed new pacakge => `origami`.
* Use `origami` as default folding system to this config.

### 2019-04-06

* Implements `jcs-message-func.el` file.
  -> Erase *Messages* buffer.
  -> Erase *Messages* buffer without closing it.

### 2019-04-05

* Make oop docstring compatible with ref and pointer in c and c++ mode.
* Fixed kill buffer after exit buffer menu mode.

### 2019-04-04

* Fully implements TypeScript docstring.

### 2019-04-03

* Implements ActionScript docstirng.

### 2019-04-02

* Implements `web-mode`'s version front curly bracket key and bind it to web-mode.
* Fixed docstring display issue in `web-mode`'s php file.
* Fixed vs curly bracket logic.

### 2019-03-31

* Added optional to scroll up/down line functions.
* Complete line related util functions.
* Remove `Alex Shinn`'s `css-mode`, use Emacs's default `css-mode` instead.
* Added larger window height check util funtion.

### 2019-03-30

* Implements ActionScript docstring entry point.
* Implements CSharp docstring entry point.
* Fixed only one file opened, switch to default Emacs buffer issue.
* Installed new pacakge => `yasnippet-snippets`.
* Added configuration for `yasnippet` and `yasnippet-snippets`.

### 2019-03-29

* Bind electric backspace key to certain modes as default key binding.
* Improved undo/redo keys performance when using `undo-tree`.
* Simplify code in `jcs-oop.el` file.

### 2019-03-28

* Implements `typescript-mode` docstring.
* Added `Startup Time` section in the `README.md` file for describing the 
current condition for using this configuration when starting up Emacs.

### 2019-03-27

* Added advice to `save-buffer` key to disable `undo-tree`.

### 2019-03-26

* Fixed compile target script, wrong param name.
* Move `jcs-helm.el` functions to `jcs-helm-func.el` file and delete `jcs-helm.el` file.
* Manually update `reload-emacs` package => 20190326.001.
* Added first visible line pos util functions.
* Make revert window state to reopn this buffer key.
* Fixed reopen this buffer key, make compatible with opening the same buffer in different/mutliple windows.

### 2019-03-25

* Removed global linum mode when using undo-tree.
* Implemented reopen this buffer key.
* Added line number related functions => `jcs-function.el` file.
* Added mixed of using `display-line-numbers-mode` and `linum-mode`, for any
file that uses `line-reminder` mode use `linum-mode`.  Other we use `display-line-numbers-mode`.
* Fixed `overwrite-mode` cursor not working.
* Added walk through each window util function.

### 2019-03-20

* Bind reload emacs and restart emacs.
* Remove self design reload emacs function.
* Manually install package => `reload-emacs`.
* Config `reload-emacs` package using `use-package` in => `jcs-plugin.el` file.

### 2019-03-19

* Rename plugin advice function name for accuracy purpose => `jcs-plugin.el` file.

### 2019-03-18

* Bind isearch forward at point key.
* Remove search forward/backward at point functions.

### 2019-03-17

* Installed new pacakge => `move-text`.

### 2019-03-13

* Bind rebind keys after define `jcs-global-key-rebind` function.

### 2019-03-12

* Installed new pacakge => `restart-emacs`.
* Retain reload emacs functionalities.
* Fixed smart indent up/down keys in `css-mode`.
* Remove unused packages.
  - auto-complete-c-headers
  - google-c-style

### 2019-03-11

* Speedup Emacs startup time.
* Move erase buffer to somewhere more reasonable.
* Use require instead of load path.
* Fixed check `truncate-lines`, this isn't minor-mode is actually a variable with t or nil.

### 2019-03-10

* Installed new pacakge => `indicators`.
* Implements toggle transparency window that will record dowwn
 the last transparent alpha level. This feature polished the
 user experience wise.

### 2019-03-08

* Implements switch window group layout vertically/horizontally key's functionality.
* Installed new pacakge => `focus`.

### 2019-03-07

* Installed new package => `dimmer`.
* Fixed speedbar not starting in the correct directory tree using `default-directory`
variable instead of fiddle method of fixing this issue.
* Manage most plugin configurations using `use-package` package.
* Revert part of the code, fixed indentation incorrect when doing docstring comment style.

### 2019-03-06

* Added screen config section => `jcs-env.el` file.
* Added goto-line-preview section and configurations.
* Start using `use-package` in the config, add `Package Management` section to the feature list.
* Fixed speedbar not opening the current file directory issue.
* Rebind some key bindings for more reasonable reason, see `./doc/keybindings.txt` file.

### 2019-03-05

* Diminish minor modes, `overwrite-mode` and `eldoc-mode`.
* Make toggle terminal command compatible to vscode preset's key bindings.
* Bind `describe-bindings` key to `C-k C-s`, compatible to vscode preset's key bindings.
* Upgrade with more math functions => `jcs-math.el` file.
* Rebind toggle cross/depends mode key to `C-~` key.
* Fixed transparent window util functions and reduced duplicate code.
* Rebind text scalle up/down key to `C-=` and `C--`.
* Rebind transparent frame increament/decreament key to `M-=` and `M--`.
* Update some key bindings to `./doc/keybindings.txt` file.

### 2019-03-04

* Added typescript docstring configurations.
* Added `.properties` extension to default as `ini-mode`.
* Fixed css and web return key => `jcs-web-func.el` file.

### 2019-03-03

* No longer needed resolve `goto-line-preview-goto-line` that does not go back
to original position issue, the package resolved itself.
* Reserve `goto-line-preview` config section.
* Update key command from => `goto-line-preview-goto-line` to `goto-line-preview`.

### 2019-03-02

* Installed new package => `goto-line-preview`.
* Remove `goto-line` key, instead we use package `goto-line-preview` from melpa.
* Make compatible with old `jcs-goto-line` key, by having check in `jcs-hook.el` file.
* Reserve minibuffer post command hook.
* Added `goto-lnie-preview` config section in the `jcs-plugin.el` file.

### 2019-03-01

* Added top level activation flag.
* Move minibuffer hook to hook file.
* Added improved goto line navigation functionalities and bind to original `goto-line` key.

### 2019-02-28

* Set keys compatible to VS Code default key bindnigs.
* Fixed toggle vertical/horizontal editor layout functionality that does not works
on the second window in the current frame. Notice this is only a temporary fixed.

### 2019-02-25

* Fixed magit installation error by updating its' dependencies.

### 2019-02-23

* Update beginning/end of visual line the same behaviours as the VSCode text editor's key behaviours.
* Make ALT-z toggle `truncate-line-mode`, so it compatible to VSCode's key presets.

### 2019-02-20

* Added jcs home and end keys functionalities.
* Bind home and end keys functions.

### 2019-02-12

* Move jcs web mode truncate line functionality to hook instead of locate every key functions.
* Remove web left/right key functions/functionalities.
* Avoid auto truncate line functionalities while navigating empty lines in web-mode.
* Revert jcs set init face.
* Load set init face in js2-mode.

### 2019-02-06

* Manually update manual packages.

### 2019-02-04

* Fixed readme description.

### 2019-02-03

* Added key bindings description to readme file.

### 2019-02-02

* Update project description and elaborates more about it.

### 2019-02-01

* Implements symbol util functions.
* Remove single line comments font lock keywords => mapc.

### 2019-01-31

* Added jcs python docstring face.

### 2019-01-29

* Fixed python tab key binding with weird action.
* Ensure python tab width is 4 instead of default of 8.

### 2019-01-21

* Remove load todo, load log and insert-timeofday command functions.

### 2019-01-15

* Use defense programming in current char string util function.

### 2019-01-08

* Added is-killed returned value to jcs-maybe-kill-this-buffer util function.
* Fixed jcs' count window util function.
* Fixed re-builder's maybe kill this buffer function using is-killed variable.

### 2019-01-05

* Implements python return function => jcs-python-func.el.
* Organize legacy code => jcs-python-func.el.

### 2019-01-04

* Fixed python insert docstring function, for second situation, between two double quotes.

### 2019-01-02

* Remove history, is no longer needed.
* Added load face order, and just reload instead of operate the list functions.
* Added sharp single line comment face.
* Compatible to electric pair command in python mode.
* Fixed move forward/backward word navigation util functions.
* Added ask python template and use it when creating new python file.
* Added python plain and class template.
* Mark version 5.2.1 and release one version.

### 2019-01-01

* Revert maybe kill this buffer function and add ecp-same arg.

### 2018-12-31

* Update definition for maybe kill this buffer function => jcs-edit.el.
* Optimize switch to prev/next buffer util functions.
* Diminish right click context mode.

### 2018-12-28

* Some modifications for maybe kill buffer key.
* Added next/prev buffer util functions.
* Added print buffer util functions.

### 2018-12-25

* Install new package => right-click-context.
* Added package to pre-install package list => right-click-context.
* Enable right-click-context as default in plugin config file => jcs-plugin.el.

### 2018-12-24

* Start support INI file, customize the `ini-mode' with jcs-ini-mode.el file.
* Fixed coding style => jcs-file-info-format.el.

### 2018-12-23

* Added electric backspace util function.
* Added electric open/close pair related functions.
* Fixed verbose char to byte and char to string util functions.

### 2018-12-21

* Added yaml func file for yaml mode functions => jcs-yaml-func.el.

### 2018-12-16

* Added new package 'auto-rename-tag' to preinstall package list.
* Added new package 'htmltagwrap' to preinstall package list.
* Diminish the 'auto-rename-tag' minor mode.
* Active diminish by requiring the package you want to diminish => `auto-rename-tag`, bug fixed.

### 2018-12-14

* Added hex and char section to last-command-event doc => doc/last-command-event.txt.

### 2018-12-13

* Added doc/last-command-event.txt for record all the last-command-event's returns
 value.

### 2018-12-11

* Added indent-info package and it's config.

### 2018-12-06

* Fixed insert header only when buffer-file-name variable available.

### 2018-12-04

* Fixed bug by adding percise check => jcs-maybe-kill-this-buffer function
 in jcs-edit.el file.
* Implements check how many times the same buffer shown in different windows
 => jcs-buffer-showns function in jcs-window.el file.

### 2018-12-03

* Mark version 5.1.9 and release one version.

### 2018-12-01

* Bug fixed, make percise return key for web-mode => jcs-web-return-key.

### 2018-11-26

* Added gitattribute custom mode hook.

### 2018-11-25

* Make one history => ### 2018-11-25.
* Implemented YAML mode hook, => jcs-yaml-mode.el file.

### 2018-11-21

* Start support Swift file, customize the `swift-mode` with jcs-swift-mode.el file.

### 2018-11-17

* Start support Rust file, customize the `rust-mode` with jcs-rust-mode.el file.

### 2018-11-13

* Start support Ruby file, customize the `ruby-mode` with jcs-ruby-mode.el file.

### 2018-11-04

* Fixed web-mode highlighting missing when apply ASP.NET Razor v3 comment highlighting rule.
* Added `jcs-post-command-hook` in `jcs-hook.el` in order to fix highlihging
missing when editing file using web-mode.

### 2018-11-03

* Start support YAML file, install major mode `yaml-mode`.

### 2018-10-22

* Implement web return key functionalities.
* Increase readabilities for util module.

### 2018-10-18

* Start support Markdown file, install major mode markdown-mode`.
* Added customize markdown mode configurations.

### 2018-10-17

* Completey remove neotree.
* Use `speedbar` and `sr-speedbar` instead of `neotree`.
* Implemented `speedbar` and `sr-speedbar` customize functions.
* Implemented `nhexl-mode' configurations.

### 2018-10-15

* Long-overdue support language `Verilog', starting from now on support this language.
* Implement deleting between functionailities, and add some custom function for certain generally use symbol in programming.

### 2018-10-13

* Fixed check current character occurs error issue at point of beginning of the buffer.
* Rename template to be more specific and precise on the naming.

### 2018-10-12

* Added Lisp header template.
* Fixed weird insert header file format's function description in each mode file.

### 2018-10-11

* Start support TypeScript file, install major mode typescript-mode.
* Added typescript header format template.
* Make `jayces-mode` to package.
* Added `tabbar` package and set the env settings/key bindings.
* Added `javadoc-lookup` package and set the env settings/key bindings.
* Start support Clojure, ClojureScript and Clojure Source file, install major mode `clojure-mode`.
* Update license and prorject version to 5.1.7.

### 2018-10-07

* Improve enable/disable truncate lines mode.
* Remove web return key, seems like we no longer need this key function anymore.
* Rename, remove emacs prefix to all doc.
* Added Emacs' syntax table document.
* Added Emacs' regular expression document.

### 2018-10-05

* Added recentf-file mode environment settings => jcs-env.el.
* Bind open recent files key => jcs-global-key.el.
* Update key binding note => open recent files key.

### 2018-10-03

* Implemented jcs-emmet-expand-line wrapper in order to fix on link goto address issue.
* Bind the key in emmet mode keymap.

### 2018-10-01

* Added jcs-count-frames function for multiple window's frame count.
* Fixed maybe kill this buffer function with the same file name but different directory issue.

### 2018-09-29

* Added more face to fixme mode list.
* Move face settings to jcs-face.el.
* Load fixedme face after all initialize, so we cover all the faces.

### 2018-09-27

* Change the `default-directory' variable when compiling a script to the directory the current script is currently at.

### 2018-09-26

* Implement the following three util functions..
   => jcs-current-whitespace-p
   => jcs-current-tab-p
   => jcs-current-whitespace-or-tab-p
* Implement jcs-text-scale-increase and jcs-text-scale-decrease
 function in order to fix the `line-reminder` plugin issue.
* Fix everytime it search forward recursive, it will centerl the window issue.
But does not happens in search backward recursive... Weird! => locate in
`jcs-nav.el` file.
* Update license and prorject version to 5.1.5.

### 2018-09-19

* Added example package files for future package example and installation location standard.
* Implemented VS like cut key in jcs-vs-func.el file.
* Bind the vs like cut key as default cuty key in global mode.

### 2018-09-16

* Added search forward/backward colon/semicolon/greater and less than sign in jcs-nav.el module.

### 2018-09-03

* Rename function check first forward/backward character with in line post-fix.
* Added check fist forward/backward character to limit to the whole buffer.

### 2018-09-01

* VS like function implemented => jcs-vs-func.el file.
* Load vs like functions to each related mode.
* Remove vs like function key binding as global key, instead we declare it inside
specific mode that needed to have vs like function key bindings in it.
* Rename next/previous blank line function with jcs prefix.

### 2018-08-29

* Fixed haxe-mode cannot switch frame issue.

### 2018-08-27

* Added quote symbol to specify the correct extension to the correct major mode.

### 2018-08-18

* Start support Haxe file, install major mode 'haxe-mode'.
* jcs-haxe-mode for own control of editing Haxe file.
* Added haxe_template.txt for Haxe file's header.

### 2018-07-28

* Package dependencis changes through melpa package manager updates.

### 2018-07-27

* Package dependecies list changes while update packages on melpa.

### 2018-07-23

* Rearrange package dependencies package list.

### 2018-07-20

* Added 'use-package' package to pre-install package list.
* Update license and prorject version to 5.1.3.

### 2018-07-19

* Added json-mode package to package dependency list.

### 2018-07-14

* 'wgrep' package added back to install list, know idea why it seems like get reject by Emacs. Anyway, is back on Emacs again.

### 2018-07-06

* Install new package 'project-abbrev', and remove manually install code for this package.

### 2018-07-05

* Added jcs-ex-pkg example package, for future self package development.
* Remove manually install 'line-reminder' package, install it on melpa. The package 'line-reminder' is currently on melpa.

### 2018-07-02

* Change package name from 'custom-abbrev' to 'project-abbrev'.

### 2018-06-22

* Added double dash comment font lock face for mode it uses '--' to do single line comment.
* Added java save functionalities/functions work with `organize-imports-java` package, when first time save reload local source paths.
* Fixed get current point face name for Emcas version 26.
* Use util jcs- prefix check current face function instead of same code everywhere.
* Added `null` and `void` face to modifier face.

### 2018-06-20

* Added haskell to support language list -> README.md.
* Added haskell mode .el file.
* Added Haskell template.

### 2018-06-19

* Added math module for future math use.
* Remove trans window module to just window module.
* Bind message buffer keymap with earse message buffer.
* Load math module and remove load trans window module.
* Simplify trans window module's code.
* Added print timestamps with multiple version function/functionality.
* Rebind 're-builder' key and 'Rename current buffer/filename' key.
* Remove timestamp version 3 properties and it function.
* Update project version to 5.1.1.
* Re-arrange readme file to sort support languages by alphabetic order.
* Bind save buffer key with set file coding system functionality in `sh-mode`.

### 2018-06-16

* Added # to all interactive function operative and add new key binding toggle enlarge window selected key.
* Added few balance window functions and enlarget current selected window function.
* Added set all local variable function. Pass in as symbol.
* Rename duplicate line function with prefex 'jcs-' infront.
* Added enlarge current selected window key binding doc.
* Added overwride mode rewrapper function functionality.
* Force maximize frame after reload Emacs and remove helm function module.
* Rename jcs-new-window to jcs-new-frame for better naming and understanding.
* Separate helm function to individual helm-func file.
* Added frame func file/module.
* Make one history => ### 2018-06-16.
* Added package-autoremove key binding note to project doc.
* Replace `blank-mode` pacakge to `whitespace` package, is built-in now.
* Added certain more keyword to highlight for programming usage, check on `jcs-env.el` file.
* Added jcs-compile function rewrapper functionality.
* Remove 'blank-mode' from pre-install package list.
* Update project version to 5.1.0.
* Fixed normal web comment highlighting.
* Better way of checking if beginning of line using 'current-column' function.

### 2018-06-14

* Added toggle read-only mode key binding and make note to emacs key bindings doc.

### 2018-06-13

* Revert back to error handling with custom-abbrev expansion key.

### 2018-06-11

* Added is default face functionality to utility module.
* Update emacs version record to Emacs 26.1.
* Update project compatible with Emacs version 26.1.
* Make one history for Emacs version 26.1 => ### 2018-06-11.
* Update project version to 5.0.5.

### 2018-06-10

* Fixed error handle still going url after custom expansion with key bindings `ctrl + return`.
* Rename function from duplicate-line to jcs-duplicate-line for consistency.

### 2018-06-09

* Manually upgrade pacakge `use-ttf` to version 20180609.

### 2018-06-05
* Remove casey text mode hook.
* Error handling decision on not finding the version control root directory.
* Added find file in project and current directory, also a design decision.
 Plus add the `jcs-find-file-in-project-and-current-dir` and
`jcs-select-find-file-current-dir` functions. Fixed the bug
for `jcs-select-find-file-in-project' function.


### 2018-06-04

* Create 'jcs-dev.el' file for development related functions file put here.
* Remake open-todo, open-update-log, makescript/runscript without asking.

### 2018-06-03

* Upgrade package 'line-reminder' package manually => 20180603.
* Manually install package 'custom-abbrev'.
* Implement `jcs-ctrl-return-key' functionality for JayCeS default control return key. It uses priority function list to handle each requirement.
* Update keybindings doc describe ctrl-return key.

### 2018-06-02

* Remove manually isntall 'com-css-sort' package, use melpa package manager instead.
* Diminish line-reminder pacakge.
* Update project version to 5.0.1.

### 2018-06-01

* Upgrade package 'line-reminder' package manually => 20180601.

### 2018-05-31

* Upgrade package 'line-reminder' package manually => 20180531.

### 2018-05-29

* Manually install 'line-reminder' package => 20180529.
* Remove none needed autoload prefix function from jcs-util file.
* Set global line reminder mode enable as default.

### 2018-05-28

* Added redo key to 'org-mode'.
* Wrong according key bindings, place it by categories.
* Added triple char style comment prefix check functionalities.
* Fixed Lua comment active docstring error.
* Fixed Visual CSharp comment active docstring error.
* Added prefix message and value delimiter arguments for `jcs-log-list' function.

### 2018-05-26

* Manually update 'use-ttf' package to 20180526.
* Package 'organize-imports-java' is on melpa, no longer need to manually install the package.

### 2018-05-25

* Update 'use-ttf' package manually to 20180525.
* Trasnfer data from `.emacs.d' to `.emacs.jcs'.
* Make one history => ### 2018-05-25.
* Future history for first version of .emacs.jcs directory tree view/template.
* Update project version to 5.0.0, huge data transfer/rename from '.emacs.d' folder to '.emacs.jcs' folder.

### 2018-05-23

* Update `use-ttf` package manually to 20180523.

### 2018-05-22

* Added first version of fonts.
* Make shown prefex for better function readability for certain function.
 Like `jcs-jump-to-shown-buffer` instead just `jcs-jump-to-buffer`.
* New manage file, jcs-font.el file.
* Split `jcs-font.el` module to individual package => `use-ttf`.
* Manually install package => `use-ttf`.

### 2018-05-20

* Use cl-lib instead of my own ugly method implementation.
* Jump to *Messages* window after do the logging/message.
* Use undo tree with the better performance without opening/closing the undo-tree-visualizer mode all the time.
* Added jump to buffer functionality
* Goto *Messages* buffer and end of buffer when do jcs type of logging functions.
* Fixed Visual Studio's C# type of commenting method. Weird action when having
two slashes, does not detect the Visual Studio's type of prefix comment symbol
pretty well.
* Added `all-the-icons' package to preinstall package list.
* Close *undo-tree* buffer when save.

### 2018-05-19

* Remove sorting CSS attributes before save, I think is never been useful.
* Manually update packages 'com-css-sort' and 'organize-imports-java'.
* Added shell key up and key down functionalities.
* Remap shell-mode key bindings for trying simulate the real shell action.
* Added shell command completion functionality/function and map it in the shell-mode.
* Fixed css-mode comment font face does not work with dash.

### 2018-05-18

* Bind upgrade package to package mode.
* Fixed depends mode not active before we enter at least once of the minibuffer.
* Added completion key binding to shell-mode.

### 2018-05-17

* Added deletetion series of functionalities and key bindgins.
* Added start/last line of buffer boolean check functionalities for future utility use.
* Improve reload emacs functionality.
* Better default with shell mode control, more like the normal/general/ common terminal prompt.

### 2018-05-16

* Added undo tree keymap comment or uncomment overwirte.
* Added save excursion for revert all buffer.

### 2018-05-15

* Added toggle undo tree related functions.
* Added package upgrade all function.
* Make one history => ### 2018-05-15, upgrade all package to the newest version.

### 2018-05-14

* Added shell backspace functionality.
* Added undo/redo key bindings with `undo-tree`.
* Added `jcs-shell-mode.el` file for shell mode managing.

### 2018-05-12

* Complete all naming convention to `jcs`.
* Make one history => ### 2018-05-11.
* Make comment methods exactly the same as Visual Studio IDE in csharp mode.
* Added header smart indent down and up for csharp mode in `jcs-cs-func.el` file.
* Split file into `jcs-hook.el` file from `jcs-after-init.el` file.
* Update project version to 4.8.1.

### 2018-05-10

* Fixed csharp distinguish between docstring and normal comment line.

### 2018-05-09

* Set default f7 key bindings to find file other window.
* Organize corresponding functions in c/c++ mode to function file.
* Manually install update `organize-imports-java' package.

### 2018-05-08

* Added first character forward/backward check functionalaities.
* Use first character forward/backward check instead of string-match to the end of line and beginning of line.
* Fixed shell not prompt the first time issue with error handling.

### 2018-05-07

* Added remove trailing whitespace current line functionality.
* Change all files header to classic JayCeS header.
* Set default major mode to org-mode.
* Added comment regexp for web-mode, compatible with ASP.NET project.

### 2018-05-05

* Added check current char string match functionality.
* Auto truncate lines in web mode, add `default' face check twice. Make default
check on the first character.
* Make kill whole line function goto the same column as current line.
* Added web-mode rewrapper function for future handy use.

### 2018-05-04

* Check end of line trigger enable/disable auto truncate lines effect.
* Remove unuse readme file -> README.txt.
* Update project version to 4.7.4.

### 2018-05-03

* Implement auto truncate lines functionalties.
* Enable auto truncate lines functions in web mode as default functionalities.
* Added get current line number functionalities.

### 2018-05-01

* Update `organize-imports-java' package manually.
* Install com-css-sort package manually.

### 2018-04-30

* Added error handling for `helm-do-ag-this-file` command. No need to switch
to cross mode manually anymore. It will just switch to cross mode automatically.
The implementation can be found in `jcs-helm-do-ag-this-file'` function.
* Fixed some autoload/interactive functions.
* Fixed failed search on move to next/previous blank line functions.
*  Fixed CSS type face highlighting.

### 2018-04-29

* Change indentation and change mode to from if statement to cond statement.
* Added toggle web mode offsetless elements.
* Set web mode offsetless element variable.
* Key bindings comment detail update.
* Change `html' template's head and body tag to the same level as html tag.
* Added default ASP .NET's extension to web-mode.
* Added manually install section in .emacs file.
* History removal, keep one history is enough.
* Revert history files.

### 2018-04-26

* Install organize-imports-java' package manully.
* Added plugin organize-imports-java' solve Java imports functionaltity.

### 2018-04-22

* Temporary install `organize-imports-java' package manully, ready for melpa to publish it.

### 2018-04-18

* Added some functionalities for modefiying list object.
* Reformat some parameters in order to match flycheck standard.

### 2018-04-17

* Check default directory for java packaging path including functionalities.
* Fixed comment file path match flycheck standard.

### 2018-04-16

* Added search version control directory and it related functionalities.

### 2018-04-07

* Fixed c++ mode namespace font lock face and compatible with c mode.
* Added java package declaration functionality.
* Added empty line between two lines of code functionalities.

### 2018-04-04

* Added `which-key` package to pre-install list.
* Added `which-key` config.
* Update project version to 4.7.1.

### 2018-03-24

* Added doc string properties file.
* Apply hot reloading to doc string properties customization.

### 2018-03-18

* Added processing mode config, starting support processing.
* Added processing template file.

### 2018-03-05

* Rename save buffer to proper naming with tabify and untabify.
* Added save tabify to global keymap.

### 2018-02-24

* Added spaces and tabs regexp for editing preprocessor type programming langauges.
* Added tabs to all spaces regexp.
* Added change font functionalities and section.
* Added change font key binding and update key bindings note/doc.

### 2018-02-15

* Added BASIC template.
* Added BASIC mode.
* Fixed :graph: regexp to limited version -> a-zA-Z0-9.

### 2018-02-12

* Old manage file info function call fixed to current version.

### 2018-02-10

* Make compatbile with c regexp missing fixed.
* Cancel using c++ for .c file, back to c-mode.

### 2018-02-05

* Clean check all templates and remove template function's keywrods.

### 2018-02-04

* Make all modes compatible with template hot reloading system.
* Added `jcs-perl-mode.el' file for Perl script editing.
* Added after init hook for preload template for half hot reloading.
* Added first version of all supported language.
* Added missing language to language support list.

### 2018-02-03

* Added scala mode for scale file editing.
* Remove coding template insertion, add template 3rd hot reloading system.
* Added prompt instead of text questioning in minibuffer for template questioning function.
* First add templates file for general coding languages.
* Added ini file for keyword variables in template.
* Added make file tempate for python and java, identify those in cc too.
* Update version to 4.6.0.

### 2018-02-02

* Added org table navigation key bindgs to org mode.
* Added org table navigation functionalities.
* Use blank and graph regexp instead of guessing all needed characters for comment highlighting in org-mode.
* Added `bochsrc' as default extension in org-mode, meaning use org-mode when editing bochsrc file.

### 2018-02-01

* Added `jcs-tab-key' function for global tab key command and key bindings.
* Insert spaces by tab width function implemented and located in util module.

### 2018-01-31

* Added jcs-log-list functionaly for debug usage.
* Fixed c/c++ mode define oop doc string array list reverse order issue with built-in lisp's `revers' function.
* Fixed comment closing c style comment block and organize elisp for better usage.

### 2018-01-28

* Added `.` for oop value's regexp.
* Make special key's regexp compatible without space.
* Update project version to 4.5.6.

### 2018-01-24

* Make align function repeat as default.
* Remove special key `' key bindings because we use built-int electric pair system instead.
* Added txt func file for editing jayces own use text file.

### 2018-01-18

* Added delete space infront of line functionalities.
* Added smart indent up/down for web-mode.
* Added web return key functionality.

### 2018-01-16

* Set default org mode no fold.
* Added align comment functionalities.

### 2018-01-15

* Deactive mark to unselect current region if align region not active.

### 2018-01-13

* Added ignore-errors for helm find gtags in order to restore previous cross/local mode.

### 2018-01-08

* Fixed nasm comment key forward and spacing issue.
* Fixed nasm return key at the end of buffer can not make new line issue.
* Align repeat functionality implemented.

### 2018-01-07

* Added `!` and `.` to special keyword regexp.
* Added special key electric functionaltity.
* Added special key key binding to global keymap.
  - Here special key mean any word between/inside \` and ' is a special keyword.
* Make sure back to original mode (cross/depend) once exit gtags's mini-buffer.
* Use default nasm comment function before do our own functionalities in `jcs-nasm-comment` function.
* Fixed python and lua mode regexp problem for doc-string indentifier.

### 2018-01-06

* Make oop doc string support multiple line coding style.
* Added current word equal functionality.
* Added move forward/backward to word functionalities.
* Added support python multi-lines coding style.
* Refactor env file for better readability, majorly use defacce instead of make-face.
* Apply option comment doc-string option choice to the function.
* Added ` between ' special key word highlighting.
* Added highlight restriction to value regexp.

### 2018-01-05

* Added nasm return and comment key.
* Added doc stirng check before insert docstring.
* Added goto first char of line functonalities for reduce duplicate code.

### 2018-01-04

* Ignore all tabs and spaces after preproc keywords.
* Added logging/debug functionalities.
* Added convert tab to space and space to tab functionalities.
* Added real backspace and real space functionalities.

### 2018-01-03

* Get notice about Ctrl-r key bindings will be rebind everytime, fix backspace key bindings issue.
* Added default link command to both application/library makefile templates.
* Fixed wrong spelling for diasm command in makefile template.

### 2018-01-02

* Change jcs-delete-forward/backward-current-char-repeat's key bindings, to avoid bind key with backspace.

### 2018-01-01

* Make sure delete region before delete line for kill whole line command.
* Complete .asm .s .S support assembly makefile template.
* Space check for compatibility py backspace command to tab.

### 2017-12-31

* Refactor util with flycheck checker.
* Added python like space keybindings to nasm mode.
* Modefied key bindings compatible with search, captial navigation and kill navigation.
* Added nasm func file for nasm mode functionalities.

### 2017-12-29

* Refactor jcs-edit file to jcs-buffer-menu file.

### 2017-12-28

* Make percise to shell toggle function.
* Added find file hook.
* Added buffer menu functions.
* Make history/backup.

### 2017-12-26

* Added isearch at point forward and backward functionalities.
* Enable auto complete mode in nasm mode as default.
* Change CMake bindings work like Python mode key bindings or functionalities.
* Added 'functions' for makefile info.
* Make compatible with .S file extension.
* Added `mount` and `buildimg` commands for makefile template.

### 2017-12-22

* Added enum doc string for c and c++ mode.
* Added linux and macosx build env and header file list.

### 2017-12-21

* Added assembly language commands and flags for makefile file formant.

### 2017-12-19

* Added '@' symbol for normal bash commands.

### 2017-12-16

* Added emacs keybindings note with open update log and todo list.
* Move move empty line up and down to nav file.
* Added open update log and open todo list functionalities.

### 2017-12-14

* Added 'jcs-preproc-fun.el' file.
* Make compatible define docstring with preproc face type.
* Refactor using when instead of if statement.

### 2017-12-13

* Added oop init set face function.
* Added local face modefication like visual studio type style.
* Customize cmake division comment.

### 2017-12-11

* Refactor using 'when' instead of 'if' statement.
* Change class comment type in Python mode.
* Added clean a lib and so lib command to 'realclean' command.
* Place casey's global key bindings to jcs global key binding file.
* Added c-flags to build dynamic library command.

### 2017-12-10

* Added package filter.
* Added 'all' option for package filter.

### 2017-12-08

* Added web-mode and php-mode doc string.

### 2017-12-07

* Makefile format work with static lib and dynamic lib.
* End with keyword with non nil keyword at third param, fix font lock not working issue.
* Added goto start of the comment and goto end of the comment functionalities.

### 2017-12-06

* Properly name default static and dynamic lib default build name.
* Fixed CMake not applying issue.

### 2017-12-05

* Fixed makefile format. static to .a, dynamic to .so, $< to $^ sign.

### 2017-12-04

* Added css highlighting.
* Added makefile templates.
* Added cmake/mke file ask template functionalities.
* Loaded txt mode for later since delete org mode as default mode.

### 2017-12-03

* Make after confirm adding header or not, change into corresponding c-mode or c++-mode.
* Added `togetherly` package.
* Added `floobits' package.
* Move cs to top in order to override the bug from csharp-mode.
* Added list of ext for c and c++ mode.

### 2017-12-01

* Change copyright character.
* Change regexp for strict type langauge in oop-func file for correct variable font face.

### 2017-11-30

* Added tag string and bracket sign for oop doc customization.
* Make sure curly bracket and bracket can be replace with each other in regexp, so it compatible with js doc comment style.
* Implement python docstring.
* Added variable type face issue.
* Be more specific on variable name regexp.

### 2017-11-29

* Make comment with auto add oop doc comment style.
* Added cs comment doc functionality.
* Added comment style doc and java-mode match with own doc highlighting.
* Force underscore a character.

### 2017-11-28

* Make defface instead of make face function in oop-func file.
* Fixed oop face regular expression highlighting issues.
* Fixed comment line break indentation issue.
* Update regular expression in oop/jsdoc for type, value and tag.

### 2017-11-27

* Added `visual-regexp` package.
* Added jsdoc type highlighting functions.

### 2017-11-22

* Enable column highlighting in web-mode.
* Fixed web-mode with correct PHP key bindings.

### 2017-11-21

* Update key bindings note with 'auto-highlight-symbols' package.
* Update global key, so it does not conflict with 'auto-highlight- symbols' package.
* Added jcs-oop-func file for Object Oriented Programming languages highlighting.
* Move diminish to after load setting file.

### 2017-11-20

* Split c and c++ mode into two file and leave cc with c and c++ common.

### 2017-11-19

* Change emacs cc-mode comment face.
* Added `auto-highlight-symbol` package to diminish list.
* Added `flycheck` package to diminish list.
* Added `flymake` package to diminish list.
* Added `helm` package to diminish list.

### 2017-11-18

* First draft readme.
* Change nasm key bindings indent up and down.

### 2017-11-17

* Installed new package `package-build` to list.
* Installed new package `package-lint` to list.

### 2017-11-15

* Added `auto-highlight-symbol` package.
* Customize package `auto-highlight-sybmol`.
* Fixed auto-highlight-symbol package face settings for my own use.

### 2017-11-13

* Update `web-mode` package with prefix indentation bug fixed.
 -> #939: https://github.com/fxbois/web-mode/issues/939
* Resolve performance issue while moving the cursor around the unicode text by adding `(setq inhibit-compacting-font-caches t)` at `jcs-env.el` file.
 -> #273: https://github.com/purcell/emacs.d/issues/273

### 2017-11-12

* Installed new `xwidgete` package.
* Installed new `pdf-tools` package.

### 2017-11-10

* Remove some of the web-mode functionalities, seems like the original `web-mode' package had improved.
* Revert 'web-mode' functionalities because I was wrong with the bug fixed from original package.

### 2017-11-08

* Remove thought was impatient mode settings.

### 2017-11-06

* Update key bindings note and add category Util for handy key bindings in emacs.
* Added emacs_commands file for record important commands for future usage and note.
* Added header for emacs lisp mode.
* Revaluate the css-sort function with local variable.
* Make backup ### 2017-11-06.
* Added save-window-excursion with css sort, make sure no moving point and scroll.

### 2017-11-05

* Make hide shell command proper UX.
* Added jump-to-window function to jcs-nav.

### 2017-11-04

* Fixed plugin bugs css-sort cursor moves after sort occurs, preferences issue make.

### 2017-11-03

* Added css-sort package.
* Added css sort key bindings to css and scss mode.
* Added css save function as default css-mode and scss-mode default save buffer functionality. Now save CSS and SCSS file with sort attribute before saving it.

### 2017-11-02

* Enable auto-complete package for web-mode's minor mode when editing PHP file.
* Added vimrc-mode package.
* Added magit package.
* Added global key bindings for magit.
* Added move current line up/down functionalities.
* Added move current line up/down global key bindings.
* Update key bindings note.
* Disable yasnippet for while editing PHP file or minor mode in web-mode.

### 2017-10-31

* Enable auto-complete package for global as default.
* Fixed COBOL mode auto-complete not showing issue.
* Fixed JavaScript mode auto-complete ac-timer and ac-update bugs by disable the ac-js2 minor mode.

### 2017-10-30

* COBOL header format fixed data division missing.
* Makefile initial format changes.

### 2017-10-29

* Change COBOL header format.
* Added initialize format for COBOL file.

### 2017-10-28

* More detail and completion to cmake file format info.

### 2017-10-27

* Added .ac extension to cmake mode for autotool, autoconf, automake family.

### 2017-10-26

* Added cobol-mode for editing COBOL file.
* Added jcs-cbl-mode for my customization.
* Added COBOL header in header format info file.

### 2017-10-24

* Fixed cs mode switch window previous key bindings.

### 2017-10-16

* Make sure comment it well on indentation with scripting language in web-mode.

### 2017-10-12

* Added jcs-vim-mode only for editing VimScript.
* Correct year file format in lua mode and vim mode.

### 2017-10-09

* Correct filename info.
* php paste will occur web-mode get interrupt, bug fixed by renable it again.

### 2017-10-05

* Added jcs-go-mode for GO programming language.
* Fix and Added jcs-align-region-or-document function for align variable cross language.
* Choose either single quote or double quote for markup.

### 2017-10-01

* Include header format file once bug fixed.

### 2017-09-28

* Added check current character a proper character not a sign or something else.
* Fix Web mode backward delete word and backward delete word capital.
* Added timestamp version 3 to jcs-util file.
* Change init format for SQL file.

### 2017-09-27

* Added `jcs-backward-delete-word` and `jcs-backward-delete-word-capital` for PHP variable naming convention.
* Added current uppercase letter and lowercase letter check in `jcs-util` file.

### 2017-09-25

* Fix file naming for some file in ex-mode directory.
* add `jcs-sql-mode.el' to ex-mode directory, meanwhile my emacs confi support editing SQL file.

### 2017-09-21

* Added adaptive-wrap package for HTML editing.
* Re-enable visual-line-mode when save in web-mode.

### 2017-09-12

* Added generated code for jcs-cc-mode specific for constructor and destructor.

### 2017-08-17

* Added jcs-sass-mode for sass language.

### 2017-08-11

* jcs python mode module test failed, fixed bugs to the better jcs python mode.
* add 'use strict' while create the js file.

### 2017-08-08

* Fix move forward capital character bug.
* Update `jcs-java-mode`.
* Intalled new package `meghanada'.

### 2017-08-07

* Installed new package `google-translate`.
* Installed new package `google-maps`.
* Installed new package `go-mode`.
* Installed new package `google-this`.

### 2017-08-06

* Implements `python-mode` compatible with whitespace, so I don't necessary use tab for programming python.
* Update some of the `jcs-shell-mode-hook` key bindings.

### 2017-08-05

* Remove package `dashboard', seems like the package no longer working.
* Change the theme color a bit.

### 2017-08-04

* Installed new package `dashboard`.
* Intalled new package `powerline`.
* Installed new package `diminish`.

### 2017-08-02

* Make `helm-colors' minibuffer RET insert name M-RET insert hex-code.

### 2017-07-29

* Fix bug capital search backward/forward and capital delete backward.

### 2017-05-25

* Added xml mode.

### 2017-05-23

* Added Sublimity to emacs file.
* Added Minimap and Animate scrolling.

### 2017-05-21

* Added increment/decrement transparency frame.
* Update Log file build.
