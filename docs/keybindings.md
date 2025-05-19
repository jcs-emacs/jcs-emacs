# List of key bindings

Here is the list of key bindings that are defined in this configuration.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [List of key bindings](#list-of-key-bindings)
  - [Global](#global)
    - [Programming](#programming)
      - [Editing](#editing)
      - [Build / Run (Output)](#build--run-output)
      - [Search](#search)
      - [Calculate](#calculate)
      - [Todo](#todo)
      - [Commenting / Uncommenting](#commenting--uncommenting)
      - [Formating](#formating)
      - [Folding / Unfolding](#folding--unfolding)
      - [Expand Region](#expand-region)
      - [Case](#case)
      - [Alignment](#alignment)
      - [Screensaver](#screensaver)
      - [Scrolling](#scrolling)
      - [Balanced Expression](#balanced-expression)
      - [Multiple Cursors](#multiple-cursors)
        - [Marking](#marking)
        - [Similarity](#similarity)
      - [Debugging](#debugging)
    - [Buffer](#buffer)
      - [Buffer Menu](#buffer-menu)
      - [Switch Buffer](#switch-buffer)
        - [Using buffer menu.](#using-buffer-menu)
        - [Using Tab](#using-tab)
      - [Kill Buffer](#kill-buffer)
      - [Save Buffer](#save-buffer)
      - [Rename Buffer](#rename-buffer)
      - [About **`*scratch*`**](#about-scratch)
      - [Goto **`*Message*`**](#goto-message)
      - [Goto **`*dashboard*`**](#goto-dashboard)
      - [Goto **`*eww*`**](#goto-eww)
      - [Goto **`*emp*`**](#goto-emp)
    - [File](#file)
      - [File Explorer](#file-explorer)
      - [Find Files](#find-files)
    - [Navigating](#navigating)
      - [Navigate Cursor](#navigate-cursor)
      - [Navigate Windows](#navigate-windows)
        - [Other](#other)
    - [Visualization](#visualization)
    - [Windows](#windows)
    - [Version Control](#version-control)
    - [Terminal / Shell](#terminal--shell)
    - [Others](#others)
      - [Minibuffer](#minibuffer)
      - [System](#system)
      - [Theme](#theme)
      - [Exit](#exit)
      - [Mark](#mark)
      - [Package List](#package-list)
      - [Process](#process)
      - [Profiler](#profiler)
      - [Describing](#describing)
      - [Eval](#eval)
    - [Windows Menu](#windows-menu)
  - [EWW](#eww)
  - [Image Mode](#image-mode)
  - [Package Mode](#package-mode)
  - [`tabulated-list` Mode](#tabulated-list-mode)
  - [Org Mode](#org-mode)
  - [C/C++/Objective-C Mode](#ccobjective-c-mode)
  - [Java Mode](#java-mode)
  - [Web Mode](#web-mode)
  - [CSS Mode](#css-mode)

<!-- markdown-toc end -->

## Global

### Programming

#### Editing

* <kbd>Ctrl</kbd>+<kbd>x</kbd> - Cut
* <kbd>Ctrl</kbd>+<kbd>c</kbd> - Copy
* <kbd>Ctrl</kbd>+<kbd>v</kbd> - Paste
* <kbd>Ctrl</kbd>+<kbd>z</kbd> - Undo
* <kbd>Ctrl</kbd>+<kbd>y</kbd> - Redo
* <kbd>TAB</kbd> - If region, indent block; else we expand it.
* <kbd>Shift</kbd>+<kbd>TAB</kbd> - If region, outdent block; else we expand it.
* <kbd>Ctrl</kbd>+<kbd>TAB</kbd> - Reindent block.
* <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>Ctrl</kbd>+<kbd>r</kbd> - Rename the whole text with the same name.
* <kbd>Ctrl</kbd>+<kbd>a</kbd> - Mark the whole buffer. (Select All)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>d</kbd> - Duplicate the whole line.
* <kbd>Ctrl</kbd>+<kbd>d</kbd> - Kill the whole line.
* <kbd>Ctrl</kbd>+<kbd>backspace</kbd> - Backspace word.
* <kbd>Ctrl</kbd>+<kbd>delete</kbd> - Delete word.
* <kbd>Alt</kbd>+<kbd>backspace</kbd> - Backspace word excluding the capital letter.
* <kbd>Alt</kbd>+<kbd>delete</kbd> - Delete word excluding the capital letter.
* <kbd>Alt</kbd>+<kbd>up</kbd> - Move the current line `up`.
* <kbd>Alt</kbd>+<kbd>down</kbd> - Move the current line `down`.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>left</kbd> - Move buffer `left`.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>right</kbd> - Move buffer `right`.
* <kbd>Ctrl</kbd>+<kbd>return</kbd> - Complete the word or go to the address at the current point.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Shift</kbd>+<kbd>,</kbd> - Decrement tab size by 2.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Shift</kbd>+<kbd>.</kbd> - Increment tab size by 2.
* <kbd>Shift</kbd>+<kbd>f10</kbd> - Right click context menu.
* <kbd>f12</kbd> - Goto declaration/definition.
* <kbd>Shift</kbd>+<kbd>f12</kbd> - Goto declaration/definition other window.
* <kbd>Alt</kbd>+<kbd>f12</kbd> - Peek declaration/definition.
* <kbd>f7</kbd> - Open the same file in the other window.
* <kbd>f8</kbd> - Open the corresponding file in the other window, if not found open the same file instead.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>i</kbd> - Change file coding system.
* <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>b</kbd> - Open RE-Builder mode window.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>r</kbd> - Reveal file in folder.

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>/</kbd> - Cycle at point.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>6</kbd> - Cycle case style.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>/</kbd> - Cycle quotes.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>/</kbd> - Cycle slash.

#### Build / Run (Output)

* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Build the program.
* <kbd>Ctrl</kbd>+<kbd>f7</kbd> - Compile the program.
* <kbd>f5</kbd> - Debug the program.
* <kbd>Ctrl</kbd>+<kbd>f5</kbd> - Run the program.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>u</kbd> - Show output window. (if any)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Previous output buffer.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Next output buffer.

#### Cleaning

* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>9</kbd> - Clear the `M-x` command history.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>0</kbd> - Clean the useless buffers.

#### Search

* <kbd>Ctrl</kbd>+<kbd>f</kbd> - Search through file.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Search through project.
* <kbd>Ctrl</kbd>+<kbd>,</kbd> - Search through the file at the current point. (backward)
* <kbd>Ctrl</kbd>+<kbd>.</kbd> - Search through the file at the current point. (forward)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>,</kbd> - Search through the project at the current point. (backward)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>.</kbd> - Search through the project at the current point. (forward)

#### Calculate

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>=</kbd> - Calculate current line
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Shift</kbd>+<kbd>=</kbd> - Calculate the entire buffer.

#### Todo

* <kbd>Ctrl</kbd>+<kbd>f10</kbd> - Go to the `previous` highlighted TODO related symbols.
* <kbd>Ctrl</kbd>+<kbd>f11</kbd> - Go to the `next` highlighted TODO related symbols.

#### Commenting / Uncommenting

* <kbd>Ctrl</kbd>+<kbd>/</kbd> - Comment/Uncomment region or line.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>c</kbd> - Comment region or line.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>u</kbd> - Uncomment region or line.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>-</kbd> - Banner comment

#### Formatting

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>f</kbd> - Format region.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>d</kbd> - Format document.

#### Folding / Unfolding

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>0</kbd> - Collapse all foldings.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>j</kbd> - Expand all foldings.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>[</kbd> - Collapse the current folding.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>]</kbd> - Expand the current folding.

#### Expand Region

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>=</kbd> - Expand the region from the current point.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>-</kbd> - Contract the region from the current point.

#### Case

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>u</kbd> - Uppercase word or region.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>d</kbd> - Downcase word or region.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>c</kbd> - Capitalize word or region.

#### Alignment

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>a</kbd> - Align region or document.

#### Screensaver

* <kbd>Alt</kbd>+<kbd>0</kbd> - Start screensaver.

#### Scrolling

* <kbd>Page Up</kbd> - Scroll the window `up` one page.
* <kbd>Page Down</kbd> - Scroll the window `down` one page.
* <kbd>Shift</kbd>+<kbd>Page Up</kbd> - Scroll the other window `up` one page.
* <kbd>Shift</kbd>+<kbd>Page Down</kbd> - Scroll the other window `down` one page.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>up</kbd> - Scroll the window `up` one line without moving cursor.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>down</kbd> - Scroll the window `down` one line without moving cursor.

#### Balanced Expression

* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd> - Move backward balanced expression (sexp) with only deep one level.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>'</kbd> - Move forward balanced expression (sexp) with only deep one level.
* <kbd>Ctrl</kbd>+<kbd>;</kbd> - Move backward balanced expression (sexp) with unlimited levels.
* <kbd>Ctrl</kbd>+<kbd>'</kbd> - Move forward balanced expression (sexp) with unlimited levels.

#### Multiple Cursors

##### Marking

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>up</kbd> - Mark the previous line.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>down</kbd> - Mark the next line.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Mark the previous line similar to current line.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Mark the next line similar to current line.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>Left-Click</kbd> - Mart at the current point.

##### Similarity

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>=</kbd> - Increase the string distance level by `1`.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>-</kbd> - Decrease the string distance level by `1`.

#### Debugging

* <kbd>f6</kbd> - Toggle `flycheck`.

* <kbd>f9</kbd> - Toggle break point.
* <kbd>Shift</kbd>+<kbd>f5</kbd> - Stop debugger.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>f5</kbd> - Restart debugger.
* <kbd>f10</kbd> - Step over.
* <kbd>f11</kbd> - Step in.
* <kbd>Shift</kbd>+<kbd>f11</kbd> - Step out.

* <kbd>Alt</kbd>+<kbd>1</kbd> - Turbo log the current region selection.

### Buffer

#### Buffer Menu

* <kbd>Alt</kbd>+<kbd>b</kbd> - Open buffer menu.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Open the buffer menu in the other window.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>b</kbd> - Open the buffer menu for the current project.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Open the buffer menu in the other window for the current project.
* <kbd>Ctrl</kbd>+<kbd>o</kbd> - Toggle diminish buffer mode.

#### Switch Buffer

##### Using buffer menu.

* <kbd>Alt</kbd>+<kbd>b</kbd> - Switch to buffer in the current window.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Switch to buffer in the other window.

##### Using Tab

* <kbd>Ctrl</kbd>+<kbd>pg-up</kbd> - Switch to the previous tab.
* <kbd>Ctrl</kbd>+<kbd>pg-down</kbd> - Switch to the next tab.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>pg-up</kbd> - Switch to the previous tab group.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>pg-down</kbd> - Switch to the next tab group.
* <kbd>Ctrl</kbd>+<kbd>insert</kbd> - Toggle tab groups.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>insert</kbd> - Switch to tab group.

#### Kill Buffer

* <kbd>Alt</kbd>+<kbd>k</kbd> - Maybe kill the current buffer. (only kill when buffer opens in one window left)
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>k</kbd> - Reopen the buffer.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>k</kbd> - Kill current buffer.

#### Save Buffer

* <kbd>Ctrl</kbd>+<kbd>s</kbd> - Untabify save file. [DEFAULT]
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>s</kbd> - Save all buffers to its current major mode.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>s</kbd> - Reverse `tabify` or `untabify` save buffer.

#### Rename Buffer

* <kbd>Alt</kbd>+<kbd>f2</kbd> - Rename current file

#### About **`*scratch*`**

* <kbd>Alt</kbd>+<kbd>s</kbd> - Open `*scratch*` buffer.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>s</kbd> - Open `*scratch*` buffer in the other window.

#### Goto **`*Message*`**

* <kbd>Alt</kbd>+<kbd>m</kbd> - Open `*Message*` buffer.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>m</kbd> - Open `*Message*` buffer in the other window.

#### Goto **`*dashboard*`**

* <kbd>Alt</kbd>+<kbd>d</kbd> - Open `*dashboard*` buffer.

#### Goto **`*eww*`**

* <kbd>Alt</kbd>+<kbd>h</kbd> - Emacs Web Wowser
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>h</kbd> - Emacs Web Wowser in the other window

#### Goto **`*emp*`**

* <kbd>Alt</kbd>+<kbd>e</kbd> - Emacs Music Player
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>e</kbd> - Emacs Music Player in the other window

### File

####  File Explorer

* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>l</kbd> - Toggle file explorer.
* <kbd>Ctrl</kbd>+<kbd>b</kbd> - Toggle file explorer.

#### Find Files

* <kbd>Alt</kbd>+<kbd>f</kbd> - Find the file in the working directory to the current window.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Find the file in the working directory in the other window.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Alt</kbd>+<kbd>f</kbd> - Find the file in the project in the current window.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Find the file in the project in the other window.

### Navigating

#### Navigate Cursor

* <kbd>Ctrl</kbd>+<kbd>right</kbd> - Navigate a word `right`.
* <kbd>Ctrl</kbd>+<kbd>left</kbd> - Navigate a word `left`.
* <kbd>Alt</kbd>+<kbd>right</kbd> - Navigate a word `right` excluding the capital letter.
* <kbd>Alt</kbd>+<kbd>left</kbd> - Navigate a word `left` excluding the capital letter.
* <kbd>Alt</kbd>+<kbd>g</kbd>, <kbd>l</kbd> - Goto line number.
* <kbd>Alt</kbd>+<kbd>g</kbd>, <kbd>c</kbd> - Goto character position.
* <kbd>Alt</kbd>+<kbd>g</kbd>, <kbd>p</kbd> - Goto last change.

#### Navigate Windows

* <kbd>Alt</kbd>+<kbd>w</kbd> - Switch to the `next` window.
* <kbd>Alt</kbd>+<kbd>q</kbd> - Switch to the `previous` window.

##### Other

* <kbd>Ctrl</kbd>+<kbd>1</kbd> - Select window 1.
* <kbd>Ctrl</kbd>+<kbd>2</kbd> - Select window 2.
* <kbd>Ctrl</kbd>+<kbd>3</kbd> - Select window 3.
* <kbd>Ctrl</kbd>+<kbd>4</kbd> - Select window 4.
* <kbd>Ctrl</kbd>+<kbd>5</kbd> - Select window 5.
* <kbd>Ctrl</kbd>+<kbd>6</kbd> - Select window 6.
* <kbd>Ctrl</kbd>+<kbd>7</kbd> - Select window 7.
* <kbd>Ctrl</kbd>+<kbd>8</kbd> - Select window 8.
* <kbd>Ctrl</kbd>+<kbd>9</kbd> - Select window 9.

### Visualization

* <kbd>Ctrl</kbd>+<kbd>=</kbd> - Text scale increase (Zoom in).
* <kbd>Ctrl</kbd>+<kbd>-</kbd> - Text scale decrease (Zoom out).
* <kbd>Ctrl</kbd>+<kbd>0(Numpad)</kbd> - Reset zoom.
* <kbd>Alt</kbd>+<kbd>=</kbd> - Increase the frame's transparency.
* <kbd>Alt</kbd>+<kbd>-</kbd> - Decrease the frame's transparency.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>f</kbd> - Change font style.
* <kbd>Alt</kbd>+<kbd>i</kbd> - Show end of line.
* <kbd>Alt</kbd>+<kbd>o</kbd> - Show end of file.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>b</kbd> - Show white space.
* <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>Ctrl</kbd>+<kbd>w</kbd> - Show white space.

### Windows

* <kbd>Alt</kbd>+<kbd>f11</kbd> - Toggle full screen.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>n</kbd> - Create the new frame.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>w</kbd> - Delete the current frame.
* <kbd>Ctrl</kbd>+<kbd>f4</kbd> - Delete the current window.
* <kbd>Ctrl</kbd>+<kbd>h</kbd>, <kbd>h</kbd> - Toggle window split between horizontal and vertical layouts.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>e</kbd> - Toggle showing the current window at the half frame.
* <kbd>Ctrl</kbd>+<kbd>\\</kbd> - Split window horizontally.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\\</kbd> - Split window vertically.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>\\</kbd> - Split window sensibly.
* <kbd>Alt</kbd>+<kbd>\`</kbd> - Toggle transparent frame.
* <kbd>Alt</kbd>+<kbd>=</kbd> - Increment frame's transparency by 5 percent.
* <kbd>Alt</kbd>+<kbd>-</kbd> - Decrement frame's transparency by 5 percent.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>j</kbd> - Enlarge window horizontally.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>l</kbd> - Shrink window horizontally.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>i</kbd> - Enlarge window vertically.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>Alt</kbd>+<kbd>k</kbd> - Shrink window vertically.

### Version Control

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>m</kbd> - Start magit.
* <kbd>Ctrl</kbd>+<kbd>0</kbd>, <kbd>g</kbd> - Start magit.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>g</kbd> - Start magit.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>c</kbd> - Checkout branch.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>d</kbd> - Delete branch.

### Terminal / Shell

* <kbd>Ctrl</kbd>+<kbd>\`</kbd> - Toggle shell window.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\`</kbd> - New shell process.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Previous shell buffer.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Next shell buffer.
* <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>t</kbd> - Open up the terminal in the current buffer.

### Others

#### Minibuffer

* <kbd>Alt</kbd>+<kbd>x</kbd> - Active the minibuffer.
* <kbd>f1</kbd> - Active the minibuffer.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>p</kbd> - Active the minibuffer.

#### System

* <kbd>Alt</kbd>+<kbd>f4</kbd> - To shutdown Emacs.
* <kbd>Alt</kbd>+<kbd>f5</kbd> - To reload Emacs.
* <kbd>Alt</kbd>+<kbd>f6</kbd> - To restart Emacs.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>,<kbd>Ctrl</kbd>+<kbd>s</kbd> - To describe key bindings with the current `major-mode`.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>,<kbd>Ctrl</kbd>+<kbd>i</kbd> - To describe the current symbol.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>q</kbd> - To focus/unfocus the described frame.
* <kbd>Ctrl</kbd>+<kbd>t</kbd> - To toggle `tabbar-mode`.
* <kbd>Alt</kbd>+<kbd>z</kbd> - To toggle `truncate-lines`.
* <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>o</kbd> - To toggle between Read-Only.
* <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>f</kbd> - To open recent files.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>\`</kbd> - To toggle `depend`/`cross` mode.

#### Theme

* <kbd>Ctrl</kbd>+<kbd>k</kbd>,<kbd>Ctrl</kbd>+<kbd>t</kbd> - Select theme.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>d</kbd> - Toggle light/dark theme.

#### Exit

* <kbd>ESC</kbd> - Exit the minibuffer and go to the top level.
* <kbd>Shift</kbd>+<kbd>ESC</kbd> - Escape keyboard.
* <kbd>Ctrl</kbd>+<kbd>g</kbd> - Exit keyboard.

#### Macro

* <kbd>Ctrl</kbd>+<kbd>x</kbd>, <kbd>x</kbd> - Macro expand.

#### Mark

* <kbd>Ctrl</kbd>+<kbd>SPC</kbd> or <kbd>Alt</kbd>+<kbd>SPC</kbd> - Set mark at the current point.
* <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd> - Pops back to the previous mark.

#### Package List

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>p</kbd> - Package list.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>x</kbd> - Package list.

#### Process

* <kbd>Alt</kbd>+<kbd>p</kbd> - Show the list of processes.

#### Profiler

* <kbd>Alt</kbd>+<kbd>7</kbd> - Toggle profiler.

#### Describing

* <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>f</kbd> - Describe function.
* <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>m</kbd> - Describe mode.
* <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>v</kbd> - Describe variable.
* <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>b</kbd> - Describe bindings.

#### Eval

* <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>b</kbd> - Eval buffer.
* <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>d</kbd> - Eval defined expression.
* <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>e</kbd> - Eval expression.
* <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>r</kbd> - Eval region.

### Windows Menu

* <kbd>Alt</kbd>+<kbd>space</kbd>, <kbd>n</kbd> - Minimize frame.
* <kbd>Alt</kbd>+<kbd>space</kbd>, <kbd>x</kbd> - Toggle minimize/maximize frame.
* <kbd>Alt</kbd>+<kbd>space</kbd>, <kbd>c</kbd> - Shutdown Emacs.

## EWW

* <kbd>Alt</kbd>+<kbd>left</kbd> - Back a page.
* <kbd>Alt</kbd>+<kbd>right</kbd> - Forward a page.
* <kbd>f5</kbd> - Reload page.
* <kbd>Ctrl</kbd>+<kbd>f5</kbd> - Reload page.
* <kbd>f12</kbd> - View source.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>a</kbd> - List buffers.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>o</kbd> - List bookmarks.
* <kbd>Ctrl</kbd>+<kbd>h</kbd> - List histories.

## Image Mode

* <kbd>Ctrl</kbd>+<kbd>r</kbd> - Rotate image.
* <kbd>Ctrl</kbd>+<kbd>0</kbd> - Maximize image.
* <kbd>Ctrl</kbd>+<kbd>=</kbd> - Increase image size.
* <kbd>Ctrl</kbd>+<kbd>-</kbd> - Decrease image size.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Flip the image horizontally.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Flip the image vertically.

## Package Mode

* <kbd>i</kbd> - Select packages to install.
* <kbd>d</kbd> - Select packages to delete.
* <kbd>x</kbd> - Execute marked packages.
* <kbd>s</kbd> - Sort packages in order.
* <kbd>u</kbd> - Unmark/Mark pacakges.
* <kbd>Shift</kbd>+<kbd>u</kbd> - Upgrade all packages if available.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>r</kbd>, <kbd>m</kbd> - Auto removes unused packages.

## `tabulated-list` Mode

* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Widen column width.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Narrow column width.

## Org Mode

* <kbd>Shift</kbd>+<kbd>up</kbd> - Move the cursor `up` one-row table.
* <kbd>Shift</kbd>+<kbd>down</kbd> - Move the cursor `down` one-row table.
* <kbd>Shift</kbd>+<kbd>right</kbd> - Move the cursor `right` one column table.
* <kbd>Shift</kbd>+<kbd>left</kbd> - Move the cursor `left` one column table.

## C/C++/Objective-C Mode

* <kbd>f8</kbd> - Switch between header/source file.
* <kbd>Shift</kbd>+<kbd>f8</kbd> - Switch between the header/source file in the other window.

## Java Mode

* <kbd>f2</kbd> - Look up the javadoc in the browser. (javadoc-lookup)
* <kbd>Shift</kbd>+<kbd>f2</kbd> - Lookup javadoc in browser. (javadoc-lookup)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>o</kbd> - Organize imports java paths.

## Web Mode

* <kbd>Ctrl</kbd>+<kbd>Return</kbd> - Emment expand.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>o</kbd> - Start the httpd server for real-time editing website.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>p</kbd> - Start the httpd server for real-time editing website.

## CSS Mode

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>s</kbd> - Sort the CSS attributes between open/close parenthesis.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>d</kbd> - Sort the CSS attribute for the whole file.
