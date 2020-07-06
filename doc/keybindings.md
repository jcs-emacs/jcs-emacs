# List of key bindings

Here is the list of key bindins that defined in this configuration.

## Global

### Programming

* **Editing**

  * <kbd>Ctrl</kbd>+<kbd>x</kbd> - Cut
  * <kbd>Ctrl</kbd>+<kbd>c</kbd> - Copy
  * <kbd>Ctrl</kbd>+<kbd>v</kbd> - Paste
  * <kbd>Ctrl</kbd>+<kbd>z</kbd> - Undo
  * <kbd>Ctrl</kbd>+<kbd>y</kbd> - Redo
  * <kbd>TAB</kbd> - Mimic the normal text editor preset behavior.
  * <kbd>Ctrl</kbd>+<kbd>TAB</kbd> - Reindent block.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>Ctrl</kbd>+<kbd>r</kbd> - Rename the whole text with the same name.
  * <kbd>Ctrl</kbd>+<kbd>a</kbd> - Mark whole buffer. (Select All)
  * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>d</kbd> - Duplicate whole line.
  * <kbd>Ctrl</kbd>+<kbd>d</kbd> - Kill whole line.
  * <kbd>Ctrl</kbd>+<kbd>backspace</kbd> - Backspace word.
  * <kbd>Ctrl</kbd>+<kbd>delete</kbd> - Delete word.
  * <kbd>Alt</kbd>+<kbd>backspace</kbd> - Backspace word excluding capital letter.
  * <kbd>Alt</kbd>+<kbd>delete</kbd> - Delete word excluding capital letter.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>backspace</kbd> -
  Delete the same character at current cursor's position backward repeatedly
  util it meet different character.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>Shift</kbd>+<kbd>backspace</kbd> -
  Delete the same character at current cursor's position forward repeatedly
  util it meet different character.
  * <kbd>Alt</kbd>+<kbd>up</kbd> - Move current line `up`.
  * <kbd>Alt</kbd>+<kbd>down</kbd> - Move current line `down`.
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>up</kbd> - Scroll window `up` one line without moving cursor.
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>down</kbd> - Scroll window `down` one line without moving cursor.
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>left</kbd> - Move buffer `left`.
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>right</kbd> - Move buffer `right`.
  * <kbd>Ctrl</kbd>+<kbd>return</kbd> - Complete word or Goto address at point.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Shift</kbd>+<kbd>,</kbd> - Decrement tab size by 2.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Shift</kbd>+<kbd>.</kbd> - Increment tab size by 2.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>=</kbd> - Calcualte the region and replace with the result.
  * <kbd>Shift</kbd>+<kbd>f10</kbd> - Right click context menu.
  * <kbd>f12</kbd> - Goto declaration/definition.
  * <kbd>Shift</kbd>+<kbd>f12</kbd> - Goto declaration/definition other window.
  * <kbd>f7</kbd> - Open same file other window.
  * <kbd>f8</kbd> - Open corresponding file other window, if not found open same file instead.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>e</kbd> - Switch file coding system.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>b</kbd> - Open RE-Builder mode window.
  * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>r</kbd> - Reveal file in folder.

  * **Build / Run (Output)**

    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Build the program. (`build.linux`/`build.bat`/`build.mac`)
    * <kbd>f5</kbd> - Run the program. (`run.linux`/`run.bat`/`run.mac`)
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>u</kbd> - Show output window. (if any)
    * <kbd>Alt</kbd>+<kbd>o</kbd> - Switch to output buffer.
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Previous output buffer.
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Next output buffer.

  * **Search**

    * <kbd>Ctrl</kbd>+<kbd>f</kbd> - Search through file.
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Search through project.
    * <kbd>Ctrl</kbd>+<kbd>,</kbd> - Search through file current point. (backward)
    * <kbd>Ctrl</kbd>+<kbd>.</kbd> - Search through file current point. (forward)
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>,</kbd> - Search through project current point. (backward)
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>.</kbd> - Search through project current point. (forward)

  * **Todo**

    * <kbd>Ctrl</kbd>+<kbd>f10</kbd> - Goto `previous` highlighted todo related symbol.
    * <kbd>Ctrl</kbd>+<kbd>f11</kbd> - Goto `next` highlighted todo related symbol.

  * **Commenting / Uncommenting**

    * <kbd>Ctrl</kbd>+<kbd>/</kbd> - Comment/Uncomment region or line.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>c</kbd> - Comment region or line.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>u</kbd> - Uncomment region or line.

  * **Formating**

    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>f</kbd> - Format region.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>d</kbd> - Format document.

  * **Folding / Unfolding**

    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>0</kbd> - Collapse all foldings.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>j</kbd> - Expand all foldings.
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>[</kbd> - Collapse current folding.
    * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>]</kbd> - Expand current folding.

  * **Case**

    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>u</kbd> - Uppercase word or region.
    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>d</kbd> - Downcase word or region.
    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>c</kbd> - Capitalize word or region.

  * **Alignment**

    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>a</kbd> - Align region or document.

  * **Symbol**

    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>[</kbd> - Delete everything between `[` and `]`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>]</kbd> - Delete everything between `[` and `]`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>(</kbd> - Delete everything between `(` and `)`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>)</kbd> - Delete everything between `(` and `)`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>{</kbd> - Delete everything between `{` and `}`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>}</kbd> - Delete everything between `{` and `}`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>'</kbd> - Delete everything between two `'`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>"</kbd> - Delete everything between two `"`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd><</kbd> - Delete everything between `<` and `>`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>></kbd> - Delete everything between `>` and `<`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>`</kbd> - Delete everything between two ```.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>~</kbd> - Delete everything between two `~`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>@</kbd> - Delete everything between two `@`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>#</kbd> - Delete everything between two `#`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>$</kbd> - Delete everything between two `$`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>%</kbd> - Delete everything between two `%`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>^</kbd> - Delete everything between two `^`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>&</kbd> - Delete everything between two `&`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>*</kbd> - Delete everything between two `*`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>-</kbd> - Delete everything between two `-`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>_</kbd> - Delete everything between two `_`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>=</kbd> - Delete everything between two `=`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>+</kbd> - Delete everything between two `+`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>`\`</kbd> - Delete everything between two `\`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>|</kbd> - Delete everything between two `|`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>:</kbd> - Delete everything between two `:`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>;</kbd> - Delete everything between two `;`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>,</kbd> - Delete everything between two `,`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>.</kbd> - Delete everything between two `.`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>/</kbd> - Delete everything between two `/`.
    * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>i</kbd>, <kbd>?</kbd> - Delete everything between two `?`.

  * **Multiple Cursors**

    #### Marking

    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>up</kbd> - Mark previous line.
    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>down</kbd> - Mark next line.
    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Mark previous line similar to current line.
    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Mark next line similar to current line.

    #### Similarity

    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>=</kbd> - Increase the string distance level by `1`.
    * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>-</kbd> - Decrease the string distance level by `1`.

* **Debugging**

  * <kbd>f6</kbd> - Toggle `flycheck`.
  * <kbd>f9</kbd> - Goto first error.
  * <kbd>f10</kbd> - Goto previous error.
  * <kbd>f11</kbd> - Goto next error.

### Buffer

* **Buffer Menu**

  * <kbd>Alt</kbd>+<kbd>b</kbd> - Open buffer menu.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Open buffer menu other window.
  * <kbd>Ctrl</kbd>+<kbd>o</kbd> - Toggle diminish buffer mode.

* **Switch Buffer**

  #### Using buffer menu.

  * <kbd>Alt</kbd>+<kbd>b</kbd> - Switch to buffer in the current window.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>b</kbd> - Switch to buffer in the other window.

  #### Using Tab

  * <kbd>Ctrl</kbd>+<kbd>pg-up</kbd> - Switch to the previous buffer in queue.
  * <kbd>Ctrl</kbd>+<kbd>pg-down</kbd> - Switch to the next buffer in queue.

* **Kill Buffer**

  * <kbd>Alt</kbd>+<kbd>k</kbd> - Kill current buffer.

* **Save Buffer**

  * <kbd>Ctrl</kbd>+<kbd>s</kbd> - Untabify save file. [DEFAULT]
  * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>s</kbd> - Save all buffers to it's current major mode.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>s</kbd> - Reverse `tabify` or `untabify` save buffer.

* **Rename Buffer**

  * <kbd>Alt</kbd>+<kbd>f2</kbd> - Rename current file

* About **`*Scratch*`**

  * <kbd>Alt</kbd>+<kbd>s</kbd> - Open `*scratch*` buffer.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>s</kbd> - Open `*scratch*` buffer in other window.

* About **`*Message*`**

  * <kbd>Alt</kbd>+<kbd>m</kbd> - Open `*Message*` buffer.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>m</kbd> - Open `*Message*` buffer in other window.

* About **`*Dashboard*`**

  * <kbd>Alt</kbd>+<kbd>d</kbd> - Open `*dashboard*` buffer.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>d</kbd> - Open `*dashboard*` buffer in other window.

### File

* **File Explorer**

  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>l</kbd> - Toggle file explorer.
  * <kbd>Ctrl</kbd>+<kbd>b</kbd> - Toggle file explorer.

* **Find Files**

  * <kbd>Alt</kbd>+<kbd>f</kbd> - Find file in working directory to current window.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Find file in working directory to other window.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Alt</kbd>+<kbd>f</kbd> - Find file in project to current window.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>f</kbd> - Find file in project to other window.

### Navigating

* **Navigate Cursor**

  * <kbd>Ctrl</kbd>+<kbd>right</kbd> - Navigate a word `right`.
  * <kbd>Ctrl</kbd>+<kbd>left</kbd> - Navigate a word `left`.
  * <kbd>Alt</kbd>+<kbd>right</kbd> - Navigate a word `right` excluding capital letter.
  * <kbd>Alt</kbd>+<kbd>left</kbd> - Navigate a word `left` excluding capital letter.
  * <kbd>Alt</kbd>+<kbd>g</kbd>, <kbd>l</kbd> - Goto line number.
  * <kbd>Alt</kbd>+<kbd>g</kbd>, <kbd>c</kbd> - Goto character position.

* **Navigate Windows**

  * <kbd>Alt</kbd>+<kbd>w</kbd> - Switch to the `next` window.
  * <kbd>Alt</kbd>+<kbd>q</kbd> - Switch to the `previous` window.

  * **Vim like**

    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>up</kbd> - Switch to `up` window.
    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>down</kbd> - Switch to `down` window.
    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>right</kbd> - Switch to `right` window.
    * <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>left</kbd> - Switch to `left` window.

  * **Ace Windows**

    * <kbd>Alt</kbd>+<kbd>e</kbd> - Aced window. (advanced way to switch window)

  * **Other**

    * <kbd>Ctrl</kbd>+<kbd>1</kbd> - Select window 1.
    * <kbd>Ctrl</kbd>+<kbd>2</kbd> - Select window 2.
    * <kbd>Ctrl</kbd>+<kbd>3</kbd> - Select window 3.
    * <kbd>Ctrl</kbd>+<kbd>4</kbd> - Select window 4.
    * <kbd>Ctrl</kbd>+<kbd>5</kbd> - Select window 5.
    * <kbd>Ctrl</kbd>+<kbd>6</kbd> - Select window 6.
    * <kbd>Ctrl</kbd>+<kbd>7</kbd> - Select window 7.
    * <kbd>Ctrl</kbd>+<kbd>8</kbd> - Select window 8.
    * <kbd>Ctrl</kbd>+<kbd>9</kbd> - Select window 9.

* **Navigate Symbol**

  * <kbd>Alt</kbd>+<kbd>)</kbd> - Search forward next `)`.
  * <kbd>Alt</kbd>+<kbd>(</kbd> - Search backward next `(`.
  * <kbd>Alt</kbd>+<kbd>]</kbd> - Search forward next `]`.
  * <kbd>Alt</kbd>+<kbd>[</kbd> - Search backward next `[`.
  * <kbd>Alt</kbd>+<kbd>}</kbd> - Search forward next `}`.
  * <kbd>Alt</kbd>+<kbd>{</kbd> - Search backward next `{`.
  * <kbd>Alt</kbd>+<kbd>'</kbd> - Search forward next `'`. (single quotation mark)
  * <kbd>Alt</kbd>+<kbd>;</kbd> - Search backward next `'`. (single quotation mark)
  * <kbd>Alt</kbd>+<kbd>"</kbd> - Search forward next `"`. (double quotation mark)
  * <kbd>Alt</kbd>+<kbd>:</kbd> - Search backward next `"`. (double quotation mark)
  * <kbd>Alt</kbd>+<kbd>></kbd> - Search forward next `>`. (Greater than sign)
  * <kbd>Alt</kbd>+<kbd><</kbd> - Search backward next `<`. (Less than sign)
  * <kbd>Alt</kbd>+<kbd>.</kbd> - Search forward next `,`. (comma)
  * <kbd>Alt</kbd>+<kbd>,</kbd> - Search backward next `,`. (comma)
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>.</kbd> - Search forward next `.`. (period)
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>,</kbd> - Search backward next `.`. (period)

### Visualization

* <kbd>Ctrl</kbd>+<kbd>=</kbd> - Text scale increase (Zoom in).
* <kbd>Ctrl</kbd>+<kbd>-</kbd> - Text scale decrease (Zoom out).
* <kbd>Ctrl</kbd>+<kbd>0(Numpad)</kbd> - Reset zoom.
* <kbd>Alt</kbd>+<kbd>=</kbd> - Frame transparent increase.
* <kbd>Alt</kbd>+<kbd>-</kbd> - Frame transparent decrease.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>f</kbd> - Change font style.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>m</kbd> - Toggle minimap.

### Windows

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>n</kbd> - Create new frame.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>d</kbd> - Delete current frame.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>w</kbd> - Delete current window.
* <kbd>Ctrl</kbd>+<kbd>h</kbd>, <kbd>h</kbd> - Toggle window split between horizontal and vertcial.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>e</kbd> - Toggle enlarge current selected window.
* <kbd>Ctrl</kbd>+<kbd>\\</kbd> - Split window horizontally.
* <kbd>Alt</kbd>+<kbd>\`</kbd> - Toggle transparent frame.
* <kbd>Alt</kbd>+<kbd>=</kbd> - Increment frame transparent by 5 percent.
* <kbd>Alt</kbd>+<kbd>-</kbd> - Decrement frame transparent by 5 percent.

### Source Control

* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>g</kbd> - Open magit interface.

### Terminal / Shell

* <kbd>Ctrl</kbd>+<kbd>\`</kbd> - Toggle shell window.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>\`</kbd> - New shell process.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>=</kbd> - Previous shell buffer.
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>-</kbd> - Next shell buffer.

### Others

* **System**

  * Active minibuffer.
    - <kbd>Alt</kbd>+<kbd>x</kbd> - Active minibuffer.
    - <kbd>f1</kbd> - Active minibuffer.
    - <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>p</kbd> - Active minibuffer.
  * <kbd>Alt</kbd>+<kbd>f4</kbd> - Shutdown Emacs.
  * <kbd>Alt</kbd>+<kbd>f5</kbd> - Reload Emacs.
  * <kbd>Alt</kbd>+<kbd>f6</kbd> - Restart Emacs.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>,<kbd>Ctrl</kbd>+<kbd>s</kbd> - Describe key bindings with current `major-mode`.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>,<kbd>Ctrl</kbd>+<kbd>i</kbd> - Describe current symbol.
  * <kbd>Ctrl</kbd>+<kbd>t</kbd> - Toggle `tabbar-mode`.
  * <kbd>Alt</kbd>+<kbd>z</kbd> - Toggle `truncate-lines`.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>o</kbd> - Read-Only mode toggle.
  * <kbd>Ctrl</kbd>+<kbd>r</kbd>, <kbd>f</kbd> - Open recent files.
  * <kbd>Ctrl</kbd>+<kbd>Alt</kbd>+<kbd>m</kbd> - Toggle `mode-line`.
  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>\`</kbd> - Toggle `depend`/`cross` mode.

* **Exit**

  * <kbd>ESC</kbd> - Exit minibuffer and go to the top level.
  * <kbd>Ctrl</kbd>+<kbd>g</kbd> - Exit keyboard.

* **Mark**

  * <kbd>Ctrl</kbd>+<kbd>SPC</kbd> or <kbd>Alt</kbd>+<kbd>SPC</kbd> - Set mark at current point.
  * <kbd>Alt</kbd>+<kbd>Shift</kbd>+<kbd>;</kbd> - Pops back to the previous mark.

* **Package List**

  * <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>Ctrl</kbd>+<kbd>p</kbd> - Package list.
  * <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>x</kbd> - Package list.

* **Process**

  * <kbd>Alt</kbd>+<kbd>p</kbd> - Show list of process.

* **Describing**

  * <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>f</kbd> - Describe function.
  * <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>m</kbd> - Describe mode.
  * <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>v</kbd> - Describe variable.
  * <kbd>Alt</kbd>+<kbd>h</kbd>, <kbd>b</kbd> - Describe bindings.

* **Eval**

  * <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>b</kbd> - Eval buffer.
  * <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>d</kbd> - Eval defined expression.
  * <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>e</kbd> - Eval expression.
  * <kbd>Ctrl</kbd>+<kbd>e</kbd>, <kbd>r</kbd> - Eval region.

## Package Mode

* <kbd>i</kbd> - Select package to install.
* <kbd>d</kbd> - Select package to delete.
* <kbd>x</kbd> - Execute marked packages.
* <kbd>s</kbd> - Sort packages in order.
* <kbd>u</kbd> - Select package to upgrade.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>r</kbd>, <kbd>m</kbd> - Auto remove unused pacakges.

## Org Mode

* <kbd>Shift</kbd>+<kbd>up</kbd> - Move cursor `up` one row table.
* <kbd>Shift</kbd>+<kbd>down</kbd> - Move cursor `down` one row table.
* <kbd>Shift</kbd>+<kbd>right</kbd> - Move cursor `right` one column table.
* <kbd>Shift</kbd>+<kbd>left</kbd> - Move cursor `left` one column table.

## C/C++/Objective-C Mode

* <kbd>f8</kbd> - Switch between header/source file.
* <kbd>Shift</kbd>+<kbd>f8</kbd> - Switch between header/source file other window.

## Java Mode

* <kbd>f2</kbd> - Lookup javadoc in browser. (javadoc-lookup)
* <kbd>Shift</kbd>+<kbd>f2</kbd> - Lookup javadoc in browser. (javadoc-lookup)
* <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>o</kbd> - Organize imports java paths.

## Web Mode

* <kbd>Ctrl</kbd>+<kbd>Return</kbd> - Emment expand.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>o</kbd> - Start httpd server for real time editing website.
* <kbd>Ctrl</kbd>+<kbd>w</kbd>, <kbd>p</kbd> - Start httpd server for real time editing website.

## CSS Mode

* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>s</kbd> - Sort the CSS attributes between open/close parenthesis.
* <kbd>Ctrl</kbd>+<kbd>k</kbd>, <kbd>d</kbd> - Sort the CSS attribute for the whole file.
