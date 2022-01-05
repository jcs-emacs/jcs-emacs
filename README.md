<p align="center">
<img src="./etc/logo/logo-shadow.png" width="500" height="253"/>
</p>

<p align="center">
<a href="https://opensource.org/licenses/BSD-2-Clause"><img src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg" alt="License"></a>
<a href="https://www.gnu.org/software/emacs/download.html"><img src="https://img.shields.io/badge/Emacs-27.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white" alt="Emacs"></a>
<a href="https://github.com/jcs-emacs/jcs-emacs/releases/latest"><img src="https://img.shields.io/github/tag/jcs-emacs/jcs-emacs.svg?label=release&logo=github" alt="Release Tag"></a>
<a href="https://github.com/jcs-emacs/jcs-emacs/actions"><img src="https://github.com/jcs-emacs/jcs-emacs/workflows/CI/badge.svg" alt="CI"></a>
</p>

<p align="center">
<a href="#"><img src="https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue" alt="Windows"></a>
<a href="#"><img src="https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white" alt="macOS"></a>
<a href="#"><img src="https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black" alt="Linux"></a>
</p>

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [jcs-emacs](#jcs-emacs)
    - [Philosophy](#philosophy)
    - [📰 News](#📰-news)
    - [💾 Installation](#💾-installation)
    - [🕒 Startup Time](#🕒-startup-time)
    - [🏆 Features](#🏆-features)
        - [Highlight](#highlight)
        - [Details](#details)
    - [📁 Package Archives](#📁-package-archives)
    - [🏆 Powered by](#🏆-powered-by)
        - [Functionalities](#functionalities)
        - [File Modes](#file-modes)
    - [Key Bindings](#key-bindings)
    - [Themes](#themes)
        - [Default](#default)
        - [Customization](#customization)
    - [Font](#font)
    - [🔨 Optimization](#🔨-optimization)
    - [Write your own customization](#write-your-own-customization)
    - [🎍 Supported Emacs versions](#🎍-supported-emacs-versions)
    - [❓ FAQ](#❓-faq)
    - [Contribute](#contribute)

<!-- markdown-toc end -->

# jcs-emacs
> Consistent Emacs configuration for all platforms.

This is Jen-Chieh Shen's emacs configuration. This configuration
started from [Casey Muratori](https://github.com/cmuratori)'s
Emacs configuration. It has been repeatedly utilized and modified
since 2015 and is now very different to his configuration.

I wouldn't claim this is the best Emacs config, but this is the
best config to myself. If you wish to learn Emacs, this could be
a good start, because this configuration has been rewritten
basically from scratch. Hopefully, this config will help someone
who is new or struggle to Emacs. This configuration should work
on most OSs, indicates Windows, macOS, and Linux.

Emacs itself supports multiple programming languages. I managed
all the programming languages I personally used. I dabble in
numerous technologies, incorporate hardware, firmware and software.
Here is the list of programming languages I know and are extended
from this configuration.

* ActionScript 2.0 or 3.0 / Ada / Agda / AppleScript / Assembly Language
* BASIC / Batchfile
* C / C++ / C# / Clojure / CMake / COBOL / CSS
* Dart / Dockerfile
* Elixir / Elm / Emacs Lisp / Erlang
* F# / Fountain
* GLSL / Go / GDScript / Groovy
* Haskell / Haxe / HTML
* INI
* Java / JavaScript / JSON / JSX
* Kotlin
* LESS / Lisp / Lua
* Makefile / Markdown
* Nix
* Object Pascal (Delphi) / Objective-C
* Pascal / Perl / PHP / PowerShell / Properties / Python
* R / Ruby / Rust
* Sass / Scala / SCSS / Shader / Shell script / SQL / Swift
* TOML / TSX / TypeScript
* Verilog / Vimscript / Vue
* XML
* YAML

This configuration polished and goes toward to the modern
text editor, or even better, it goes beyond modern IDE.
e.g. [Atom](https://atom.io/), [Brackets](http://brackets.io/),
[Sublime Text 2](https://www.sublimetext.com/2) or [3](https://www.sublimetext.com/),
[Visual Studio Code](https://code.visualstudio.com/), etc.

## Philosophy

I have experienced many different kinds of IDEs and text editors. But I’ve had a
hard time finding the best tool to use. I’ve jumped from one working field to
another trying to find something that suits my needs. So instead of struggling
with the tool itself, I chose Emacs and configured the entire thing from scratch,
to suit my needs.

Here are a few goals that I want this config to accomplished.

* Having the same set of key bindings across different IDEs and text editors as
many as possible.
* Having the same font and theme across different OSs and environments.
* Automating trivial or redundant tasks.
* Improve user experiences approach to modern text editor or IDE.
* Make compatible to most features work inside terminal as well.

Having these implementations makes my life easier, and having a genuinely portable
workspace, which is great because it lets me work on different machine efficiently,
without having to get used to an new IDE.

## 📰 News

Here is the list of few important and recent changes to this configuration.

* `7.1.0` - Replace `quelpa` with `github-elpa`.
* `7.0.0` - Switch from [quickhelp](https://github.com/company-mode/company-quickhelp) to [box](https://github.com/sebastiencs/company-box) for company frontend.
* `6.5.0` - Add pinned archive feature to package module.
* `6.4.2` - You can now resolve package dependency graph while deleting package.
* `6.4.1` - Start with `tree-sitter` support.
* `6.4.0` - Switched from `projectile` to built-in `project` for project management.
* `6.3.1` - Replaced `docstring` module to external `docstr` package.
* `6.2.5` - Use `recipes` folder instead to specify manually installed packages in the configuration.
* `6.2.4` - Replaced [ag](https://github.com/Wilfred/ag.el) silver searcher to [searcher](https://github.com/jcs-elpa/searcher).
* `6.2.2` - Correct upgrade logic for manually installed packages.

## 💾 Installation

To install, clone this repo and copy the core `files`/`directories` to the
emacs config directory accordingly. Make sure you backup your own configuration
before you start the installation.

```bash
git clone https://github.com/jcs-emacs/jcs-emacs ~/.emacs.d
```

Then startup Emacs; it will automatically installs all necessary
packages due to this configuration.

**❗❗ [ATTENTION] ❗❗ -- MAKE SURE YOU INSTALLED THE CORRECT VERSION OF EMACS!**

## 🕒 Startup Time

The average startup time for this configuration is around `15` to `25` seconds.
You can use command `emacs-init-time` to check the startup time on your machine.
Not quite sure what causes that much of performance, hopefully, I'm able to lower
the startup time down to `5` to `15` seconds.

**Edit 1:** After version `5.3.2`, the average startup time is around `5` to `15`
seconds. Solved this issue by removing unnecessary `require` keyword load file
and use `:defer` keyword with `use-package` package to delay some packages
load time.

📝 *P.S. Here is a great article about
[Speeding Up Emacs](https://anuragpeshne.github.io/essays/emacsSpeed.html)
written by
[Anurag Peshne](https://github.com/anuragpeshne).*

**Edit 2:** If you compiled the source code then the startup time can lower down
more from `0.5` to `1.5` seconds.

**Edit 3:** Using [esup](https://github.com/jschaf/esup) package to test and
optimize the configuration. Call `package-refresh-contents` only when package
installation is needed. By doing thing, lower the startup time from around `4` to
`8` seconds.

📝 *P.S. Some good hints from one [StackExchange](https://emacs.stackexchange.com/)
question,
[What can I do to speed up my start-up?](https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up)
answered by
[Jordon Biondo](https://github.com/jordonbiondo).*

📝 *P.S. The above cases are tested on Windows. Other OS that are not Windows should
start up under a second.*

## 🏆 Features

### Highlight

Here is the list of the major highlights to this configuration. Hope you would
like my taste!

* **Out of the box** - Out of the box anywhere.
* **Cross Platform** - Work on all operating system including terminal.
* **Fast Startup** - Lazy loading for all unnecessary packages on startup.
* **Old-Fashioned** - Doesn't use any beatiful GUI because it may not work in terminal.
* **Multiple Languages** - Support multiple programming languages.
* **Consistent** - Having the same coding experience in different major mode.
* **Easy to use** - Design close to other modern text editors. Shouldn't spend you too much time.
* **Dual Windows** - Design to people who like multiple windows opened simultaneously.
* **Keyboard Focused** - You can do everything by using keyboard and mouse are just optional.

### Details

This is the list of features that are built-in to this configuration. These features
are heavily base on my personal habits, and so these could be very tiny things. But
I believed detials make things better and make life smoother.

* [*Auto Install Package*](./docs/features/auto-install-package) - Automatically installs the package that this config relies on.
* [*Better Dashboard*](./docs/features/better-dashboard) - Implementation for improving experiences using dashboard.
* [*Buffer Menu Search*](./docs/features/buffer-menu-search) - Able to search in the `*Buffer List*` buffer.
* [*Build Run*](./docs/features/build-run) - Implementation for executing script for building and running the software.
* [*Capital Word*](./docs/features/capital-word) - You can navigate/kill word by seeing capital letter.
* [*Changelog Helper*](./docs/features/changelog-helper) - Help to create changelog while creating `CHANGELOG` file using template.
* [*Consistent Key Bindings*](./docs/features/consistent-key-bindings) - Has consistent key bindings across all modes.
* [*Curly Bracket Modes*](./curly-bracket-modes) - Use curly bracket depends on different mode.
* [*Display File*](./docs/features/display-file) - Utility function to use to view a file on the other window.
* [*Charset Table*](./docs/features/display-file/charset-table) - Built-in functions that displays character sets.
* ~~[*Docstring Completion*](./docs/features/docstring-completion) - Some standard docstring completion implementations built-in to this configuration.~~ (has move to [docstr](https://github.com/jcs-elpa/docstr))
* Enhanced Multiple Cursors
  * [*Similar Multiple Cursors*](./docs/features/similar-multiple-cursors) - Mark with cursor by similarity.
  * [*VS Multiple Cursors*](./docs/features/vs-multiple-cursors) - VSCode like methods to `mark` and `unmark` multiple cursor.
* [*Fast Incremental Search*](./docs/features/fast-incremental-search) - Fast keys for incremental search forward/backward to the cursor is currently pointing.
* [*Feebleline Design*](./docs/features/feebleline-design) - Personal displayed design for `feebleline`.
* [*Indent Control*](./docs/features/indent-control) - Generic control the indentation level for each mode, for more information see [indent-control](https://github.com/alt-elpa/indent-control).
* Previous/Next Keys
  * [*Normal Move*]() - Act like other normal editors.
  * ~~[*Indent Move*](./docs/features/indent-move) - Automatically indent when cursor moves up and down.~~
  * [*Smart-Move*](./docs/features/smart-move) - Smart enough to move cursor to the beginning of the line.
* [*Line Numbers Modding*](./docs/features/line-numbers-modding) - Mixed used of `linum` and `display-line-numbers-mode` base on the file usage.
* [*License Helper*](./docs/features/license-helper) - Help to create license while creating `LICENSE` file using template.
* [*Mini State*](./docs/features/mini-state) - Mini mode state use to visually see what backend is the config currently running. (Deprecated)
  * [*Explicit States*](./docs/features/mini-state/explicit-states) - Automatcially switch mini state depends on certain circumstances.
* ~~[*Mode Line Toggle*](./docs/features/mode-line-toggle) - Toggle to `show` or `hide` the mode line.~~
* [*Modern Text Editor*](./docs/features/modern-text-editor) - Design to have the preset settings close to modern text editors but faster.
* [*Multiple Output*](./docs/features/multiple-output) - Handle multiple output/compilation buffers.
* [*Navigate Blank Line*](./docs/features/navigate-blank-line) - Use `C-<up>` and `C-<down>` to navigate previous and next blank line.
* [*Navigate Table*](./docs/features/navigate-table) - Navigate `org-mode`'s table easier by using arrow keys.
* [*Preview HTML*](./docs/features/preview-html) - Preview rendered HTML file on the other window.
* [*Simulate Shell*](./docs/features/simulate-shell) - Completely simulate shell behaviors, make better user experience when using shell in Emacs.
* [*Switch Window*](./docs/features/switch-window) - Fast keys switch between windows quickly.
* [*Tabify/Untabify Modes*](./docs/features/tabify-untabify-modes) - Tabify or Untabify the file depends on the mode you currently in.
* ~~[*Transparent Window*](./docs/features/transparent-window) - Keys to increase/decrease the transparency of the frame.~~ (has move to [transwin](https://github.com/jcs-elpa/transwin))
* [*Trim Trailing Whitespace*](./docs/features/trim-trailing-whitespace) - Remove trailing spaces and tabs automatically on save.
* ~~[*Video Player*](./docs/features/video-player) - Play media on the top window.~~
* [*Visualize Undo/Redo*](./docs/features/visualize-undo-redo) - Improved the user experience on undoing and redoing by showing the `undo-tree-visualizer` at the other window.
* [*VS Curly Bracket*](./docs/features/vs-curly-bracket) - Visual Studio IDE like curly bracket implementation.
* [*VS Multiple Terminal*](./docs/features/vs-multiple-terminal) - VSCode like multiple shell control.
* [*VS Navigate Word*](./docs/features/vs-navigate-word) - Visual Studio IDE like navigating between word implementation.

## 📁 Package Archives

A list of package archives that this configuration uses.

* *[ELPA](https://elpa.gnu.org/packages/)* - Emacs Lisp Package Archive
* *[MELPA](https://melpa.org/#/)* - Milkypostman’s Emacs Lisp Package Archive
* *[elpa (jcs)](https://github.com/jcs-emacs/elpa)* - Emacs Lisp Package Archive for this configuration

📝 *P.S. Here is a good talk comparing all package archives from a
[StackExchange](https://emacs.stackexchange.com/) question,
[What are the practical differences between the various Emacs Package Repositories?](https://emacs.stackexchange.com/questions/268/what-are-the-practical-differences-between-the-various-emacs-package-repositorie)
answered by
[Tikhon Jelvis](https://github.com/TikhonJelvis).*

## 🏆 Powered by

Here is the list of all packages that powered and make this configuration works.
Thanks to all the package maintainers; this configuration cannot be made without
them, and if you wish to support them you can go to this
[elisp-maintainers](https://github.com/tarsius/elisp-maintainers)
repo/site and search for the maintainer you want to support. There should be some
kind of methods that you could support the maintainer you want.

### Functionalities

* *Abbreivation Definition* - powered by [project-abbrev](https://github.com/elpa-host/project-abbrev).
* *Alt-Codes* - powered by [alt-codes](https://github.com/elpa-host/alt-codes).
* *Auto Completion* - powered by [company](https://github.com/company-mode/company-mode).
* *Auto Highlight Symbol* - powered by [auto-highlight-symbol-mode](https://github.com/mhayashi1120/auto-highlight-symbol-mode).
* *Banner* - powered by [dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
* *Binary/Hex Editor* - powered by [nhexl-mode](https://github.com/emacsmirror/nhexl-mode).
* *Collaborative Editing* - powered by [togetherly](https://github.com/zk-phi/togetherly).
* *Completion Frontend* - powered by [ivy](https://github.com/abo-abo/swiper).
* *Context Menu* - powered by [right-click-context](https://github.com/zonuexe/right-click-context).
* *Document String* - powered by [docstr](https://github.com/jcs-elpa/docstr).
* *End of Line* - powered by [show-eol](https://github.com/elpa-host/show-eol).
* *Execute Commands* - powered by [compile](https://www.emacswiki.org/emacs/CompilationMode).
* *File Explorer* - powered by [treemacs](https://github.com/Alexander-Miller/treemacs).
* *File Header* - powered by [file-header](https://github.com/alt-elpa/file-header).
* *Folding/Unfolding* - powered by [ts-fold](https://github.com/jcs090218/ts-fold).
* *Font* - powered by [use-ttf](https://github.com/elpa-host/use-ttf).
* *Goto Declaration/Definition* - powered by [dumb-jump](https://github.com/jacktasia/dumb-jump).
* *Highlight Indentation* - powered by [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides).
* *Highlight Matched Pairs* - powered by [show-paren-mode](https://www.emacswiki.org/emacs/ShowParenMode).
* *Highlight Same Region* - powered by [region-occurrences-highlighter](https://github.com/alvarogonzalezsotillo/region-occurrences-highlighter).
* *Indentation Management* - powered by [indent-control](https://github.com/alt-elpa/indent-control).
* *Language Server Protocol* - powered by [lsp-mode](https://github.com/emacs-lsp/lsp-mode).
* *Line Annotation* - powered by [line-reminder](https://github.com/elpa-host/line-reminder).
* *Line Numbers* - powered by [display-line-numbers](https://github.com/emacs-mirror/emacs/blob/master/lisp/display-line-numbers.el) and [linum](https://github.com/emacs-mirror/emacs/blob/master/lisp/linum.el).
* *Minimap* - none, originally powered by ~~[sublimity](https://github.com/zk-phi/sublimity)~~.
* *Mode Line* - powered by [powerline](https://github.com/milkypostman/powerline).
* *Multiple Cursor* - powered by [iedit](https://github.com/victorhge/iedit) and [multiple-cursors](https://github.com/magnars/multiple-cursors.el).
* *Multiple Terminal* - powered by [multi-shell](https://github.com/jcs-elpa/multi-shell).
* *Music Player* - none.
* *Navigation/Searcher* - powered by [searcher](https://github.com/jcs-elpa/searcher) and [isearch-project](https://github.com/elpa-host/isearch-project).
* *Package Archive* - see [here](#file_folder-package-archives).
* *Package Management* - powered by [leaf](https://github.com/conao3/leaf.el) and [github-elpa](https://github.com/10sr/github-elpa).
* *PDF Viewer* - powered by [doc-view-mode](https://www.emacswiki.org/emacs/DocViewMode) and [ghostscript](https://www.ghostscript.com/index.html).
* *Project Management* - powered by [project](https://elpa.gnu.org/packages/project.html).
* *Recent Files* - powered by [recentf](https://www.emacswiki.org/emacs/RecentFiles).
* *Regexp* - powered by [re-builder](https://www.emacswiki.org/emacs/ReBuilder) and [visual-regexp](https://github.com/benma/visual-regexp.el).
* *Restart Emacs* - powered by [restart-emacs](https://github.com/iqbalansari/restart-emacs).
* *Scroll Bar* - powered by [yascroll](https://github.com/m2ym/yascroll-el).
* *Shell* - powered by [shell](https://www.emacswiki.org/emacs/ShellMode) and [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).
* *Smooth Scrolling* - none, originally powered by ~~[sublimity](https://github.com/zk-phi/sublimity)~~.
* *Snippet* - powered by [yasnippet](https://github.com/joaotavora/yasnippet).
* *Source Control Management* - powered by [magit](https://github.com/magit/magit).
* *Startup Screen* - powered by [dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
* *Syntax Check* - powered by [flycheck](http://www.flycheck.org/en/latest/).
* *Tab Bar* - powered by [centaur-tabs](https://github.com/ema2159/centaur-tabs).
* ~~*Tags* - powered by [gtags](https://www.gnu.org/software/global/).~~
* *Text Editing* - powered by [org-mode](https://orgmode.org/).
* *Theme* - powered by [vs-dark-theme](https://github.com/jcs-elpa/vs-dark-theme) and [vs-light-theme](https://github.com/jcs-elpa/vs-light-theme). **(For more info see [here](#themes))**
* *Todo* - powered by [hl-todo](https://github.com/tarsius/hl-todo).
* *Toggle Mode Line* - powered by [feebleline](https://github.com/tautologyclub/feebleline).
* *Undo/Redo* - powered by [undo-tree](https://www.emacswiki.org/emacs/UndoTree).
* *Video Player* - none.
* *White Space* - powered by [whitespace](https://www.emacswiki.org/emacs/WhiteSpace).

### File Modes

If you want to change the default mode to certain file type, you might want to
checkout the `./.emacs.jcs/jcs-mode.el` file. You will see a list of mode
that are opened by default mode to certain extension.

* [List of modes](./doc/programming_modes.md)

📝 *P.S. The [awesome-emacs](https://github.com/emacs-tw/awesome-emacs)
is a list of Emacs package that you can choose other alternatives to replace any
similar packages listed here. Is also a good place to seek and learn what's inside
Emacs!*

## Key Bindings

This configuration have all modes bind to the same set of key bindings. It benefits
the developer would not need to change their key bindings while after the mode
swichted. The key bindings set can be modified in `./.emacs.jcs/jcs-key.el` file.

* [List of key bindings](./doc/keybindings.md)

📝 P.S. * My work requires me to use
[Visual Studio IDE](https://visualstudio.microsoft.com/)
that being said the key bindings set are most likely compatible to
[Visual Studio IDE](https://visualstudio.microsoft.com/).
* ~~Excepts keys that bind to `C-x` and `C-c`, Emacs are deeply binds to these
two keys, and many packages also use these two keys for there preset keys. As
you may know these two keys are often `cut` and `copy`, is awkward that I solved
this by adding the same key stroke once again, hence the `cut` key is
`C-x C-x` and the `copy` key is `C-c C-c`.~~

**Edit 1:** After version `5.8.3`, the `cut` and `copy` keys had been corrected
and no longer the hassle to this configuration. Thanks to `use-package` again,
letting me bind `C-x` and `C-c` keys for higher precedence.

## Themes

### Default

The theme was to design close to [Visual Studio IDE](https://visualstudio.microsoft.com/)
preset light/dark theme. I believed [Microsoft](https://www.microsoft.com/zh-tw/)
has a great UI/UX team, since I don't have any experience or work related to UI/UX,
I would just like to have the theme color as close to it as possible. Anyway, if you
want to customize the theme yourself, then check out the `./.emacs.jcs/jcs-theme.el`
file. All the theme related variables can be found in that file.

| Light Theme                                   | Dark Theme                                   |
|:----------------------------------------------|:---------------------------------------------|
|<img src="./etc/screenshot/startup-light.png"/>|<img src="./etc/screenshot/startup-dark.png"/>|

### Customization

In common, you can install any theme you want as long it is available on the
internet. You can check out the Emacs Theme Gallery site [here](https://pawelbx.github.io/emacs-theme-gallery/).
For instance, [vs-dark-theme](https://github.com/jcs-elpa/vs-dark-theme) and
[vs-light-theme](https://github.com/jcs-elpa/vs-light-theme) are now individual
packages built outside from this configuration.

## Font

Font uses `use-ttf` package to keep cross OS consistency. The default font is
`Ubuntu Mono` and loaded by using `UbuntuMono-R.ttf` located under
`./.emacs.jcs/fonts/` folder. If you don't like the this font, you can add your
own `.ttf` file and add the path to `use-ttf-default-ttf-fonts` list.
Lastly, set the name of the `.ttf` file to `use-ttf-default-ttf-font-name`
variable.

📝
P.S.
* See the file `./.emacs.jcs/jcs-plugin.el` to see how the font is been set
in `(leaf use-ttf)` section.
* For more details about the font settings in this configuration, check out the
`use-ttf`package [repo](https://github.com/elpa-host/use-ttf).

## 🔨 Optimization

If you would like to optimize the configuration, you can run the following command
compile all the source code to byte code so Emacs can run faster during both
`initial time` and `run time`.

```bash
# change directory to the `jcs` config directory
$ cd ./.emacs.jcs

# compile all the config source code
$ emacs --batch --eval "(byte-recompile-directory \"./\" 0)"
```

## Write your own customization

You are able to customize the configuration by editing the file locate at
`~/.emacs.jcs/jcs-config.el`. You should put all your own customize code there,
then other than that are the core files.

## 🎍 Supported Emacs versions

The config should run on Emacs **27.1** or higher, but I will recommend to always
run on the latest Emacs version available on your machine. The ultimate goal is to
design to have each version of config can run on their each according Emacs version
base on the version what I'm currently running on my present machine. For each
version record, you can check the [version_record](./version_record.txt) file at
the root of the project directory.

## ❓ FAQ

Here is the list of *Frequently Asked Questions*.

* [List of FAQ](./doc/FAQ.md)

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)

If you would like to contribute to this project, you may either clone and make pull
requests to this repository. Or you can clone the project and establish your own
branch of this tool. Any methods are welcome!
