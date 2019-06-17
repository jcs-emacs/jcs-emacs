<p align="center">
  <img src="./screenshot/logo-shadow.png" width="500" height="253"/>
</p>

<p align="center">
  <a href="https://travis-ci.com/jcs090218/jcs-emacs-init"><img src="https://travis-ci.com/jcs090218/jcs-emacs-init.svg?branch=master" alt="Build Status"></a>
  <a href="https://www.gnu.org/software/emacs/download.html"><img src="https://img.shields.io/badge/Emacs-26.2-blue.svg" alt="Emacs"></a>
  <a href="https://github.com/jcs090218/jcs-emacs-init/releases/latest"><img src="https://img.shields.io/github/tag/jcs090218/jcs-emacs-init.svg?label=release" alt="Release Tag"></a>
  <a href="https://opensource.org/licenses/BSD-2-Clause"><img src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg" alt="License"></a>
  <a href="https://www.paypal.me/jcs090218"><img alt="Support me" src="https://img.shields.io/badge/Support%20Me-%F0%9F%92%97-ff69b4.svg"></a>
</p>


# jcs-emacs-init
> An Emacs configuration bundle.

This is Jen-Chieh Shen's emacs configuration. Repeatedly utilized 
and modified since 2015. Hopefully this will help someone who is 
new or struggling with Emacs. This configuration should works on 
most OSs, indicates Windows, Linux and MacOS.

Emacs itself supports multiple programming languages. I managed 
all the programming languages I personally used. I dabble in 
numerous technologies, incorporate hardware, firmware and software. 
Here is the list of programming languages I know and are extended 
from this configuration.

* ActionScript 2.0 or 3.0 / Assembly Language
* BASIC / Batchfile
* C / C++ / C# / Clojure / CMake / COBOL / CSS
* Dart
* Elixir / Emacs Lisp / Erlang
* GLSL / Go
* Haskell / Haxe / HTML
* INI
* JSON / Java / JavaScript
* Lisp / Lua
* Makefile / Markdown
* Object Pascal (Delphi) / Objective-C
* Pascal / Perl / PHP / Properties / Python
* Ruby / Rust
* Sass / Scala / SCSS / Shader / Shell script / SQL / Swift
* Text / TypeScript
* Verilog / Vim script
* XML
* YAML

This configuration polished and goes toward to the modern 
text editor, or even better, it goes beyond modern IDE. 
e.g. [Atom](https://atom.io/), [Brackets](http://brackets.io/), 
[Sublime Text 2](https://www.sublimetext.com/2) or [3](https://www.sublimetext.com/), 
[Visual Studio Code](https://code.visualstudio.com/), etc. 


## Philosophy

I have experienced many different kinds of IDEs and text 
editors. But I’ve had a hard time finding the best tool 
to use. I’ve jumped from one working field to another 
trying to find something that suits my needs. So instead 
of struggling with the tool itself, I chose Emacs and 
configured the entire thing from scratch, to suit my needs.

Here are a few goals that I want this config to 
accomplished.

* Having the same set of key bindings across different 
IDEs and text editors as many as possible.
* Having the same font and theme across different OSs and 
environments.
* Automating trivial or redundant tasks.
* Improve user experiences approach to modern text editor 
or IDE.
* Make compatible to most features work inside terminal 
as well.

Having these implementations makes my life easier, and 
having a genuinely portable workspace, which is great 
because it lets me work on different machine efficiently, 
without having to get used to an new IDE.


## Startup Time

The average startup time for this configuration is around 
`15` to `25` seconds. You can use command `emacs-init-time` 
to check the startup time on your machine. Not quite sure what 
causes that much of performance, hopefully, I'm able to lower 
the startup time down to `5` to `15` seconds.

**Edit 1:** After version `5.3.2`, the average startup time is 
around `5` to `15` seconds. Solved this issue by removing 
unnecessary `require` keyword load file and use `:defer` keyword 
with `use-package` package to delay some packages load time.

*P.S. Here is a great article about 
[Speeding Up Emacs](https://anuragpeshne.github.io/essays/emacsSpeed.html)
 written by 
[Anurag Peshne](https://github.com/anuragpeshne).*

**Edit 2:** If you compiled the source code then the startup 
time can lower down more from `0.5` to `1.5` seconds.

**Edit 3:** Using [esup](https://github.com/jschaf/esup) package 
to test and optimize the configuration. Call `package-refresh-contents` 
only when package installation is needed. By doing thing, lower 
the startup time from around `4` to `8` seconds.

*P.S. Some good hints from one 
[StackExchange](https://emacs.stackexchange.com/) question, 
[What can I do to speed up my start-up?](https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up) 
answered by 
[Jordon Biondo](https://rubygems.pkg.github.com/jordonbiondo).*

**Edit 4:** Moved all modes enabling configuration to 
`after-init-hook`. This lower the startup time from `2` to 
`6` seconds.


## Features

This is the list of features that are built-in to this 
configuration. These features are heavily base on my personal 
habits, and so these could be very tiny things. But I believed 
detials make things better and make life smoother.

* [*Auto Install Package*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/auto-install-package) - 
Automatically installs the package that this config relies on.
* [*Auto Truncate Lines*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/auto-truncate-lines) - 
Automatically enable/disable `truncate-lines-mode` depends 
on certain situation. (`web-mode` only)
* [*Build Run*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/build-run) - 
Implementation for executing script for building and running 
the software.
* [*Capital Word*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/capital-word) - 
You can navigate/kill word by seeing capital letter.
* [*Consistent Key Bindings*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/consistent-key-bindings) - 
Has consistent key bindings across all modes.
* [*Curly Bracket Modes*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/curly-bracket-modes) - 
Use curly bracket depends on different mode.
* [*Docstring Completion*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/docstring-completion) - 
Some standard docstring completion implementations built-in 
to this configuration.
* [*Fast Incremental Search*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/fast-incremental-search) - 
Fast keys for incremental search forward/backward 
to the cursor is currently pointing.
* [*Indent Move*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/indent-move) - 
Automatically indent when cursor moves up and down.
* [*Line Numbers Modding*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/line-numbers-modding) - 
Mixed used of `linum` and `display-line-numbers-mode` 
base on the file usage.
* [*Mini State*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/mini-state) - 
Mini mode state use to visually see what backend is the 
config currently running.
* [*Mode Line Toggle*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/mode-line-toggle) - 
Toggle to `show` or `hide` the mode line.
* [*Modern Text Editor*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/modern-text-editor) - 
Design to have the preset settings close to modern text 
editors but faster.
* [*Navigate Blank Line*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/navigate-blank-line) - 
Use `C-<up>` and `C-<down>` to navigate previous and next 
blank line.
* [*Navigate Table*](https://github.com/jcs090218/jcs-emacs-init/tree/master/features/navigate-table) - 
Navigate `org-mode`'s table easier by using arrow keys.
* [*Simulate Shell*](https://github.com/jcs090218/jcs-emacs-init/tree/master/features/simulate-shell) - 
Completely simulate shell behaviors, make better user 
experience when using shell in Emacs.
* [*Switch Window*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/switch-window) - 
Fast keys switch between windows quickly.
* [*Tabify/Untabify Modes*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/tabify-untabify-modes) -
Tabify or Untabify the file depends on the mode you 
currently in.
* [*Trim Trailing Whitespace*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/trim-trailing-whitespace) - 
Remove trailing spaces and tabs automatically on save.
* [*Visualize Undo/Redo*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/visualize-undo-redo) - 
Improved the user experience on undoing and redoing by 
showing the `undo-tree-visualizer` at the other window.
* [*VS Curly Bracket*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/vs-curly-bracket) - 
Visual Studio IDE like curly bracket implementation.
* [*VS Multiple Cursors*](https://github.com/jcs090218/jcs-emacs-init/blob/master/features/vs-multiple-cursors) - 
VSCode like methods to `mark` and `unmark` multiple cursor.


## Package Archives

A list of package archives that this configuration uses.

* *[elpa](https://elpa.gnu.org/packages/)* - 
The default package repository for GNU Emacs.
* ~~*[marmalade](https://marmalade-repo.org/)* - 
Marmalade is an ELPA compatible package repository that allowed 
users to upload their own packages.~~
* *[melpa](https://melpa.org/#/)* - 
MELPA is a growing collection of `package.el`-compatible Emacs Lisp
packages built automatically on our server from the upstream source
code using simple recipes.


## Powered by

Here is the list of all packages that powered and make this 
configuration works. Thanks to all the package maintainers; 
this configuration cannot be made without them, and if you 
wish to support them you can go to this 
[elisp-maintainers](https://github.com/tarsius/elisp-maintainers) 
repo/site and search for the maintainer you want to support. 
There should be some kind of methods that you could support the 
maintainer you want.

### Functionalities

* *Abbreivation Definition* - powered by 
[project-abbrev](https://github.com/elpa-host/project-abbrev).
* *Auto Completion* - powered by 
[company](https://github.com/company-mode/company-mode).
* *Auto Highlight Symbol* - powered by 
[auto-highlight-symbol-mode](https://github.com/mhayashi1120/auto-highlight-symbol-mode).
* *Banner* - powered by 
[dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
* *Binary/Hex Editor* - powered by 
[nhexl-mode](https://github.com/emacsmirror/nhexl-mode).
* *Collaborative Editing* - powered by 
[togetherly](https://github.com/zk-phi/togetherly).
* *Context Menu* - powered by 
[right-click-context](https://github.com/zonuexe/right-click-context).
* *Docstring* - none, built-in to this configuration.
* *End of Line* - powered by 
[show-eol](https://github.com/jcs090218/show-eol).
* *Execute Commands* - powered by 
[compile](https://www.emacswiki.org/emacs/CompilationMode).
* *File Explorer* - powered by 
[sr-speedbar](http://cedet.sourceforge.net/speedbar.shtml).
* *File Header* - powered by 
[file-header](https://github.com/alt-elpa/file-header).
* *Folding/Unfolding* - powered by 
[origami](https://github.com/gregsexton/origami.el).
* *Font* - powered by 
[use-ttf](https://github.com/elpa-host/use-ttf).
* *Goto Char* - powered by 
[goto-char-preview](https://github.com/elpa-host/goto-char-preview).
* *Goto Line* - powered by 
[goto-line-preview](https://github.com/elpa-host/goto-line-preview).
* *Highlight Matched Pairs* - powered by 
[show-paren-mode](https://www.emacswiki.org/emacs/ShowParenMode).
* *Highlight Same Region* - powered by 
[region-occurrences-highlighter](https://github.com/alvarogonzalezsotillo/region-occurrences-highlighter).
* *Line Annotation* - powered by 
[line-reminder](https://github.com/elpa-host/line-reminder).
* *Line Numbers* - powered by 
[display-line-numbers](https://github.com/emacs-mirror/emacs/blob/master/lisp/display-line-numbers.el)
and 
[linum](https://github.com/emacs-mirror/emacs/blob/master/lisp/linum.el).
* *Minimap* - powered by 
[sublimity](https://github.com/zk-phi/sublimity).
* *Mode Line* - powered by 
[powerline](https://github.com/milkypostman/powerline).
* *Multiple Cursor* - powered by 
[iedit](https://github.com/victorhge/iedit)
and 
[multiple-cursors](https://github.com/magnars/multiple-cursors.el).
* *Navigation/Searcher* - powered by 
[ag](https://github.com/Wilfred/ag.el)
and 
[isearch-project](https://github.com/elpa-host/isearch-project).
* *Package Archive* - powered by 
[gnu](https://elpa.gnu.org/) 
and 
[melpa](http://melpa.org/),
* *Package Management* - powered by 
[use-package](https://github.com/jwiegley/use-package).
* *PDF Viewer* - powered by 
[doc-view-mode](https://www.emacswiki.org/emacs/DocViewMode)
and 
[ghostscript](https://www.ghostscript.com/index.html).
* *Project Search* - powered by 
[projectile](https://github.com/bbatsov/projectile).
* *Recent Files* - powered by 
[recentf](https://www.emacswiki.org/emacs/RecentFiles).
* *Regexp* - powered by 
[re-builder](https://www.emacswiki.org/emacs/ReBuilder).
* *Reload Emacs* - powered by 
[reload-emacs](https://github.com/alt-elpa/reload-emacs).
* *Restart Emacs* - powered by 
[restart-emacs](https://github.com/iqbalansari/restart-emacs).
* *Scroll Bar* - powered by 
[yascroll](https://github.com/m2ym/yascroll-el).
* *Shell* - powered by 
[shell](https://www.emacswiki.org/emacs/ShellMode) and 
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).
* *Simplify Usage* - powered by 
[helm](https://github.com/emacs-helm/helm).
* *Smooth Scrolling* - powered by 
[sublimity](https://github.com/zk-phi/sublimity).
* *Snippet* - powered by 
[yasnippet](https://github.com/joaotavora/yasnippet).
* *Source Control Management* - powered by 
[magit](https://github.com/magit/magit).
* *Startup Screen* - powered by 
[dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
* *Syntax Check* - powered by 
[flycheck](http://www.flycheck.org/en/latest/).
* *Tab Bar* - powered by 
[tabber](https://github.com/dholm/tabbar).
* *Tags* - powered by 
[gtags](https://www.gnu.org/software/global/).
* *Text Editing* - powered by 
[org-mode](https://orgmode.org/).
* *Theme* - none, self customized but close to 
[Visual Studio IDE](https://visualstudio.microsoft.com/)'s themes.
* *Todo* - powered by 
[hl-todo](https://github.com/tarsius/hl-todo).
* *Undo/Redo* - powered by 
[undo-tree](https://www.emacswiki.org/emacs/UndoTree).
* *White Space* - powered by 
[whitespace](https://www.emacswiki.org/emacs/WhiteSpace).

### File Modes

If you want to change the default mode to certain file type, 
you might want to checkout the `./.emacs.jcs/jcs-mode.el` file. 
You will see a list of mode that are opened by default mode to 
certain extension.

#### Programming Modes

| Language | Mode | Description | Version |
| --- | --- | --- | --- |
| ActionScript 2.0 or 3.0 | [actionscript-mode](https://github.com/austinhaas/actionscript-mode) | A simple mode for editing Actionscript 3 files | [![MELPA](https://melpa.org/packages/actionscript-mode-badge.svg)](https://melpa.org/#/actionscript-mode) |
| Assembly Language | [nasm-mode](https://github.com/skeeto/nasm-mode) | NASM x86 assembly major mode | [![MELPA](https://melpa.org/packages/nasm-mode-badge.svg)](https://melpa.org/#/nasm-mode) |
| BASIC | [basic-mode](https://github.com/dykstrom/basic-mode) | major mode for editing BASIC code | [![MELPA](https://melpa.org/packages/basic-mode-badge.svg)](https://melpa.org/#/basic-mode) |
| Batchfile | [bat-mode](https://www.emacswiki.org/emacs/BatMode) | Major mode for editing Dos scripts (batch files) | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| C | [c-mode](https://www.emacswiki.org/emacs/CcMode) | Major mode for editing C code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| C++ | [c++-mode](https://www.emacswiki.org/emacs/CPlusPlusMode) | Major mode for editing C++ code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| C# | [charp-mode](https://www.emacswiki.org/emacs/CSharpMode) | A major-mode for editing C# in emacs | [![MELPA](https://melpa.org/packages/csharp-mode-badge.svg)](https://melpa.org/#/csharp-mode) |
| Clojure | [clojure-mode](https://github.com/clojure-emacs/clojure-mode) | Major mode for Clojure code | [![MELPA](https://melpa.org/packages/clojure-mode-badge.svg)](https://melpa.org/#/clojure-mode) |
| CMake | [cmake-mode](https://www.emacswiki.org/emacs/CMakeMode) | major-mode for editing CMake sources | [![MELPA](https://melpa.org/packages/cmake-mode-badge.svg)](https://melpa.org/#/cmake-mode) |
| COBOL | [cobol-mode](https://www.emacswiki.org/emacs/CobolMode) | Mode for editing COBOL code | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| CSS | [css-mode](https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/css-mode.el) | Major mode to edit CSS files | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Dart | [dart-mode](https://github.com/bradyt/dart-mode) | Major mode for editing Dart files | [![MELPA](https://melpa.org/packages/dart-mode-badge.svg)](https://melpa.org/#/dart-mode) |
| Elixir | [elixir-mode](https://github.com/elixir-editors/emacs-elixir) | Major mode for editing Elixir files | [![MELPA](https://melpa.org/packages/elixir-mode-badge.svg)](https://melpa.org/#/elixir-mode) |
| Emacs Lisp | [elisp-mode](https://www.emacswiki.org/emacs/EmacsLispMode) | Emacs Lisp mode | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Erlang | [erlang-mode](https://www.emacswiki.org/emacs/ErlangMode) | Major modes for editing and running Erlang | [![MELPA](https://melpa.org/packages/erlang-badge.svg)](https://melpa.org/#/erlang) |
| GLSL | [glsl-mode](https://github.com/jimhourihan/glsl-mode) | major mode for Open GLSL shader files | [![MELPA](https://melpa.org/packages/glsl-mode-badge.svg)](https://melpa.org/#/glsl-mode) |
| Go | [go-mode](https://github.com/dominikh/go-mode.el) | Major mode for the Go programming language | [![MELPA](https://melpa.org/packages/go-mode-badge.svg)](https://melpa.org/#/go-mode) |
| Haskell | [haskell-mode](https://github.com/haskell/haskell-mode) | A Haskell editing mode | [![MELPA](https://melpa.org/packages/haskell-mode-badge.svg)](https://melpa.org/#/haskell-mode) |
| Haxe | [haxe-mode](https://www.emacswiki.org/emacs/HaxeMode) | An Emacs major mode for Haxe | [![MELPA](https://melpa.org/packages/haxe-mode-badge.svg)](https://melpa.org/#/haxe-mode) |
| HTML | [web-mode](https://github.com/fxbois/web-mode) | major mode for editing web templates | [![MELPA](https://melpa.org/packages/web-mode-badge.svg)](https://melpa.org/#/web-mode) |
| INI | [ini-mode](https://www.emacswiki.org/emacs/IniMode) | Improved JavaScript editing mode | [![MELPA](https://melpa.org/packages/js2-mode-badge.svg)](https://melpa.org/#/js2-mode) |
| JSON | [json-mode](https://github.com/joshwnj/json-mode) | Major mode for editing JSON files. | [![MELPA](https://melpa.org/packages/json-mode-badge.svg)](https://melpa.org/#/json-mode) |
| Java | [java-mode](https://www.emacswiki.org/emacs/JavaDevelopmentEnvironment) | Major mode for editing Java code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| JavaScript | [js2-mode](https://github.com/mooz/js2-mode) | Improved JavaScript editing mode | [![MELPA](https://melpa.org/packages/js2-mode-badge.svg)](https://melpa.org/#/js2-mode) |
| Lisp | [lisp-mode](https://github.com/emacs-mirror/emacs/blob/master/lisp/emacs-lisp/lisp-mode.el) | Major mode for editing Lisp code for Lisps other than GNU Emacs Lisp. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Lua | [lua-mode](https://github.com/immerrr/lua-mode) | a major-mode for editing Lua scripts | [![MELPA](https://melpa.org/packages/lua-mode-badge.svg)](https://melpa.org/#/lua-mode) |
| Makefile | [makefile-mode](https://www.emacswiki.org/emacs/MakefileMode) | A major mode for editing makefiles. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Object Pascal (Delphi) | [opascal-mode](https://github.com/jwiegley/emacs-release/blob/master/lisp/progmodes/opascal.el) | major mode for editing Object Pascal source in Emacs | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Objective-C | [objc-mode](https://www.emacswiki.org/emacs/ObjectiveCMode) | Major mode for editing Objective C code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Pascal | [pascal-mode](https://www.emacswiki.org/emacs/PascalMode) | Major mode for editing Pascal source in Emacs. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Perl | [perl-mode](https://www.emacswiki.org/emacs/PerlMode) | Major mode for editing Perl code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| PHP | [web-mode](https://github.com/fxbois/web-mode) | major mode for editing web templates | [![MELPA](https://melpa.org/packages/web-mode-badge.svg)](https://melpa.org/#/web-mode) |
| Properties | [conf-javaprop-mode](http://doc.endlessparentheses.com/Fun/conf-javaprop-mode.html) | Conf Mode starter for Java properties files. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Python | [python-mode](https://github.com/emacsmirror/python-mode) | Python major mode | [![MELPA](https://melpa.org/packages/python-mode-badge.svg)](https://melpa.org/#/python-mode) |
| Ruby | [ruby-mode](https://www.emacswiki.org/emacs/RubyMode) | Major mode for editing Ruby code. | ![Builtin](https://img.shields.io/badge/builtin-1.2-blue.svg) |
| Rust | [rust-mode](https://github.com/rust-lang/rust-mode) | A major emacs mode for editing Rust source code | [![MELPA](https://melpa.org/packages/rust-mode-badge.svg)](https://melpa.org/#/rust-mode) |
| Shader | [shader-mode](https://github.com/midnightSuyama/shader-mode) | Major mode for shader | [![MELPA](https://melpa.org/packages/shader-mode-badge.svg)](https://melpa.org/#/shader-mode) |
| Sass | [ssass-mode](https://github.com/AdamNiederer/ssass-mode) | Edit Sass without a Turing Machine | [![MELPA](https://melpa.org/packages/ssass-mode-badge.svg)](https://melpa.org/#/ssass-mode) |
| Scala | [scala-mode](https://github.com/ensime/emacs-scala-mode) | Major mode for editing Scala | [![MELPA](https://melpa.org/packages/scala-mode-badge.svg)](https://melpa.org/#/scala-mode) |
| SCSS | [scss-mode](https://github.com/antonj/scss-mode) | Major mode for editing SCSS files | [![MELPA](https://melpa.org/packages/scss-mode-badge.svg)](https://melpa.org/#/scss-mode) |
| Shell script | [sh-mode](https://www.emacswiki.org/emacs/ShMode) | Major mode for editing shell scripts. | ![Builtin](https://img.shields.io/badge/builtin-2.0.6-blue.svg) |
| SQL | [sql-mode](https://www.emacswiki.org/emacs/SqlMode) | Major mode to edit SQL. | ![Builtin](https://img.shields.io/badge/builtin-3.6-blue.svg) |
| Swift | [swift-mode](https://github.com/swift-emacs/swift-mode) | Major-mode for Apple's Swift programming language. | [![MELPA](https://melpa.org/packages/swift-mode-badge.svg)](https://melpa.org/#/swift-mode) |
| TypeScript | [typescript-mode](https://github.com/emacs-typescript/typescript.el) | Major mode for editing typescript | [![MELPA](https://melpa.org/packages/typescript-mode-badge.svg)](https://melpa.org/#/typescript-mode) |
| Verilog | [verilog-mode](https://github.com/veripool/verilog-mode) | Major mode for editing Verilog code. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| Vim script | [vimrc-mode](https://github.com/mcandre/vimrc-mode) | Major mode for vimrc files | [![MELPA](https://melpa.org/packages/vimrc-mode-badge.svg)](https://melpa.org/#/vimrc-mode) |
| XML | [nxml-mode](https://www.emacswiki.org/emacs/NxmlMode) | Major mode for editing XML. | ![Builtin](https://img.shields.io/badge/builtin-depends-blue.svg) |
| YAML | [yaml-mode](https://www.emacswiki.org/emacs/YamlMode) | Simple mode to edit YAML. | [![MELPA](https://melpa.org/packages/yaml-mode-badge.svg)](https://melpa.org/#/yaml-mode) |

#### Other Modes

| Language | Mode | Description | Version |
| --- | --- | --- | --- |
| gitattributes | [gitattributes-mode](https://github.com/magit/git-modes) | Major mode for editing .gitattributes files | [![MELPA](https://melpa.org/packages/gitattributes-mode-badge.svg)](https://melpa.org/#/gitattributes-mode) |
| gitconfig | [gitconfig-mode](https://github.com/magit/git-modes) | Major mode for editing .gitconfig files | [![MELPA](https://melpa.org/packages/gitconfig-mode-badge.svg)](https://melpa.org/#/gitconfig-mode) |
| gitignore | [gitignore-mode](https://github.com/magit/git-modes) | Major mode for editing .gitignore files | [![MELPA](https://melpa.org/packages/gitignore-mode-badge.svg)](https://melpa.org/#/gitignore-mode) |
| Markdown | [markdown-mode](https://github.com/jrblevin/markdown-mode) | Major mode for Markdown-formatted text | [![MELPA](https://melpa.org/packages/markdown-mode-badge.svg)](https://melpa.org/#/markdown-mode) |
| Org | [org-mode](https://orgmode.org/) | Outline-based notes management and organizer | ![Builtin](https://img.shields.io/badge/builtin-9.1.9-blue.svg) |

*P.S. The [awesome-emacs](https://github.com/emacs-tw/awesome-emacs) 
is a list of Emacs package that you can choose other 
alternatives to replace any similar packages listed here. 
Is also a good place to seek and learn what's inside Emacs!*


## Key Bindings

This configuration have all modes bind to the same set of 
key bindings. It benefits the developer would not need to change 
their key bindings while after the mode swichted. The key bindings 
set can be check in `./.emacs.jcs/jcs-key.el` file. 

P.S. 
* My work requires me to use 
[Visual Studio IDE](https://visualstudio.microsoft.com/) 
that being said the key bindings set are most likely compatible 
to 
[Visual Studio IDE](https://visualstudio.microsoft.com/). 
* Excepts keys that bind to `C-x` and `C-c`, Emacs are deeply binds 
to these two keys, and many packages also use these two keys 
for there preset keys. As you may know these two keys are often 
`cut` and `copy`, is awkward that I solved this by adding the 
same key stroke once again, hence the `cut` key is `C-x C-x` and 
the `copy` key is `C-c C-c`. 


## Themes

The theme was to design close to 
[Visual Studio IDE](https://visualstudio.microsoft.com/) 
preset light/dark theme. I believed 
[Microsoft](https://www.microsoft.com/zh-tw/) 
has a great UI/UX team, since I don't have any experience 
or work related to UI/UX, I would just like to have the 
theme color as close to it as possible. Anyway, if you 
want to customize the theme yourself, then check out the 
`./.emacs.jcs/jcs-theme.el` file. All the theme related 
variables can be found in that file.

| Light Theme                               | Dark Theme                               |
|:------------------------------------------|:-----------------------------------------|
|<img src="./screenshot/startup-light.png"/>|<img src="./screenshot/startup-dark.png"/>|


## Font

Font uses `use-ttf` package to keep cross OS consistency. 
The default font is `Ubuntu Mono` and loaded by using 
`UbuntuMono-R.ttf` located under `./.emacs.jcs/fonts/` 
folder. If you don't like the this font, you can add your 
own `.ttf` file and add the path to 
`use-ttf-default-ttf-fonts` list. Lastly, set the name 
of the `.ttf` file to `use-ttf-default-ttf-font-name`
variable.

P.S.
* See the file `./.emacs.jcs/jcs-plugin.el` to see how the 
font is been set in `(use-package use-ttf)` section.
* For more details about the font settings in this 
configuration, check out the `use-ttf`package 
[repo](https://github.com/elpa-host/use-ttf).


## Installation

To install, clone this repo and copy the core `files`/`directories` 
to the emacs config directory accordingly. Make sure you backup 
your own configuration before you installed.
```bash
# clone this repo
$ git clone https://github.com/jcs090218/jcs-emacs-init.git

# change current directory to project directory
$ cd jcs-emacs-init

# copy init file to home
$ cp ./.emacs ~/

# copy core directories to home
$ cp -r ./.emacs.d ~/
$ cp -r ./.emacs.jcs ~/
```


## Optimization

If you would like to optimize the configuration, you can 
run the following command compile all the source code to 
byte code so Emacs can run faster during both `initial time` 
and `run time`. 
```bash
# change directory to the `jcs` config directory
$ cd ./.emacs.jcs

# compile all the config source code
$ emacs --batch --eval "(byte-recompile-directory \"./\" 0)"
```


## Supported Emacs versions

The config should run on Emacs 24.3 or higher, but I will 
recommend to always run on the latest Emacs version available 
on your machine. The ultimate goal is to design to have each 
version of config can run on their each according Emacs version 
base on the version what I'm currently running on my present 
machine. For each version record, you can check the 
[version_record](https://github.com/jcs090218/jcs-emacs-init/blob/master/version_record.txt)
file at the root of the project directory.
