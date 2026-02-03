<p align="center">
<picture>
  <source media="(prefers-color-scheme: light)" srcset="./docs/etc/logo/light/sink.png">
  <source media="(prefers-color-scheme: dark)" srcset="./docs/etc/logo/dark/sink.png">
  <img width="25%" src="">
</picture>
</p>

<p align="center">
<picture>
  <source media="(prefers-color-scheme: light)" srcset="./docs/etc/logo/light/text.svg">
  <source media="(prefers-color-scheme: dark)" srcset="./docs/etc/logo/dark/text.svg">
  <img width="40%" src="">
</picture>
</p>

<p align="center">
<a href="https://opensource.org/licenses/BSD-2-Clause"><img src="https://img.shields.io/badge/License-BSD%202--Clause-orange.svg" alt="License"></a>
<a href="https://www.gnu.org/software/emacs/download.html"><img src="https://img.shields.io/badge/Emacs-30.1+-7F5AB6.svg?logo=gnu%20emacs&logoColor=white" alt="Emacs"></a>
<a href="https://github.com/jcs-emacs/jcs-emacs/releases/latest"><img src="https://img.shields.io/github/tag/jcs-emacs/jcs-emacs.svg?label=release&logo=github" alt="Release Tag"></a>
<a href="#"><img src="https://raw.githubusercontent.com/jcs-emacs/badges/master/config/packages.svg" alt="Packages"></a>
</p>

<p align="center">
<a href="#"><img src="https://img.shields.io/badge/-Windows-lightblue?logo=windows&style=flat&logoColor=blue" alt="Windows"></a>
<a href="#"><img src="https://img.shields.io/badge/-macOS-lightgrey?logo=apple&style=flat&logoColor=white" alt="macOS"></a>
<a href="#"><img src="https://img.shields.io/badge/-Linux-fcc624?logo=linux&style=flat&logoColor=black" alt="Linux"></a>
</p>

> Emacs configuration works across all OSs

[![Compile](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/compile.yml/badge.svg)](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/compile.yml)
[![Startup](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/startup.yml/badge.svg)](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/startup.yml)
[![Speed](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/speed.yml/badge.svg)](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/speed.yml)
[![Mode](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/mode.yml/badge.svg)](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/mode.yml)
[![Packages](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/packages.yml/badge.svg)](https://github.com/jcs-emacs/jcs-emacs/actions/workflows/packages.yml)

This is Jen-Chieh Shen's emacs configuration started from [Casey Muratori][]'s
Emacs configuration. It has been repeatedly utilized and modified
since 2015 and is now very different from his configuration.

I wouldn't claim this is the best Emacs config, but this is the
best config to myself. If you wish to learn Emacs, this could be
a good start, because this configuration has been rewritten
basically from scratch. Hopefully, this config will help someone
who is new or struggle to Emacs. This configuration should work
on most OSs, indicating Windows, macOS, and Linux.

Emacs itself supports multiple programming languages. I managed
all the programming languages I personally used. I dabble in
numerous technologies, incorporating hardware, firmware, and software.
Here is the list of programming languages I know and are extended
from this configuration.

- ActionScript / Ada / Agda / AppleScript / Assembly
- BASIC / Batchfile / Beancount
- C / C++ / C# / Clojure / COBOL / Coq / Crystal / CSS / CUDA / Cython
- Dart / Dockerfile
- Elixir / Elm / Emacs Lisp / Erlang
- F# / Faust / Fennel / Fountain
- GLSL / Go / Godot / Groovy
- Haskell / Haxe / HLSL / HTML
- Idris / INI
- Janet / Java / JavaScript / JSON / JSX / Julia
- Kotlin
- LaTeX / LESS / Lisp / Lua
- Magik / Makefile / Markdown / Mermaid / Mint
- Nim / Nix / Noir
- Object Pascal (Delphi) / Objective-C / OCaml / Odin
- P4 / Pascal / Perl / PHP / PowerShell / Properties / Python
- R / Racket / Raku / Ruby / Rust
- Sass / Scala / SCSS / ShaderLab / Shell script / SQL / Swift
- Terraform / TOML / TSX / TypeScript
- Verilog / VHDL / Vimscript / Vue
- XML
- YAML
- Zig

This configuration polished and goes toward to the modern
text editor, or even better, it goes beyond modern IDE.
e.g. [Atom](https://atom.io/), [Brackets](http://brackets.io/),
[Sublime Text 2](https://www.sublimetext.com/2) or [3](https://www.sublimetext.com/),
[Visual Studio Code](https://code.visualstudio.com/), etc.

---

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [📚 Philosophy](#-philosophy)
- [📰 News](#-news)
- [💾 Installation](#-installation)
- [🕒 Startup Time](#-startup-time)
- [🏆 Features](#-features)
  - [Highlight](#highlight)
  - [Details](#details)
- [📁 Package Archives](#-package-archives)
- [🏆 Powered by](#-powered-by)
  - [Functionalities](#functionalities)
- [🧰 Write your own customization](#-write-your-own-customization)
- [❓ FAQ](#-faq)
- [🛠️ Contribute](#-contribute)
- [⚜️ License](#-license)

<!-- markdown-toc end -->

## 📚 Philosophy

I have experienced many different kinds of IDEs and text editors. But I’ve had a
hard time finding the best tool to use. I’ve jumped from one working field to
another trying to find something that suits my needs. So instead of struggling
with the tool itself, I chose Emacs and configured the entire thing from scratch,
to suit my needs.

Here are a few goals that I want this config to accomplish.

- Having the same set of key bindings across different IDEs and text editors as
many as possible.
- Having the same font and theme across different OSs and environments.
- Automating trivial or redundant tasks.
- Improve user experiences approach to modern text editor or IDE.
- Make compatible with most features work inside the terminal as well.

Having these implementations makes my life easier, and having a genuinely portable
workspace, which is great because it lets me work on different machines efficiently,
without having to get used to a new IDE.

![](./docs/etc/screenshot/startup-dark.png)

## 📰 News

Here is the list of few essential and recent changes to this configuration.

- `9.3.0` - Move user configuration files to its own directory.
- `9.1.0` - Replace package `topsy` with `breadcrumb-mode` for better UX
- `9.0.1` - Add AI support and replace a few external packages with built-in.
- `9.0.0` - Support more languages and add debugging utilities
- `8.2.2` - Add `emp` as the default Music Player
- `8.2.1` - Replace package `docstr` with `ts-docstr` for better parsing capability
- `8.2.0` - Drop support for Emacs 27.x
- `8.1.0` - Remove config tabify/untabify save, and replace it with `whitespace-cleanup-mode`.
- `8.0.1` - Switch from `ivy` to `vertico`.
- `8.0.0` - Move everything to `.emacs.d`. ([#32](https://github.com/jcs-emacs/jcs-emacs/pull/32))

## 💾 Installation

To install, clone this repo to `~/.emacs.d`, i.e. ensure that the `init.el`
contained in this repo ends up at `~/.emacs.d/init.el`:

```bash
git clone https://github.com/jcs-emacs/jcs-emacs ~/.emacs.d
```

Then startup Emacs; will automatically install all necessary packages due
to this configuration.

> [!CAUTION]
>
> **MAKE SURE YOU INSTALLED THE CORRECT VERSION OF EMACS!**

## 🕒 Startup Time

![](https://raw.githubusercontent.com/jcs-emacs/badges/master/config/speed/dos.svg)
![](https://raw.githubusercontent.com/jcs-emacs/badges/master/config/speed/mac.svg)
![](https://raw.githubusercontent.com/jcs-emacs/badges/master/config/speed/unix.svg)
(in seconds)

The average startup time for this configuration is around `15` to `25` seconds.
You can use the command `emacs-init-time` to check the startup time on your machine.
Not quite sure what causes that much performance, hopefully, I'm able to lower
the startup time down to `5` to `15` seconds.

**Edit 1:** After version `5.3.2`, the average startup time is around `5` to `15`
seconds. Solved this issue by removing the unnecessary `require` keyword load file
and using `:defer` keyword with `use-package` package to delay some packages
load time.

📝 *P.S. Here is a great article about
[Speeding Up Emacs](https://anuragpeshne.github.io/essays/emacsSpeed.html)
written by
[Anurag Peshne](https://github.com/anuragpeshne).*

**Edit 2:** Using [esup](https://github.com/jschaf/esup) package to test and
optimize the configuration. Call `package-refresh-contents` only when package
installation is needed. By doing the thing, lower the startup time from around
`4` to `8` seconds.

📝 *P.S. Some good hints from one [StackExchange](https://emacs.stackexchange.com/)
question,
[What can I do to speed up my start-up?](https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up)
answered by
[Jordon Biondo](https://github.com/jordonbiondo).*

📝 *P.S. The above cases are tested on Windows. Other OSs are not Windows should
start within a second.*

## 🏆 Features

### Highlight

Here is the list of the major highlights of this configuration. Hope you would
like my taste!

- **Out of the box** - Out of the box anywhere.
- **Cross Platform** - Work on all operating systems, including the terminal.
- **Fast Startup** - Lazy loading for all unnecessary packages on startup.
- **Old-Fashioned** - Doesn't use any beautiful GUI because it may not work in the terminal.
- **Multiple Languages** - Support multiple programming languages.
- **Consistent** - Having the same coding experience in different major modes.
- **Easy to use** - Design close to other modern text editors. It shouldn't spend you too much time.
- **Dual Windows** - Design for people who like multiple windows opened simultaneously.
- **Keyboard Focused** - You can do everything by using the keyboard and the mouse are just optional.

### Details

This is the list of features that are built-in to this configuration. These features
are heavily based on my personal habits, and so these could be very tiny things. But
I believe details make things better and make life smoother.

- [*Auto Install Package*](./docs/features/auto-install-package) - Automatically installs the package that this config relies on.
- [*Build Run*](./docs/features/build-run) - Implementation for executing script for building and running the software.
- [*Capital Word*](./docs/features/capital-word) - You can navigate/kill word by seeing capital letter.
- [*Changelog Helper*](./docs/features/changelog-helper) - Help to create changelog while creating `CHANGELOG` file using template.
- [*Consistent Key Bindings*](./docs/features/consistent-key-bindings) - Has consistent key bindings across all modes.
- [*Curly Bracket Modes*](./curly-bracket-modes) - Use curly bracket depends on different mode.
- [*Display File*](./docs/features/display-file) - Utility function to use to view a file on the other window.
- [*Charset Table*](./docs/features/display-file/charset-table) - Built-in functions that displays character sets.
- Enhanced Multiple Cursors
- [*Similar Multiple Cursors*](./docs/features/similar-multiple-cursors) - Mark with cursor by similarity.
- [*VS Multiple Cursors*](./docs/features/vs-multiple-cursors) - VSCode like methods to `mark` and `unmark` multiple cursor.
- [*Fast Incremental Search*](./docs/features/fast-incremental-search) - Fast keys for incremental search forward/backward to the cursor is currently pointing.
- [*Indent Control*](./docs/features/indent-control) - Generic control the indentation level for each mode, for more information see [indent-control](https://github.com/alt-elpa/indent-control).
- [*License Helper*](./docs/features/license-helper) - Help to create license while creating `LICENSE` file using template.
- [*Mini State*](./docs/features/mini-state) - Mini mode state use to visually see what backend is the config currently running. (Deprecated)
- [*Explicit States*](./docs/features/mini-state/explicit-states) - Automatcially switch mini state depends on certain circumstances.
- [*Modern Text Editor*](./docs/features/modern-text-editor) - Design to have the preset settings close to modern text editors but faster.
- [*Multiple Output*](./docs/features/multiple-output) - Handle multiple output/compilation buffers.
- [*Navigate Blank Line*](./docs/features/navigate-blank-line) - Use `C-<up>` and `C-<down>` to navigate previous and next blank line.
- [*Navigate Table*](./docs/features/navigate-table) - Navigate `org-mode`'s table easier by using arrow keys.
- [*Preview HTML*](./docs/features/preview-html) - Preview rendered HTML file on the other window.
- [*Switch Window*](./docs/features/switch-window) - Fast keys switch between windows quickly.
- [*Trim Trailing Whitespace*](./docs/features/trim-trailing-whitespace) - Remove trailing spaces and tabs automatically on save.
- [*Visualize Undo/Redo*](./docs/features/visualize-undo-redo) - Improved the user experience on undoing and redoing by showing the `undo-tree-visualizer` at the other window.
- [*VS Curly Bracket*](./docs/features/vs-curly-bracket) - Visual Studio IDE like curly bracket implementation.
- [*VS Multiple Terminal*](./docs/features/vs-multiple-terminal) - VSCode like multiple shell control.
- [*VS Navigate Word*](./docs/features/vs-navigate-word) - Visual Studio IDE like navigating between word implementation.

## 📁 Package Archives

A list of package archives that this configuration uses.

- *[GNU](https://elpa.gnu.org/)* - The standard GNU ELPA
- *[NonGNU](https://elpa.nongnu.org/)* - The standard NonGNU ELPA
- *[MELPA](https://melpa.org/#/)* - Milkypostman’s ELPA
- *[JCS-ELPA](https://jcs-emacs.github.io/jcs-elpa/)* - Jen-Chieh Shen's ELPA for this confinguration

📝 *P.S. Here is a good talk comparing all package archives from a
[StackExchange](https://emacs.stackexchange.com/) question,
[What are the practical differences between the various Emacs Package Repositories?](https://emacs.stackexchange.com/questions/268/what-are-the-practical-differences-between-the-various-emacs-package-repositorie)
answered by
[Tikhon Jelvis](https://github.com/TikhonJelvis).*

## 🏆 Powered by

Here is the list of all packages that powered and made this configuration works.
Thanks to all the package maintainers; this configuration cannot be made without
them, and if you wish to support them, you can go to this
[elisp-maintainers](https://github.com/tarsius/elisp-maintainers)
repo/site and search for the maintainer you want to support. There should be some
kind of methods to support the maintainer you wish to.

### Functionalities

- *Abbreivation Definition* - powered by [project-abbrev](https://github.com/elpa-host/project-abbrev).
- *Alt-Codes* - powered by [alt-codes](https://github.com/elpa-host/alt-codes).
- *Auto Completion* - powered by [company](https://github.com/company-mode/company-mode).
- *Auto Highlight Symbol* - powered by [auto-highlight-symbol-mode](https://github.com/mhayashi1120/auto-highlight-symbol-mode).
- *Banner* - powered by [dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
- *Binary/Hex Editor* - powered by [hexl-mode](https://www.emacswiki.org/emacs/HexlMode).
- *Breadcrumb* - powered by [breadcrumb](https://github.com/joaotavora/breadcrumb).
- *Collaborative Editing* - powered by [togetherly](https://github.com/zk-phi/togetherly).
- *Completion Frontend* - powered by [vertico](https://github.com/minad/vertico).
- *Context Menu* - powered by [context-menu-mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Menu-Mouse-Clicks.html).
- *Document String* - powered by [ts-docstr](https://github.com/emacs-vs/ts-docstr).
- *End of Line* - powered by [show-eol](https://github.com/elpa-host/show-eol).
- *Execute Commands* - powered by [compile](https://www.emacswiki.org/emacs/CompilationMode).
- *File Explorer* - powered by [treemacs](https://github.com/Alexander-Miller/treemacs).
- *File Header* - powered by [file-header](https://github.com/alt-elpa/file-header).
- *Folding/Unfolding* - powered by [ts-fold](https://github.com/emacs-tree-sitter/ts-fold).
  - *Persistence Folding* - powered by [savefold.el](https://github.com/jcfk/savefold.el).
- *Font* - powered by [use-ttf](https://github.com/elpa-host/use-ttf).
- *Goto Declaration/Definition* - powered by [dumb-jump](https://github.com/jacktasia/dumb-jump).
- *Highlight Indentation* - powered by [highlight-indent-guides](https://github.com/DarthFennec/highlight-indent-guides).
- *Highlight Matched Pairs* - powered by [show-paren-mode](https://www.emacswiki.org/emacs/ShowParenMode).
- *Highlight Same Region* - powered by [region-occurrences-highlighter](https://github.com/alvarogonzalezsotillo/region-occurrences-highlighter).
- *Indentation Management* - powered by [indent-control](https://github.com/alt-elpa/indent-control).
- *Language Server Protocol* - powered by [lsp-mode](https://github.com/emacs-lsp/lsp-mode).
- *Line Annotation* - powered by [line-reminder](https://github.com/elpa-host/line-reminder).
- *Line Numbers* - powered by [display-line-numbers](https://github.com/emacs-mirror/emacs/blob/master/lisp/display-line-numbers.el) and [linum](https://github.com/emacs-mirror/emacs/blob/master/lisp/linum.el).
- *Minimap* - powered by [minimap](http://elpa.gnu.org/packages/minimap.html).
- *Mode Line* - powered by [jcs-modeline](https://github.com/jcs-emacs/jcs-modeline).
- *Multiple Cursor* - powered by [iedit](https://github.com/victorhge/iedit) and [multiple-cursors](https://github.com/magnars/multiple-cursors.el).
- *Multiple Terminal* - powered by [multi-shell](https://github.com/jcs-elpa/multi-shell).
- *Music Player* - powered by [emp](https://github.com/jcs-elpa/emp).
- *Navigation/Searcher* - powered by [searcher](https://github.com/jcs-elpa/searcher) and [isearch-project](https://github.com/elpa-host/isearch-project).
- *Package Archive* - see [here](#file_folder-package-archives).
- *Package Management* - powered by [use-package](https://github.com/jwiegley/use-package) and [github-elpa](https://github.com/10sr/github-elpa).
- *PDF Viewer* - powered by [doc-view-mode](https://www.emacswiki.org/emacs/DocViewMode) and [ghostscript](https://www.ghostscript.com/index.html).
- *Project Management* - powered by [project](https://elpa.gnu.org/packages/project.html).
- *Recent Files* - powered by [recentf](https://www.emacswiki.org/emacs/RecentFiles).
- *Regexp* - powered by [re-builder](https://www.emacswiki.org/emacs/ReBuilder) and [visual-regexp](https://github.com/benma/visual-regexp.el).
- *Remote Access* - powered by [tramp](https://www.emacswiki.org/emacs/TrampMode).
- *Restart Emacs* - powered by [restart-emacs](https://github.com/iqbalansari/restart-emacs).
- *Scroll Bar* - powered by [auto-scroll-bar](https://github.com/jcs-elpa/auto-scroll-bar).
- *Shell* - powered by [shell](https://www.emacswiki.org/emacs/ShellMode).
- *Smooth Scrolling* - none, originally powered by ~~[sublimity](https://github.com/zk-phi/sublimity)~~.
- *Snippet* - powered by [yasnippet](https://github.com/joaotavora/yasnippet).
- *Startup Screen* - powered by [dashboard](https://github.com/emacs-dashboard/emacs-dashboard).
- *Syntax Check* - powered by [flycheck](http://www.flycheck.org/en/latest/).
- *Tab Bar* - powered by [centaur-tabs](https://github.com/ema2159/centaur-tabs).
- ~~*Tags* - powered by [gtags](https://www.gnu.org/software/global/).~~
- *Theme* - powered by [vs-dark-theme](https://github.com/jcs-elpa/vs-dark-theme) and [vs-light-theme](https://github.com/jcs-elpa/vs-light-theme). **(For more info see [here](#themes))**
- *Todo* - powered by [hl-todo](https://github.com/tarsius/hl-todo).
- *Undo/Redo* - powered by [undo-tree](https://www.emacswiki.org/emacs/UndoTree).
- *Version Control Management* - powered by [magit](https://github.com/magit/magit).
- *Video Player* - none.
- *White Space* - powered by [whitespace](https://www.emacswiki.org/emacs/WhiteSpace).

## 🧰 Write your own customization

You can customize the configuration by editing the file located at
`~/.emacs.jcs.d/init.el`. You should put all your own customize code
there, then other than that are the core files.

```elisp
;;; init.el --- Your own configuration  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

... your code here ...

;;; init.el ends here
```

If you intend to make more substantial customizations, it’s
best to fork the repository and modify the configuration to
suit your needs. Be sure to regularly merge updates from this
repo to keep your setup compatible with the latest package and
Emacs releases.

*💡 Please note that I’m unable to provide support for customized versions of this configuration.*

## ❓ FAQ

Here is the list of *Frequently Asked Questions*.

- [List of FAQ](./doc/FAQ.md)

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple?logo=gnuemacs&logoColor=white)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may clone and make pull
requests to this repository. Or you can clone the project and establish your own
branch of this tool. Any methods are welcome!

## ⚜️ License

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

See [`LICENSE`](./LICENSE) for details.


<!-- Links -->

[Emacs]: https://www.gnu.org/software/emacs/

[Casey Muratori]: https://github.com/cmuratori
