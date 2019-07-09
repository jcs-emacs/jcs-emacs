# Line Numbers Modding
> Mixed used of `linum` and `display-line-numbers-mode` 
base on the file usage.

I personally used [line-reminder](https://github.com/elpa-host/line-reminder) 
package and it package depends on `linum`. The 
config would have trouble dealing with large file. 
To resolve this issue I active the line numbers 
depends on the mode and file type.

**Edit:** The package [line-reminder](https://github.com/elpa-host/line-reminder)
no longer depends on `linum` and can be use with 
built in `display-line-numbers`. Hence the modding wouldn't 
be that important anymore. Notice `indicators` only supports 
GUI version and not the terminal version so we only enabled 
`linum` when is in the terminal, else we use `display-line-numbers`.


#### Modes that use `linum`:
* ~~Any file that are editable.~~
* Emacs in terminal only.

#### Modes that use `display-line-numbers-mode`:
* ~~Read-only file.~~
* ~~Packages, Help, etc.~~
* In GUI version of Emacs.
