# Line Numbers Modding
> Mixed used of `linum` and `display-line-numbers-mode` 
base on the file usage.

I personally used `line-reminder` package and it 
package depends on `linum`. The config would have 
trouble dealing with large file. To resolve this 
issue I active the line numbers depends on the 
mode and file type.

#### Modes that use `linum`:
* Any file that are editable.

#### Modes that use `display-line-numbers-mode`:
* Read-only file.
* Packages, Help, etc.
