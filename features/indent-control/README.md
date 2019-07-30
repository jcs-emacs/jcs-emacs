# Indent Control
> Generic control the indentation level for each mode.

* Decouple from user knowing each major mode's indentation level variable. (if have)
* Keep the indentation level across buffers. If you changed the indentation level
in `buffer A` and switch to `buffer B` with the same major mode; they will have 
the same indentation level.
