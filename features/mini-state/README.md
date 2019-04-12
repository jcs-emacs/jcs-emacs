# Mini-State
> Mini mode state use to visually see what backend is the 
config currently running.


### Depend State
State that allows you to use any external programs. 
e.g. use 
[The Silver Searcher](https://github.com/ggreer/the_silver_searcher) 
with `ag`. Depend state means the state depends on the machine 
you are currently on.
<img src="./cross-state.png" width="870" height="30"/>

### Cross State
State that uses universal solution from the depends state. Instead 
of using 
[The Silver Searcher](https://github.com/ggreer/the_silver_searcher) 
with `ag`, we use `isearch` for searching implementation. Cross 
state means the state that allows you to use the same method 
to do job.
<img src="./depend-state.png" width="870" height="30"/>

### Minibuffer state
<img src="./minibuffer-state.png" width="870" height="30"/>
