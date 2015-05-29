# ASCOPE-EXT

An extension for **ascope** (http://emacswiki.org/emacs/ascope.el),
which provides some extra features which are not supported in **ascope**:

 - cscope database manipulation
 - a minor mode, ascope-mode for key bindings
 - more search options (egrep-pattern, filename, assignment-to-the-symbol)

## LICENSE

MIT License

## INSTALLATION

#### *el-get* installation

Since the recipe is not yet in official repository, you should download ascope-ext.rcp
from https://raw.githubusercontent.com/zeph1e/ascope-ext/master/ascope-ext.rcp
and copy it into your local recipe directory.
And then add a following line into your emacs init script(.emacs or .emacs.d/init.el):

```lisp
(el-get-bundle! ascope-ext)
```

#### Manual installation

Download ascope-ext.el and add following lines into your emacs init script:

```lisp
;; Assume that you downloaded ascope-ext.el into ~/.emacs.d/lisp/ascope-ext/ directory:
(add-to-list 'load-path "~/.emacs.d/lisp/ascope-ext")
(require 'ascope-ext)
(ascope-install-hooks)
```
## KEYBINDINGS


| Key     | Function                              |
|---------|---------------------------------------|
| C-c s / | Set initial directory (source root).  |
| C-c s s | Find this symbol.                     |
| C-c s d | Find global definition.               |
| C-c s g | Find global definition.               |
| C-c s C | Find called functions.                |
| C-c s c | Find functions calling this function. |
| C-c s t | Find this text string.                |
| C-c s e | Find this egrep pattern.              |
| C-c s i | Find files including file.            |
| C-c s f | Find this file.                       |
| C-c s = | Find assignments to this symbol.      |
| C-c s a | Find assignments to this symbol.      |
| C-c s A | Find all symbol assignments.          |


## Features

Following features are provided or are intended to provide.

#### Database
You may generate cscope.out by calling ascope-create-database or just ascope-init to set initial directory.

#### Minor mode
A minor mode, ascope-mode is provide which supports keymap for ascope init & search.
ascope-mode gets installed automatically into major modes(c-mode, c++-mode, idl-mode, dired-mode).

#### More Search Options
You may search following search options which were not supported in ascope:
 - Find files which have given string in their name.
 - Locate where matches to given egrep pattern.
 - Locate assignments to a symbole in the source.

## TODOs

 - Fix found item printing in file search. It doesn't looks good.
 - Search history: multiple result buffers and the way to iterate.
 - more...
