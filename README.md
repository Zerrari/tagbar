# Tagbar

## Intro

[Tagbar](https://github.com/preservim/tagbar) inplements in Emacs.

## Install Guide

Firstly,you should have ctags installed.

If you use `macos`
```
$ homebrew ctags
```

use `which ctags` to check if install ctags.

## Usage 

You can bind  keys to toggle tagbar.

For example,add fllowing line to you emacs config
```
(global-set-key (kbd "C-c C-t") 'tagbar-toggle)
```

Then you can go to the function definition by typing `C-c C-f`

Quit the tagbar by typing `C-c C-q`

## Todo 

+ change the function name
