Introduction
============

This is a simple irc logger written in elisp! It's used for logging all the irc channels
you've joined and automatically organizing them into folders by date. It also supports adding
irc channels at run time.


Intallation
===========

1. clone this repo to any location
2. add the path of the repo to `load-path`
3. byte compile the source file `erc-logger.el` using `byte-compile-file`

Configuration
=============

write the following config with any file name (eg. `.erc.emacs`)
<pre>
;; .erc.emacs

(add-to-list 'load-path "__PATH_TO_THE_REPO__")
(require 'erc-logger)
(require 'erc-join)

;; You may customize some options using
;; M-x customize-group <RET> erc-logger <RET>

(erc-autojoin-mode 1)
(erc :server "irc.libera.chat" :port 6667 :nick "__YOUR_NICKNAME__" :password "__YOUR_PASSWORD__")
(setq erc-autojoin-channels-alist '(("libera.chat" "#c" "#c++" "#linux"
                                     "#lisp" "##c++" "#clasp" "#clojure" "#guile"
                                      "#vim" "#neovim" "#emacs" "#erc")))
(erc-logger-log-start))))
</pre>

Usage
=====

You can run with emacs daemon
```{bash}
emacs -l /path/to/.erc.emacs --daemon=/path/to/socket-file
```
and connect using emacs-client 
```{bash}
emacs-client -nw -c -s /path/to/socket-file
```
You can join new channel at run time using the irc command `/join`.
