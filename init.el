; Window settings
(menu-bar-mode -1) ; get rid of the annoying menubars/toolbars etc.
(tool-bar-mode 0)
(scroll-bar-mode nil)

; Usability
(fset 'yes-or-no-p 'y-or-n-p) ; yes/no turns to y/n

; starting a daemon process
(server-start)

; Always-on modes
(ido-mode)
