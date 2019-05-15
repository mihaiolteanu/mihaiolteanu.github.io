---
title: "Add counsel-sly support in Emacs"
published: true
---

I use [sly](https://github.com/joaotavora/sly) instead of [slime](https://common-lisp.net/project/slime/) for interacting with the Common
Lisp REPL. One thing that I miss is the enhancement that [counsel](https://github.com/abo-abo/swiper)
offers for some of the most used commands, history-search being one of
them. For example, starting with an empty history ring, type in some
commands and then search through history (C-r). Those commands are now
available for filtering and can be called again.

![counsel sly history](/assets/counsel-sly/counsel-sly-history.gif)

## Emacs config file

Copy/paste the code below in your emacs init file or just copy the
relevant functions if you already have a `use-package sly` entry in
your init file. You can keep the keybindings or you can define your
own.

```emacs-lisp
(use-package sly
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")

  ;; Define functionality for interacting with the sly repl using counsel 
  (defun counsel-sly-mrepl-shortcut ()
    (interactive)
    (ivy-read
     "Action: " 
     (mapcar #'car sly-mrepl-shortcut-alist)
     :action (lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))))

  (defun sly-read-package-name (prompt &optional initial-value allow-blank)
    (ivy-read
     "Package: "
     (sly-eval `(slynk:list-all-package-names t))
     :action #'identity))

  (defun counsel-sly-mrepl-history ()
    (interactive)
    (ivy-read
     "History: "
     (ring-elements comint-input-ring)
     :action (lambda (e)
               (insert e))))

  (defun eval-grab-output (string)
    (let ((res nil))
      (sly-eval-async `(slynk:eval-and-grab-output ,string)
        (lambda (result)
          (cl-destructuring-bind (output value) result
            (setf res (car (read-from-string value))))))
      (while (null res)
        (sleep-for 0.1))
      res))

  (defun counsel-sly-eval (string action)
    (let ((result (eval-grab-output string)))
      (ivy-read
       "Symbol: "
       result
       :action action)))

  (defun send-input (expr)
    (insert expr)
    (comint-send-input))

  (defun counsel-sly-package-internal-symbols ()
    (interactive)
    (counsel-sly-eval "(common-lisp-user::package-internal-symbols \*package\*)"
                      `(1 ("o" ,#'insert "insert")
                          ("f" ,(lambda (candidate)
                                  (send-input (format "(find-symbol \"%s\")" candidate)))
                           "find symbol")
                          )))

  ;; sly-mrepl-mode-map symbol not available at the time of
  ;; use-package, so :bind cannot be used here
  (with-eval-after-load 'sly-mrepl
    (define-key sly-mrepl-mode-map [remap isearch-backward] 'counsel-sly-mrepl-history)
    (define-key sly-mrepl-mode-map (kbd ",,") 'counsel-sly-mrepl-shortcut)
    (define-key sly-mrepl-mode-map (kbd ",p") 'sly-mrepl-set-package)
    (define-key sly-mrepl-mode-map (kbd ",s") 'counsel-sly-package-internal-symbols)
    (define-key sly-mrepl-mode-map (kbd ",r") 'sly-restart-inferior-lisp))
  )
```

## Sbcl config file

And in your Common Lisp compiler config file (~/.sbclrc, for example),
add this definition since this will be evaluated by sbcl and not by emacs.
```common-lisp
(defun package-internal-symbols (package)
  (let ((lst '()))
    (do-all-symbols (s)
      (when (eq (find-package package) (symbol-package s))
        (push s lst)))
    lst))
```

## Other available commands

Besides the history search, we can list all the available sly
commands, change the current package from a list of all available
packages and choose an internal symbol from the current package.

![counsel sly commands](/assets/counsel-sly/counsel-sly-commands.gif)

