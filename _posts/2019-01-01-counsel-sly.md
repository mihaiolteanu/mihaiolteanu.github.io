---
title: "Add Emacs interactivity with ivy counsel (counsel-sly example)"
published: true
---

# Why use ivy and basic usage

Whenever there are collections you want to filter interactively and
apply a given action to the selected entry from that collection, there
is ivy. It's simple to use and versatile.

The basic of examples:

```emacs-lisp
(ivy-read
 "Select: "
 '("select" "one of these" "entries"))
```

Eval (C-x C-e) this in a *scratch* buffer and you get a nice temporary
buffer from you can select any of the 3 entries from the given
list. There is no action selected in this simple example, so the
selected entry is just returned, which might also be useful,
depending on the use case.

## Adding an action
While using Emacs on a Windows machine and having tons of links to
requirement documents, excel sheets, quality reports and whatnot, I
wanted to have a nice way of opening up a list of all these links in a
temporary buffer, start typing keywords for what I was looking for
and, upon finding what I wanted, just presing RET and bam! the
document would open in Excel or explorer or whatever other application
was set to use that particular file.

```emacs-lisp
(defun useful-documents-and-paths ()
  (interactive)
  (ivy-read
   "Select:"
   '("holiday-planning                 - /c/path/to/some_excel_sheet.xlsx"
     "customer-requirements            - /c/path/to/some_folder"
     "secret-plan-to-conquer-the-world - /c/this/is/secret/okay.txt")
   :action (lambda (x)
             (let ((path (car (last (split-string x)))))
               (if (file-directory-p path)
                   (w32-shell-execute "explore" path) ;Open folders in explorer
                   (find-file (car (last (split-string path)))))))))
```

This uses the action keyword to specify a function to apply to the
selected entry. In this case, I've splitted the entry, took the actual
path, figured out if it was a folder, in which case I would open it
with explorer, or if it was a file, in which case I would let the
system figure out with what application this file should be opened,
based on the file extension. This worked quite nicely and I was able
to instantly open any part of the project without having to figure
out, remember, buzz a coleague or write down on actual paper where the
info that I'm looking for actually is, a method that is still used
nowadays on a massive scale, sadly.

Or, again, having tons of pdf files for customer requirements, each in
it's folder and subfolders, counting by the dozens, I wanted to be able
to open any of them without having to click my way around the file
system. Just present them all in a temporary buffer, filter
interactively for a name and open it with the default pdf reader.

```emacs-lisp
(defun customer-documents ()
  (interactive)
  (ivy-read "document: "
    (directory-files-recursively "/c/work/project/docs/" "")
    :action (lambda (x) (find-file x))))
```

# Extending sly mode with counsel functionality

Inside the [sly](https://github.com/joaotavora/sly) REPL, the `,` key
displays a list of actions, from restarting the inferior lisp to
changing the package. Counsel can be here used to open the list of
actions in a temporary buffer:

```emacs-lisp
(defun counsel-sly-mrepl-shortcut ()
    (interactive)
    (ivy-read
     "Action: " 
     (mapcar #'car sly-mrepl-shortcut-alist)
     :action (lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))))
```

One of the uses I needed for sly is interactive history search. The
list of elements in this case being the past commands given in the REPL:

```emacs-lisp
(defun counsel-sly-mrepl-history ()
    (interactive)
    (ivy-read
     "History: "
     (ring-elements comint-input-ring)
     :action (lambda (e)
               (insert e))))
```
Of course, you'll need to rebind the default search history key (usually `C-r`) to the above
function for the best of results.

```emacs-lisp
(define-key sly-mrepl-mode-map [remap isearch-backward] 'counsel-sly-mrepl-history)
```

There are other possibilities for extending sly, like changing the
package, list/select the package internal or external symbols, etc,
but these two from above I found most useful. Check the
[init.el](https://github.com/mihaiolteanu/.emacs.d/blob/master/init.el)
file from my emacs.d repo for the full sly config (`use-package sly`),
if interested.

