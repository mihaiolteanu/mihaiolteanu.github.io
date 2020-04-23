---
title: "How to create and jump to eshell buffers with a single command"
published: true
---

April 2020

I haven't got to actually use [eshell](https://www.gnu.org/software/emacs/manual/html_mono/eshell.html), the Emacs Shell, due to one simple
reason: creating and managing the eshell buffers seamlessly is a real pain. In
the mouseless world that Emacs helped create, it is bad form to click your way
around the code or through windows. Since I'm using the StumpWM, one of the
keybindings that I've set up is for easily switching between Emacs, browser and
the shell, and everything is full-screen. So when I've tried to use eshell, I
faced a problem: switching to my shell only takes one key press, but in Emacs, I
have to switch buffers back and forth, I have to create eshell buffers all the
time and there is nothing to differentiate eshell buffers from the rest of the
buffers. Management, or better yet, lack of predictability, became an issue.

My solution is to use the `window-configuration-to-register` function to help
save the window configuration of Emacs before switching to eshell buffers and
back. You can save a lot of things in the Emacs [registers](https://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html), including your
current view, which means saving the buffers that are visible right now together
with their window position. I can imagine using this functionality when working
with multiple projects and on each project you currently have two or three files
open that you're using. If you switch often between these, it is easier to save
your window positions in a register, bind that to a key or an interactive
function, and easily jump between them. It's like you have multiple views, or
desktops. So that is a nice Emacs feature, and the time for its use has come!

{% highlight emacs-lisp %}
(defun eshell-toggle ()
  "Toggle between eshell buffers.
If you are in a shell buffer, switch the window configuration
back to your code buffers.  Otherwise, create at least one shell
buffer if it doesn't exist already, and switch to it.  On every
toggle, the current window configuration is saved in a register."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (jump-to-register ?W)
    ;; Save current window config and jump to shell
    (window-configuration-to-register ?W)
    (condition-case nil
        (jump-to-register ?Z)
      (error
       (eshell)
       (when (= (length (window-list)) 2)
         (other-window 1)
         (eshell 1)
         (other-window 1))))
    (window-configuration-to-register ?Z)))
{% endhighlight %}

So, like the comment says, let's assume you're just starting Emacs and you have
some buffers open, you write some code, play some snake, etc. Eventually, you
want to install something on your system. You call `eshell-toggle`, and since
it's the first call, it creates a new eshell buffer and it switches to it, but
not before saving your current window configuration. You install your new
software, create new eshell buffers, rearrange your window positions, you create
new windows, etc. When you're done with that, you call `eshell-toggle`,
and you're back to editing code again. Of course, not before saving the window
configuration in a different register, a register dedicated to the eshell
buffers. At this point, you have two registers, one with a window configuration
for your eshell buffers and one with your code, org files or whatever, and you
can switch between these two by calling a single function. It's like switching
between your Emacs window and your favorite shell window from your Window
Manager, only better: you're still inside Emacs.

That's even more convenient if you bind the function to something easily
reachable,

{% highlight emacs-lisp %}
(global-set-key (kbd "C-q") 'eshell-toggle)
{% endhighlight %}

One short note: Usually I'm using two windows, that's why the above function
will try and create two eshell windows as well. You can adjust to your needs and
create just one, for example, by deleting the `when` form. Also, if you want to
programatically call an `ls` when the eshell buffers starts, for example, that's
also doable. The `eshell` command returns the name of the newly created eshell
buffer, so that means you can do something like this,

{% highlight emacs-lisp %}
(with-current-buffer (eshell)
  (eshell-return-to-prompt)
  (insert "ls")
  (eshell-send-input))
{% endhighlight %}

Also, the second `eshell` call passes the argument 1 to also create a new eshell
instance. Otherwise, there will indeed be two eshell buffers, but if you try to
`cd` into one of them, for example, the other one will also change its working
directory. Maybe not something you'd want.
