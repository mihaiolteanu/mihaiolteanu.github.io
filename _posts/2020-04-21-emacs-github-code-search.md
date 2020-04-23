---
title: "Run a github code search from Emacs"
published: true
---

April 2020

Github has the [code search](https://github.com/search) ability. I use it to
search examples for functions that I've never used before and I'm not quite sure
how they work. Sometimes even the documentation might not help. Either it is
missing, either it is too complicated or using the function involves multiple
steps, like creating a mutex, creating a tread, locking the mutex, waiting for
another thread, etc. Or, in the case of requesting a url, for example, you have
the success callback, you have the error callback, the method call, etc. In this
cases it is sometimes easier to bring in the help of github and get some hints
from other people's code.

One detail that really helps finding what you need, is searching the code by
programming language, otherwise you have way to many results and the majority of
them are not relevant. Since I'm using this search feature a lot, I've written
myself a small, truly non-complicated little interactive function in Emacs to
speed things up,

{% highlight emacs-lisp %}
(defun github-code-search ()
  "Search code on github for a given language."
  (interactive)
  (let ((language (completing-read
                   "Language: "
                   '(" " "C" "Common Lisp" "Emacs Lisp")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l="
             language
             "&type=code&q="
             code))))
{% endhighlight %}

Replace the language list with your most used languages. A blank string can be
included for generic searches (i.e. ignore the language).  Calling this function
will open up a list of your preferred languages, from which you can select one,

![github-select-language](/assets/github-select-language.png)

then it will request the code you want to search, and then it will open a new
tab in your favorite browser with this github search. 

This is easy stuff, easily attainable with Emacs. It saves the clicks needed to
open up a github code search tab, search the code and then select the
language. Sometimes I've had to search this feature with google, since I
couldn't remember the path. These lines of code solve that. Worst still,
sometimes, the programming language I was searching for was not even in the list
offered by the search results, so in that case, I've had to manually add the
language in my search filter, and you can only add that with the advanced search
option. More clicks, more wasted time. Finally, the last advantage that I can
think of is that, when you have a powerful tool right at your fingertips, you
tend to actually use it, and use it often.
