# Introduction

This is my org config.

There are many like it, but this one is mine.

Mostly copied from the many like it. DRY FTW.

There's nothing in here that is not done better in other places but
you may find it useful. I've tried to keep track of places that I
stole ideas and code from, but may have missed some references.

# Usage

Clone this repository and, assuming that you have never run emacs
before, run `ln -s /path/to/repo/clone ${HOME}/.emacs.d`.

Then, start Emacs. GUI or terminal mode shouldn't matter, though this
config is mostly used (90%+) on MacOS. Any MacOS specific things
should only apply themselves on that platform though.

## First run

The first run might (will almost certainly) fail with a note about
'void' and 'org'. This happens when we update Orgmode from Melpa.
Restart Emacs and the installation should continue.

## PDF viewing

At another point you will be asked if you want to compile `epdfinfo`.
It's optional, but if you decline then you will be asked again when
you start Emacs the next time.

I use `epdfinfo` so I haven't done anything to change this, but if
you don't need it then update `org.org` and comment out the
`use-package` section that installs `org-pdfview`. That should make
this stop.

# Structure

There was no overriding reason for breaking the config down into
various different files, other then personal preference. If you're a
packrat the merging all the files together is fine.

It does make it easier to re-order things if I need to though. I.e.,
in `settings.org` I can use the arrow keys to move entire sections
around and not worry too much that I'm breaking the code within. At
worst, I'll break the code that reads the file, and that's easy to fix
when I do.

# .gitignore

The `.gitignore` included here has explanatory comments. Tweak as required.
