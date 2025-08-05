# PHILOSOPHY

This document outlines the philosophy and design decisions made in cim.
Cim is heavily inspired by modal editors such as vim and helix, and is a direct
descendant of bim.

## Justifying the lack of features

### Customisable config

Cim is only going to be used by me. If anyone else wants to use this editor but
doesn't like something about it, it is THEIR job to patch and tweak the editor
until they like it.

Now this does mean that they will have to write rust, which might be scary for
some people. I view this as a price to pay for using cim. It is very much a DIY
text editor.

### Colour schemes

Cim will be used by me. I have a set of configs and preferences that are baked
into my shell environment (such as my background picture and colour scheme). The
editor will work in conjunction with these decisions, not against it or parallel
to it.

### LSP

I am aware how useful and amazing LSPs are for programming and for writing. The
issue is that LSPs are often resource consuming, and are hard to integrate into
editors without massive overcomplication. Thus I will not be bothering with LSP,
as if you want a modal editor with native LSP support you can just use helix.

### Popups at the cursor

A major pet peeve of mine is when editors blast you with a load of popups that
are useless and block the view of the actual code or text that is being edited
(this was one of my complaints towards helix with LSP). This editor will have
bim style popups, i.e. popups at the bottom of the screen.

### Mouse support

I prefer using the keyboard for everything. Some people might be more used to
an editor like VSC*de, meaning they cannot get out of the habit of using the
mouse to navigate. I never really liked using the mouse to navigate, as that
meant I had to take my hand off of the keyboard, meaning I cannot instantly type
after getting where I want.

### Text wrapping

Too complicated, and I personally don't like it either.
