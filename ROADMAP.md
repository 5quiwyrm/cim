# ROADMAP

This document outlines the roadmap for the development of cim.

## A note about cross-platform compliance

This editor will primarily be built for windows terminal. I have no guarantee as
to whether this editor will work on other operating systems or even other
shells, and I don't really care too much if they do.

That is not to say that this won't change. The "windows terminal" words in the
previous paragraph might change to "kitty terminal", or even "fish" if things do
change on my development laptop. The bottom line is that I won't really be
concerned with the development beyond the environment I personally use cim in.

## Feature list

This editor WILL have:

- Mode-based editing (not necessarily vim-style)
- Multi-buffer
- Syntax highlighting*
- Autocomplete*
- Basic shell interaction (running shell commands)
- File picker
- Unicode support
- Selections and registers
- Undo

*: The extent to which this is implemented is variable for languages

This editor will NOT have:

- Customisable configs (see PHILOSOPHY.md)
- Colour schemes (see PHILOSOPHY.md)
- LSP (see PHILOSOPHY.md)
- Popups at the cursor (see PHILOSOPHY.md)
- Swapfiles (self-explanatory)
- Mouse support (see PHILOSOPHY.md)
- Text wrapping (see PHILOSOPHY.md)

This list is to stop people from complaining that a feature isn't present and
flaming cim for this.

## Implemetation roadmap

### Phase 1 - Usable editor

I will not be trying the bootstrapping idea like last time. Instead, I will be
building the bulk of the editor in bim, until cim becomes better than bim. Once
cim is better than bim, I will switch over to cim.

This phase will include features such as:

- [X] Mode-based editing (not necessarily vim-style)
- [X] Unicode support
- [X] Undo
- [X] Multi-buffer

### Phase 2 - Matching bim

By the end of this phase the editor will be comparable, or even better than bim.
This is the point at which I would probably switch over to cim.

This phase will include features such as:

- [P] Selections and registers
- [ ] Syntax highlighting
- [ ] Autocomplete
- [ ] Language-based configuration
- [ ] Basic shell interaction (running shell commands)
- [ ] File picker

### Phase 3 - Surpassing bim

At this point cim will be as good or even better than bim. Here I would split
goals into a 3 main classes, namely:

1. Optimisation - Making the editor run faster and smoother
2. UX improvements - Iterating and improving the user experience in keybinds and
other related things
3. Features - Implementing new features that aren't outlined in the editor spec,
nor are they explicitly rejected, but are nice to have.

This means that work on cim will continue as a maintainance project.
