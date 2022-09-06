# Axiom

## What is Axiom?
Axiom is yet another set of defaults for emacs, with one (and only one) purpose:
it attempts to get out of your way, and add as little magic as possible.

Axiom is not a full distribution, like Doom or Spacemacs, but is more akin to
Prelude, although much more amateur.

## Design Decisions
### One Big File
Axiom's config lives in a single file: `config.org`. This is entirely in an
effort to make things easier to grok for new users. This way, there is only one
file to be perused, and users need not be bothered to learn how packages work.

The use of an `org` file rather than an elisp file is to improve discoverability.
Users can simply expand and collapse sections to painlessly explore the configuration.

### Included Packages
Axiom intends to be a set of sane defaults for emacs, and nothing more.
As such, dependency on packages is minimized as much as is practical.

Ideally, Axiom is mostly an aesthetic overhaul, with the only major functional changes
being the inclusion of Ivy, magit and eglot.

This is intended to allow a new emacs user to learn emacs (and not a custom layer sitting on it)
without sacrificing the kinds of features present in their old editor. LSP is only becoming
more relevant, and magit is the only thing close to the kind of git-integration that is becoming
expected in the modern world of editors.

Ivy is chosen as a way to improve the average interface to emacs, helping to ease the transition
without adding features that could become a crutch. Most of Ivy's functionality for the new
user is in its display of options, replacing the awful(opinion!) default.


### Extras
Settings for some languages have been pre-configured (primarily Python)
this is unabashedly because they are languages I use often, and Axiom is developed primarily
for my own use.

# Things You May Prefer
Axiom is not necessarily for everyone; you may prefer one of the following:
  * [Spacemacs](https://www.spacemacs.org)
  * [Doom Emacs](https://github.com/hlissner/doom-emacs)
  * [Prelude](https://github.com/bbatsov/prelude)
