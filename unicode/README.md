An alternative unicode input mode for Emacs, Vim and Atom.

# Vim

- Put `unicode.vim` in your `~/.vim/plugin/` folder.

# Emacs

- Put `unicode.el` in your `~/.emacs.d/lib` folder.
- Add `(add-to-list 'load-path "~/.emacs.d/lib")` to your `~/.emacs.d/init.el` file.

# Atom

- Add `unicode-init.coffee` to your `~/.atom/init.coffee` file.
- Add `unicode-keymap.cson` to your `~/.atom/keymap.cson` file.

# Fonts

- Try `MenloX.ttf` for programming with unicode; it's a mashup of Menlo (for
  latin character) and STIX (for unicode characters) for characters foreign to
  Menlo.

# Supported Characters

- See `unicode-input.txt` for a list of all characters and their input codes
  (each implicitly preceded by `\`).
