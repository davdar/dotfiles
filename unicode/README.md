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

- Try `XDejaVuSansMono-*.ttf` for programming with unicode; it's a mashup of
  DejaVuSansMono (for latin character) and SourceCodepro, DejaVuMathTeXGyre,
  Symbola, Quivira and ArialUnicode (for unicode characters). The other
  `X<fontname>-*` fonts have the same extension fonts, just a different base
  font.

# Supported Characters

- See `unicode-input.txt` for a list of all characters and their input codes
  (each implicitly preceded by `\`).
