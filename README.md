# Liquorice

**Liquorice** is an Emacs library for helping color theme definition and a set
of accompanying color themes. The included themes are:

- *Liquorice*: a dark color theme based on the author's old Salmiakki
  (eng. *liquorice*) theme.

- *Vanilla*: a light color theme with that should go well with Liquorice
  theme.

- *Red October*: a dark color theme.

- *Eggplant*: an experimental mid-range color theme strives to retain good
  contrast.

## Status

Very much **work-in-progress** as of 2015-05-23.


## Installing

This package is not available through MELPA or any other Emacs package
management system.

Currently you need to install the theme the "old way" by modifying your Emacs
startup script. Add something along these lines into your `.emacs` file:

    (let ((liquorice-path "/path/to/liquorice/directory"))
      (add-to-list 'load-path liquorice-path)
      (add-to-list 'custom-theme-load-path
                   (concat liquorice-path "/themes"))
    (load-theme 'liquorice t)


## Development

Pull requests are welcome.


## License

This package is licensed under GPL3 license. Please see the source files for
copyright notices.
