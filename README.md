# Liquorice


**Liquorice** is a dark color theme for Emacs based on the author's old
Salmiakki theme (*salmiakki* is *liquorice* in English).


## Installing

This package is not available through MELPA or any other Emacs package
management system.

Currently you need to install the theme the "old way". In other words add
something like this into your `.emacs` file:

    (let ((liquorice-path "/path/to/liquorice/directory))
      (add-to-list 'load-path liquorice-path)
      (add-to-list 'custom-theme-load-path liquorice-path))
    (load-theme 'liquorice t)


## Development

Pull requests are welcome.


## License

This package is licensed under GPL3 license. Please see the source files for
copyright notices.
