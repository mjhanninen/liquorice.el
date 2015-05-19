# Liquorice

**Liquorice** is a dark color theme for Emacs based on the author's old
Salmiakki theme (*salmiakki* is *liquorice* in English).


## Status

Very much **work-in-progress** as of 2015-05-19.


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
