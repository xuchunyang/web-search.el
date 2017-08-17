# web-search.el

Web search from Emacs and the terminal. Just opens in your browser.

From Emacs, use `M-x web-search foo` to search for "foo" on the default website,
if prefixed with one or two `C-u`, you will be asked which website(s) to search
on, see the docstring to learn more.

From the terminal,

```
$ web-search -h
Web search from the terminal.

Usage: web-search <query> [options]

Options:
  -h, --help              display help
  -l, --list-providers    list supported providers
      --list-tags         list available tags
  -o, --output            output only mode
  -p, --provider string   search provider (default "Google")
  -t, --tag string        search tag
  -v, --verbose           verbose mode
      --version           display version
```

# Examples

Search for puppies on google.
```
;; From Emacs
M-x web-search puppies

# From the terminal
$ web-search puppies
```

Search for rhinos on wikipedia
```
;; From Emacs
C-u M-x web-search Wikipedia rhinos

# From the terminal
$ web-search -p wikipedia rhinos
```

Search providers tagged "Code" for string-length.
```
;; From Emacs
C-u C-u M-x web-search Code string-length

# From the terminal
$ web-search -t code string-length
```

## Customization

See the user option `web-search-providers` and `web-search-default-provider`. To
customize the command line program `web-search`, you can put your configuration
into `$HOME/.config/web-search-config.el` or `$HOME/.web-search-config.el`, it
will be loaded *after* `web-search.el` has been loaded.

For example, to change the default provider from Google to Bing:

```elisp
(setq web-search-default-provider "Bing")
```

and to search something on [Emacs China](https://emacs-china.org/):

```elisp
(push '("Emacs China" "https://emacs-china.org/search?q=%s")
      web-search-providers)
```

## Bash/Zsh Completion

Put the following line to your Bash or Zsh configuration file such as`~/.bashrc`
or `~/.zshrc`.

```sh
source /path/to/web-search-completion.bash
```

## Supported Providers

<!-- (dolist (p web-search-providers) (insert "* " (car p) "\n")) -->
* 500px
* Arch Package
* Arch Wiki
* Bing
* Debian Manpages
* Debian Package
* Gist
* GitHub
* Google
* Hacker News
* MacPorts
* Pinterest
* Reddit
* RubyGems
* Stack Overflow
* Wikipedia
* YouTube
* Zhihu

## Acknowledgment

This project is inspired by https://github.com/zquestz/s.
