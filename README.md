# web-search.el [![Build Status](https://travis-ci.org/xuchunyang/web-search.el.svg?branch=master)](https://travis-ci.org/xuchunyang/web-search.el) [![MELPA](https://melpa.org/packages/web-search-badge.svg)](https://melpa.org/#/web-search)

Web search from Emacs and the terminal. Open URL in your default web browser.

From Emacs, type `M-x web-search`.

From the terminal, use `web-search`:

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
      --completion        display completion code for Bash

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

## Bash Completion

Put the following line to your Bash configuration file such as `~/.bashrc`

```sh
eval "$(web-search --completion)"
```

## Supported Providers

<!-- (dolist (p web-search-providers) (insert "* " (car p) "\n")) -->
* 500px
* Arch Package
* Arch Wiki
* Bing
* Debian Manpages
* Debian Package
* DuckDuckGo
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
