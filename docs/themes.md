# Themes

## How theming works

A theme is a property list mapping **face keywords** to **(R G B A)
colour tuples** in the 0.0-1.0 range:

```lisp
(define-theme :default
  :bg     '(0.110 0.118 0.137 1.0)
  :normal '(0.880 0.880 0.880 1.0)
  :accent '(0.580 0.760 0.960 1.0)
  :ok     '(0.500 0.870 0.480 1.0)
  :warn   '(0.960 0.760 0.300 1.0)
  :urgent '(0.960 0.420 0.420 1.0)
  :muted  '(0.560 0.580 0.620 1.0))
```

Every module's text fragments carry a face, not a literal colour.
The active theme decides what each face actually looks like.  This
is what lets you switch a whole palette with one keyword:

```lisp
(theme :nordish)
```

The face keywords have stable semantics every module relies on:

| Face       | Used for                                                     |
| ---------- | ------------------------------------------------------------ |
| `:bg`      | bar background                                               |
| `:normal`  | the default text colour                                      |
| `:accent`  | emphasis (clock, focused workspace, ...)                     |
| `:ok`      | positive states (charging, connected, ...)                   |
| `:warn`    | low / cautionary states (low battery, weak wifi, ...)        |
| `:urgent`  | critical states (battery < 10 %, errors, ...)                |
| `:muted`   | secondary text (separators, "off", brackets, ...)            |

## Built-in themes

Listed in priority order if you have no preference — each
demonstrates a different aesthetic, all use the same face palette
above.

### `:default`

Neutral dark with cool accent.  Used when no `(theme …)` is set.

```
:bg     #1c1f23      :normal #e0e0e0      :accent #94c2f5
:ok     #80de7a      :warn   #f5c24d      :urgent #f56b6b
:muted  #8e94a0
```

### `:minimal`

Pure black / white / grey.  No accent.  For typography purists.

### `:nordish`

Frost-tone palette inspired by [Nord](https://www.nordtheme.com/).

### `:gruvboxish`

Warm earthy palette inspired by [Gruvbox](https://github.com/morhetz/gruvbox).

### `:catppuccinish`

Pastel mocha palette inspired by [Catppuccin](https://catppuccin.com/).

### `:doomish`

High-contrast dark palette inspired by [Doom Emacs](https://github.com/doomemacs/doomemacs)'s
default theme.

To list whatever's currently registered (built-ins + user themes):

```sh
lispbar --list-themes
```

## Writing your own theme

Drop a file under `~/.config/lispbar/themes/`:

```lisp
;; ~/.config/lispbar/themes/dracula.lisp
(in-package :lispbar)

(define-theme :dracula
  :bg     '(0.157 0.165 0.212 1.0)
  :normal '(0.945 0.945 0.945 1.0)
  :accent '(0.741 0.576 0.976 1.0)
  :ok     '(0.314 0.980 0.482 1.0)
  :warn   '(0.945 0.980 0.549 1.0)
  :urgent '(1.000 0.333 0.333 1.0)
  :muted  '(0.380 0.420 0.490 1.0))
```

Reference it from `config.lisp`:

```lisp
(theme :dracula)
```

The keyword `:dracula` is the public name.  By convention themes
that mimic an existing palette use the `-ish` suffix
(`:nordish`, `:gruvboxish`) to flag that they're an inspiration
not a literal port; pure originals use their own name.

### Convert from hex

Most palettes you'll find online use `#RRGGBB`.  Divide each pair
by 255 to get the 0.0-1.0 doubles Lispbar wants:

```
#1e1e2e   ->  (0.118 0.118 0.184)    ; alpha = 1.0
#cdd6f4   ->  (0.804 0.839 0.957)
#89b4fa   ->  (0.537 0.706 0.980)
```

The fourth element is alpha (transparency).  Use `1.0` for fully
opaque; lower values let the desktop show through.

A quick sh one-liner if you have hex strings handy:

```sh
hex2rgba() {
  s="${1#\#}"
  printf '%.3f %.3f %.3f 1.0\n' \
    "$(echo $((16#${s:0:2})) / 255 | bc -l)" \
    "$(echo $((16#${s:2:2})) / 255 | bc -l)" \
    "$(echo $((16#${s:4:2})) / 255 | bc -l)"
}
hex2rgba '#cba6f7'    # -> 0.796 0.651 0.969 1.0
```

### Partial themes

Every face you omit falls back to the active theme's `:normal`.
This means you can write a "tint" theme that only changes a couple
of faces:

```lisp
(define-theme :high-contrast
  :urgent '(1.0 0.0 0.0 1.0)
  :ok     '(0.0 1.0 0.0 1.0))
;; Everything else still uses the previously active theme.
```

Note that the bar's background uses `:bg`, so a partial theme that
omits `:bg` will inherit the previous theme's background.

### Choosing colours

Two practical rules:

1. **`:normal` should have enough contrast against `:bg`**.  WCAG AA
   needs ~4.5:1.  Online checkers help; eyeball-test by looking at
   the bar from across the room.
2. **`:urgent` should pop**.  A user glancing at the bar should
   recognise `BAT -5%` as a problem at any zoom level.  Avoid
   pastels for this face.

### Adaptive theme (planned)

There's an `:adaptive` keyword reserved for a future theme that
reads the user's GTK / Qt colour scheme.  Not yet implemented; for
now stick to one of the explicit themes.

### Switching themes at runtime

Apply via the function (without editing config):

```sh
lispbar --once   # any module's update will trigger the theme
```

There's no runtime "switch theme without restart" command yet.
Edit `(theme :X)` in `~/.config/lispbar/config.lisp` and restart.

## Built-in theme definitions

For reference, here are the in-binary palettes verbatim, in case
you want to copy one and adjust:

```lisp
(define-theme :default
  :bg     '(0.110 0.118 0.137 1.0)
  :normal '(0.880 0.880 0.880 1.0)
  :accent '(0.580 0.760 0.960 1.0)
  :ok     '(0.500 0.870 0.480 1.0)
  :warn   '(0.960 0.760 0.300 1.0)
  :urgent '(0.960 0.420 0.420 1.0)
  :muted  '(0.560 0.580 0.620 1.0))

(define-theme :minimal
  :bg     '(0.0 0.0 0.0 1.0)
  :normal '(1.0 1.0 1.0 1.0)
  :accent '(1.0 1.0 1.0 1.0)
  :ok     '(1.0 1.0 1.0 1.0)
  :warn   '(1.0 1.0 1.0 1.0)
  :urgent '(1.0 1.0 1.0 1.0)
  :muted  '(0.7 0.7 0.7 1.0))

(define-theme :nordish
  :bg     '(0.180 0.204 0.251 1.0)
  :normal '(0.847 0.871 0.914 1.0)
  :accent '(0.533 0.753 0.816 1.0)
  :ok     '(0.639 0.745 0.549 1.0)
  :warn   '(0.922 0.796 0.545 1.0)
  :urgent '(0.749 0.380 0.416 1.0)
  :muted  '(0.380 0.420 0.490 1.0))

(define-theme :gruvboxish
  :bg     '(0.157 0.157 0.157 1.0)
  :normal '(0.922 0.859 0.698 1.0)
  :accent '(0.980 0.741 0.184 1.0)
  :ok     '(0.722 0.733 0.149 1.0)
  :warn   '(0.996 0.502 0.098 1.0)
  :urgent '(0.984 0.286 0.204 1.0)
  :muted  '(0.486 0.435 0.392 1.0))

(define-theme :catppuccinish
  :bg     '(0.118 0.118 0.184 1.0)
  :normal '(0.804 0.839 0.957 1.0)
  :accent '(0.537 0.706 0.980 1.0)
  :ok     '(0.651 0.890 0.631 1.0)
  :warn   '(0.976 0.886 0.686 1.0)
  :urgent '(0.953 0.545 0.659 1.0)
  :muted  '(0.424 0.439 0.525 1.0))

(define-theme :doomish
  :bg     '(0.157 0.173 0.204 1.0)
  :normal '(0.733 0.761 0.812 1.0)
  :accent '(0.318 0.686 0.937 1.0)
  :ok     '(0.596 0.745 0.396 1.0)
  :warn   '(0.925 0.745 0.482 1.0)
  :urgent '(1.000 0.424 0.420 1.0)
  :muted  '(0.357 0.384 0.408 1.0))
```
