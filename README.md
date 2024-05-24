# g`⣼⢠⡄⢤⠄⡤⢠⢀⡤`

gdotris is a clone of the popular Tetris clone,
[dotris](https://github.com/trevarj/dotris), written by me for the Guile Scheme
programming language for the [Spring Lisp Game Jam
2024](https://itch.io/jam/spring-lisp-game-jam-2024).

At the time of writing this, this is my largest Guile program. The goal of this
project was to explore Guile Scheme and learn about the ecosystem around
it, in hopes that I will be able to actually write something useful in it one
day.

This version will be full of bugs and not be as feature complete or efficient (I
already notice a small performance difference compared to the C version), so if
you really like the game and want to have it installed on your computer, please
see the original version.

## Requirements
- A terminal emulator. For the best experience, use one that overrides the
  font's braille characters, like Kitty, and also can enlarge the font size,
  since the braille dots are tiny.
- Guile 3.0.9
- [guile-ncurses](https://www.gnu.org/software/guile-ncurses/)

## Installation

### Arch
```sh
$ sudo pacman -S guile guile-ncurses
```

### Guix
TBA

## How To Play

### Controls

Key                  | Description
---                  | ---
`Left Arrow` or `a`  | Move left
`Right Arrow`or `d`  | Move right
`Up Arrow` or `w`    | Rotate clockwise
`Down Arrow`or `s`   | Soft drop (move down)
`Spacebar`           | Hard drop
`C`                  | Hold Tetrimino / Restore Held
`Q`                  | Quit game
`R`                  | Restart game
