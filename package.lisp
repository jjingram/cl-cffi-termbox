(defpackage #:termbox
  (:use #:cl #:cffi)
  (:export
    +key-f1+ +key-f2+ +key-f3+ +key-f4+ +key-f5+ +key-f6+ +key-f7+ +key-f8+
    +key-f9+ +key-f10+ +key-f11+ +key-f12+ +key-insert+ +key-delete+ +key-home+
    +key-end+ +key-pgup+ +key-pgdn+ +key-arrow-up+ +key-arrow-down+ +key-arrow-left+
    +key-arrow-right+ +key-mouse-left+ +key-mouse-right+ +key-mouse-middle+
    +key-mouse-release+ +key-mouse-wheel-up+ +key-mouse-wheel-down+

    +key-ctrl-tidle+ +key-ctrl-2+ +key-ctrl-a+ +key-ctrl-b+ +key-ctrl-c+ +key-ctrl-d+
    +key-ctrl-e+ +key-ctrl-f+ +key-ctrl-g+ +key-backspace+ +key-ctrl-h+ +key-tab+
    +key-ctrl-i+ +key-ctrl-j+ +key-ctrl-k+ +key-ctrl-l+ +key-enter+ +key-ctrl-m+
    +key-ctrl-n+ +key-ctrl-o+ +key-ctrl-p+ +key-ctrl-q+ +key-ctrl-r+ +key-ctrl-s+
    +key-ctrl-t+ +key-ctrl-u+ +key-ctrl-v+ +key-ctrl-w+ +key-ctrl-x+ +key-ctrl-y+
    +key-ctrl-z+ +key-esc+ +key-ctrl-lsq-bracket+ +key-ctrl-3+ +key-ctrl-4+
    +key-ctrl-backslash+ +key-ctrl-5+ +key-ctrl-rsq-bracket+ +key-ctrl-6+ +key-ctrl-7+
    +key-ctrl-slash+ +key-ctrl-underscore+ +key-space+ +key-backspace-2+ +key-ctrl-8+

    +mod-alt+ +mod-motion+

    +default+ +black+ +red+ +green+ +yellow+ +blue+ +magenta+ +cyan+ +white+

    +bold+ +underline+ +reverse+

    cell

    +event-key+ +event-resize+ +event-mouse+

    event

    +eunsupported-terminal+ +efailed-to-open-tty+ +epipe-trap-error+

    init init-file init-fd shutdown

    width height

    clear set-clear-attributes

    present

    +hide-cursor+

    set-cursor

    put-cell change-cell

    cell-buffer

    +input-current+ +input-esc+ +input-alt+ +input-mouse+

    select-input-mode

    +output-current+ +output-normal+ +output-256+ +output-216+ +output-grayscale+

    select-output-mode

    peek-event poll-event))
