(in-package #:termbox)

(define-foreign-library libtermbox
  (:darwin "libtermbox.dylib")
  (:unix "libtermbox.so")
  (t (:default "libtermbox")))

(use-foreign-library libtermbox)

;;; Key constants. See also struct event's key field.
;;;
;;; These are a safe subset of terminfo keys, which exist on all popular
;;; terminals. Termbox uses only them to stay truly portable.
(defconstant +key-f1+ (- #xFFFF 0))
(defconstant +key-f2+ (- #xFFFF 1))
(defconstant +key-f3+ (- #xFFFF 2))
(defconstant +key-f4+ (- #xFFFF 3))
(defconstant +key-f5+ (- #xFFFF 4))
(defconstant +key-f6+ (- #xFFFF 5))
(defconstant +key-f7+ (- #xFFFF 6))
(defconstant +key-f8+ (- #xFFFF 7))
(defconstant +key-f9+ (- #xFFFF 8))
(defconstant +key-f10+ (- #xFFFF 9))
(defconstant +key-f11+ (- #xFFFF 10))
(defconstant +key-f12+ (- #xFFFF 11))
(defconstant +key-insert+ (- #xFFFF 12))
(defconstant +key-delete+ (- #xFFFF 13))
(defconstant +key-home+ (- #xFFFF 14))
(defconstant +key-end+ (- #xFFFF 15))
(defconstant +key-pgup+ (- #xFFFF 16))
(defconstant +key-pgdn+ (- #xFFFF 17))
(defconstant +key-arrow-up+ (- #xFFFF 18))
(defconstant +key-arrow-down+ (- #xFFFF 19))
(defconstant +key-arrow-left+ (- #xFFFF 20))
(defconstant +key-arrow-right+ (- #xFFFF 21))
(defconstant +key-mouse-left+ (- #xFFFF 22))
(defconstant +key-mouse-right+ (- #xFFFF 23))
(defconstant +key-mouse-middle+ (- #xFFFF 24))
(defconstant +key-mouse-release+ (- #xFFFF 25))
(defconstant +key-mouse-wheel-up+ (- #xFFFF 26))
(defconstant +key-mouse-wheel-down (- #xFFFF 27))

;;; These are all ASCII code points below SPACE character and a BACKSPACE key.
(defconstant +key-ctrl-tidle+ #x00)
(defconstant +key-ctrl-2+ #x00 "clash with 'ctrl-tilde'")
(defconstant +key-ctrl-a+ #x01)
(defconstant +key-ctrl-b+ #x02)
(defconstant +key-ctrl-c+ #x03)
(defconstant +key-ctrl-d+ #x04)
(defconstant +key-ctrl-e+ #x05)
(defconstant +key-ctrl-f+ #x06)
(defconstant +key-ctrl-g+ #x07)
(defconstant +key-backspace+ #x08)
(defconstant +key-ctrl-h+ #x08 "clash with 'backspace'")
(defconstant +key-tab+ #x09)
(defconstant +key-ctrl-i+ #x09 "clash with 'tab'")
(defconstant +key-ctrl-j+ #x0A)
(defconstant +key-ctrl-k+ #x0B)
(defconstant +key-ctrl-l+ #x0C)
(defconstant +key-enter+ #x0D)
(defconstant +key-ctrl-m+ #x0D "clash with 'enter'")
(defconstant +key-ctrl-n+ #x0E)
(defconstant +key-ctrl-o+ #x0F)
(defconstant +key-ctrl-p+ #x10)
(defconstant +key-ctrl-q+ #x11)
(defconstant +key-ctrl-r+ #x12)
(defconstant +key-ctrl-s+ #x13)
(defconstant +key-ctrl-t+ #x14)
(defconstant +key-ctrl-u+ #x15)
(defconstant +key-ctrl-v+ #x16)
(defconstant +key-ctrl-w+ #x17)
(defconstant +key-ctrl-x+ #x18)
(defconstant +key-ctrl-y+ #x19)
(defconstant +key-ctrl-z+ #x1A)
(defconstant +key-esc+ #x1B)
(defconstant +key-ctrl-lsq-bracket+ #x1B "clash with 'escape'")
(defconstant +key-ctrl-3+ #x1B "clash with 'escape'")
(defconstant +key-ctrl-4+ #x1C)
(defconstant +key-ctrl-backslash+ #x1C "clash with 'ctrl-4'")
(defconstant +key-ctrl-5+ #x1D)
(defconstant +key-ctrl-rsq-bracket+ #x1D "clash with 'ctrl-5'")
(defconstant +key-ctrl-6+ #x1E)
(defconstant +key-ctrl-7+ #x1F)
(defconstant +key-ctrl-slash+ #x1F "clash with 'ctrl-7'")
(defconstant +key-ctrl-underscore+ #x1F "clash with 'ctrl-7'")
(defconstant +key-space+ #x20)
(defconstant +key-backspace-2+ #x7F)
(defconstant +key-ctrl-8+ #x7F "clash with 'backspace-2'")

;;; There are non-existing ones.
;;;
;;; +key-ctrl-1+ clash with '1'
;;; +key-ctrl-9+ clash with '9'
;;; +key-ctrl-0+ clash with '0'

;;; Alt modifier constant, see tb_event.mod field and tb_select_input_mode function.
;;; Mouse motion modifier.
(defconstant +mod-alt+ #x01)
(defconstant +mod-motion+ #x02)

;;; Colors (see struct cell's fg and bg fields).
(defconstant +default+ #x00)
(defconstant +black+ #x01)
(defconstant +red+ #x02)
(defconstant +green+ #x03)
(defconstant +yellow+ #x04)
(defconstant +blue+ #x05)
(defconstant +magenta+ #x06)
(defconstant +cyan+ #x07)
(defconstant +white+ #x08)

;;; Attributes, it is possible to use multiple attributes by combining them
;;; using bitwise OR ('|'). Although, colors cannot be combined. But you can
;;; combine attributes and a single color. See also struct tb_cell's fg and bg
;;; fields.
(defconstant +bold+ #x0100)
(defconstant +underline+ #x0200)
(defconstant +reverse+ #x0400)

;;; A cell, single conceptual entity on the terminal screen. The terminal
;;; screen is basically a 2d array of cells. It has the following fields:
;;;  - 'ch' is a unicode character
;;;  - 'fg' foreground color and attributes
;;;  - 'bg' background color and attributes
(defcstruct cell
  (ch :uint32)
  (fg :uint16)
  (bg :uint16))

(defconstant +event-key+ 1)
(defconstant +event-resize+ 2)
(defconstant +event-mouse+ 3)

;;; An event, single interaction from the user. The 'mod' and 'ch' fields are
;;; valid if 'type' is +event-key+. The 'w' and 'h' fields are valid if 'type'
;;; is +event-resize+. The 'x' and 'y' fields are valid if 'type' is
;;; +event-mouse+. The 'key' field is valid if 'type' is either +event-key+ or
;;; +event-mouse+. The fields 'key' and 'ch' are mutually exclusive; only one
;;; of them can be non-zero at a time.
(defcstruct event
  (type :uint8)
  (mod :uint8) ; modifiers to either 'key' or 'ch' below
  (key :uint16) ; one of the 'key' constants
  (ch :uint32) ; unicode character
  (w :int32)
  (h :int32)
  (x :int32)
  (y :int32))

(defconstant +eunsupported-terminal+ -1)
(defconstant +efailed-to-open-tty+ -2)
(defconstant +epipe-trap-error+ -3)

(defcfun (init "tb_init") :int
  "Initializes the termbox library. This function should be called before any
other functions. Function 'init' is same as '(init-file \"/dev/tty\")' After
successful initialization, the library must be finalized using the 'shutdown'
function.")

(defcfun (init-file "tb_init_file") :int
  (name (:pointer :char)))

(defcfun (init-fd "tb_init_fd") :int
  (inout :int))

(defcfun (shutdown "tb_shutdown") :void)

(defcfun (width "tb_width") :int
  "Returns the size of the internal back buffer (which is the same as
terminal's window size in characters). The internal buffer can be resized
after 'clear' or 'present' function calls. 'width' has an unspecified
negative value when called before 'init' or after 'shutdown'.")

(defcfun (height "tb_height") :int
  "Returns the size of the internal back buffer (which is the same as
terminal's window size in characters). The internal buffer can be resized
after 'clear' or 'present' function calls. 'height' has an unspecified
negative value when called before 'init' or after 'shutdown'.")

(defcfun (clear "tb_clear") :void
  "Clears the internal back buffer using the +default+ color or the
color/attributes set by 'set-clear-attributes' function.")

(defcfun (set-clear-attributes "tb_set_clear_attributes") :void
  (fg :uint16)
  (bg :uint16))

(defconstant +hide-cursor+ -1)

(defcfun (set-cursor "tb_set_cursor") :void
  "Sets the position of the cursor. Upper-left character is (0, 0). If you pass
+hide-cursor+ as both coordinates, then the cursor will be hidden. Cursor
is hidden by default."
  (cx :int)
  (cy :int))

(defcfun (put-cell "tb_put_cell") :void
  "Changes cell's parameters in the internal back buffer at the specified
position."
  (x :int)
  (y :int)
  (cell (:pointer event)))

(defcfun (change-cell "tb_change_cell") :void
  "Changes cell's parameters in the internal back buffer at the specified
position."
  (x :int)
  (y :int)
  (ch :uint32)
  (fg :uint16)
  (bg :uint16))

(defcfun (cell-buffer "tb_cell_buffer") cell
  "Returns a pointer to internal cell back buffer. You can get its dimensions
using 'width' and 'height' functions. The pointer stays valid as long
as no 'clear' and 'present' calls are made. The buffer is one-dimensional
buffer containing lines of cells starting from the top.")
