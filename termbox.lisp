(in-package #:tb)

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
(defconstant +key-mouse-wheel-down+ (- #xFFFF 27))

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
(defcstruct %cell
  (ch :uint32)
  (fg :uint16)
  (bg :uint16))

(defclass cell ()
  ((ch :accessor cell-ch :initarg :ch :initform #\space)
   (fg :accessor cell-fg :initarg :fg :initform +default+)
   (bg :accessor cell-bg :initarg :bg :initform +default+))
  (:documentation
    "A cell, single conceptual entity on the terminal screen. The terminal
screen is basically a 2d array of cells. It has the following fields:
 - 'ch' is a unicode character
 - 'fg' foreground color and attributes
 - 'bg' background color and attributes"))

(defconstant +event-key+ 1)
(defconstant +event-resize+ 2)
(defconstant +event-mouse+ 3)

;;; An event, single interaction from the user. The 'mod' and 'ch' fields are
;;; valid if 'type' is +event-key+. The 'w' and 'h' fields are valid if 'type'
;;; is +event-resize+. The 'x' and 'y' fields are valid if 'type' is
;;; +event-mouse+. The 'key' field is valid if 'type' is either +event-key+ or
;;; +event-mouse+. The fields 'key' and 'ch' are mutually exclusive; only one
;;; of them can be non-zero at a time.
(defcstruct %event
  (type :uint8)
  (mod :uint8) ; modifiers to either 'key' or 'ch' below
  (key :uint16) ; one of the 'key' constants
  (ch :uint32) ; unicode character
  (w :int32)
  (h :int32)
  (x :int32)
  (y :int32))

(defclass event ()
  ((type :accessor event-type :initarg :type)
   (mod :accessor event-mod :initarg :mod :documentation "modifiers to either
        'key' or 'ch'")
   (key :accessor event-key :initarg :key :documentation "one of the 'key'
        constants")
   (ch :accessor event-ch :initarg :ch :documentation "unicode character")
   (w :accessor event-w :initarg :w)
   (h :accessor event-h :initarg :h)
   (x :accessor event-x :initarg :x)
   (y :accessor event-y :initarg :y))
  (:documentation
    "An event, single interaction from the user. The 'mod' and 'ch' fields are
    valid if 'type' is +event-key+. The 'w' and 'h' fields are valid if 'type'
    is +event-resize+. The 'x' and 'y' fields are valid if 'type' is
    +event-mouse+. The 'key' field is valid if 'type' is either +event-key+ or
    +event-mouse+. The fields 'key' and 'ch' are mutually exclusive; only one
    of them can be non-zero at a time."))

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

(defcfun (present "tb_present") :void
  "Synchronizes the internal back buffer with the terminal.")

(defconstant +hide-cursor+ -1)

(defcfun (set-cursor "tb_set_cursor") :void
  "Sets the position of the cursor. Upper-left character is (0, 0). If you pass
+hide-cursor+ as both coordinates, then the cursor will be hidden. Cursor
is hidden by default."
  (cx :int)
  (cy :int))

(defcfun (%put-cell "tb_put_cell") :void
  "Changes cell's parameters in the internal back buffer at the specified
position."
  (x :int)
  (y :int)
  (cell (:pointer (:struct %cell))))

(defmethod put-cell (x y (c cell))
  (with-foreign-object (%c '(:pointer (:struct %cell)))
    (with-slots (ch fg bg) c
      (setf (foreign-slot-value %c '(:struct %cell) 'ch) (char-code ch)
            (foreign-slot-value %c '(:struct %cell) 'fg) fg
            (foreign-slot-value %c '(:struct %cell) 'bg) bg)
      (%put-cell x y %c))))

(defcfun (%change-cell "tb_change_cell") :void
  "Changes cell's parameters in the internal back buffer at the specified
position."
  (x :int)
  (y :int)
  (ch :uint32)
  (fg :uint16)
  (bg :uint16))

(defun change-cell (x y ch fg bg)
  "Changes cell's parameters in the internal back buffer at the specified
position."
  (%change-cell x y (char-code ch) fg bg))

(defcfun (%cell-buffer "tb_cell_buffer") (:pointer (:struct %cell))
  "Returns a pointer to internal cell back buffer. You can get its dimensions
using 'width' and 'height' functions. The pointer stays valid as long
as no 'clear' and 'present' calls are made. The buffer is one-dimensional
buffer containing lines of cells starting from the top.")

;;; TODO: copy memory over all at once rather than individual calls to
;;; 'memcpy'.
(defun blit (offset cells)
  "Copies the buffer from 'cells' at the specified position, assuming the
buffer is a two-dimensional array of size ('w' x 'h'), represented as a
one-dimensional buffer containing lines of cells starting from the top."
  (let ((w (width))
        (h (height))
        (cell-buffer (%cell-buffer)))
    (loop for i from offset below (min (+ offset (length cells)) (* w h))
          do (let ((c (elt cells (- i offset))))
               (with-slots (ch fg bg) c
                 (with-foreign-object (%c '(:struct %cell))
                   (setf (foreign-slot-value %c '(:struct %cell) 'ch) (char-code ch)
                         (foreign-slot-value %c '(:struct %cell) 'fg) fg
                         (foreign-slot-value %c '(:struct %cell) 'bg) bg)
                   (foreign-funcall "memcpy"
                                    :pointer (mem-aptr cell-buffer '(:struct %cell) i)
                                    :pointer %c
                                    :int (foreign-type-size '(:struct %cell))
                                    :void)))))))

(defconstant +input-current+ 0)
(defconstant +input-esc+ 1)
(defconstant +input-alt+ 2)
(defconstant +input-mouse+ 4)

(defcfun (select-input-mode "tb_select_input_mode") :int
  "Sets the termbox input mode. Termbox has two input modes:
1. Esc input mode.
   When ESC sequence is in the buffer and it doesn't match any known
   ESC sequence => ESC means +key-esc+.
2. Alt input mode.
   When ESC sequence is in the buffer and it doesn't match any known
   sequence => ESC enables +mod-alt+ modifier for the next keyboard event.

You can also apply +input-mouse+ via bitwise OR operation to either of the
modes (e.g. +input-esc+ | +input-mouse+). If none of the main two modes
were set, but the mouse mode was, +input-esc+ mode is used. If for some
reason you've decided to use (+input-esc+ | +input-alt+) combination, it
will behave as if only +input-esc+ was selected.

If 'mode' is +input-current+, it returns the current input mode.

Default termbox input mode is +input-esc+."
  (mode :int))

(defconstant +output-current+ 0)
(defconstant +output-normal+ 1)
(defconstant +output-256+ 2)
(defconstant +output-216+ 3)
(defconstant +output-grayscale+ 4)

(defcfun (select-output-mode "tb_select_output_mode") :int
  "Sets the termbox output mode. Termbox has three output options:
1. +output-normal+     => [1..8]
   This mode provides 8 different colors:
   black, red, green, yellow, blue, magenta, cyan, white
   Shortcut: +black+, +red+, ...
   Attributes: +bold+, +underline+, +reverse+

   Example usage:
       (change-cell x y #\@ (bit-ior +black+ +bold) TB_RED)

2. +output-256+        => [0..256]
   In this mode you can leverage the 256 terminal mode:
   0x00 - 0x07: the 8 colors as in +output-normal+
   0x08 - 0x0f: * | +bold+
   0x10 - 0xe7: 216 different colors
   0xe8 - 0xff: 24 different shades of grey

   Example usage:
       (change-cell x y #\@ 184 240)
       (change-cell x y #\@ #xb8 #xf0)

3. +output-216+        => [0..216]
   This mode supports the 3rd range of the 256 mode only.
   But you don't need to provide an offset.

4. +output-grayscale+  => [0..23]
   This mode supports the 4th range of the 256 mode only.
   But you dont need to provide an offset.

If 'mode' is +output-current+, it returns the current output mode.

Default termbox output mode is +output-normal+."
  (mode :int))

(defcfun (%peek-event "tb_peek_event") :int
  "Wait for an event up to 'timeout' milliseconds and fill the 'event'
structure with it, when the event is available. Returns the type of the
event (one of +event-*+ constants) or -1 if there was an error or 0 in case
there were no event during 'timeout' period."
  (event (:pointer (:struct %event)))
  (timeout :int))

(defun peek-event (timeout)
  (with-foreign-object (e '(:pointer (:struct %event)))
    (let ((ret (%peek-event e timeout)))
      (case ret
        (-1 (error "peek-event error"))
        (0 nil)
        (otherwise
          (with-foreign-slots ((type mod key ch w h x y)
                                e (:struct %event))
            (make-instance 'event :type type :mod mod :key key :ch ch :w w
                            :h h :x x :y y)))))))

(defcfun (%poll-event "tb_poll_event") :int
  "Wait for an event forever and fill the 'event' structure with it, when the
event is available. Returns the type of the event (one of +event-*+
constants) or -1 if there was an error."
  (event (:pointer (:struct %event))))

(defun poll-event ()
  (with-foreign-object (e '(:pointer (:struct %event)))
    (let ((ret (%poll-event e)))
      (case ret
        (-1 (error "poll-event error"))
        (otherwise
          (with-foreign-slots ((type mod key ch w h x y)
                                e (:struct %event))
            (make-instance 'event :type type :mod mod :key key :ch ch :w w
                           :h h :x x :y y)))))))

(defmacro with-termbox (&body body)
  "Initializes termbox and cleanly shuts termbox down while executing 'body'
in between."
  `(unwind-protect
       (let ((ret (init)))
         (case ret
           (+eunsupported-terminal+ (error "unsupported terminal"))
           (+efailed-to-open-tty+ (error "failed to open tty"))
           (+epipe-trap-error+ (error "pipe trap error"))
           (otherwise (progn ,@body))))
     (shutdown)))
