(in-package #:termbox)

(define-foreign-library libtermbox
  (:darwin "libtermbox.dylib")
  (:unix "libtermbox.so")
  (t (:default "libtermbox")))

(load-foreign-library libtermbox)

;;; Key constants. See also struct tb_event's key field.
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
