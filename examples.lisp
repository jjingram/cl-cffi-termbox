;;; TODO: add more examples.
(defpackage #:termbox-examples
  (:use #:cl)
  (:export example-1))

(in-package #:termbox-examples)

(defun example-1 ()
  (tb:with-termbox
    (let ((w (tb:width))
          (h (tb:height))
          (cells (loop for x across "Hello, World!"
                       collect (make-instance 'tb:cell
                                              :ch x
                                              :fg tb:+default+
                                              :bg tb:+default+))))
      (tb:clear)
      (tb:set-cursor tb:+hide-cursor+ tb:+hide-cursor+)
      (tb:blit 0 cells)
      (tb:present)
      (tb:poll-event))))
