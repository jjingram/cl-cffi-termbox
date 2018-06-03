(in-package #:tb.example)

(defun run ()
  (tb:with-termbox
    (let ((w (tb:width))
          (h (tb:height))
          (cell (make-instance 'tb:cell :ch #\@ :fg tb:+default+ :bg tb:+default+))
          (cells (loop for x across "Hello, World!"
                            collect (make-instance 'tb:cell :ch x :fg tb:+default+ :bg tb:+default+))))
      (tb:clear)
      (tb:set-cursor 0 0)
      (tb:put-cells 0 cells)
      (tb:put-cell (truncate w 2) (truncate h 2) cell)
      (tb:set-cursor tb:+hide-cursor+ tb:+hide-cursor+)
      (tb:present)
      (tb:poll-event))))
