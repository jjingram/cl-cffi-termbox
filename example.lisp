(in-package #:tb.example)

(defun run ()
  (tb:with-termbox
    (let ((cell (make-instance 'tb:cell :ch #\@ :fg tb:+default+ :bg tb:+default+)))
      (tb:clear)
      (tb:set-cursor 0 0)
      (tb:put-cell 1 1 cell)
      (tb:set-cursor 0 0)
      (tb:present)
      (tb:poll-event))))
