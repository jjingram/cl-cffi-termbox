(asdf:defsystem #:termbox
  :description "Common Lisp CFFI bindings to termbox"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "MIT"
  :version "0.0.2"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "termbox")))

(asdf:defsystem #:termbox/examples
  :description "Examples of using cl-cffi-termbox"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "Public Domain"
  :version "0.0.1"
  :depends-on (#:termbox)
  :serial t
  :components ((:file "examples")))
