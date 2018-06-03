(asdf:defsystem #:termbox
  :description "Common Lisp CFFI bindings to termbox"
  :author "Jarrod Jeffrey Ingram <jarrod.jeffi@gmail.com>"
  :license "MIT"
  :version "0.0.2"
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "termbox")
               (:file "example")))
