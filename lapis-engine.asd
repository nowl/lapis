;;; -*- lisp -*-

(defsystem lapis-engine
  :description "Light-weight core lapis routines."
  :long-description "Small wrapper around SDL and OpenGL for use with common lisp."
  :version "0.1"
  :author "John Process <esologic@gmail.com>"
  :licence "GNU Public License"
  :depends-on ("cffi")
  :components
  ((:file "lapis")
   (:file "package")
   (:file "gameobject")
   (:file "gamestate")
   (:file "mainloop"))
  :serial t)
  
