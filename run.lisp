(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cffi"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :lapis))

(lapis:init)
(lapis:set-video-mode 1024 768 0 1)

(defparameter *gs* (lapis::make-gamestate :objects nil))
(lapis::set-gamestate *gs*)

(defun render1 (obj i)
  (lapis:fill-rect 0.0 0.0 (* 100 (random 1.0)) 100.0 0.0 (random 1.0) 0.9))

(defun update1 (obj tick)
  )

(push (lapis::make-game-object :render-func #'render1
                               :update-func #'update1)
      (lapis::gamestate-objects *gs*))

(lapis:mainloop)
(quit)
