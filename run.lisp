(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lapis-engine))

(lapis:init)

(lapis:set-gamestate (lapis:make-gamestate :name "play state"))
(lapis:new-game-object "obj 1" 
                       :message-handler-func
                       (lambda (message recv)
                         (destructuring-bind (command &rest args) (lapis::message-payload message)
                           (when (or (eq command :quit)
                                     (and (eq command :key)
                                          (= (car args) 27)))
                             (setf lapis::*mainloop-running* nil)))))

(lapis:make-broadcast-receiver "obj 1" "sdl-event")

(lapis:set-video-mode 1024 768 0 1)

(defun render1 (obj i)
  (lapis:fill-rect 0.0 0.0 (* 100 (random 1.0)) 100.0 0.0 (random 1.0) 0.9))

(defun update1 (obj tick)
  )

(lapis:new-game-object "render obj" :render-func #'render1 :update-func #'update1)


(lapis:mainloop)
(quit)