(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp")
  (require :lapis-engine))

(lapis:init)

(lapis:set-gamestate (lapis:make-gamestate :name "play state"))

(lapis:new-game-object "obj 1" 
                       :message-handler-func
                       (lambda (message recv)
                         (destructuring-bind (command &rest args) (message-payload message)
                           (declare (ignore args))
                           (if (eq command 'quit)
                               (setf *mainloop-running* nil)))))

(lapis:make-broadcast-receiver "obj 1" "sdl-event")


(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
  (lapis:set-video-mode 1024 768 0 1))

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
;;(lapis:end)
