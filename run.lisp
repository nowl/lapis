(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lapis-engine))

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)

(lapis:init)

(lapis:set-gamestate (lapis:make-gamestate :name "play state"))
(lapis:new-game-object "obj 1" 
                       :message-handler-func
                       (lambda (message recv)
                         (destructuring-bind (command &rest args) (lapis:message-payload message)
                           (when (eq command :resize)
                             (setf *screen-width* (first args)
                                   *screen-height* (second args))
                             (lapis:set-video-mode *screen-width* *screen-height* 0 1))
                           (when (or (eq command :quit)
                                     (and (eq command :key)
                                          (= (car args) 27)))
                             (setf lapis:*mainloop-running* nil)))))

(lapis:make-broadcast-receiver "obj 1" "sdl-event")

(lapis:set-video-mode *screen-width* *screen-height* 0 1)

(lapis:enable-smooth-lines)

(lapis:set-ticks-per-second 30)


(defparameter *lines* nil)
         
(defun make-lines ()
  (setf *lines*
        (cdr
         (loop with prev-x with prev-y for i below 100 collect
              (let ((x (* *screen-width* (random 1.0)))
                    (y (* *screen-height* (random 1.0))))
                (prog1
                    (when (and prev-x prev-y)
                      (list (list prev-x prev-y)
                            (list x y)
                            (list (random 1.0) (random 1.0) (random 1.0))
                            (list (random 1.0) (random 1.0) (random 1.0))))
                  (setf prev-x x
                        prev-y y))))))) 

(make-lines)       

(defun render1 (obj i)
  (lapis:fill-rect 0.0 0.0 (* 100 (random 1.0)) 100.0 0.0 (random 1.0) 0.9)
  (loop for line in *lines* do
       (destructuring-bind ((sx sy) (ex ey) (sr sg sb) (er eg eb)) line
         (lapis:draw-line sx sy ex ey
                          sr sg sb
                          er eg eb))))

(defun update1 (obj tick)
  (when (= 0 (mod tick (* 30 2)))
    (make-lines)))

(lapis:new-game-object "render obj" :render-func #'render1 :update-func #'update1)


(lapis:mainloop)
(quit)