(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lapis-engine))

(defun send-message-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(lapis:send-message ,@(read stream t nil t)))
(set-dispatch-macro-character #\# #\m #'send-message-reader)
(defun get-meta-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(lapis:get-meta ,@(read stream t nil t)))
(set-dispatch-macro-character #\# #\g #'get-meta-reader)
(defun set-meta-reader (stream subchar arg)
  (declare (ignore subchar arg))
  `(lapis:set-meta ,@(read stream t nil t)))
(set-dispatch-macro-character #\# #\s #'set-meta-reader)

(defparameter *screen-width* 1024)
(defparameter *screen-height* 768)

(lapis:init)

(lapis:set-gamestate (lapis:make-gamestate :name "play state"))
(lapis:new-game-object "obj 1" 
                       :message-handler-func
                       (lambda (message recv)
                         (destructuring-bind (command &rest args) (lapis:message-payload message)
                           (when (eq command :mouse)
                             #m(:type "mouse move event" :payload `(:mouse-move-event ,(first args) ,(second args))))
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

#|
(defparameter *lines* nil)
(defparameter *quads* nil)
(defparameter *points* nil)
         
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

(defun make-quads ()
  (setf *quads*
        (loop for i below 100 collect
             (let ((x (* *screen-width* (random 1.0)))
                   (y (* *screen-height* (random 1.0))))
               (prog1
                   (list (list x y)
                         (list (* 100 (random 1.0)) (* 100 (random 1.0)))
                         (list (random 1.0) (random 1.0) (random 1.0))))))))


(defun make-points ()
  (setf *points*
        (loop for i below 10000 collect
             (let ((x (* *screen-width* (random 1.0)))
                   (y (* *screen-height* (random 1.0))))
               (prog1
                   (list (list x y)
                         (list (random 1.0) (random 1.0) (random 1.0))))))))


(make-lines)       
(make-quads)
(make-points)

(defun render1 (obj i)
  (lapis:draw-rect 0.0 0.0 (* (/ *screen-width* 10) (random 1.0)) 100.0 0.0 (random 1.0) 0.9)
  (loop for line in *lines* do
       (destructuring-bind ((sx sy) (ex ey) (sr sg sb) (er eg eb)) line
         (lapis:draw-line sx sy ex ey
                          sr sg sb
                          er eg eb)))
  (loop for quad in *quads* do
       (destructuring-bind ((sx sy) (w h) (r g b)) quad
         (lapis:draw-rect sx sy w h r g b)))
  (loop for point in *points* do
       (destructuring-bind ((x y) (r g b)) point
         (loop for i from (- x 2) below (+ x 3) do
              (lapis:draw-point i y r g b))
         (loop for i from (- y 2) below (+ y 3) do
              (lapis:draw-point x i r g b)))))

(defun update1 (obj)
  (lapis:update-every (60 :ticks obj tick :make-lines-update)
    (make-lines))
  (lapis:update-every (30 :ticks obj tick :make-quads-update)
    (make-quads))
  (lapis:update-every (15 :ticks obj tick :make-points-update)
    (make-points)))

(lapis:new-game-object "render obj" :render-func #'render1 :update-func #'update1)
|#

(lapis:new-game-object "test obj"
                       :render-func #'(lambda (obj int)
                                        (lapis-ffi:gl-begin lapis-ffi::+gl-quads+)
                                        (lapis:draw-rect (lapis:get-meta obj :x)
                                                         (lapis:get-meta obj :y)
                                                         32.0 32.0
                                                         0.0 0.0 1.0)
                                        (lapis-ffi:gl-end))
                       :message-handler-func 
                       #'(lambda (message recv)
                           (let ((pay (lapis:message-payload message)))
                             (when (eq (first pay) :mouse-move-event)
                               (destructuring-bind (x y) (cdr pay)
                                 (when (and (>= x #g(recv :x))
                                            (< x (+ #g(recv :x) 32))
                                            (>= y #g(recv :y))
                                            (< y (+ #g(recv :y) 32)))
                                   #m(:receiver "highlighter"
                                                :type :change-pos
                                                :payload (list #g(recv :x) #g(recv :y) 32 32))
                                   #m(:receiver "highlighter"
                                                :type :toggle
                                                :payload t)))))))

(loop with rd = 30 for xr below 10 do
     (lapis:new-game-object (symbol-name (gensym))
                            :render-func #'(lambda (obj int)
                                             (lapis-ffi:gl-begin lapis-ffi::+gl-lines+)
                                             (let ((x (* 3 xr (/ rd 2.0))) (y 100) (r 0.0) (g 0.0) (b 1.0))
                                               (lapis:draw-line (+ x rd) y (+ x (* rd 0.5)) (+ y (* rd (sqrt 3) 0.5)) r g b r g b)
                                               (lapis:draw-line (+ x (* rd 0.5)) (+ y (* rd (sqrt 3) 0.5)) (- x (* rd 0.5)) (+ y (* rd (sqrt 3) 0.5)) r g b r g b)
                                               (lapis:draw-line (- x (* rd 0.5)) (+ y (* rd (sqrt 3) 0.5)) (- x rd) y r g b r g b)
                                               (lapis:draw-line (- x rd) y (- x (* rd 0.5)) (- y (* rd (sqrt 3) 0.5)) r g b r g b)
                                               (lapis:draw-line (- x (* rd 0.5)) (- y (* rd (sqrt 3) 0.5)) (+ x (* rd 0.5)) (- y (* rd (sqrt 3) 0.5)) r g b r g b)
                                               (lapis:draw-line (+ x (* rd 0.5)) (- y (* rd (sqrt 3) 0.5)) (+ x rd) y r g b r g b))
                                             
                                             (lapis-ffi:gl-end))))

#s("test obj" :x 150.0)
#s("test obj" :y 150.0)

(lapis:make-broadcast-receiver "test obj" "mouse move event")

(lapis:new-game-object
 "highlighter"
 :render-func #'(lambda (obj int)
                  (when (lapis:get-meta obj :render)
                    (let ((x #g(obj :x))
                          (y #g(obj :y))
                          (w #g(obj :w))
                          (h #g(obj :h))
                          (r 1.0) (g 0.0) (b 0.0))
                      (lapis-ffi:gl-begin lapis-ffi::+gl-lines+)
                      (lapis:draw-line x y (+ w x) y r g b r g b)
                      (lapis:draw-line (+ w x) y (+ w x) (+ h y) r g b r g b)
                      (lapis:draw-line (+ w x) (+ h y) x (+ h y) r g b r g b)
                      (lapis:draw-line x (+ h y) x y r g b r g b)
                      (lapis-ffi:gl-end))))

 :update-func #'(lambda (obj)                  
                  (let ((lct #g(obj :last-change-tick)))
                    (when (and lct
                               (> (- lapis:*game-tick* lct) 30))
                      #s(obj :render nil))))        
                  
 :message-handler-func
 #'(lambda (message recv)
     (let ((type (lapis:message-type message))
           (pay (lapis:message-payload message)))
       (ecase type
         (:change-pos
          (destructuring-bind (x y w h) pay
            #s(recv :x x)
            #s(recv :y y)
            #s(recv :w w)
            #s(recv :h h))
          #s(recv :render t))
         (:toggle
          (let ((val pay))
            #s(recv :render val)
            #s(recv :last-change-tick lapis:*game-tick*)))))))
                    

(lapis:mainloop)
(quit)