(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "~/quicklisp/setup.lisp"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :lapis-engine))

;;(ql:quickload "swank")
;;(swank:create-server :port 4005 :style :spawn :dont-close t)

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

(lapis:new-game-object "quit listener"
                       :message-handler-func
                       (lambda (message recv)
                         (let ((pl (lapis:message-payload message)))
                           (when (or (equal (lapis:message-type message) "quit event")
                                     (and (equal (lapis:message-type message) "key event")
                                          (= (car pl) 27)))
                             (setf lapis:*mainloop-running* nil)))))

(lapis:make-broadcast-receiver "quit listener" "key event")
(lapis:make-broadcast-receiver "quit listener" "quit event")

(lapis:new-game-object "resizer"
                       :message-handler-func
                       (lambda (message recv)
                         (let ((pl (lapis:message-payload message)))
                           (setf *screen-width* (first pl)
                                 *screen-height* (second pl))
                           (lapis:set-video-mode *screen-width* *screen-height* 0 1))))

(lapis:make-broadcast-receiver "resizer" "resize event")

(lapis:new-game-object "sdl-event dispatcher" 
                       :message-handler-func
                       (lambda (message recv)
                         (destructuring-bind (command &rest args) (lapis:message-payload message)
                           (cond
                             ((eq command :mouse-motion) #m(:type "mouse move event" :payload args))
                             ((eq command :mouse-button) #m(:type "mouse button event" :payload args))
                             ((eq command :resize) #m(:type "resize event" :payload args))                              
                             ((eq command :quit) #m(:type "quit event"))
                             ((eq command :key) #m(:type "key event" :payload args))))))

(lapis:make-broadcast-receiver "sdl-event dispatcher" "sdl-event")

(lapis:set-video-mode *screen-width* *screen-height* 0 1)

(lapis:enable-smooth-lines)

(lapis:set-ticks-per-second 60)

(defparameter *mousex* 0)
(defparameter *mousey* 0)
(defparameter *mouse-left-state* :up)
(defparameter *dragging-position* (list 0 0))

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
                           (let ((pl (lapis:message-payload message))
                                 (mt (lapis:message-type message)))
                             (when (equal mt "mouse move event")
                               (destructuring-bind (x y) pl
                                 (setf *mousex* x
                                       *mousey* y)
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

(defun hex-render (x-pos x-offset y-pos y-offset radius r g b)
  (lapis-ffi:gl-begin lapis-ffi::+gl-lines+)
  (let ((x (+ x-offset (* 3 x-pos (/ radius 2.0))))
        (y (+ (* y-pos radius (sqrt 3))
              y-offset
              (if (= 0 (mod x-pos 2))
                  (* (sqrt 3) radius 0.5)
                  0)))
        (r 0.0) (g 0.0) (b 1.0))
    (lapis:draw-line (+ x radius) y (+ x (* radius 0.5)) (+ y (* radius (sqrt 3) 0.5)) r g b r g b)
    (lapis:draw-line (+ x (* radius 0.5)) (+ y (* radius (sqrt 3) 0.5))
                     (- x (* radius 0.5)) (+ y (* radius (sqrt 3) 0.5)) r g b r g b)
    (lapis:draw-line (- x (* radius 0.5)) (+ y (* radius (sqrt 3) 0.5)) (- x radius) y r g b r g b)
    (lapis:draw-line (- x radius) y (- x (* radius 0.5)) (- y (* radius (sqrt 3) 0.5)) r g b r g b)
    (lapis:draw-line (- x (* radius 0.5)) (- y (* radius (sqrt 3) 0.5))
                     (+ x (* radius 0.5)) (- y (* radius (sqrt 3) 0.5)) r g b r g b)
    (lapis:draw-line (+ x (* radius 0.5)) (- y (* radius (sqrt 3) 0.5)) (+ x radius) y r g b r g b))
  
  (lapis-ffi:gl-end))

(loop with rd = 50 for xrt fixnum below 10 do
     (loop for yrt fixnum below 10 do
          (let ((xr xrt)
                (yr yrt))
            (let ((obj
                   (lapis:new-game-object (symbol-name (gensym "hex_"))                            
                                          :render-func #'(lambda (obj int)
                                                           (hex-render xr #g(obj :map-scroll-x)
                                                                       yr #g(obj :map-scroll-y)
                                                                       rd 0.0 0.0 1.0))
                                          :message-handler-func #'(lambda (message recv)
                                                                    (let ((pl (lapis:message-payload message))
                                                                          (mt (lapis:message-type message)))
                                                                      (cond
                                                                        ((and (equal mt "mouse move event")
                                                                              #g(recv :dragging))
                                                                         #s(recv :map-scroll-x (+ #g(recv :prev-scroll-x) (- (first pl) #g(recv :click-x))))
                                                                         #s(recv :map-scroll-y (+ #g(recv :prev-scroll-y) (- (second pl) #g(recv :click-y)))))
                                                                        ((and (equal mt "mouse button event")
                                                                              (= (first pl) 1)
                                                                              (= (fourth pl) 1))
                                                                         #s(recv :dragging t)
                                                                         #s(recv :click-x (second pl))
                                                                         #s(recv :click-y (third pl)))
                                                                        ((and (equal mt "mouse button event")
                                                                              (= (first pl) 0)
                                                                              (= (fourth pl) 1))
                                                                         #s(recv :prev-scroll-x (+ #g(recv :prev-scroll-x) (- (second pl) #g(recv :click-x))))
                                                                         #s(recv :prev-scroll-y (+ #g(recv :prev-scroll-y) (- (third pl) #g(recv :click-y))))
                                                                         #s(recv :dragging nil))))))))                                                                         

              (lapis:make-broadcast-receiver obj "mouse move event")
              (lapis:make-broadcast-receiver obj "mouse button event")

              #s(obj :dragging nil)
              #s(obj :click-x 0)
              #s(obj :click-y 0)
              #s(obj :prev-scroll-x 0)
              #s(obj :prev-scroll-y 0)
              #s(obj :map-scroll-x 0)
              #s(obj :map-scroll-y 0)))))

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
