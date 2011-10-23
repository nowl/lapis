(in-package :lapis)

(export '(set-max-frame-skip
          set-ticks-per-second
          mainloop
          set-gamestate
          *mainloop-running*
          *game-tick*))

(defparameter *ticks-per-second* 15)
(defparameter *time-per-tick* (/ 1000.0 *ticks-per-second*))
(defparameter *max-frame-skip* 5)
(defparameter *mainloop-running* t)

(defparameter *game-tick* 0)

(defun set-ticks-per-second (num)
  (setf *ticks-per-second* num
        *time-per-tick* (/ 1000.0 *ticks-per-second*)))

(defun set-max-frame-skip (num)
  (setf *max-frame-skip* num))

(defun set-gamestate (gamestate)
  (setf *gamestate* gamestate))

(defun handle-events ()
  (with-foreign-object (event 'sdl-event)
    (loop until (/= (poll-event event) 1) do
         (when (sdl-event-mouse-motionp event)
           (multiple-value-bind (x y) (sdl-event-mouse-motion event)
             (send-message :type "sdl-event" :payload `(:mouse-motion ,x ,y))))
         (when (sdl-event-mouse-buttonp event)
           (multiple-value-bind (type x y button) (sdl-event-mouse-button event)
             (send-message :type "sdl-event" :payload `(:mouse-button , type ,x ,y ,button))))
         (when (sdl-event-quitp event)
           (send-message :type "sdl-event" :payload '(:quit)))
         (when (sdl-event-resizep event)
           (multiple-value-bind (width height) (sdl-event-resize event)
             (send-message :type "sdl-event" :payload `(:resize ,width ,height))))
         (when (sdl-event-keyp event)
           (let ((key (sdl-event-key event)))
             (send-message :type "sdl-event" :payload `(:key ,key)))))))

(defun update ()
  (when *gamestate*
    (loop for obj being the hash-values of (gamestate-objects-by-name *gamestate*) do
         (process-messages obj)
         (when (game-object-update-func obj)
           (funcall (game-object-update-func obj) obj)))))

(defun render (interpolation)
  (when *gamestate*
    (prepare-render)
    (loop for obj being the hash-values of (gamestate-objects-by-name *gamestate*) do
         (when (game-object-render-func obj)
           (funcall (game-object-render-func obj) obj interpolation)))
    (flip)))

(defun mainloop ()
  (let ((next-game-tick (get-tick))
        (fps-counter 0)
        (fps-start-time (get-tick)))

    (loop while *mainloop-running* do
        
         (handle-events)

         (let ((loops 0)
               (tick (get-tick)))
           (loop while (and (> tick next-game-tick)
                            (< loops *max-frame-skip*)) do
                (incf *game-tick*)
                (update)
                (incf next-game-tick *time-per-tick*)
                (incf loops)
                (setf tick (get-tick)))

           (let ((interpolation
                  (/ (+ tick *time-per-tick* (- next-game-tick)) *time-per-tick*)))
             (render interpolation))
         
           (incf fps-counter)           
           #-debug
           (when (> (- (get-tick) fps-start-time) 1000)
             (format t "fps = ~d~%" fps-counter)
             (setf fps-counter 0
                   fps-start-time (get-tick)))))))
