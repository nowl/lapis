(in-package :lapis)

(defparameter *ticks-per-second* 15)
(defparameter *time-per-tick* (/ 1000.0 *ticks-per-second*))
(defparameter *max-frame-skip* 5)
(defparameter *mainloop-running* t)
(defparameter *gamestate* nil)

(defun set-ticks-per-second (num)
  (setf *ticks-per-second* num
        *time-per-tick* (/ 1000.0 *ticks-per-second*)))

(defun set-max-frame-skip (num)
  (setf *max-frame-skip* num))

(defun set-gamestate (gamestate)
  (setf *gamestate* gamestate))

(defun handle-events ()
  (with-foreign-object (event 'sdl-event)
    (when (= (poll-event event) 1)
      
      (when (sdl-event-mouse-motionp event)
        (multiple-value-bind (x y) (sdl-event-mouse-motion event)
          (format t "mouse ~d,~d~%" x y)))
      
      (when (sdl-event-quitp event)
        (setf *mainloop-running* nil))
      (when (sdl-event-keyp event)
        (let ((key (sdl-event-key event)))
          (if (= key 27)
              (setf *mainloop-running* nil)
              (format t "result ~d~%" key)))))))

(defun update (game-tick)
  (when *gamestate*
    (loop for obj in (gamestate-objects *gamestate*) do
         (funcall (game-object-update-func obj) obj game-tick))))

(defun render (interpolation)
  (when *gamestate*
    (prepare-render)
    (loop for obj in (gamestate-objects *gamestate*) do
         (funcall (game-object-render-func obj) obj interpolation))
    (flip)))

(defun mainloop ()
  (let ((next-game-tick (get-tick))
        (fps-counter 0)
        (game-tick 0)
        (fps-start-time (get-tick)))

    (loop while *mainloop-running* do
        
         (handle-events)

         (let ((loops 0)
               (tick (get-tick)))
           (loop while (and (> tick next-game-tick)
                            (< loops *max-frame-skip*)) do
                (incf game-tick)
                (update game-tick)
                (incf next-game-tick *time-per-tick*)
                (incf loops)
                (setf tick (get-tick)))

           (let ((interpolation
                  (/ (+ tick *time-per-tick* (- next-game-tick)) *time-per-tick*)))
             (render interpolation))
         
           (incf fps-counter)           
           #+debug
           (if (> (- (get-tick) fps-start-time) 1000)
               (setf fps-counter 0
                     fps-start-time (get-tick))
               (format t "fps = ~d~%" fps-counter))))))
