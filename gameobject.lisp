(in-package :lapis)

(export '(set-meta
          get-meta
          update-every))

(defstruct game-object
  (name)
  (render-func)
  (update-func)
  (message-handler-func)
  (messages)
  (meta (make-hash-table)))

(defun process-messages (obj)
  (declare (game-object obj))
  (loop for message in (game-object-messages obj) do
       (funcall (game-object-message-handler-func obj) message obj))
  (setf (game-object-messages obj) nil))

(defun set-meta (obj key val)
  (declare ((or game-object string) obj)
           (keyword key))
  (let ((o (find-object obj)))
    (setf (gethash key (game-object-meta o)) val)))
(defun get-meta (obj key)
  (declare ((or game-object string) obj)
           (keyword key))
  (let ((o (find-object obj)))
    (gethash key (game-object-meta o))))

(defmacro update-every ((value type obj tick key) &body body)
  (let ((last-run (gensym)))
    (ecase type
      (:ticks
       `(let ((,last-run (gethash ,key (game-object-meta ,obj))))
          (when (or (null ,last-run)
                    (>=  ,tick (+ ,last-run ,value)))
            (setf (gethash ,key (game-object-meta ,obj))
                  ,tick)
            ,@body))))))